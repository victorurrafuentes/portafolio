-- ============================================================================
-- 02_scoring_mensual.sql
-- Scoring mensual: aplica el modelo y deja la salida lista para el panel.
--
-- Script de referencia con nombres ficticios (proyecto-demo.recupero_demo).
-- En produccion corre una vez al mes sobre el ultimo cierre; aqui el mes se
-- indica con un indice relativo (mes_idx).
--
-- Salida: una fila por cuenta con su probabilidad de pago y dos veintiles
--   veintil_prob       ordena por probabilidad de pago
--   veintil_probdeuda  ordena por probabilidad de pago x monto castigado
-- (20 = mayor valor).
-- ============================================================================

DECLARE mes_scoring INT64 DEFAULT 16;  -- mes a puntuar


-- ----------------------------------------------------------------------------
-- 1. Validacion previa: el mes a puntuar debe existir en la tabla de entrada
-- ----------------------------------------------------------------------------
ASSERT EXISTS (
  SELECT 1
  FROM `proyecto-demo.recupero_demo.tablon_modelo`
  WHERE mes_idx = mes_scoring
) AS 'El mes a puntuar no existe en tablon_modelo';


-- ----------------------------------------------------------------------------
-- 2. Tabla de salida (se crea una sola vez)
--    Particionada por mes y agrupada por las columnas que usa el panel.
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS `proyecto-demo.recupero_demo.scoring_mensual` (
  id_cuenta          INT64   OPTIONS (description = 'Identificador de la cuenta'),
  mes_idx            INT64   OPTIONS (description = 'Mes de scoring (indice relativo)'),
  tramo              STRING  OPTIONS (description = 'RECIENTE o ANTIGUO'),
  prob_pago_90d      FLOAT64 OPTIONS (description = 'Probabilidad de pago a 90 dias'),
  monto_castigado    FLOAT64 OPTIONS (description = 'Deuda castigada de la cuenta'),
  veintil_prob       INT64   OPTIONS (description = 'Veintil de probabilidad (20 = mayor)'),
  veintil_probdeuda  INT64   OPTIONS (description = 'Veintil de probabilidad x deuda (20 = mayor)')
)
PARTITION BY RANGE_BUCKET(mes_idx, GENERATE_ARRAY(1, 121, 1))
CLUSTER BY tramo, veintil_prob
OPTIONS (description = 'Scoring mensual de cartera castigada (datos simulados)');


-- ----------------------------------------------------------------------------
-- 3. Scoring idempotente
--    Reemplaza el mes dentro de una transaccion: si algo falla, la tabla queda
--    como estaba y se puede volver a ejecutar sin duplicar filas.
-- ----------------------------------------------------------------------------
BEGIN
  BEGIN TRANSACTION;

  DELETE FROM `proyecto-demo.recupero_demo.scoring_mensual`
  WHERE mes_idx = mes_scoring;

  INSERT INTO `proyecto-demo.recupero_demo.scoring_mensual`
    (id_cuenta, mes_idx, tramo, prob_pago_90d, monto_castigado, veintil_prob, veintil_probdeuda)
  WITH predicciones AS (
    SELECT
      id_cuenta,
      mes_idx,
      tramo,
      monto_castigado,
      -- La probabilidad se toma por etiqueta (1 = paga), no por posicion en el arreglo
      (
        SELECT p.prob
        FROM UNNEST(predicted_pago_a_90_dias_probs) AS p
        WHERE CAST(p.label AS STRING) = '1'
      ) AS prob_pago_90d
    FROM ML.PREDICT(
      MODEL `proyecto-demo.recupero_demo.modelo_recupero_rf_v1`,
      (
        -- Solo las columnas necesarias, filtradas por mes antes de predecir.
        -- Las reglas de elegibilidad de la cartera se omiten en esta demo.
        SELECT
          id_cuenta, mes_idx, tramo, monto_castigado,
          pag_01, pag_02, pag_03,
          mor_01, mor_02,
          ext_01, ext_02, ext_03, ext_04, ext_05
        FROM `proyecto-demo.recupero_demo.tablon_modelo`
        WHERE mes_idx = mes_scoring
      )
    )
  )
  SELECT
    id_cuenta,
    mes_idx,
    tramo,
    ROUND(prob_pago_90d, 6)                                      AS prob_pago_90d,
    monto_castigado,
    NTILE(20) OVER (ORDER BY prob_pago_90d)                      AS veintil_prob,
    NTILE(20) OVER (ORDER BY prob_pago_90d * monto_castigado)    AS veintil_probdeuda
  FROM predicciones;

  COMMIT TRANSACTION;

EXCEPTION WHEN ERROR THEN
  ROLLBACK TRANSACTION;
  RAISE USING MESSAGE = @@error.message;
END;


-- ----------------------------------------------------------------------------
-- 4. Verificacion rapida del resultado
-- ----------------------------------------------------------------------------
SELECT
  mes_idx,
  COUNT(*)                         AS cuentas,
  ROUND(AVG(prob_pago_90d), 4)     AS prob_media,
  COUNT(DISTINCT veintil_prob)     AS veintiles
FROM `proyecto-demo.recupero_demo.scoring_mensual`
WHERE mes_idx = mes_scoring
GROUP BY mes_idx;
