-- ============================================================================
-- 01_entrenar_modelo.sql
-- Modelo de probabilidad de pago a 90 dias para cartera castigada
-- BigQuery ML | Random Forest | ajuste de hiperparametros
--
-- Script de referencia con nombres ficticios (proyecto-demo.recupero_demo).
-- Las variables estan anonimizadas por familia:
--   pag_*  historial de pagos
--   mor_*  morosidad
--   ext_*  informacion crediticia externa
-- ============================================================================


-- ----------------------------------------------------------------------------
-- 1. Entrenamiento
--    Solo se usa el periodo base (M01-M12). Los meses posteriores quedan fuera
--    del entrenamiento y sirven para evaluar el modelo fuera de tiempo.
-- ----------------------------------------------------------------------------
CREATE OR REPLACE MODEL `proyecto-demo.recupero_demo.modelo_recupero_rf_v1`
OPTIONS (
  MODEL_TYPE               = 'RANDOM_FOREST_CLASSIFIER',
  INPUT_LABEL_COLS         = ['pago_a_90_dias'],

  -- Ajuste de hiperparametros por busqueda aleatoria
  NUM_TRIALS               = 20,
  MAX_PARALLEL_TRIALS      = 4,
  HPARAM_TUNING_ALGORITHM  = 'RANDOM_SEARCH',
  HPARAM_TUNING_OBJECTIVES = ['ROC_AUC'],

  NUM_PARALLEL_TREE        = HPARAM_RANGE(100, 300),
  MAX_TREE_DEPTH           = HPARAM_RANGE(4, 9),
  SUBSAMPLE                = HPARAM_RANGE(0.6, 0.9),
  COLSAMPLE_BYTREE         = HPARAM_RANGE(0.5, 1.0),
  MIN_TREE_CHILD_WEIGHT    = HPARAM_RANGE(1, 10),

  AUTO_CLASS_WEIGHTS       = TRUE,  -- pocas cuentas pagan: se compensa el desbalance
  ENABLE_GLOBAL_EXPLAIN    = TRUE   -- habilita la importancia global de variables
) AS
SELECT
  -- Historial de pagos
  pag_01, pag_02, pag_03,
  -- Morosidad
  mor_01, mor_02,
  -- Informacion crediticia externa
  ext_01, ext_02, ext_03, ext_04, ext_05,
  -- Variable objetivo
  pago_a_90_dias
FROM `proyecto-demo.recupero_demo.tablon_modelo`
WHERE periodo = 'ENTRENAMIENTO';


-- ----------------------------------------------------------------------------
-- 2. Resultado del ajuste: una fila por prueba de hiperparametros
-- ----------------------------------------------------------------------------
SELECT *
FROM ML.TRIAL_INFO(MODEL `proyecto-demo.recupero_demo.modelo_recupero_rf_v1`);


-- ----------------------------------------------------------------------------
-- 3. Evaluacion fuera de tiempo: periodo de validacion (M13-M15),
--    que el modelo no vio durante el entrenamiento.
-- ----------------------------------------------------------------------------
SELECT *
FROM ML.EVALUATE(
  MODEL `proyecto-demo.recupero_demo.modelo_recupero_rf_v1`,
  (
    SELECT
      pag_01, pag_02, pag_03,
      mor_01, mor_02,
      ext_01, ext_02, ext_03, ext_04, ext_05,
      pago_a_90_dias
    FROM `proyecto-demo.recupero_demo.tablon_modelo`
    WHERE periodo = 'VALIDACION'
  )
);


-- ----------------------------------------------------------------------------
-- 4. Importancia global de las variables
-- ----------------------------------------------------------------------------
SELECT
  feature,
  attribution
FROM ML.GLOBAL_EXPLAIN(MODEL `proyecto-demo.recupero_demo.modelo_recupero_rf_v1`)
ORDER BY attribution DESC;
