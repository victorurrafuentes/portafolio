# Glosario del proyecto (demo con datos simulados)

**Título:** Seguimiento de recupero de cartera castigada
**Variable objetivo:** `pago_a_90_dias` (1 si la cuenta paga dentro de los 90 días siguientes)
**Salida del modelo:** `prob_pago_90d`

## Periodos (índice de mes relativo)
| Periodo | Meses |
|---|---|
| ENTRENAMIENTO (baseline) | M01 a M12 |
| VALIDACION | M13 a M15 |
| SEGUIMIENTO | M16 a M21 |

## Variables del modelo (10, anonimizadas por familia)
| Variables | Familia |
|---|---|
| `pag_01`, `pag_02`, `pag_03` | Historial de pagos |
| `mor_01`, `mor_02` | Morosidad |
| `ext_01` a `ext_05` | Información crediticia externa |

## Columnas de estructura (no son variables del modelo)
| Columna | Uso |
|---|---|
| `meses_desde_castigo` | Separa los tramos RECIENTE y ANTIGUO (umbral inventado) |
| `monto_castigado` | Deuda de la cuenta, para el cruce probabilidad x deuda |

## Fuentes (descripción general)
Historial interno de pagos, información crediticia externa (buró).

## Objetos técnicos ficticios
- Proyecto y dataset: `proyecto-demo.recupero_demo`
- Tablas: `tablon_modelo`, `scoring_mensual`, `seguimiento_pagos`, `seguimiento_estabilidad`, `seguimiento_modelo`
- Modelo: `modelo_recupero_rf_v1`
