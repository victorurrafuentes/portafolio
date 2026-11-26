# Datos simulados del panel

Todos los valores son sintéticos. No provienen de ninguna cartera, empresa ni modelo real.
Meses en índice relativo: M01 a M12 entrenamiento, M13 a M15 validación, M16 a M21 seguimiento.
Cada mes contiene 4.000 cuentas castigadas simuladas.

## seguimiento_pagos.csv
Pagos y recupero a 90 días por veintil. El veintil 20 agrupa a las cuentas con mayor puntaje.
| Columna | Descripción |
|---|---|
| `mes_idx`, `mes_label`, `periodo` | Mes (1 a 21, "M01"...) y periodo (ENTRENAMIENTO, VALIDACION, SEGUIMIENTO) |
| `tramo` | RECIENTE o ANTIGUO, según los meses desde el castigo |
| `base_veintil` | PROB (veintil de probabilidad) o PROB_X_DEUDA (veintil de probabilidad por deuda) |
| `veintil` | 1 a 20, calculado dentro de cada mes |
| `total`, `pagadores` | Cuentas y cuentas que pagaron a 90 días |
| `deuda_total`, `monto_recuperado` | Deuda castigada y monto recuperado (moneda simulada) |

## seguimiento_estabilidad.csv
Distribución de cada variable por decil del periodo base (los deciles se fijan en ENTRENAMIENTO).
| Columna | Descripción |
|---|---|
| `mes_idx`, `mes_label`, `periodo` | Igual que arriba |
| `variable` | `pag_01`..`pag_03` (pagos), `mor_01`..`mor_02` (morosidad), `ext_01`..`ext_05` (información externa) |
| `bin` | Decil del periodo base (menos de 10 en variables discretas) |
| `n` | Cuentas en el bin ese mes |
| `tasa_pago` | Tasa de pago a 90 días dentro del bin (vacía si n = 0) |

## seguimiento_modelo.csv
Agregado por veintil de probabilidad, para calcular ROC, Gini y KS.
| Columna | Descripción |
|---|---|
| `mes_idx`, `mes_label`, `periodo` | Igual que arriba |
| `veintil_prob` | 1 a 20 |
| `total`, `pagadores` | Cuentas y cuentas que pagaron a 90 días |
