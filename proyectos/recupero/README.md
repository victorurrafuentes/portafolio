# Seguimiento de recupero de cartera castigada

Demo de un flujo de modelamiento y seguimiento: un modelo de probabilidad de pago a 90 días entrenado en BigQuery ML, y un panel en R Shiny para monitorearlo mes a mes.

> **Todos los datos son simulados.** No provienen de ninguna cartera, empresa ni modelo real. Los nombres de proyecto, dataset, tablas y variables son ficticios, y los valores simulados no están calibrados con resultados reales.

## Qué muestra

| Pieza | Carpeta | Qué es |
|---|---|---|
| Modelo en BQML | `sql/` | SQL de referencia: entrenamiento de un Random Forest con búsqueda de hiperparámetros, evaluación, importancia global y scoring mensual idempotente. Se muestra para explicar el diseño; no se ejecuta aquí. |
| Panel de seguimiento | `app/` | App Shiny de tres pestañas sobre datos simulados: pagos y recuperación por veintil, estabilidad de variables (PSI) e indicadores del modelo (ROC, Gini, KS, IV). |
| Datos simulados | `data/` | Tres tablas agregadas que alimentan el panel, con su diccionario. |
| Pruebas | `pruebas/` | Script que ejecuta la capa de cálculo y exporta resultados para contrastarlos con una implementación independiente. |
| Glosario | `docs/` | Nombres ficticios y definiciones de periodos, variables y objetos. |

## Enfoque del modelo

- **Objetivo:** que una cuenta castigada pague en los 90 días siguientes (`pago_a_90_dias`).
- **Salida:** `prob_pago_90d`, ordenada en veintiles para priorizar la gestión.
- **Variables:** diez variables anonimizadas por familia: historial de pagos (`pag_*`), morosidad (`mor_*`) e información externa (`ext_*`).
- **Periodos:** M01–M12 entrenamiento, M13–M15 validación, M16–M21 seguimiento.

## Qué mide el panel

1. **Pagos y recuperación:** tasa de pago y monto recuperado por veintil, contra el promedio mensual del periodo de entrenamiento.
2. **Estabilidad de variables:** PSI por variable y mes (umbrales 0,10 y 0,25), y distribución por bin frente al periodo base.
3. **Indicadores del modelo:** AUC, Gini y KS mensuales calculados desde los veintiles, e IV y KS por variable.

En los datos simulados se introdujo deriva en algunas variables desde M16, para que el panel tenga algo que detectar.

## Cómo ejecutarlo

Requiere R 4.1 o superior y los paquetes `shiny`, `bslib`, `dplyr`, `tidyr`, `readr`, `tibble`, `plotly`, `DT` y `scales`.

```r
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "readr", "tibble", "plotly", "DT", "scales"))
setwd("app")
shiny::runApp()
```

El panel carga la tipografía desde Google Fonts, por lo que necesita conexión para verse como en las capturas; sin ella usa una tipografía del sistema.

## Alcance

El SQL es de referencia y no puede reproducirse sin un proyecto de BigQuery propio. Lo reproducible es el panel: con los datos simulados de `data/` corre de punta a punta.
