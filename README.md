# Portafolio de modelamiento de riesgo

Sitio hecho con [Quarto](https://quarto.org). Todo el contenido usa **datos simulados o públicos**: nada proviene de carteras, empresas o modelos reales.

## Estructura

| Ruta | Contenido |
|---|---|
| `index.qmd` | Portada y lista de proyectos |
| `recupero.qmd` | Reporte mensual de seguimiento (se genera desde los datos) |
| `proyectos/recupero/` | Repositorio del proyecto: SQL de referencia, panel Shiny, datos simulados |
| `_quarto.yml`, `styles.css` | Configuración y estilo del sitio |

## Generar el sitio

Requiere Quarto, R 4.1 o superior y los paquetes `dplyr`, `tidyr`, `readr`, `tibble`, `plotly`, `DT`, `scales`, `htmltools`, `knitr` y `rmarkdown`.

```bash
quarto render                          # sitio completo, queda en _site/
REPORTE_MES=M20 quarto render recupero.qmd   # reporte de otro mes
quarto preview                         # vista previa con recarga
```

## Publicar

El contenido de `_site/` es estático: sirve en GitHub Pages u otro hosting estático. Con `quarto publish gh-pages` se sube desde la misma carpeta.

## Pendiente

Completar el contacto en `index.qmd` y sumar las páginas en preparación.
