# Portafolio de modelamiento de riesgo

Sitio personal con proyectos de modelamiento de riesgo, hecho con [Quarto](https://quarto.org). Todo el contenido usa **datos simulados o públicos**: nada proviene de carteras, empresas o modelos reales.

**Sitio publicado:** https://victorurrafuentes.github.io/portafolio/

## Proyectos

| Proyecto | Qué muestra |
|---|---|
| [Seguimiento de recupero de cartera castigada](proyectos/recupero/) | Modelo de probabilidad de pago a 90 días en BigQuery ML y panel de monitoreo mensual en R Shiny, con un reporte que se genera desde los datos. |

## Estructura

| Ruta | Contenido |
|---|---|
| `index.qmd` | Portada y lista de proyectos |
| `recupero.qmd` | Reporte mensual de seguimiento generado desde los datos |
| `proyectos/recupero/` | SQL de referencia, panel Shiny y datos simulados |
| `_quarto.yml`, `styles.css` | Configuración y estilo del sitio |

## Generar el sitio

Requiere Quarto, R 4.1 o superior y los paquetes `dplyr`, `tidyr`, `readr`, `tibble`, `plotly`, `DT`, `scales`, `htmltools`, `knitr` y `rmarkdown`.

```bash
quarto render      # sitio completo, queda en _site/
quarto preview     # vista previa con recarga
```

El reporte de recupero usa por defecto el último mes (M21). Para otro mes, define `REPORTE_MES` antes de renderizar:

```bash
REPORTE_MES=M20 quarto render recupero.qmd        # Linux y Mac
```

```
set REPORTE_MES=M20 && quarto render recupero.qmd   # Windows (cmd)
```

## Publicar

El contenido de `_site/` es estático y sirve en GitHub Pages u otro hosting estático. Con `quarto publish gh-pages`, desde esta carpeta, se genera y se sube el sitio.
