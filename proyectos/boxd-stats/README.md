# boxd-stats

Aplicación en R Shiny que analiza el historial de Letterboxd de una persona: películas registradas por año, directores, actores, géneros y países más vistos, géneros por década y hábitos del diario. Es un proyecto personal.

**App en línea:** https://sfb-apps.shinyapps.io/boxd-stats-app/

La interfaz de la app está en inglés.

## Cómo funciona

1. La persona sube el export de su cuenta de Letterboxd (disponible en la configuración de datos de Letterboxd).
2. La app lee el diario y el listado de películas vistas.
3. Cada película se completa con información de [TMDB](https://www.themoviedb.org/) (director, reparto, géneros, país y duración).
4. Se muestran 11 pestañas con resúmenes y gráficos interactivos.

La app incluye un modo de demostración con un export ficticio (`demo_export.zip`), sin datos reales.

## Ejecutarla en tu computador

Requiere R 4.1 o superior y una clave gratuita de la API de TMDB.

```r
install.packages(c("shiny", "shinyjs", "dplyr", "tidyr", "readr", "lubridate", "ggplot2",
                   "plotly", "httr", "jsonlite", "future", "future.apply", "progressr"))
```

1. Copia `.Renviron.example` como `.Renviron` y escribe tu clave en `TMDB_API_KEY`. `FORMSPREE_ENDPOINT` es opcional: sin ella, la app oculta el formulario de feedback.
2. Reinicia R, para que lea el archivo.
3. Ejecuta `shiny::runApp()` desde esta carpeta.

## Privacidad

Los archivos que sube cada persona se procesan en una carpeta temporal durante la sesión y no se guardan. La app no registra quién la usa. Solo si alguien envía feedback de forma voluntaria, el mensaje incluye su nombre de usuario de Letterboxd.

## Créditos

Esta aplicación usa la API de TMDB, pero no está respaldada ni certificada por TMDB. No tiene relación con Letterboxd.
