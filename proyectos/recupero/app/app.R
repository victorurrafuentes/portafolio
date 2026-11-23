# ============================================================================
# Seguimiento de recupero de cartera castigada (demo con datos simulados)
# Panel Shiny con tres pestanas:
#   1. Pagos y recuperacion por veintil
#   2. Estabilidad de variables (PSI)
#   3. Indicadores del modelo (ROC, Gini, KS, IV)
#
# Ejecutar desde esta carpeta:  shiny::runApp()
# Paquetes: shiny, bslib, dplyr, tidyr, readr, plotly, DT, scales
# ============================================================================

library(shiny)
library(dplyr)
library(plotly)
library(DT)

for (archivo in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(archivo, encoding = "UTF-8")

datos <- leer_datos("data")

# ---- Datos derivados que no dependen de los filtros -------------------------

meses_comparables <- datos$pagos |>
  filter(periodo != "ENTRENAMIENTO") |>
  distinct(mes_idx, mes_label, periodo) |>
  arrange(mes_idx)

opciones_mes <- list(
  "Validación"  = as.character(meses_comparables$mes_label[meses_comparables$periodo == "VALIDACION"]),
  "Seguimiento" = as.character(meses_comparables$mes_label[meses_comparables$periodo == "SEGUIMIENTO"])
)
ultimo_mes <- tail(as.character(meses_comparables$mes_label), 1)

variables <- sort(unique(datos$estab$variable))
opciones_variable <- split(variables, familia_variable(variables))

psi_mensual      <- psi_por_mes(datos$estab)
iv_ks            <- iv_ks_por_periodo(datos$estab)
metricas_mensual <- metricas_por_mes(datos$modelo)
roc_base         <- curva_roc(agregado_base_modelo(datos$modelo))

# ---- Interfaz ---------------------------------------------------------------

tema <- bslib::bs_theme(
  version = 5, bg = PAL$fondo, fg = PAL$texto, primary = PAL$acento,
  base_font = bslib::font_google("Space Grotesk", local = FALSE)
)

selector_mes <- function(id, etiqueta = "Mes a comparar") {
  selectInput(id, etiqueta, choices = opciones_mes, selected = ultimo_mes, width = "190px")
}

ui <- fluidPage(
  theme = tema,
  tags$head(tags$link(rel = "stylesheet", href = "estilos.css")),

  div(
    class = "cabecera",
    span(class = "etiqueta", "Demo con datos simulados"),
    h1("Seguimiento de recupero de cartera castigada"),
    p("Desempeño de un modelo de probabilidad de pago a 90 días: recuperación frente al periodo base,",
      "estabilidad de las variables y poder de discriminación mes a mes.")
  ),

  tabsetPanel(
    id = "pestanas", type = "pills",

    # ---- 1. Pagos y recuperacion ----
    tabPanel(
      "Pagos y recuperación",
      div(
        class = "filtros",
        selector_mes("mes_pagos"),
        radioButtons("base_veintil", "Veintil calculado sobre",
                     choices = c("Probabilidad" = "PROB", "Probabilidad × deuda" = "PROB_X_DEUDA"),
                     inline = TRUE),
        checkboxGroupInput("tramos", "Tramo de la cartera",
                           choices = c("Reciente" = "RECIENTE", "Antiguo" = "ANTIGUO"),
                           selected = c("RECIENTE", "ANTIGUO"), inline = TRUE)
      ),
      uiOutput("tarjetas_pagos"),
      div(
        class = "rejilla-2",
        div(class = "panel", plotlyOutput("graf_tasa", height = "340px")),
        div(class = "panel", plotlyOutput("graf_monto", height = "340px"))
      ),
      div(class = "panel", plotlyOutput("graf_evolucion", height = "300px")),
      div(class = "panel", h3("Detalle por veintil"), DTOutput("tabla_veintil"))
    ),

    # ---- 2. Estabilidad ----
    tabPanel(
      "Estabilidad de variables",
      div(
        class = "panel",
        h3("PSI por variable y mes"),
        DTOutput("tabla_psi"),
        p(class = "nota", "Verde bajo 0,10: estable. Ámbar entre 0,10 y 0,25: revisar. Rojo sobre 0,25: cambio relevante.")
      ),
      div(
        class = "filtros",
        selectInput("variable", "Variable", choices = opciones_variable, width = "220px"),
        selector_mes("mes_estab")
      ),
      div(
        class = "rejilla-2",
        div(class = "panel", plotlyOutput("graf_distribucion", height = "340px")),
        div(class = "panel", h3("Detalle por bin"), DTOutput("tabla_distribucion"))
      )
    ),

    # ---- 3. Indicadores del modelo ----
    tabPanel(
      "Indicadores del modelo",
      div(class = "filtros", selector_mes("mes_modelo")),
      uiOutput("tarjetas_modelo"),
      div(
        class = "rejilla-2",
        div(class = "panel", plotlyOutput("graf_roc", height = "360px")),
        div(class = "panel", plotlyOutput("graf_gini", height = "360px"))
      ),
      div(class = "panel", h3("Métricas por mes"), DTOutput("tabla_metricas")),
      div(
        class = "rejilla-2",
        div(class = "panel", h3("IV por variable"), DTOutput("tabla_iv"),
            p(class = "nota", "Verde desde 0,10: buen poder predictivo. Ámbar entre 0,02 y 0,10: débil. Rojo bajo 0,02: sin aporte.")),
        div(class = "panel", h3("KS por variable"), DTOutput("tabla_ks"),
            p(class = "nota", "Verde desde 20. Ámbar entre 10 y 20. Rojo bajo 10."))
      )
    )
  ),

  div(
    class = "pie",
    "Todos los datos de este panel son simulados. No provienen de ninguna cartera, empresa ni modelo real."
  )
)

# ---- Servidor ---------------------------------------------------------------

server <- function(input, output, session) {

  # ---- 1. Pagos y recuperacion ----
  agregado   <- reactive({ req(input$tramos); agregar_pagos(datos$pagos, input$base_veintil, input$tramos) })
  base_pagos <- reactive(linea_base_veintil(agregado()))
  totales    <- reactive(totales_mensuales(agregado()))
  datos_mes  <- reactive(filter(agregado(), mes_label == input$mes_pagos))

  output$tarjetas_pagos <- renderUI({
    tarjetas_pagos(comparar_con_base(totales(), input$mes_pagos), input$mes_pagos)
  })
  output$graf_tasa  <- renderPlotly(grafico_tasa_pago(base_pagos(), datos_mes(), input$mes_pagos))
  output$graf_monto <- renderPlotly(grafico_monto_recuperado(base_pagos(), datos_mes(), input$mes_pagos))
  output$graf_evolucion <- renderPlotly({
    monto_base <- totales() |> filter(periodo == "ENTRENAMIENTO") |> summarise(m = mean(monto_recuperado)) |> pull(m)
    grafico_evolucion(totales(), monto_base)
  })
  output$tabla_veintil <- renderDT(tabla_veintil(datos_mes()))

  # ---- 2. Estabilidad ----
  distribucion <- reactive({
    req(input$variable)
    distribucion_bins(datos$estab, input$variable, input$mes_estab)
  })

  output$tabla_psi          <- renderDT(tabla_psi(psi_mensual))
  output$graf_distribucion  <- renderPlotly(grafico_distribucion(distribucion(), input$variable, input$mes_estab))
  output$tabla_distribucion <- renderDT(tabla_distribucion(distribucion()))

  # ---- 3. Indicadores del modelo ----
  output$tarjetas_modelo <- renderUI(tarjetas_modelo(metricas_mensual, input$mes_modelo))

  output$graf_roc <- renderPlotly({
    roc_mes <- datos$modelo |>
      filter(mes_label == input$mes_modelo) |>
      curva_roc()
    grafico_roc(roc_base, roc_mes, input$mes_modelo)
  })

  output$graf_gini <- renderPlotly({
    gini_base <- metricas_mensual |> filter(periodo == "ENTRENAMIENTO") |> summarise(g = mean(gini)) |> pull(g)
    grafico_gini_mensual(metricas_mensual, gini_base)
  })

  output$tabla_metricas <- renderDT(tabla_metricas(metricas_mensual))
  output$tabla_iv       <- renderDT(tabla_iv(iv_ks))
  output$tabla_ks       <- renderDT(tabla_ks_variable(iv_ks))
}

shinyApp(ui, server)
