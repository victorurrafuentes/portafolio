# ============================================================================
# graficos.R
# Un grafico plotly por funcion. Reciben datos ya calculados (ver calculos.R).
# ============================================================================

# Marcas verticales que separan los periodos en graficos mensuales
separadores_periodo <- function(cortes = c(12.5, 15.5), etiquetas = c("Validación", "Seguimiento")) {
  lineas <- lapply(cortes, function(x) {
    list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper",
         line = list(color = PAL$linea, width = 1.5, dash = "dot"))
  })
  textos <- Map(function(x, t) {
    list(x = x + 0.15, y = 1, yref = "paper", text = t, showarrow = FALSE,
         xanchor = "left", yanchor = "bottom", font = list(size = 11, color = PAL$suave))
  }, cortes, etiquetas)
  list(shapes = lineas, annotations = textos)
}

# ---- Pagos y recupero --------------------------------------------------

grafico_tasa_pago <- function(base, mes_df, etiqueta_mes) {
  plotly::plot_ly() |>
    plotly::add_trace(
      data = base, x = ~veintil, y = ~tasa_pago, type = "scatter", mode = "lines+markers",
      name = "Periodo base", line = list(color = PAL$base, width = 2.5, dash = "dot"),
      marker = list(color = PAL$base, size = 6),
      hovertemplate = "Veintil %{x}<br>Periodo base: %{y:.1%}<extra></extra>"
    ) |>
    plotly::add_trace(
      data = mes_df, x = ~veintil, y = ~tasa_pago, type = "scatter", mode = "lines+markers",
      name = etiqueta_mes, line = list(color = PAL$acento, width = 2.8),
      marker = list(color = PAL$acento, size = 7),
      hovertemplate = paste0("Veintil %{x}<br>", etiqueta_mes, ": %{y:.1%}<extra></extra>")
    ) |>
    tema_plotly("Tasa de pago a 90 días por veintil", "Veintil", "Tasa de pago",
                xaxis_extra = list(dtick = 1, range = c(0.5, 20.5)), yaxis_extra = list(tickformat = ".0%"))
}

grafico_monto_recuperado <- function(base, mes_df, etiqueta_mes) {
  plotly::plot_ly() |>
    plotly::add_bars(
      data = base, x = ~veintil, y = ~monto_recuperado / 1e6, name = "Promedio mensual base",
      marker = list(color = PAL$base),
      hovertemplate = "Veintil %{x}<br>Base: %{y:.1f} MM<extra></extra>"
    ) |>
    plotly::add_bars(
      data = mes_df, x = ~veintil, y = ~monto_recuperado / 1e6, name = etiqueta_mes,
      marker = list(color = PAL$acento),
      hovertemplate = paste0("Veintil %{x}<br>", etiqueta_mes, ": %{y:.1f} MM<extra></extra>")
    ) |>
    tema_plotly("Monto recuperado por veintil", "Veintil", "Monto recuperado (MM)",
                xaxis_extra = list(dtick = 1), barmode = "group")
}

grafico_evolucion <- function(totales, monto_base) {
  sep <- separadores_periodo()
  plotly::plot_ly(
    totales, x = ~mes_idx, y = ~monto_recuperado / 1e6, type = "scatter", mode = "lines+markers",
    name = "Monto recuperado", line = list(color = PAL$acento, width = 2.8),
    marker = list(color = PAL$acento, size = 7), text = ~as.character(mes_label),
    hovertemplate = "%{text}: %{y:.1f} MM<extra></extra>"
  ) |>
    plotly::add_lines(
      x = range(totales$mes_idx), y = c(monto_base, monto_base) / 1e6,
      name = "Promedio mensual base", line = list(color = PAL$base, width = 2, dash = "dash"),
      hovertemplate = "Promedio base: %{y:.1f} MM<extra></extra>", inherit = FALSE
    ) |>
    tema_plotly(
      "Evolución mensual del monto recuperado", NULL, "Monto recuperado (MM)",
      xaxis_extra = list(tickmode = "array", tickvals = totales$mes_idx,
                         ticktext = as.character(totales$mes_label)),
      yaxis_extra = list(rangemode = "tozero"),
      shapes = sep$shapes, annotations = sep$annotations
    )
}

# ---- Estabilidad -----------------------------------------------------------

grafico_distribucion <- function(dist, variable, etiqueta_mes) {
  base <- dist |> dplyr::filter(serie == "Periodo base")
  mes  <- dist |> dplyr::filter(serie != "Periodo base")

  plotly::plot_ly() |>
    plotly::add_bars(
      data = base, x = ~bin, y = ~pct, name = "Periodo base", marker = list(color = PAL$base),
      hovertemplate = "Bin %{x}<br>Base: %{y:.1%}<extra></extra>"
    ) |>
    plotly::add_bars(
      data = mes, x = ~bin, y = ~pct, name = etiqueta_mes, marker = list(color = PAL$acento),
      hovertemplate = paste0("Bin %{x}<br>", etiqueta_mes, ": %{y:.1%}<extra></extra>")
    ) |>
    tema_plotly(paste0("Distribución de ", variable, " por bin"), "Bin (decil del periodo base)",
                "% de cuentas", xaxis_extra = list(dtick = 1),
                yaxis_extra = list(tickformat = ".0%"), barmode = "group")
}

# ---- Indicadores del modelo ------------------------------------------------

grafico_roc <- function(roc_base, roc_mes, etiqueta_mes) {
  plotly::plot_ly() |>
    plotly::add_lines(
      x = c(0, 1), y = c(0, 1), name = "Modelo aleatorio", hoverinfo = "skip",
      line = list(color = PAL$linea, width = 1.5, dash = "dash")
    ) |>
    plotly::add_lines(
      data = roc_base, x = ~fpr, y = ~tpr, name = "Línea base (entrenamiento)",
      line = list(color = PAL$base, width = 2.5, dash = "dot"),
      hovertemplate = "FPR %{x:.1%} · TPR %{y:.1%}<extra>Línea base</extra>"
    ) |>
    plotly::add_lines(
      data = roc_mes, x = ~fpr, y = ~tpr, name = etiqueta_mes,
      line = list(color = PAL$acento, width = 2.8),
      hovertemplate = paste0("FPR %{x:.1%} · TPR %{y:.1%}<extra>", etiqueta_mes, "</extra>")
    ) |>
    tema_plotly("Curva ROC", "Tasa de falsos positivos", "Tasa de verdaderos positivos",
                xaxis_extra = list(range = c(0, 1), tickformat = ".0%"),
                yaxis_extra = list(range = c(0, 1), tickformat = ".0%"))
}

grafico_gini_mensual <- function(metricas, gini_base) {
  sep <- separadores_periodo()
  plotly::plot_ly(
    metricas, x = ~mes_idx, y = ~gini, type = "scatter", mode = "lines+markers",
    name = "Gini mensual", line = list(color = PAL$acento, width = 2.8),
    marker = list(color = PAL$acento, size = 7), text = ~as.character(mes_label),
    hovertemplate = "%{text}: %{y:.1%}<extra></extra>"
  ) |>
    plotly::add_lines(
      x = range(metricas$mes_idx), y = c(gini_base, gini_base), name = "Línea base (entrenamiento)",
      line = list(color = PAL$base, width = 2, dash = "dash"),
      hovertemplate = "Línea base: %{y:.1%}<extra></extra>", inherit = FALSE
    ) |>
    tema_plotly(
      "Gini por mes", NULL, "Gini",
      xaxis_extra = list(tickmode = "array", tickvals = metricas$mes_idx,
                         ticktext = as.character(metricas$mes_label)),
      yaxis_extra = list(tickformat = ".0%"),
      shapes = sep$shapes, annotations = sep$annotations
    )
}
