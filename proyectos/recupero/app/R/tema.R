# ============================================================================
# tema.R
# Paleta, tipografia y estilo comun de los graficos.
# ============================================================================

PAL <- list(
  fondo      = "#0f0c1a",
  superficie = "#181331",
  linea      = "#2d2653",
  texto      = "#ece9f8",
  suave      = "#9e97bf",
  acento     = "#a487ff",   # serie actual
  base       = "#7f8db0",   # periodo base
  verde      = "#3ecf8e",
  ambar      = "#f5b83d",
  rojo       = "#ef5b6b"
)

FUENTE <- "Space Grotesk, system-ui, sans-serif"

# Aplica el estilo oscuro a un grafico plotly.
# xaxis_extra / yaxis_extra permiten ajustar un eje sin repetir el resto.
tema_plotly <- function(p, titulo = NULL, xlab = NULL, ylab = NULL,
                        xaxis_extra = list(), yaxis_extra = list(), ...) {
  eje <- function(titulo_eje, extra) {
    utils::modifyList(
      list(
        title = list(text = titulo_eje, font = list(size = 12)),
        gridcolor = PAL$linea, zerolinecolor = PAL$linea,
        linecolor = PAL$linea, tickfont = list(size = 11)
      ),
      extra
    )
  }

  p |>
    plotly::layout(
      title = list(
        text = titulo, x = 0.01, xanchor = "left",
        font = list(color = PAL$texto, size = 15, family = FUENTE)
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font   = list(color = PAL$suave, family = FUENTE),
      xaxis  = eje(xlab, xaxis_extra),
      yaxis  = eje(ylab, yaxis_extra),
      legend = list(orientation = "h", x = 0, y = -0.28, font = list(size = 12)),
      margin = list(t = 50, r = 16, l = 8),
      hoverlabel = list(bgcolor = PAL$superficie, font = list(color = PAL$texto)),
      ...
    ) |>
    plotly::config(displayModeBar = FALSE)
}
