# ============================================================================
# componentes.R
# Formatos, tarjetas y tablas (DT) del panel.
# ============================================================================

# ---- Formatos --------------------------------------------------------------

fmt_pct <- function(x, decimales = 1) {
  scales::percent(x, accuracy = 10^(-decimales), decimal.mark = ",", big.mark = ".")
}

fmt_mm <- function(x) {
  paste0(format(round(x / 1e6, 1), big.mark = ".", decimal.mark = ",", nsmall = 1, trim = TRUE), " MM")
}

familia_variable <- function(variable) {
  dplyr::case_when(
    startsWith(variable, "pag_") ~ "Historial de pagos",
    startsWith(variable, "mor_") ~ "Morosidad",
    startsWith(variable, "ext_") ~ "Información externa",
    TRUE ~ "Otras"
  )
}

# ---- Tarjetas --------------------------------------------------------------

tarjeta <- function(etiqueta, valor, detalle, razon = NULL) {
  clase <- if (is.null(razon)) "" else if (razon >= 1) " sube" else " baja"
  htmltools::tags$div(
    class = "tarjeta",
    htmltools::tags$div(class = "tarjeta-etiqueta", etiqueta),
    htmltools::tags$div(class = "tarjeta-valor", valor),
    htmltools::tags$div(class = paste0("tarjeta-detalle", clase), detalle)
  )
}

tarjetas_pagos <- function(cmp, etiqueta_mes) {
  htmltools::tags$div(
    class = "tarjetas",
    tarjeta(
      paste("Monto recuperado,", etiqueta_mes), fmt_mm(cmp$mes$monto_recuperado),
      paste0(fmt_pct(cmp$razon_monto), " del promedio base (", fmt_mm(cmp$base$monto), ")"),
      cmp$razon_monto
    ),
    tarjeta(
      paste("Tasa de pago,", etiqueta_mes), fmt_pct(cmp$mes$tasa_pago),
      paste0(fmt_pct(cmp$razon_pago), " del promedio base (", fmt_pct(cmp$base$tasa_pago), ")"),
      cmp$razon_pago
    ),
    tarjeta(
      paste("Recuperado / deuda,", etiqueta_mes), fmt_pct(cmp$mes$tasa_recup, 2),
      paste0(fmt_pct(cmp$razon_recup), " del promedio base (", fmt_pct(cmp$base$tasa_recup, 2), ")"),
      cmp$razon_recup
    )
  )
}

tarjetas_modelo <- function(metricas, etiqueta_mes) {
  base <- metricas |>
    dplyr::filter(periodo == "ENTRENAMIENTO") |>
    dplyr::summarise(gini = mean(gini), auc = mean(auc), ks = mean(ks))
  mes <- metricas |> dplyr::filter(mes_label == etiqueta_mes)

  dif <- function(x, y, escala = 100) {
    d <- (x - y) * escala
    paste0(ifelse(d >= 0, "+", "−"), formatC(abs(d), format = "f", digits = 1, decimal.mark = ","), " pts vs. base")
  }

  htmltools::tags$div(
    class = "tarjetas",
    tarjeta(paste("Gini,", etiqueta_mes), fmt_pct(mes$gini), dif(mes$gini, base$gini), mes$gini - base$gini + 1),
    tarjeta(paste("AUC,", etiqueta_mes), fmt_pct(mes$auc), dif(mes$auc, base$auc), mes$auc - base$auc + 1),
    tarjeta(paste("KS,", etiqueta_mes), paste0(formatC(mes$ks, format = "f", digits = 1, decimal.mark = ","), "%"),
            dif(mes$ks, base$ks, 1), mes$ks - base$ks + 1)
  )
}

# ---- Tablas ----------------------------------------------------------------

opciones_tabla <- list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)

# Colorea celdas segun cortes: menor al primer corte, entre cortes, mayor al ultimo.
# invertir = TRUE cuando "mas es mejor" (IV, KS); FALSE cuando "menos es mejor" (PSI).
semaforo <- function(tabla, columnas, cortes, invertir = FALSE) {
  fondos <- c("rgba(62,207,142,.18)", "rgba(245,184,61,.20)", "rgba(239,91,107,.26)")
  textos <- c(PAL$verde, PAL$ambar, "#ff9aa6")
  if (invertir) { fondos <- rev(fondos); textos <- rev(textos) }

  DT::formatStyle(
    tabla, columnas,
    backgroundColor = DT::styleInterval(cortes, fondos),
    color = DT::styleInterval(cortes, textos),
    fontWeight = "600", textAlign = "center"
  )
}

barra <- function(tabla, columna, valores) {
  DT::formatStyle(
    tabla, columna,
    background = DT::styleColorBar(c(0, max(valores, na.rm = TRUE)), "rgba(164,135,255,.45)"),
    backgroundSize = "100% 72%", backgroundRepeat = "no-repeat", backgroundPosition = "center"
  )
}

tabla_veintil <- function(df) {
  d <- df |>
    dplyr::arrange(dplyr::desc(veintil)) |>
    dplyr::transmute(
      Veintil = veintil, Cuentas = total, Pagadores = pagadores, `Tasa de pago` = tasa_pago,
      `Deuda total` = deuda_total, `Monto recuperado` = monto_recuperado, `Recuperado / deuda` = tasa_recup
    )

  DT::datatable(d, rownames = FALSE, class = "compact", options = opciones_tabla) |>
    DT::formatRound(c("Cuentas", "Pagadores"), digits = 0, mark = ".") |>
    DT::formatPercentage(c("Tasa de pago", "Recuperado / deuda"), digits = 1, dec.mark = ",") |>
    DT::formatCurrency(c("Deuda total", "Monto recuperado"), currency = "$", interval = 3,
                       mark = ".", dec.mark = ",", digits = 0) |>
    barra("Tasa de pago", d$`Tasa de pago`) |>
    barra("Deuda total", d$`Deuda total`) |>
    barra("Monto recuperado", d$`Monto recuperado`) |>
    barra("Recuperado / deuda", d$`Recuperado / deuda`)
}

# Tabla ancha variable x columna con semaforo (PSI por mes, IV o KS por periodo)
tabla_semaforo <- function(largo, columna_nombre, columna_valor, cortes, invertir, digitos) {
  ancho <- largo |>
    dplyr::mutate(Familia = familia_variable(variable)) |>
    dplyr::select(Variable = variable, Familia, dplyr::all_of(c(columna_nombre, columna_valor))) |>
    tidyr::pivot_wider(names_from = dplyr::all_of(columna_nombre), values_from = dplyr::all_of(columna_valor)) |>
    dplyr::arrange(Variable)

  columnas_num <- setdiff(names(ancho), c("Variable", "Familia"))
  if (is.factor(largo[[columna_nombre]])) {
    columnas_num <- levels(largo[[columna_nombre]])[levels(largo[[columna_nombre]]) %in% columnas_num]
    ancho <- ancho[, c("Variable", "Familia", columnas_num)]
  }

  DT::datatable(ancho, rownames = FALSE, class = "compact", options = opciones_tabla) |>
    DT::formatRound(columnas_num, digits = digitos, dec.mark = ",") |>
    semaforo(columnas_num, cortes, invertir)
}

tabla_psi <- function(psi) {
  tabla_semaforo(psi, "mes_label", "psi", cortes = c(0.10, 0.25), invertir = FALSE, digitos = 3)
}

tabla_iv <- function(ivks) {
  ivks |>
    dplyr::mutate(periodo = factor(periodo, levels = c("ENTRENAMIENTO", "VALIDACION", "SEGUIMIENTO"))) |>
    tabla_semaforo("periodo", "IV", cortes = c(0.02, 0.10), invertir = TRUE, digitos = 3)
}

tabla_ks_variable <- function(ivks) {
  ivks |>
    dplyr::mutate(periodo = factor(periodo, levels = c("ENTRENAMIENTO", "VALIDACION", "SEGUIMIENTO"))) |>
    tabla_semaforo("periodo", "KS", cortes = c(10, 20), invertir = TRUE, digitos = 1)
}

tabla_metricas <- function(metricas) {
  d <- metricas |>
    dplyr::transmute(Mes = as.character(mes_label), Periodo = periodo, AUC = auc, Gini = gini, KS = ks / 100)

  DT::datatable(d, rownames = FALSE, class = "compact", options = opciones_tabla) |>
    DT::formatPercentage(c("AUC", "Gini", "KS"), digits = 1, dec.mark = ",") |>
    barra("Gini", d$Gini)
}

tabla_distribucion <- function(dist) {
  d <- dist |>
    dplyr::mutate(serie = ifelse(serie == "Periodo base", "base", "mes")) |>
    dplyr::select(bin, serie, pct, tasa_pago) |>
    tidyr::pivot_wider(names_from = serie, values_from = c(pct, tasa_pago)) |>
    dplyr::arrange(bin) |>
    dplyr::transmute(
      Bin = bin, `% base` = pct_base, `% mes` = pct_mes,
      `Tasa de pago base` = tasa_pago_base, `Tasa de pago mes` = tasa_pago_mes
    )

  DT::datatable(d, rownames = FALSE, class = "compact", options = opciones_tabla) |>
    DT::formatPercentage(c("% base", "% mes", "Tasa de pago base", "Tasa de pago mes"),
                         digits = 1, dec.mark = ",")
}
