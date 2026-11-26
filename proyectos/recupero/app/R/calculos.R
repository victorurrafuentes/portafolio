# ============================================================================
# calculos.R
# Funciones de calculo del panel: no usan Shiny ni graficos, solo datos.
# Cada funcion recibe un data frame y devuelve un data frame (o una lista).
# ============================================================================

# ---- Lectura ---------------------------------------------------------------

leer_datos <- function(carpeta = "data") {
  leer <- function(archivo) {
    readr::read_csv(file.path(carpeta, archivo), show_col_types = FALSE)
  }

  datos <- list(
    pagos  = leer("seguimiento_pagos.csv"),
    estab  = leer("seguimiento_estabilidad.csv"),
    modelo = leer("seguimiento_modelo.csv")
  )

  # Etiquetas de mes ordenadas cronologicamente
  etiquetas <- datos$pagos |>
    dplyr::distinct(mes_idx, mes_label) |>
    dplyr::arrange(mes_idx) |>
    dplyr::pull(mes_label)

  datos |>
    lapply(function(df) dplyr::mutate(df, mes_label = factor(mes_label, levels = etiquetas)))
}

# ---- Pagos y recupero --------------------------------------------------

# Agrega la tabla de pagos para una base de veintil y un conjunto de tramos
agregar_pagos <- function(pagos, base, tramos) {
  pagos |>
    dplyr::filter(base_veintil == base, tramo %in% tramos) |>
    dplyr::group_by(mes_idx, mes_label, periodo, veintil) |>
    dplyr::summarise(
      dplyr::across(c(total, pagadores, deuda_total, monto_recuperado), sum),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      tasa_pago  = pagadores / total,
      tasa_recup = monto_recuperado / deuda_total
    )
}

# Linea base por veintil: tasa de pago ponderada y monto promedio mensual
linea_base_veintil <- function(agregado) {
  agregado |>
    dplyr::filter(periodo == "ENTRENAMIENTO") |>
    dplyr::group_by(veintil) |>
    dplyr::summarise(
      tasa_pago        = sum(pagadores) / sum(total),
      monto_recuperado = sum(monto_recuperado) / dplyr::n_distinct(mes_idx),
      .groups = "drop"
    )
}

# Totales por mes (todos los veintiles juntos)
totales_mensuales <- function(agregado) {
  agregado |>
    dplyr::group_by(mes_idx, mes_label, periodo) |>
    dplyr::summarise(
      dplyr::across(c(total, pagadores, deuda_total, monto_recuperado), sum),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      tasa_pago  = pagadores / total,
      tasa_recup = monto_recuperado / deuda_total
    )
}

# Compara un mes contra el promedio mensual del periodo base
comparar_con_base <- function(totales, mes_elegido) {
  base <- totales |>
    dplyr::filter(periodo == "ENTRENAMIENTO") |>
    dplyr::summarise(
      monto      = mean(monto_recuperado),
      tasa_pago  = mean(tasa_pago),
      tasa_recup = mean(tasa_recup)
    )

  mes <- totales |> dplyr::filter(mes_label == mes_elegido)

  list(
    base = base,
    mes  = mes,
    razon_monto = mes$monto_recuperado / base$monto,
    razon_pago  = mes$tasa_pago / base$tasa_pago,
    razon_recup = mes$tasa_recup / base$tasa_recup
  )
}

# ---- Estabilidad (PSI) -----------------------------------------------------

# PSI por variable y mes, contra la distribucion del periodo base
psi_por_mes <- function(estab, piso = 1e-4) {
  base <- estab |>
    dplyr::filter(periodo == "ENTRENAMIENTO") |>
    dplyr::group_by(variable, bin) |>
    dplyr::summarise(n = sum(n), .groups = "drop") |>
    dplyr::group_by(variable) |>
    dplyr::mutate(p_base = n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::select(variable, bin, p_base)

  estab |>
    dplyr::filter(periodo != "ENTRENAMIENTO") |>
    dplyr::group_by(mes_idx, mes_label, variable) |>
    dplyr::mutate(p_mes = n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::left_join(base, by = c("variable", "bin")) |>
    dplyr::mutate(
      a = pmax(p_mes, piso),
      b = pmax(p_base, piso),
      aporte = (a - b) * log(a / b)
    ) |>
    dplyr::group_by(mes_idx, mes_label, variable) |>
    dplyr::summarise(psi = sum(aporte), .groups = "drop")
}

# Distribucion por bin: periodo base frente a un mes
distribucion_bins <- function(estab, variable_elegida, mes_elegido) {
  filtrado <- estab |> dplyr::filter(variable == variable_elegida)

  base <- filtrado |>
    dplyr::filter(periodo == "ENTRENAMIENTO") |>
    dplyr::group_by(bin) |>
    dplyr::summarise(pagadores = sum(n * dplyr::coalesce(tasa_pago, 0)), n = sum(n), .groups = "drop") |>
    dplyr::filter(n > 0) |>
    dplyr::mutate(serie = "Periodo base", pct = n / sum(n), tasa_pago = pagadores / n)

  mes <- filtrado |>
    dplyr::filter(mes_label == mes_elegido, n > 0) |>
    dplyr::mutate(pagadores = n * dplyr::coalesce(tasa_pago, 0)) |>
    dplyr::mutate(serie = as.character(mes_elegido), pct = n / sum(n)) |>
    dplyr::select(bin, n, pagadores, serie, pct, tasa_pago)

  dplyr::bind_rows(base |> dplyr::select(bin, n, pagadores, serie, pct, tasa_pago), mes)
}

# ---- Poder predictivo por variable (IV y KS) -------------------------------

# Calcula IV y KS de cada variable a partir de los bins, por periodo
iv_ks_por_periodo <- function(estab, piso = 1e-4) {
  estab |>
    dplyr::mutate(
      pagan    = n * dplyr::coalesce(tasa_pago, 0),
      no_pagan = n - pagan
    ) |>
    dplyr::group_by(variable, periodo, bin) |>
    dplyr::summarise(pagan = sum(pagan), no_pagan = sum(no_pagan), .groups = "drop") |>
    dplyr::group_by(variable, periodo) |>
    dplyr::arrange(bin, .by_group = TRUE) |>
    dplyr::mutate(
      p_pagan    = pagan / sum(pagan),
      p_no_pagan = no_pagan / sum(no_pagan),
      woe        = log(pmax(p_pagan, piso) / pmax(p_no_pagan, piso)),
      aporte_iv  = (p_pagan - p_no_pagan) * woe,
      acum_dif   = abs(cumsum(p_pagan) - cumsum(p_no_pagan))
    ) |>
    dplyr::summarise(IV = sum(aporte_iv), KS = max(acum_dif) * 100, .groups = "drop")
}

# ---- Indicadores del modelo (ROC, Gini, KS) --------------------------------

# Curva ROC a partir de un agregado por veintil (20 = mayor probabilidad)
curva_roc <- function(agregado_veintil) {
  d <- agregado_veintil |>
    dplyr::arrange(dplyr::desc(veintil_prob)) |>
    dplyr::mutate(
      no_pagan = total - pagadores,
      tpr = cumsum(pagadores) / sum(pagadores),
      fpr = cumsum(no_pagan) / sum(no_pagan)
    )

  tibble::tibble(fpr = c(0, d$fpr), tpr = c(0, d$tpr))
}

# AUC (trapecios), Gini y KS de una curva ROC
metricas_roc <- function(roc) {
  auc <- sum(diff(roc$fpr) * (utils::head(roc$tpr, -1) + utils::tail(roc$tpr, -1)) / 2)
  tibble::tibble(auc = auc, gini = 2 * auc - 1, ks = max(roc$tpr - roc$fpr) * 100)
}

# Metricas por mes (todos los meses de la tabla del modelo)
metricas_por_mes <- function(modelo) {
  modelo |>
    dplyr::group_by(mes_idx, mes_label, periodo) |>
    dplyr::group_modify(~ metricas_roc(curva_roc(.x))) |>
    dplyr::ungroup()
}

# Agregado del periodo base (para la curva de referencia)
agregado_base_modelo <- function(modelo) {
  modelo |>
    dplyr::filter(periodo == "ENTRENAMIENTO") |>
    dplyr::group_by(veintil_prob) |>
    dplyr::summarise(total = sum(total), pagadores = sum(pagadores), .groups = "drop")
}

# Resumen del poder del modelo por periodo, con los meses de cada periodo agrupados.
# Si se indica un mes, se agrega una fila con ese mes (periodo = "MES").
resumen_periodos <- function(modelo, mes_elegido = NULL) {
  fila <- function(df, periodo, base = FALSE) {
    etiquetas <- as.character(df$mes_label[order(df$mes_idx)])
    primero <- etiquetas[1]
    ultimo  <- etiquetas[length(etiquetas)]
    agregado <- df |>
      dplyr::group_by(veintil_prob) |>
      dplyr::summarise(total = sum(total), pagadores = sum(pagadores), .groups = "drop")
    dplyr::bind_cols(
      tibble::tibble(
        periodo = periodo,
        meses   = if (primero == ultimo) primero else paste0(primero, " a ", ultimo),
        base    = base
      ),
      metricas_roc(curva_roc(agregado))
    )
  }

  filas <- lapply(c("ENTRENAMIENTO", "VALIDACION", "SEGUIMIENTO"), function(p) {
    fila(dplyr::filter(modelo, periodo == p), p, base = (p == "ENTRENAMIENTO"))
  })
  if (!is.null(mes_elegido)) {
    filas <- c(filas, list(fila(dplyr::filter(modelo, mes_label == mes_elegido), "MES")))
  }

  tabla <- dplyr::bind_rows(filas)
  tabla$dif_gini <- tabla$gini - tabla$gini[tabla$base]
  tabla
}
