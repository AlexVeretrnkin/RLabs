# =========================
# Framework Popularity: TS Toolkit
# =========================

# ---- deps ----
pkgs <- c("tseries","forecast","healthyR.ts","readr","ggplot2",
          "tidyr","dplyr","patchwork","xts")
for (p in pkgs) if (!require(p, character.only = TRUE)) install.packages(p, quiet = TRUE)
invisible(lapply(pkgs, require, character.only = TRUE))

# ---- helpers ----
make_ts <- function(vec, start_year = 2009, start_month = 1, freq = 12, drop_tail = 0) {
  x <- if (drop_tail > 0) head(vec, -drop_tail) else vec
  ts(as.numeric(x), start = c(start_year, start_month), frequency = freq)
}

plot_series <- function(x, title = "Часовий ряд", ylab = "Значення") {
  df <- ts_to_df_monthly(x)
  ggplot(df, aes(Date, Value)) +
    geom_line(linewidth = 0.9) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title = title, x = "Рік", y = ylab) +
    theme_minimal()
}

smooth_ma_plot <- function(x, orders = c(5,12), title = "Згладжування (MA)") {
  base <- ts_to_df_monthly(x) |> dplyr::mutate(Series = "Оригінал")
  ma_dfs <- lapply(orders, function(o) {
    z <- forecast::ma(x, order = o)
    ts_to_df_monthly(z) |> dplyr::mutate(Series = paste0("MA(", o, ")"))
  }) |> dplyr::bind_rows()
  ggplot() +
    geom_line(data = base, aes(Date, Value, color = Series), linewidth = 0.9) +
    geom_line(data = ma_dfs, aes(Date, Value, color = Series), linewidth = 0.9) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title = title, x = "Рік", y = "Значення", color = "Ряд") +
    theme_minimal()
}

decompose_plot <- function(x, type = "additive", title = "Декомпозиція") {
  dc <- decompose(x, type = type)
  df <- tibble(
    Time = time(x),
    Observed = as.numeric(dc$x),
    Trend = as.numeric(dc$trend),
    Seasonal = as.numeric(dc$seasonal),
    Random = as.numeric(dc$random)
  ) |>
    pivot_longer(-Time, names_to = "Component", values_to = "Value")
  ggplot(df, aes(Time, Value)) +
    geom_line() +
    facet_wrap(~Component, scales = "free_y", ncol = 1) +
    labs(title = title, x = "Рік", y = "Значення") +
    theme_minimal()
}

ts_to_df_monthly <- function(y_ts) {
  stopifnot(frequency(y_ts) == 12)
  n  <- length(y_ts)
  y0 <- start(y_ts)[1]; m0 <- start(y_ts)[2]
  d0 <- as.Date(sprintf("%d-%02d-01", y0, m0))
  dates <- seq(d0, by = "month", length.out = n)
  tibble::tibble(Date = dates, Value = as.numeric(y_ts))
}

future_months <- function(last_date, h) {
  seq(lubridate::floor_date(last_date, "month") %m+% months(1), by = "month", length.out = h)
}

fit_and_forecast <- function(x, h = 24) {
  lambda <- tryCatch(BoxCox.lambda(x, lower = 0, upper = 1), error = function(e) NULL)
  fit_arima <- forecast::auto.arima(
    x,
    lambda  = lambda,   # якщо NULL — auto.arima ігнорує трансформацію
    biasadj = TRUE,
    stepwise = TRUE, approximation = FALSE, seasonal = TRUE
  )
  fc_arima <- forecast::forecast(fit_arima, h = h)
  
  fit_ets <- forecast::ets(x)
  fc_ets  <- forecast::forecast(fit_ets, h = h)
  
  list(arima = list(fit = fit_arima, fc = fc_arima),
       ets   = list(fit = fit_ets,   fc = fc_ets),
       lambda = lambda)
}


clip01 <- function(x) pmin(pmax(x, 0), 100)


stationarity_report <- function(x, seasonal_k = frequency(x)) {
  res_raw <- tryCatch(tseries::adf.test(x, alternative = "stationary", k = seasonal_k), error = function(e) NULL)
  # базові трансформації
  can_log <- all(is.finite(x)) && min(x, na.rm = TRUE) > 0
  x_log <- if (can_log) log(x) else x
  x_diff <- diff(x)
  x_diff_log <- if (can_log) diff(log(x)) else NULL
  
  res <- list(
    adf_raw_p = if (!is.null(res_raw)) res_raw$p.value else NA_real_,
    can_log = can_log
  )
  res$adf_diff_p <- tryCatch(tseries::adf.test(na.omit(x_diff), alternative = "stationary", k = seasonal_k)$p.value, error = function(e) NA_real_)
  if (can_log) {
    res$adf_log_p <- tryCatch(tseries::adf.test(na.omit(x_log), alternative = "stationary", k = seasonal_k)$p.value, error = function(e) NA_real_)
    res$adf_diff_log_p <- tryCatch(tseries::adf.test(na.omit(x_diff_log), alternative = "stationary", k = seasonal_k)$p.value, error = function(e) NA_real_)
  } else {
    res$adf_log_p <- NA_real_
    res$adf_diff_log_p <- NA_real_
  }
  res
}

forecast_plot <- function(x, fc, title = "Прогноз") {
  df_act <- ts_to_df_monthly(x)
  
  last_date <- max(df_act$Date)
  last_val  <- tail(df_act$Value, 1)
  h         <- length(fc$mean)
  dates     <- future_months(last_date, h)
  
  df_fc <- tibble::tibble(
    Date = c(last_date, dates),
    Forecast = clip01(c(last_val, as.numeric(fc$mean))),
    Lo80 = clip01(c(last_val, as.numeric(fc$lower[,"80%"]))),
    Hi80 = clip01(c(last_val, as.numeric(fc$upper[,"80%"]))),
    Lo95 = clip01(c(last_val, as.numeric(fc$lower[,"95%"]))),
    Hi95 = clip01(c(last_val, as.numeric(fc$upper[,"95%"])))
  )
  
  ggplot() +
    geom_line(data = df_act, aes(Date, Value), linewidth = 0.9, color = "black") +
    geom_ribbon(data = df_fc, aes(Date, ymin = Lo95, ymax = Hi95), alpha = 0.15) +
    geom_ribbon(data = df_fc, aes(Date, ymin = Lo80, ymax = Hi80), alpha = 0.25) +
    geom_line(data = df_fc, aes(Date, Forecast), linewidth = 1.1) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title = title, x = "Рік", y = "Індекс 0–100") +
    theme_minimal()
}


residuals_diagnostics <- function(model, lb_lag = 24, title_prefix = "Залишки") {
  res <- residuals(model)
  pval <- tryCatch(Box.test(res, type = "Ljung-Box", lag = lb_lag)$p.value, error = function(e) NA_real_)
  p1 <- ggAcf(res) + ggtitle(paste0(title_prefix, ": ACF"))
  p2 <- ggplot(data.frame(e = res), aes(e)) +
    geom_histogram(aes(y = after_stat(density)), bins = max(10, floor(length(na.omit(res))/10))) +
    stat_function(fun = dnorm, args = list(mean = mean(res, na.rm = TRUE), sd = sd(res, na.rm = TRUE))) +
    ggtitle(paste0(title_prefix, ": Розподіл")) + theme_minimal()
  list(p_value_lb = pval, acf_plot = p1, dist_plot = p2)
}


# ---- основний конвеєр для однієї змінної ----
analyze_series <- function(df, column_name,
                           date_col = NULL,         # опційно: якщо є стовпчик дат (yyyy-mm)
                           start_year = 2009,
                           start_month = 1,
                           freq = 12,
                           drop_tail = 0,
                           horizon = 24,
                           pretty_name = NULL) {
  
  stopifnot(column_name %in% names(df))
  nm <- ifelse(is.null(pretty_name), column_name, pretty_name)
  vec <- df[[column_name]]
  
  x <- make_ts(vec, start_year, start_month, freq, drop_tail)
  
  # Графіки ряду
  p_series <- plot_series(x, title = paste("Популярність", nm), ylab = "Індекс / %")
  p_ma     <- smooth_ma_plot(x, orders = c(5,12), title = paste("Згладжування (", nm, ")", sep = ""))
  
  # Декомпозиція
  p_decomp <- decompose_plot(x, type = "additive", title = paste("Декомпозиція —", nm))
  
  # Стаціонарність
  stat <- stationarity_report(x, seasonal_k = frequency(x))
  
  # Моделі + прогноз
  models <- fit_and_forecast(x, h = horizon)
  p_fc_arima <- forecast_plot(x, models$arima$fc, title = paste("Прогноз ARIMA —", nm))
  p_fc_ets   <- forecast_plot(x, models$ets$fc,   title = paste("Прогноз ETS (Holt-Winters) —", nm))
  
  # Діагностика залишків
  diag_arima <- residuals_diagnostics(models$arima$fit, title_prefix = paste("ARIMA —", nm))
  diag_ets   <- residuals_diagnostics(models$ets$fit,   title_prefix = paste("ETS —", nm))
  
  # Якість (in-sample) базово
  acc_arima <- tryCatch(accuracy(models$arima$fit), error = function(e) NULL)
  acc_ets   <- tryCatch(accuracy(models$ets$fit),   error = function(e) NULL)
  
  list(
    name = nm,
    ts = x,
    plots = list(
      series = p_series,
      ma = p_ma,
      decomp = p_decomp,
      fc_arima = p_fc_arima,
      fc_ets = p_fc_ets,
      resid_acf_arima = diag_arima$acf_plot,
      resid_dist_arima = diag_arima$dist_plot,
      resid_acf_ets = diag_ets$acf_plot,
      resid_dist_ets = diag_ets$dist_plot
    ),
    stationarity = stat,
    models = models,
    diagnostics = list(
      ljung_box_p_arima = diag_arima$p_value_lb,
      ljung_box_p_ets   = diag_ets$p_value_lb
    ),
    accuracy = list(arima = acc_arima, ets = acc_ets)
  )
}

# ---- пакетна обробка для кількох колонок ----
analyze_many <- function(df, columns,
                         start_year = 2009,
                         start_month = 1,
                         freq = 12,
                         drop_tail = 0,
                         horizon = 24,
                         pretty_names = NULL) {
  if (!is.null(pretty_names)) stopifnot(length(pretty_names) == length(columns))
  results <- vector("list", length(columns))
  names(results) <- columns
  for (i in seq_along(columns)) {
    results[[i]] <- analyze_series(
      df = df, column_name = columns[i],
      start_year = start_year, start_month = start_month, freq = freq,
      drop_tail = drop_tail, horizon = horizon,
      pretty_name = if (is.null(pretty_names)) columns[i] else pretty_names[i]
    )
  }
  results
}

acf_pacf_residuals <- function(fit, max_lag = 48, title_prefix = "Залишки") {
  r <- residuals(fit)
  list(
    acf  = forecast::ggAcf(na.omit(r),  lag.max = max_lag) + ggtitle(paste0(title_prefix, " — ACF")),
    pacf = forecast::ggPacf(na.omit(r), lag.max = max_lag) + ggtitle(paste0(title_prefix, " — PACF"))
  )
}


# setwd("F:/RLabs")
# data <- readr::read_csv("search_engine_data.csv")
# res <- analyze_many(
#  df = data,
#  columns = c("Google"),            # або: c("React","Angular","Vue","Svelte","Next")
#  start_year = 2009,
#  start_month = 1,
#  freq = 12,
#  drop_tail = 10,                   # як у твоєму коді
#  horizon = 24
# )
 # Вивести будь-який графік:
# print(res$Google$plots$series)
#print(res$Google$plots$fc_arima)
#print(res$Google$plots$fc_ets)
#res$Google$stationarity
#res$Google$diagnostics
#res$Google$accuracy
 
#arima_plots <- acf_pacf_residuals(res$Google$models$arima$fit, max_lag = 12, title_prefix = "ARIMA — Google")
#ets_plots   <- acf_pacf_residuals(res$Google$models$ets$fit,   max_lag = 12, title_prefix = "ETS (HW) — Google")
 
#print(arima_plots$acf);  print(arima_plots$pacf)
#print(ets_plots$acf);    print(ets_plots$pacf)
 
#forecast::checkresiduals(res$Google$models$arima$fit)  # робить Ljung–Box + графіки
#forecast::checkresiduals(res$Google$models$ets$fit)
 
