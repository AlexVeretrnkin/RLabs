# =============================
# Analyze NPM downloads (ABS) — aligned start, robust ARIMA
# =============================

# 0) deps
pkgs <- c("readr","dplyr","lubridate","ggplot2","tidyr","scales","forecast","zoo","tibble","purrr")
for (p in pkgs) if (!require(p, character.only=TRUE)) install.packages(p, quiet=TRUE)
invisible(lapply(pkgs, require, character.only=TRUE))

in_csv_raw <- "out/npm_downloads_wide_monthly.csv"   # <-- твій файл
out_dir    <- "ts_out/forecast_RAV_monthly_npm"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 1) Read & rename
dat <- readr::read_csv(in_csv_raw, show_col_types = FALSE) |>
  dplyr::rename(
    date    = period,
    React   = react,
    Vue     = vue,
    Angular = `@angular/core`
  ) |>
  dplyr::arrange(date)

# 2) Align common start (drop leading zeros per series → take max of starts)
df_long <- dat |>
  tidyr::pivot_longer(c(React, Angular, Vue), names_to = "series", values_to = "value")

nz_starts <- df_long |>
  dplyr::group_by(series) |>
  dplyr::summarise(start = min(date[value > 0], na.rm = TRUE), .groups = "drop")

global_start <- max(nz_starts$start, na.rm = TRUE)
message("🔹 Common start for all series: ", as.character(global_start))

df_long_trimmed <- df_long |>
  dplyr::filter(date >= global_start)

dat <- df_long_trimmed |>
  tidyr::pivot_wider(names_from = series, values_from = value) |>
  dplyr::arrange(date)

# 3) (опціонально) якщо користуєшся своїм ядром
# source("Frameworks.R")

# ---------- Robust helpers (dates) ----------
ts_to_df_monthly <- function(x) {
  tt <- zoo::as.yearmon(time(x))
  tibble::tibble(
    Date  = as.Date(tt),
    Value = as.numeric(x)
  )
}
to_month_date <- function(d) {
  if (inherits(d, "Date"))   return(lubridate::floor_date(d, "month"))
  if (inherits(d, "POSIXt")) return(lubridate::floor_date(as.Date(d), "month"))
  if (is.numeric(d))         return(as.Date(zoo::as.yearmon(d)))
  as.Date(character())
}
future_months <- function(last_date, h) {
  if (!is.finite(h) || h <= 0) return(as.Date(character()))
  ld <- to_month_date(last_date)
  if (length(ld) == 0 || is.na(ld)) return(as.Date(character()))
  seq(from = ld %m+% months(1), by = "month", length.out = h)
}

# ---------- Plot override ----------
forecast_plot <- function(x, fc, title = "Прогноз (NPM)") {
  df_act <- ts_to_df_monthly(x)
  if (nrow(df_act) == 0 || all(!is.finite(df_act$Value))) {
    stop("Порожній або невалідний ряд у forecast_plot()")
  }
  last_date <- suppressWarnings(max(df_act$Date, na.rm = TRUE))
  h <- if (!is.null(fc) && !is.null(fc$mean)) length(fc$mean) else 0
  
  if (h == 0) {
    return(
      ggplot(df_act, aes(Date, Value)) +
        geom_line(linewidth = 0.9) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        labs(title = paste(title, "(без прогнозу)"), x = "Рік", y = "NPM downloads") +
        theme_minimal()
    )
  }
  
  dates <- future_months(last_date, h)
  if (length(dates) != h) {
    return(
      ggplot(df_act, aes(Date, Value)) +
        geom_line(linewidth = 0.9) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        labs(title = paste(title, "(без прогнозу)"), x = "Рік", y = "NPM downloads") +
        theme_minimal()
    )
  }
  
  last_val <- tail(df_act$Value, 1)
  df_fc <- tibble::tibble(
    Date     = c(last_date, dates),
    Forecast = c(last_val, as.numeric(fc$mean)),
    Lo80     = c(last_val, as.numeric(fc$lower[,"80%"])),
    Hi80     = c(last_val, as.numeric(fc$upper[,"80%"])),
    Lo95     = c(last_val, as.numeric(fc$lower[,"95%"])),
    Hi95     = c(last_val, as.numeric(fc$upper[,"95%"]))
  )
  
  ggplot() +
    geom_line(data = df_act, aes(Date, Value), linewidth = 0.9, color = "black") +
    geom_ribbon(data = df_fc, aes(Date, ymin = Lo95, ymax = Hi95), alpha = 0.15) +
    geom_ribbon(data = df_fc, aes(Date, ymin = Lo80, ymax = Hi80), alpha = 0.25) +
    geom_line(data = df_fc, aes(Date, Forecast), linewidth = 1.1) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title = title, x = "Рік", y = "NPM downloads") +
    theme_minimal()
}

# ---------- Stable ARIMA (lambda=0 + biasadj, fallback to ETS-damped) ----------
fit_and_forecast <- function(x, h = 24) {
  x_clean <- tryCatch(forecast::tsclean(x), error = function(e) x)
  
  fit_a <- tryCatch(
    forecast::auto.arima(
      x_clean,
      lambda        = 0,       # log трансформація всередині
      biasadj       = TRUE,    # корекція інверсії
      seasonal      = TRUE,
      stepwise      = TRUE,
      approximation = FALSE,
      allowmean     = FALSE,
      allowdrift    = FALSE
    ),
    error = function(e) NULL
  )
  fc_a <- tryCatch(forecast::forecast(fit_a, h = h), error = function(e) NULL)
  
  fit_e <- tryCatch(forecast::ets(x_clean, model = "ZZZ", damped = TRUE), error = function(e) NULL)
  fc_e  <- tryCatch(forecast::forecast(fit_e, h = h), error = function(e) NULL)
  
  use_arima <- !is.null(fc_a)
  if (use_arima) {
    last_val <- tail(as.numeric(x_clean), 1)
    max_fc   <- suppressWarnings(max(as.numeric(fc_a$mean), na.rm = TRUE))
    if (!is.finite(max_fc) || (max_fc > 20 * last_val)) use_arima <- FALSE
  }
  
  list(
    arima = list(fit = if (use_arima) fit_a else NULL, fc = if (use_arima) fc_a else NULL),
    ets   = list(fit = fit_e, fc = fc_e),
    lambda = 0
  )
}

# ---------- Minimal analyze_many (якщо не використовуєш Frameworks.R) ----------
make_ts <- function(vec, start_year, start_month, freq = 12) {
  ts(as.numeric(vec), start = c(start_year, start_month), frequency = freq)
}
analyze_one <- function(series_name, df, start_year, start_month, freq = 12, horizon = 24) {
  x <- make_ts(df[[series_name]], start_year, start_month, freq)
  fits <- fit_and_forecast(x, h = horizon)
  
  plots <- list(
    fc_arima = if (!is.null(fits$arima$fc)) forecast_plot(x, fits$arima$fc, paste(series_name, "— ARIMA")) else NULL,
    fc_ets   = if (!is.null(fits$ets$fc))   forecast_plot(x, fits$ets$fc,   paste(series_name, "— ETS"))   else NULL
  )
  
  lb_a <- tryCatch(Box.test(residuals(fits$arima$fit), type="Ljung-Box", lag = 12,
                            fitdf = if (!is.null(fits$arima$fit)) length(fits$arima$fit$coef) else 0)$p.value,
                   error = function(e) NA_real_)
  lb_e <- tryCatch(Box.test(residuals(fits$ets$fit), type="Ljung-Box", lag = 12,
                            fitdf = if (!is.null(fits$ets$fit)) length(fits$ets$fit$par) else 0)$p.value,
                   error = function(e) NA_real_)
  
  list(models = fits, plots = plots,
       diagnostics = list(ljung_box_p_arima = lb_a, ljung_box_p_ets = lb_e))
}
analyze_many <- function(df, columns, start_year, start_month, freq = 12, horizon = 24) {
  res <- list()
  for (nm in columns) {
    res[[nm]] <- analyze_one(nm, df, start_year, start_month, freq, horizon)
  }
  res
}

# ---------- Run ----------
cols <- intersect(c("React","Angular","Vue"), names(dat))
stopifnot(length(cols) > 0)

res <- analyze_many(
  df          = dat,
  columns     = cols,
  start_year  = lubridate::year(min(dat$date)),
  start_month = lubridate::month(min(dat$date)),
  freq        = 12,
  horizon     = 24
)

# 5) Save plots
for (nm in cols) {
  if (!is.null(res[[nm]]$plots$fc_arima)) {
    ggsave(file.path(out_dir, sprintf("%s_fc_arima.png", nm)), res[[nm]]$plots$fc_arima,
           width = 11, height = 4.5, dpi = 300)
  }
  if (!is.null(res[[nm]]$plots$fc_ets)) {
    ggsave(file.path(out_dir, sprintf("%s_fc_ets.png", nm)), res[[nm]]$plots$fc_ets,
           width = 11, height = 4.5, dpi = 300)
  }
}

# 6) Ljung–Box summary
diag_rows <- lapply(cols, function(nm) {
  lb_a <- res[[nm]]$diagnostics$ljung_box_p_arima
  lb_e <- res[[nm]]$diagnostics$ljung_box_p_ets
  tibble::tibble(series = nm, lb_p_arima = lb_a, lb_p_ets = lb_e)
})
readr::write_csv(dplyr::bind_rows(diag_rows), file.path(out_dir, "ljung_box_summary.csv"))

# 7) ACF/PACF для залишків
for (nm in cols) {
  if (!is.null(res[[nm]]$models$arima$fit)) {
    acf_arima  <- forecast::ggAcf(residuals(res[[nm]]$models$arima$fit), lag.max = 48) +
      ggtitle(paste(nm, "— ACF ARIMA залишків"))
    pacf_arima <- forecast::ggPacf(residuals(res[[nm]]$models$arima$fit), lag.max = 48) +
      ggtitle(paste(nm, "— PACF ARIMA залишків"))
    ggsave(file.path(out_dir, sprintf("%s_arima_resid_acf.png", nm)), acf_arima, width = 9, height = 4.5, dpi = 300)
    ggsave(file.path(out_dir, sprintf("%s_arima_resid_pacf.png", nm)), pacf_arima, width = 9, height = 4.5, dpi = 300)
  }
  if (!is.null(res[[nm]]$models$ets$fit)) {
    acf_ets  <- forecast::ggAcf(residuals(res[[nm]]$models$ets$fit), lag.max = 48) +
      ggtitle(paste(nm, "— ACF ETS залишків"))
    pacf_ets <- forecast::ggPacf(residuals(res[[nm]]$models$ets$fit), lag.max = 48) +
      ggtitle(paste(nm, "— PACF ETS залишків"))
    ggsave(file.path(out_dir, sprintf("%s_ets_resid_acf.png", nm)), acf_ets, width = 9, height = 4.5, dpi = 300)
    ggsave(file.path(out_dir, sprintf("%s_ets_resid_pacf.png", nm)), pacf_ets, width = 9, height = 4.5, dpi = 300)
  }
}

# 8) Overview plots (ABS / log / index)
si_labels <- label_number(scale_cut = cut_si("unit"))

long <- dat |>
  tidyr::pivot_longer(all_of(cols), names_to = "Framework", values_to = "Downloads")

p_abs <- ggplot(long, aes(date, Downloads, color = Framework)) +
  geom_line(linewidth = 0.9) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = si_labels) +
  labs(title = "NPM downloads — React • Angular • Vue (aligned start)",
       x = NULL, y = "NPM downloads", color = "Framework") +
  theme_minimal(base_size = 13) + theme(legend.position = "top")
ggsave(file.path(out_dir, "RAV_alltime_absolute_aligned.png"), p_abs, width = 12, height = 6, dpi = 300)

p_log <- ggplot(long, aes(date, Downloads, color = Framework)) +
  geom_line(linewidth = 0.9) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_log10(labels = si_labels) +
  labs(title = "NPM downloads (log10) — React • Angular • Vue (aligned start)",
       x = NULL, y = "log10(NPM downloads)", color = "Framework") +
  theme_minimal(base_size = 13) + theme(legend.position = "top")
ggsave(file.path(out_dir, "RAV_alltime_log10_aligned.png"), p_log, width = 12, height = 6, dpi = 300)

long_indexed <- long |>
  dplyr::group_by(Framework) |>
  dplyr::mutate(
    .first_idx = which((!is.na(Downloads)) & (Downloads > 0))[1],
    base = ifelse(is.na(.first_idx), NA_real_, Downloads[.first_idx]),
    Index = 100 * Downloads / base
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-.first_idx)

p_idx <- ggplot(long_indexed, aes(date, Index, color = Framework)) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  geom_line(linewidth = 0.9) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Indexed (100 = first non-zero, aligned start)",
       x = NULL, y = "Index", color = "Framework") +
  theme_minimal(base_size = 13) + theme(legend.position = "top")
ggsave(file.path(out_dir, "RAV_alltime_index100_aligned.png"), p_idx, width = 12, height = 6, dpi = 300)

message("Готово. Вихід: ", out_dir)
