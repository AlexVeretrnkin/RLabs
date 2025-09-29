# =============================
# Analyze NPM downloads with Frameworks.R (ABS values)
# =============================

# 1) Read & rename to GT-like shape
pkgs <- c("readr","dplyr","lubridate","ggplot2","tidyr","scales")
for (p in pkgs) if (!require(p, character.only=TRUE)) install.packages(p, quiet=TRUE)
invisible(lapply(pkgs, require, character.only=TRUE))

in_csv_raw <- "out/npm_downloads_wide_monthly.csv"   # твій файл з попереднього кроку
out_dir    <- "ts_out/forecast_RAV_monthly_npm"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

dat <- readr::read_csv(in_csv_raw, show_col_types = FALSE) |>
  dplyr::rename(
    date    = period,
    React   = react,
    Vue     = vue,
    Angular = `@angular/core`
  ) |>
  dplyr::arrange(date)

# 2) Підключаємо твій тулкіт
source("Frameworks.R")

# 3) Переозначаємо 2 функції з Frameworks.R для ABS значень (без кліпу [0,100])
clip01 <- function(x) x  # без обрізання

forecast_plot <- function(x, fc, title = "Прогноз (NPM)") {
  df_act <- ts_to_df_monthly(x)
  last_date <- max(df_act$Date)
  last_val  <- tail(df_act$Value, 1)
  h         <- length(fc$mean)
  dates     <- future_months(last_date, h)
  
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

# --- Override: стабільний ARIMA для NPM (log1p, без дрейфу, з фолбеком) ---
fit_and_forecast <- function(x, h = 24) {
  # 1) Робастне очищення від викидів та пропусків
  x_clean <- tryCatch(forecast::tsclean(x), error = function(e) x)
  # 2) Лог-масштаб з нуль-безпечною трансформацією
  y_log <- log1p(as.numeric(x_clean))
  
  # 3) Спроба авто-ARIMA на логах, але з обмеженнями
  fit_try <- try(
    forecast::auto.arima(
      y_log,
      seasonal      = TRUE,
      d             = 1,    # фіксуємо інтеграцію
      D             = 1,
      max.d         = 1,
      max.D         = 1,
      stepwise      = TRUE,
      approximation = FALSE,
      allowmean     = FALSE,
      allowdrift    = FALSE
    ),
    silent = TRUE
  )
  
  if (inherits(fit_try, "try-error")) {
    fit_try <- NULL
  }
  
  # 4) Якщо авто-ARIMA невдала або дає непридатний прогноз — фолбек на airline
  use_fallback <- FALSE
  if (!is.null(fit_try)) {
    fc_try <- forecast::forecast(fit_try, h = h)
    bad <- any(!is.finite(fc_try$mean)) ||
      max(fc_try$mean, na.rm = TRUE) > 50 + log1p(max(x_clean, na.rm = TRUE)) # грубий поріг
    if (bad) use_fallback <- TRUE
  } else {
    use_fallback <- TRUE
  }
  
  if (use_fallback) {
    fit_try <- forecast::Arima(
      y_log,
      order   = c(0,1,1),                         # airline
      seasonal= list(order = c(0,1,1), period=12),
      include.drift = FALSE, include.mean = FALSE
    )
    fc_try <- forecast::forecast(fit_try, h = h)
  }
  
  # 5) Акуратний бек-трансформ у натуральний масштаб
  inv_fc <- function(fc) {
    fc$mean  <- expm1(fc$mean)
    fc$lower <- apply(fc$lower, 2, expm1)
    fc$upper <- apply(fc$upper, 2, expm1)
    # без від’ємних
    fc$mean[!is.finite(fc$mean)] <- NA_real_
    fc$lower[!is.finite(fc$lower)] <- NA_real_
    fc$upper[!is.finite(fc$upper)] <- NA_real_
    fc
  }
  fc_arima <- inv_fc(fc_try)
  
  # ETS залишаємо як було (на сирих значеннях)
  fit_ets <- forecast::ets(x_clean)
  fc_ets  <- forecast::forecast(fit_ets, h = h)
  
  list(
    arima = list(fit = fit_try, fc = fc_arima),   # fit у лог-просторі — це ок
    ets   = list(fit = fit_ets, fc = fc_ets),
    lambda = 0
  )
}


# 4) Запускаємо analyze_many з реальної стартової дати
cols <- intersect(c("React","Angular","Vue"), names(dat))
stopifnot(length(cols) > 0)

res <- analyze_many(
  df          = dat,
  columns     = cols,
  start_year  = lubridate::year(min(dat$date)),
  start_month = lubridate::month(min(dat$date)),
  freq        = 12,
  drop_tail   = 0,
  horizon     = 24
)

# 5) Зберігаємо графіки як у GT-потоці
for (nm in cols) {
  if (!is.null(res[[nm]]$plots$fc_arima)) {
    ggsave(file.path(out_dir, sprintf("%s_fc_arima.png", nm)), res[[nm]]$plots$fc_arima,
           width = 11, height = 4.5, dpi = 300)
  }
  if (!is.null(res[[nm]]$plots$fc_ets)) {
    ggsave(file.path(out_dir, sprintf("%s_fc_ets.png", nm)), res[[nm]]$plots$fc_ets,
           width = 11, height = 4.5, dpi = 300)
  }
  if (!is.null(res[[nm]]$plots$decomp)) {
    ggsave(file.path(out_dir, sprintf("%s_decomp.png", nm)), res[[nm]]$plots$decomp,
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

# 7) ACF/PACF для залишків (ARIMA & ETS)
for (nm in cols) {
  # --- ARIMA ---
  if (!is.null(res[[nm]]$models$arima$fit)) {
    acf_arima  <- forecast::ggAcf(residuals(res[[nm]]$models$arima$fit), lag.max = 48) +
      ggtitle(paste(nm, "— ACF ARIMA залишків"))
    pacf_arima <- forecast::ggPacf(residuals(res[[nm]]$models$arima$fit), lag.max = 48) +
      ggtitle(paste(nm, "— PACF ARIMA залишків"))
    ggsave(file.path(out_dir, sprintf("%s_arima_resid_acf.png", nm)), acf_arima,
           width = 9, height = 4.5, dpi = 300)
    ggsave(file.path(out_dir, sprintf("%s_arima_resid_pacf.png", nm)), pacf_arima,
           width = 9, height = 4.5, dpi = 300)
  }
  
  # --- ETS ---
  if (!is.null(res[[nm]]$models$ets$fit)) {
    acf_ets  <- forecast::ggAcf(residuals(res[[nm]]$models$ets$fit), lag.max = 48) +
      ggtitle(paste(nm, "— ACF ETS залишків"))
    pacf_ets <- forecast::ggPacf(residuals(res[[nm]]$models$ets$fit), lag.max = 48) +
      ggtitle(paste(nm, "— PACF ETS залишків"))
    ggsave(file.path(out_dir, sprintf("%s_ets_resid_acf.png", nm)), acf_ets,
           width = 9, height = 4.5, dpi = 300)
    ggsave(file.path(out_dir, sprintf("%s_ets_resid_pacf.png", nm)), pacf_ets,
           width = 9, height = 4.5, dpi = 300)
  }
}

# --- 8) Спільні графіки за весь період ---
long <- dat |>
  tidyr::pivot_longer(all_of(cols), names_to = "Framework", values_to = "Downloads")

# сучасний підпис SI (k/M/B) — scales >= 1.2:
si_labels <- label_number(scale_cut = cut_si("unit"))

# 8.1 Абсолютні значення (лінії в одному графіку)
p_abs <- ggplot(long, aes(date, Downloads, color = Framework)) +
  geom_line(linewidth = 0.9) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = si_labels) +
  labs(title = "NPM downloads — React • Angular • Vue (весь період)",
       x = NULL, y = "NPM downloads", color = "Framework") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")
ggsave(file.path(out_dir, "RAV_alltime_absolute.png"), p_abs,
       width = 12, height = 6, dpi = 300)

# 8.2 Те саме, але в log10-шкалі
p_log <- ggplot(long, aes(date, Downloads, color = Framework)) +
  geom_line(linewidth = 0.9) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_log10(labels = si_labels) +
  labs(title = "NPM downloads (log10) — React • Angular • Vue",
       x = NULL, y = "log10(NPM downloads)", color = "Framework") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")
ggsave(file.path(out_dir, "RAV_alltime_log10.png"), p_log,
       width = 12, height = 6, dpi = 300)

# 8.3 Індексовані криві: 100 = перший валідний місяць кожного фреймворку
long_indexed <- long |>
  dplyr::group_by(Framework) |>
  dplyr::mutate(
    # перший НЕ-NA і НЕ-нуль (щоб уникнути ділення на нуль/аномалій на старті)
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
  labs(title = "Індексовані криві (100 = перший повний місяць кожного фреймворку)",
       x = NULL, y = "Index (first non-zero month = 100)", color = "Framework") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")
ggsave(file.path(out_dir, "RAV_alltime_index100.png"), p_idx,
       width = 12, height = 6, dpi = 300)

message("Готово. Вихід: ", out_dir)
