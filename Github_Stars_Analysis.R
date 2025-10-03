# =============================
# Analyze GitHub Stars with Frameworks.R (ABS values)
# ABS значення для аналізу — це МІСЯЧНІ ДОДАННЯ зірок (сума day-stars за місяць).

# 1) Deps & I/O
pkgs <- c("readr","dplyr","lubridate","ggplot2","tidyr","scales","forecast")
for (p in pkgs) if (!require(p, character.only=TRUE)) install.packages(p, quiet=TRUE)
invisible(lapply(pkgs, require, character.only=TRUE))

# Шляхи до CSV із Daily Stars Explorer
csv_vue     <- "git_stars/vuejs_core-stars-history.csv"     # vuejs/core
csv_react   <- "git_stars/facebook_react-stars-history.csv"  # facebook/react
csv_angular <- "git_stars/angular_angular-stars-history.csv"  # angular/angular

out_dir <- "ts_out/forecast_RAV_monthly_github_stars"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 2) Підключаємо тулкіт
source("Frameworks.R")

# 3) Переозначаємо 2 функції з Frameworks.R для ABS значень (без кліпу [0,100])
clip01 <- function(x) x  # без обрізання

forecast_plot <- function(x, fc, title = "Прогноз (GitHub Stars)") {
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
    labs(title = title, x = "Рік", y = "GitHub stars (monthly adds)") +
    theme_minimal()
}

# 4) Зчитуємо та готуємо дані (місячні суми day-stars)
read_dse <- function(path){
  df <- readr::read_csv(path, show_col_types = FALSE)
  names(df) <- tolower(names(df))
  stopifnot(all(c("date","day-stars","total-stars") %in% names(df)))
  df |>
    dplyr::mutate(date = lubridate::dmy(date)) |>
    dplyr::arrange(date) |>
    dplyr::mutate(month = lubridate::floor_date(date, "month", week_start = 1)) |>
    dplyr::group_by(month) |>
    dplyr::summarise(value = sum(`day-stars`, na.rm = TRUE), .groups = "drop")
}

vue_m     <- read_dse(csv_vue)     |> dplyr::rename(Vue = value)
react_m   <- read_dse(csv_react)   |> dplyr::rename(React = value)
angular_m <- read_dse(csv_angular) |> dplyr::rename(Angular = value)

# Об'єднуємо по місяцях у wide, як у GT-потоці
# Важливо: робимо full_join, щоб не втратити ранні/пізні періоди, далі впорядковуємо
monthly_wide <- list(vue_m, react_m, angular_m) |>
  Reduce(function(a,b) dplyr::full_join(a,b, by = "month"), x = _) |>
  dplyr::arrange(month)

# Перейменовуємо у стандартний формат тулкіта: period + стовпці за фреймворками
dat <- monthly_wide |>
  dplyr::rename(period = month) |>
  dplyr::select(period, React, Angular, Vue)

# 5) Запускаємо analyze_many з реальної стартової дати
cols <- intersect(c("React","Angular","Vue"), names(dat))
stopifnot(length(cols) > 0)

res <- analyze_many(
  df          = dat,
  columns     = cols,
  start_year  = lubridate::year(min(dat$period, na.rm = TRUE)),
  start_month = lubridate::month(min(dat$period, na.rm = TRUE)),
  freq        = 12,
  drop_tail   = 0,
  horizon     = 24
)

# 6) Збереження прогнозів/декомпозицій як у GT-потоці
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

# 7) Ljung–Box summary
diag_rows <- lapply(cols, function(nm) {
  lb_a <- res[[nm]]$diagnostics$ljung_box_p_arima
  lb_e <- res[[nm]]$diagnostics$ljung_box_p_ets
  tibble::tibble(series = nm, lb_p_arima = lb_a, lb_p_ets = lb_e)
})
readr::write_csv(dplyr::bind_rows(diag_rows), file.path(out_dir, "ljung_box_summary.csv"))

# 8) ACF/PACF для залишків (ARIMA & ETS)
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

# 9) Спільні графіки на весь період
long <- dat |>
  tidyr::pivot_longer(all_of(cols), names_to = "Framework", values_to = "StarsMonthly")

si_labels <- label_number(scale_cut = cut_si("unit"))

# 9.1 Абсолютні значення (місячні додавання зірок)
p_abs <- ggplot(long, aes(period, StarsMonthly, color = Framework)) +
  geom_line(linewidth = 0.9) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = si_labels) +
  labs(title = "GitHub Stars (monthly adds) — React • Angular • Vue",
       x = NULL, y = "GitHub stars (monthly adds)", color = "Framework") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave(file.path(out_dir, "RAV_github_stars_monthly_abs.png"), p_abs,
       width = 12, height = 6, dpi = 300)

# 9.2 Те саме, але в log10-шкалі
p_log <- ggplot(long, aes(period, StarsMonthly, color = Framework)) +
  geom_line(linewidth = 0.9) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_log10(labels = si_labels) +
  labs(title = "GitHub Stars (monthly adds, log10) — React • Angular • Vue",
       x = NULL, y = "log10(monthly adds)", color = "Framework") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave(file.path(out_dir, "RAV_github_stars_monthly_log10.png"), p_log,
       width = 12, height = 6, dpi = 300)

# 9.3 Індексовані криві: 100 = перший НЕ-нульовий місяць для кожного фреймворку
long_indexed <- long |>
  dplyr::group_by(Framework) |>
  dplyr::mutate(
    .first_idx = which((!is.na(StarsMonthly)) & (StarsMonthly > 0))[1],
    base = ifelse(is.na(.first_idx), NA_real_, StarsMonthly[.first_idx]),
    Index = 100 * StarsMonthly / base
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-.first_idx)

p_idx <- ggplot(long_indexed, aes(period, Index, color = Framework)) +
  geom_hline(yintercept = 100, linetype = "dotted") +
  geom_line(linewidth = 0.9) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "GitHub Stars — indexed (100 = first non-zero month)",
       x = NULL, y = "Index (first non-zero month = 100)", color = "Framework") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave(file.path(out_dir, "RAV_github_stars_monthly_index100.png"), p_idx,
       width = 12, height = 6, dpi = 300)

message("Готово. Вихід: ", out_dir)
