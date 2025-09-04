# ==============================
# Analyze & Plot (React/Angular/Vue) — monthly CSV + analyze_many
# - без розриву між фактом і прогнозом
# - прогнози ARIMA & ETS з інтервалами, обрізка до [0,100]
# ==============================

# --- deps ---
pkgs <- c("readr","dplyr","lubridate","ggplot2","forecast","tseries","zoo","tibble","tidyr")
for (p in pkgs) if (!require(p, character.only=TRUE)) install.packages(p, quiet=TRUE)
invisible(lapply(pkgs, require, character.only=TRUE))

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

# --- твоя функція analyze_many ---
source("Frameworks.R")  # analyze_many має бути визначена тут

# --- ВКАЖИ свій місячний файл ---
# приклади:
# in_csv <- "ts_out/gt_full5_5y_monthly_cat31.csv"   # Variant B
# in_csv <- "ts_out/gt_ui_5y_monthly_cat31.csv"      # UI-репліка (monthly)
in_csv  <- "ts_out/gt_ui_5y_monthly_cat31.csv"       # <-- змінюй за потреби
out_dir <- "ts_out/forecast_RAV_monthly"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- зчитування і перевірки ---
dat <- readr::read_csv(in_csv, show_col_types = FALSE) %>% dplyr::arrange(date)
stopifnot("date" %in% names(dat))
cols <- intersect(c("React","Angular","Vue"), names(dat))
if (length(cols) == 0) stop("У файлі немає жодного з очікуваних стовпців: React/Angular/Vue.")

# ---------- хелпери ----------
ts_to_df_monthly <- function(y_ts) {
  stopifnot(frequency(y_ts) == 12)
  n  <- length(y_ts)
  y0 <- start(y_ts)[1]; m0 <- start(y_ts)[2]
  d0 <- as.Date(sprintf("%d-%02d-01", y0, m0))
  dates <- seq(d0, by = "month", length.out = n)
  tibble(Date = dates, Value = as.numeric(y_ts))
}
mk_ts <- function(x, start_date, freq = 12) {
  ts(as.numeric(x), start = c(lubridate::year(start_date), lubridate::month(start_date)), frequency = freq)
}
future_seq <- function(last_date, h) {
  seq(lubridate::floor_date(last_date, "month") %m+% months(1), by = "month", length.out = h)
}
get_series_ts <- function(name, res, dat) {
  if (!is.null(res[[name]]$ts)) return(res[[name]]$ts)
  mk_ts(dat[[name]], min(dat$date))
}
clip01 <- function(x) pmin(pmax(x, 0), 100)

# --- запуск analyze_many на місячній частоті (freq = 12) ---
res <- analyze_many(
  df          = dat,
  columns     = cols,
  start_year  = lubridate::year(min(dat$date)),
  start_month = lubridate::month(min(dat$date)),
  freq        = 12,
  horizon     = 24
)

# --- збереження окремих графіків, якщо їх повертає analyze_many ---
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

# --- Ljung–Box + ACF/PACF (на випадок якщо analyze_many не записала p-value) ---
diag_rows <- list()
for (nm in cols) {
  lb_arima <- res[[nm]]$diagnostics$lb_p_arima %||% NA_real_
  lb_ets   <- res[[nm]]$diagnostics$lb_p_ets   %||% NA_real_
  if (is.na(lb_arima) && !is.null(res[[nm]]$models$arima$fit)) {
    r <- residuals(res[[nm]]$models$arima$fit)
    lb_arima <- suppressWarnings(Box.test(r, type = "Ljung-Box", lag = 24)$p.value)
    png(file.path(out_dir, sprintf("%s_arima_resid_acf.png", nm)), 900, 500);  acf(r,  main = sprintf("%s: ACF ARIMA resid", nm)); dev.off()
    png(file.path(out_dir, sprintf("%s_arima_resid_pacf.png", nm)), 900, 500); pacf(r, main = sprintf("%s: PACF ARIMA resid", nm)); dev.off()
  }
  if (is.na(lb_ets) && !is.null(res[[nm]]$models$ets$fit)) {
    r <- residuals(res[[nm]]$models$ets$fit)
    lb_ets <- suppressWarnings(Box.test(r, type = "Ljung-Box", lag = 24)$p.value)
    png(file.path(out_dir, sprintf("%s_ets_resid_acf.png", nm)), 900, 500);  acf(r,  main = sprintf("%s: ACF ETS resid", nm)); dev.off()
    png(file.path(out_dir, sprintf("%s_ets_resid_pacf.png", nm)), 900, 500); pacf(r, main = sprintf("%s: PACF ETS resid", nm)); dev.off()
  }
  diag_rows[[length(diag_rows)+1]] <- tibble(series = nm, lb_p_arima = lb_arima, lb_p_ets = lb_ets)
}
diag_df <- dplyr::bind_rows(diag_rows)
readr::write_csv(diag_df, file.path(out_dir, "ljung_box_summary.csv"))

# ==== Комбінований графік: Actual + ARIMA & ETS (без розриву і в межах [0,100]) ====

H <- 24  # горизонт прогнозу, змінюй за потреби

actual_list <- list(); fc_list <- list()
for (nm in cols) {
  y <- get_series_ts(nm, res, dat)
  df_act <- ts_to_df_monthly(y) |> dplyr::mutate(Framework = nm)
  
  # моделі з analyze_many (fallback на авто)
  fit_arima <- res[[nm]]$models$arima$fit; if (is.null(fit_arima)) fit_arima <- forecast::auto.arima(y)
  fit_ets   <- res[[nm]]$models$ets$fit;   if (is.null(fit_ets))   fit_ets   <- forecast::ets(y)
  
  fc_a <- forecast::forecast(fit_arima, h = H)
  fc_e <- forecast::forecast(fit_ets,   h = H)
  
  last_date <- max(df_act$Date)
  last_val  <- tail(df_act$Value, 1)
  dates     <- future_seq(last_date, H)
  
  # пришиваємо останню фактичну точку і КЛІПАЄМО до [0,100]
  df_fc_arima <- tibble(
    Date = c(last_date, dates), Framework = nm, Model = "ARIMA",
    Mean = clip01(c(last_val, as.numeric(fc_a$mean))),
    Lo80 = clip01(c(last_val, as.numeric(fc_a$lower[,"80%"]))),
    Hi80 = clip01(c(last_val, as.numeric(fc_a$upper[,"80%"])))
  )
  df_fc_ets <- tibble(
    Date = c(last_date, dates), Framework = nm, Model = "ETS",
    Mean = clip01(c(last_val, as.numeric(fc_e$mean))),
    Lo80 = clip01(c(last_val, as.numeric(fc_e$lower[,"80%"]))),
    Hi80 = clip01(c(last_val, as.numeric(fc_e$upper[,"80%"])))
  )
  
  actual_list[[nm]] <- df_act
  fc_list[[paste0(nm,"_ARIMA")]] <- df_fc_arima
  fc_list[[paste0(nm,"_ETS")]]   <- df_fc_ets
}

actual_df <- dplyr::bind_rows(actual_list)
fc_df     <- dplyr::bind_rows(fc_list)

model_cols  <- c("ARIMA" = "#1F77B4", "ETS" = "#FF7F0E")
model_fills <- c("ARIMA" = "#1F77B4", "ETS" = "#FF7F0E")

p_both <- ggplot() +
  # фактичні дані
  geom_line(data = actual_df, aes(Date, Value, group = Framework),
            color = "black", linewidth = 0.9) +
  # інтервали 80%
  geom_ribbon(
    data = dplyr::filter(fc_df, Model == "ARIMA"),
    aes(Date, ymin = Lo80, ymax = Hi80, fill = Model, group = interaction(Framework, Model)),
    alpha = 0.15, inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = dplyr::filter(fc_df, Model == "ETS"),
    aes(Date, ymin = Lo80, ymax = Hi80, fill = Model, group = interaction(Framework, Model)),
    alpha = 0.15, inherit.aes = FALSE
  ) +
  # лінії прогнозів (без розриву)
  geom_line(
    data = fc_df,
    aes(Date, Mean, color = Model, linetype = Model, group = interaction(Framework, Model)),
    linewidth = 1.2
  ) +
  scale_color_manual(values = model_cols) +
  scale_fill_manual(values  = model_fills, guide = "none") +
  scale_linetype_manual(values = c("ARIMA" = "solid", "ETS" = "dashed")) +
  labs(
    title = "React • Angular • Vue — прогнози ARIMA & ETS (без розриву, [0–100])",
    x = NULL, y = "Індекс 0–100", color = "Модель", linetype = "Модель"
  ) +
  facet_wrap(~ Framework, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(file.path(out_dir, "RAV_combined_ARIMA_ETS.png"), p_both, width = 12, height = 10, dpi = 300)

message("Готово. Дивись у ", out_dir,
        "\n- *_fc_arima.png / *_fc_ets.png / *_decomp.png (якщо є)",
        "\n- *_resid_acf.png / *_resid_pacf.png",
        "\n- ljung_box_summary.csv",
        "\n- RAV_combined_ARIMA_ETS.png (без розриву, інтервали в межах 0–100)")
