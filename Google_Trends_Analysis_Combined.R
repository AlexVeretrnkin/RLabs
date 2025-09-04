# ==========================================
# Run forecasts via your analyze_many (Frameworks.R)
# Focus: React, Angular, Vue
# ==========================================

# --- deps ---
pkgs <- c("readr","dplyr","lubridate","ggplot2","forecast","tseries","zoo","tibble")
for (p in pkgs) if (!require(p, character.only=TRUE)) install.packages(p, quiet=TRUE)
invisible(lapply(pkgs, require, character.only=TRUE))

# --- підключаємо ТВОЮ реалізацію analyze_many ---
source("Frameworks.R")  # тут визначено analyze_many

# --- вхідні дані (Variant B, вже згенеровані) ---
in_csv  <- "ts_out/gt_full5_5y_monthly_cat31.csv"  # змінюй за потреби
out_dir <- "ts_out/forecast_RAV_via_analyze_many"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

dat <- readr::read_csv(in_csv, show_col_types = FALSE) %>% arrange(date)
cols <- intersect(c("React","Angular","Vue"), names(dat))
stopifnot(length(cols) == 3)

# ---------- ХЕЛПЕРИ ДАТ ----------
# конвертує місячний ts у tibble(Date, Value)
ts_to_df_monthly <- function(y_ts) {
  stopifnot(frequency(y_ts) == 12)
  n  <- length(y_ts)
  y0 <- start(y_ts)[1]
  m0 <- start(y_ts)[2]
  d0 <- as.Date(sprintf("%d-%02d-01", y0, m0))
  dates <- seq(d0, by = "month", length.out = n)
  tibble(Date = dates, Value = as.numeric(y_ts))
}
# створює місячний ts зі стартом за першою датою
mk_ts <- function(x, start_date, freq = 12) {
  ts(as.numeric(x), start = c(lubridate::year(start_date), lubridate::month(start_date)), frequency = freq)
}
# послідовність майбутніх місяців
future_seq <- function(last_date, h) {
  seq(lubridate::floor_date(last_date, "month") %m+% months(1), by = "month", length.out = h)
}

# --- виклик ТВОЄЇ analyze_many ---
res <- analyze_many(
  df          = dat,
  columns     = cols,
  start_year  = lubridate::year(min(dat$date)),
  start_month = lubridate::month(min(dat$date)),
  freq        = 12,
  horizon     = 12
)

# ===== Допоміжне: отримати ts з res або з df =====
get_series_ts <- function(name) {
  if (!is.null(res[[name]]$ts)) return(res[[name]]$ts)
  mk_ts(dat[[name]], min(dat$date))
}

# ===== CV метрики (RMSE/MAPE) для вибору переможця =====
cv_rmse <- function(y, f_fun) sqrt(mean(tsCV(y, f_fun, h = 1)^2, na.rm = TRUE))
cv_mape <- function(y, f_fun) {
  e <- tsCV(y, f_fun, h = 1)
  idx <- which(!is.na(e) & !is.na(y))
  mean(abs(e[idx] / as.numeric(y[idx])) * 100, na.rm = TRUE)
}
f_fun_arima <- function(x, h) forecast(auto.arima(
  x,
  lambda  = tryCatch(BoxCox.lambda(x, lower=0, upper=1), error=function(e) NULL),
  biasadj = TRUE,
  seasonal= TRUE,
  stepwise= TRUE
), h = h)
f_fun_ets <- function(x, h) forecast(ets(x), h = h)

# ===== Збір метрик, діагностики та збереження плотів з res =====
summary_rows <- list()

for (nm in cols) {
  y <- get_series_ts(nm)
  
  fit_arima <- res[[nm]]$models$arima$fit
  fit_ets   <- res[[nm]]$models$ets$fit
  
  lb_arima <- res[[nm]]$diagnostics$lb_p_arima
  lb_ets   <- res[[nm]]$diagnostics$lb_p_ets
  if (is.null(lb_arima) && !is.null(fit_arima)) {
    lb_arima <- suppressWarnings(Box.test(residuals(fit_arima), type="Ljung-Box", lag=24)$p.value)
  }
  if (is.null(lb_ets) && !is.null(fit_ets)) {
    lb_ets <- suppressWarnings(Box.test(residuals(fit_ets), type="Ljung-Box", lag=24)$p.value)
  }
  
  # CV (на базі таких же стратегій)
  rmse_a <- cv_rmse(y, f_fun_arima); mape_a <- cv_mape(y, f_fun_arima)
  rmse_e <- cv_rmse(y, f_fun_ets);   mape_e <- cv_mape(y, f_fun_ets)
  winner <- ifelse(rmse_a <= rmse_e, "ARIMA", "ETS")
  
  # --- збереження готових графіків із res (вони вже ggplot) ---
  if (!is.null(res[[nm]]$plots$fc_arima)) {
    ggsave(file.path(out_dir, paste0(nm, "_fc_arima.png")), res[[nm]]$plots$fc_arima, width=11, height=4.5, dpi=300)
  }
  if (!is.null(res[[nm]]$plots$fc_ets)) {
    ggsave(file.path(out_dir, paste0(nm, "_fc_ets.png")), res[[nm]]$plots$fc_ets, width=11, height=4.5, dpi=300)
  }
  if (!is.null(res[[nm]]$plots$decomp)) {
    ggsave(file.path(out_dir, paste0(nm, "_decomp.png")), res[[nm]]$plots$decomp, width=11, height=4.5, dpi=300)
  }
  
  arima_desc <- if (!is.null(fit_arima)) paste(capture.output(fit_arima)[1]) else NA_character_
  ets_desc   <- if (!is.null(fit_ets))   paste(capture.output(fit_ets)[1])   else NA_character_
  
  summary_rows[[length(summary_rows)+1]] <- tibble(
    series = nm,
    arima  = arima_desc,
    ets    = ets_desc,
    lb_p_arima = lb_arima %||% NA_real_,
    lb_p_ets   = lb_ets   %||% NA_real_,
    rmse_arima = rmse_a,  rmse_ets = rmse_e,
    mape_arima = mape_a,  mape_ets = mape_e,
    winner     = winner
  )
}

summary_df <- dplyr::bind_rows(summary_rows) %>% dplyr::arrange(series)
readr::write_csv(summary_df, file.path(out_dir, "summary_RAV_via_analyze_many.csv"))
print(summary_df)

# ===== Комбінований графік переможців (із коректною віссю дат) =====
make_fc <- function(nm, horizon=24) {
  y <- get_series_ts(nm)
  if (summary_df$winner[summary_df$series==nm] == "ARIMA") {
    fit <- res[[nm]]$models$arima$fit
    if (is.null(fit)) fit <- auto.arima(y) # fallback
    forecast(fit, h = horizon)
  } else {
    fit <- res[[nm]]$models$ets$fit
    if (is.null(fit)) fit <- ets(y) # fallback
    forecast(fit, h = horizon)
  }
}

H <- 24
build_df <- function(nm) {
  y <- get_series_ts(nm)
  df_act <- ts_to_df_monthly(y) |> dplyr::mutate(Framework = nm, Type = "Actual")
  fc <- make_fc(nm, H)
  dates <- future_seq(max(df_act$Date), H)
  df_fc <- tibble(Date = dates, Value = as.numeric(fc$mean), Framework = nm, Type = "Forecast")
  dplyr::bind_rows(df_act, df_fc)
}

df_long <- dplyr::bind_rows(lapply(cols, build_df))
cols_pal <- c("React"="#D55E00","Angular"="#0072B2","Vue"="#009E73")

p_comb <- ggplot(df_long, aes(Date, Value, color = Framework, linetype = Type)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = cols_pal) +
  labs(title = "React vs Angular vs Vue — best model per series (via analyze_many)",
       x = NULL, y = "Index 0–100") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave(file.path(out_dir, "RAV_combined_via_analyze_many.png"), p_comb, width = 12, height = 5, dpi = 300)

message("Готово. Вивід у: ", out_dir,
        "\n- *_fc_arima.png / *_fc_ets.png / *_decomp.png (із analyze_many)",
        "\n- summary_RAV_via_analyze_many.csv (RMSE/MAPE/LB + переможець)",
        "\n- RAV_combined_via_analyze_many.png (без попередження про <ts>)")
