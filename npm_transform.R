# =============================================
# npm_transform.R — канонічний пайплайн трансформацій для NPM downloads
# =============================================

# ---- deps ----
# (мінімальний стабільний стек)
req_pkgs <- c(
  "readr", "dplyr", "tidyr", "lubridate", "stringr",
  "purrr", "tibble", "forcats", "zoo", "ggplot2",
  "forecast"   # tsclean, BoxCox, BoxCox.lambda
)
for (p in req_pkgs) if (!require(p, character.only = TRUE)) install.packages(p, quiet = TRUE)
invisible(lapply(req_pkgs, require, character.only = TRUE))

# ---- helpers ----
log10p <- function(x) log10(x + 1)

# Якщо у вас daily дані: вважаємо останній (поточний) місяць неповним й опційно дропаємо
is_month_complete <- function(dates) {
  df <- tibble(date = as_date(dates)) |>
    mutate(month = floor_date(date, "month")) |>
    group_by(month) |>
    summarise(min_d = min(date), max_d = max(date), n = n(), .groups = "drop") |>
    mutate(
      days_in_month = days_in_month(month),
      complete = (day(max_d) >= 28) & (n >= 20)
    ) |>
    select(month, complete)
  df
}

# strategies: "keep" (нічого не робимо), "eps" (замінити 0 на epsilon), "locf" (carry-forward), "na_interp" (NA -> інтерполяція)
replace_zeros <- function(x, strategy = c("eps", "keep", "locf", "na_interp"), eps = 0.5) {
  strategy <- match.arg(strategy)
  if (strategy == "keep") return(x)
  if (strategy == "eps")  return(ifelse(x == 0, eps, x))
  if (strategy == "locf") return(zoo::na.locf(ifelse(x == 0, NA, x), na.rm = FALSE))
  if (strategy == "na_interp") return(zoo::na.approx(ifelse(x == 0, NA, x), na.rm = FALSE))
}

# ---- outlier handling per series ----
clean_outliers_ts <- function(x, freq = 12) {
  if (length(x) < (2 * freq)) return(x)
  xts <- ts(as.numeric(x), frequency = freq)
  suppressWarnings(forecast::tsclean(xts))
}

# ---- core transform: daily -> monthly, zeros, outliers, log/index ----
# Вхід: tibble(date, series, value) з daily NPM downloads (або вже monthly)
# Параметри:
#   aggregate = c("sum","mean") — для NPM логічно sum за місяць
#   drop_last_incomplete = TRUE — дропаємо останній неповний місяць
#   zero_strategy = c("eps","keep","locf","na_interp")
#   outliers = c("none","tsclean")
#   scale = c("none","log10p","index_first_nonzero")
transform_npm <- function(df,
                          aggregate = c("sum","mean"),
                          drop_last_incomplete = TRUE,
                          zero_strategy = c("eps","keep","locf","na_interp"),
                          outliers = c("tsclean","none"),
                          scale = c("log10p","none","index_first_nonzero")) {
  aggregate <- match.arg(aggregate)
  zero_strategy <- match.arg(zero_strategy)
  outliers <- match.arg(outliers)
  scale <- match.arg(scale)
  
  df <- df |>
    mutate(date = as_date(date)) |>
    filter(!is.na(series), !is.na(value))
  
  # 1) daily -> monthly
  df_m <- df |>
    mutate(month = floor_date(date, "month")) |>
    group_by(series, month) |>
    summarise(value = if (aggregate == "sum") sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE), .groups = "drop")
  
  # 2) drop last incomplete (якщо вхід був daily)
  if (drop_last_incomplete && any(duplicated(df$month) | is.na(df$month))) {
    comp <- is_month_complete(df$date)
    df_m <- df_m |>
      left_join(comp, by = c("month" = "month")) |>
      mutate(complete = replace_na(complete, TRUE)) |>
      filter(complete) |>
      select(series, month, value)
  }
  
  # 3) zeros per series
  df_m <- df_m |>
    group_by(series) |>
    arrange(month, .by_group = TRUE) |>
    mutate(value = replace_zeros(value, zero_strategy)) |>
    ungroup()
  
  # 4) outliers (optional)
  if (outliers == "tsclean") {
    df_m <- df_m |>
      group_by(series) |>
      arrange(month, .by_group = TRUE) |>
      mutate(value = as.numeric(clean_outliers_ts(value, freq = 12))) |>
      ungroup()
  }
  
  # 5) scaling
  df_m <- df_m |>
    group_by(series) |>
    arrange(month, .by_group = TRUE) |>
    mutate(value_t = case_when(
      scale == "log10p" ~ log10p(value),
      scale == "index_first_nonzero" ~ {
        first_nz <- suppressWarnings(min(which(!is.na(value) & value > 0)))
        if (is.infinite(first_nz)) NA_real_ else 100 * value / value[first_nz]
      },
      TRUE ~ as.numeric(value)
    )) |>
    ungroup()
  
  df_m
}

# ---- diagnostics: Ljung–Box p-values для ARIMA та ETS ----
fit_and_lb <- function(x_ts, lb_lag = 12) {
  res <- list(lb_p_arima = NA_real_, lb_p_ets = NA_real_)
  # ARIMA
  fit_a <- tryCatch(forecast::auto.arima(x_ts, stepwise = TRUE, approximation = FALSE, seasonal = TRUE), error = function(e) NULL)
  if (!is.null(fit_a)) {
    lb_a <- tryCatch(Box.test(residuals(fit_a), type = "Ljung-Box", lag = lb_lag, fitdf = length(fit_a$coef)), error = function(e) NULL)
    if (!is.null(lb_a)) res$lb_p_arima <- as.numeric(lb_a$p.value)
  }
  # ETS
  fit_e <- tryCatch(forecast::ets(x_ts), error = function(e) NULL)
  if (!is.null(fit_e)) {
    lb_e <- tryCatch(Box.test(residuals(fit_e), type = "Ljung-Box", lag = lb_lag, fitdf = length(fit_e$par)), error = function(e) NULL)
    if (!is.null(lb_e)) res$lb_p_ets <- as.numeric(lb_e$p.value)
  }
  as_tibble(res)
}

series_to_ts <- function(df_series, start_year, start_month, freq = 12) {
  v <- df_series$value_t
  idx <- which(!is.na(v))
  if (length(idx) < 6) return(NULL)
  v2 <- v[min(idx):max(idx)]
  ts(as.numeric(v2), start = c(start_year, start_month), frequency = freq)
}

# ---- end-to-end: RAW vs TRANSFORMED ----
compare_raw_vs_transformed <- function(df_daily,
                                       aggregate = "sum",
                                       zero_strategy = "eps",
                                       outliers = "tsclean",
                                       scale = "log10p",
                                       lb_lag = 12) {
  raw_m <- transform_npm(df_daily, aggregate = aggregate, drop_last_incomplete = TRUE,
                         zero_strategy = "keep", outliers = "none", scale = "none") |>
    rename(value_raw = value, value_t_raw = value_t)
  
  tr_m <- transform_npm(df_daily, aggregate = aggregate, drop_last_incomplete = TRUE,
                        zero_strategy = zero_strategy, outliers = outliers, scale = scale)
  
  j <- raw_m |>
    select(series, month, value_raw, value_t_raw) |>
    inner_join(tr_m |>
                 select(series, month, value_tr = value, value_t_tr = value_t),
               by = c("series","month"))
  
  res <- j |>
    group_by(series) |>
    arrange(month, .by_group = TRUE) |>
    summarise(
      start_year = year(first(month)), start_month = month(first(month)),
      diag_raw = list({
        x <- series_to_ts(cur_data() |> select(value_t_raw) |> rename(value_t = value_t_raw),
                          start_year = start_year[1], start_month = start_month[1])
        if (is.null(x)) tibble(lb_p_arima = NA_real_, lb_p_ets = NA_real_) else fit_and_lb(x, lb_lag = lb_lag)
      }),
      diag_tr = list({
        x <- series_to_ts(cur_data() |> select(value_t_tr) |> rename(value_t = value_t_tr),
                          start_year = start_year[1], start_month = start_month[1])
        if (is.null(x)) tibble(lb_p_arima = NA_real_, lb_p_ets = NA_real_) else fit_and_lb(x, lb_lag = lb_lag)
      }),
      .groups = "drop"
    ) |>
    mutate(
      lb_p_arima_raw = purrr::map_dbl(diag_raw, ~ .x$lb_p_arima),
      lb_p_ets_raw   = purrr::map_dbl(diag_raw, ~ .x$lb_p_ets),
      lb_p_arima_tr  = purrr::map_dbl(diag_tr,  ~ .x$lb_p_arima),
      lb_p_ets_tr    = purrr::map_dbl(diag_tr,  ~ .x$lb_p_ets)
    ) |>
    select(series, lb_p_arima_raw, lb_p_ets_raw, lb_p_arima_tr, lb_p_ets_tr)
  
  res
}