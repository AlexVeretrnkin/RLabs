# =========================
# NPM Downloads: full-history fetcher + QA summaries
# =========================

# ---- deps ----
pkgs <- c("httr", "jsonlite", "dplyr", "tidyr", "purrr", "lubridate", "readr", "stringr")
for (p in pkgs) if (!require(p, character.only = TRUE)) install.packages(p, quiet = TRUE)
invisible(lapply(pkgs, require, character.only = TRUE))

# ---- config ----
packages <- c("react", "vue", "@angular/core")   # можеш додати інші
start_date <- as.Date("2015-01-01")              # npm API має дані приблизно з 2015-01-10
end_date   <- Sys.Date() - 1                     # до вчора

# Розбиваємо на шматки (API надійно віддає ~18 міс макс; беремо 365 днів для простоти)
chunk_years <- TRUE
chunk_span_days <- if (chunk_years) 365 else 540

# ---- helpers ----
safe_get_json <- function(url, retries = 5, pause_sec = 1) {
  for (i in seq_len(retries)) {
    resp <- try(httr::GET(url, httr::user_agent("Oleksii-frameworks-research/1.0")), silent = TRUE)
    if (inherits(resp, "try-error")) { Sys.sleep(pause_sec * i); next }
    if (httr::status_code(resp) == 200) {
      return(jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = TRUE))
    }
    # 429/5xx — почекаємо й повторимо
    Sys.sleep(pause_sec * i)
  }
  stop("Failed to GET: ", url)
}

build_range_url <- function(pkg, from, to) {
  enc_pkg <- URLencode(pkg, reserved = TRUE) # щоб коректно працювало з @angular/core
  sprintf("https://api.npmjs.org/downloads/range/%s:%s/%s",
          format(from, "%Y-%m-%d"), format(to, "%Y-%m-%d"), enc_pkg)
}

fetch_range <- function(pkg, from, to) {
  url <- build_range_url(pkg, from, to)
  js  <- safe_get_json(url)
  # Формат відповіді: js$downloads — масив {day, downloads}
  if (is.null(js$downloads) || length(js$downloads) == 0) {
    return(tibble::tibble(package = pkg, date = as.Date(character()), downloads = integer()))
  }
  tibble::tibble(
    package   = pkg,
    date      = as.Date(js$downloads$day),
    downloads = as.integer(js$downloads$downloads)
  )
}

fetch_full_history <- function(pkg, from, to, span_days = 365) {
  bounds <- seq(from, to, by = sprintf("%d days", span_days))
  if (tail(bounds, 1) < to) bounds <- c(bounds, to)
  purrr::map2_dfr(bounds[-length(bounds)], bounds[-1], ~{
    # щоб інтервали не перекривались і включали кінцеву дату
    left  <- .x
    right <- min(.y - 1, to)
    fetch_range(pkg, left, right)
  }) |>
    distinct(package, date, .keep_all = TRUE) |>
    arrange(date)
}

agg_monthly <- function(df) {
  df |>
    mutate(year = year(date), month = month(date)) |>
    group_by(package, year, month) |>
    summarise(downloads = sum(downloads, na.rm = TRUE), .groups = "drop") |>
    mutate(period = as.Date(sprintf("%04d-%02d-01", year, month)))
}

make_wide <- function(df_monthly) {
  df_monthly |>
    select(package, period, downloads) |>
    tidyr::pivot_wider(names_from = package, values_from = downloads) |>
    arrange(period)
}

qa_summary <- function(df_monthly) {
  # Нетто-приріст — зміна за місяць; p95 — періодичний 95-й перцентиль
  df_monthly |>
    arrange(package, period) |>
    group_by(package) |>
    mutate(
      growth_abs = downloads - lag(downloads),
      ma_3       = zoo::rollapply(downloads, width = 3, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
      ma_6       = zoo::rollapply(downloads, width = 6, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
      p95_6      = zoo::rollapply(downloads, width = 6, FUN = function(x) quantile(x, 0.95, na.rm = TRUE),
                                  align = "right", fill = NA)
    ) |>
    ungroup() |>
    select(package, period, downloads, growth_abs, ma_3, ma_6, p95_6)
}

# ---- run ----
message("Fetching daily data…")
daily <- purrr::map_dfr(packages, ~fetch_full_history(.x, start_date, end_date, span_days = chunk_span_days))

# Видаляємо можливі "дірки" (npm інколи повертає 0 або NA для окремих днів)
daily <- daily |>
  filter(!is.na(date)) |>
  mutate(downloads = replace_na(downloads, 0L))

# ---- outputs ----
dir.create("out", showWarnings = FALSE)

readr::write_csv(daily, "out/npm_downloads_daily.csv")

monthly <- agg_monthly(daily)
readr::write_csv(monthly, "out/npm_downloads_monthly.csv")

wide_monthly <- make_wide(monthly)
readr::write_csv(wide_monthly, "out/npm_downloads_wide_monthly.csv")

# QA summary (періоди, нетто-приріст, середні, p95)
if (!requireNamespace("zoo", quietly = TRUE)) install.packages("zoo", quiet = TRUE)
library(zoo)
qa <- qa_summary(monthly)
readr::write_csv(qa, "out/qa_summary.csv")

message("Done. Files in ./out:
- npm_downloads_daily.csv
- npm_downloads_monthly.csv
- npm_downloads_wide_monthly.csv
- qa_summary.csv")
