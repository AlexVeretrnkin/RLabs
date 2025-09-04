# =========================
# Google Trends export (5 років, ggplot + базові пристрої)
# =========================

# --- пакети ---
pkgs <- c("gtrendsR","dplyr","tidyr","lubridate","zoo","readr","ggplot2")
for (p in pkgs) if (!require(p, character.only=TRUE)) install.packages(p, quiet=TRUE)
invisible(lapply(pkgs, require, character.only=TRUE))

# (опційно) робоча директорія
setwd("F:/RLabs")

# --- діапазон: останній повний місяць і -5 років ---
last_full_month <- function(today = Sys.Date()) {
  lubridate::floor_date(today, "month") - months(1)
}
end_date   <- last_full_month()
start_date <- (end_date %m-% years(5)) %m+% months(1)  # 60 повних місяців

# --- ключові слова (до 5 за раз) ---
frameworks <- tibble::tribble(
  ~name,    ~keyword,
  "React",  "React",
  "Angular","Angular",
  "Vue",    "Vue",
  #  "Svelte", "Svelte (software)",
  #  "Next.js","Next.js"
)

# --- завантаження Google Trends і агрегація до місяця ---
pull_gtrends_monthly <- function(fr_df,
                                 start_date, end_date,
                                 geo = "", category = 31, gprop = "web", hl = "uk-UA") {
  res <- gtrendsR::gtrends(
    keyword = fr_df$keyword,
    time    = paste(format(start_date, "%Y-%m-%d"), format(end_date, "%Y-%m-%d")),
    gprop   = gprop, geo = geo, category = category, hl = hl, onlyInterest = TRUE
  )
  iot <- res$interest_over_time
  if (is.null(iot) || nrow(iot) == 0) stop("Google Trends: порожня відповідь")
  
  iot %>%
    transmute(
      date = as.Date(date),
      name = factor(keyword, levels = fr_df$keyword, labels = fr_df$name),
      # "<1" → 0.5, щоб не втратити слабкі сигнали
      hits = suppressWarnings(as.numeric(replace(hits, hits == "<1", "0.5")))
    ) %>%
    group_by(name, ym = zoo::as.yearmon(date)) %>%
    summarise(index = mean(hits, na.rm = TRUE), .groups = "drop") %>%
    mutate(date = as.Date(lubridate::ymd(paste0(format(ym, "%Y-%m"), "-01")))) %>%
    select(name, date, index) %>%
    tidyr::pivot_wider(names_from = name, values_from = index) %>%
    arrange(date)
}

# --- тягнемо (geo="" = Worldwide; для України постав "UA") ---
gt_5y <- pull_gtrends_monthly(frameworks, start_date, end_date, geo = "", category = 31, hl = "uk-UA")

# --- вихідна директорія ---
out_dir <- file.path(getwd(), "ts_out")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- збереження CSV ---
csv_path <- file.path(out_dir, "gt_frameworks_5y_monthly.csv")
readr::write_csv(gt_5y, csv_path)
message(sprintf("CSV saved: %s (%s … %s)",
                csv_path, format(min(gt_5y$date), "%Y-%m"), format(max(gt_5y$date), "%Y-%m")))

# --- дані для графіка ---
gt_long <- gt_5y %>% tidyr::pivot_longer(-date, names_to = "framework", values_to = "index")

# Палітра Okabe–Ito (контрастна, дружня до дальтонізму)
cols <- c(
  "React"   = "#0072B2", # blue
  "Angular" = "#D55E00", # vermillion
  "Vue"     = "#009E73", # bluish green
  "Svelte"  = "#CC79A7", # reddish purple
  "Next.js" = "#E69F00"  # orange
)

# Річні вертикальні лінії (на 1 січня кожного року у вікні)
year_lines <- seq(
  lubridate::floor_date(min(gt_long$date), "year"),
  lubridate::floor_date(max(gt_long$date), "year"),
  by = "year"
)
year_df <- data.frame(x = year_lines)

# --- побудова графіка (ggplot, без нестандартних пристроїв) ---
p <- ggplot(gt_long, aes(date, index, color = framework)) +
  # річні маркери
  geom_vline(data = year_df, aes(xintercept = x), inherit.aes = FALSE,
             linetype = "dashed", linewidth = 0.4, alpha = 0.5, color = "#9AA0A6") +
  # ряди
  geom_line(linewidth = 1.8) +
  scale_color_manual(values = cols, guide = guide_legend(override.aes = list(linewidth = 2.5))) +
  labs(
    title = sprintf("Google Trends (останні 5 років): %s…%s",
                    format(min(gt_long$date), "%Y-%m"), format(max(gt_long$date), "%Y-%m")),
    x = NULL, y = "Індекс 0–100 (у межах цього набору та вікна)", color = NULL
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03))) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# --- збереження графіків: PNG + PDF (стандартні пристрої) ---
png_path <- file.path(out_dir, "gt_5y_contrast.png")
pdf_path <- file.path(out_dir, "gt_5y_contrast.pdf")

# PNG: якщо встановлено ragg — використаємо його як девайс (чіткіший рендер),
# інакше — базовий PNG
png_device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
ggsave(png_path, p, width = 12, height = 5, dpi = 300, device = png_device)

# PDF: векторний, без svglite
if (capabilities("cairo")) {
  ggsave(pdf_path, p, width = 12, height = 5, device = cairo_pdf)
} else {
  ggsave(pdf_path, p, width = 12, height = 5)  # базовий pdf() як fallback
}

message(sprintf("Plots saved:\n- %s\n- %s", png_path, pdf_path))

# --- (опційно) одразу під твій аналіз часових рядів ---
# source("framework_ts.R")
# if (exists("analyze_many")) {
#   res <- analyze_many(
#     df = gt_5y,
#     columns = intersect(c("React","Angular","Vue","Svelte","Next.js"), names(gt_5y)),
#     start_year  = year(min(gt_5y$date)),
#     start_month = month(min(gt_5y$date)),
#     freq = 12, horizon = 24
#   )
#   print(res$React$plots$fc_arima)
# }
