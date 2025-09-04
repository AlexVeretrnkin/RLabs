# ===============================
# Google Trends (cat=31, last 5y)
# UI replica (Angular/React/Vue) + Full 5 frameworks
# ===============================

# --- deps ---
pkgs <- c("gtrendsR","dplyr","tidyr","lubridate","zoo","readr","ggplot2")
for (p in pkgs) if (!require(p, character.only=TRUE)) install.packages(p, quiet=TRUE)
invisible(lapply(pkgs, require, character.only=TRUE))

# --- helpers ---
safe_gtrends <- function(keywords, time_str = "today+5-y", geo = "", category = 31,
                         gprop = "web", hl = "uk-UA") {
  # деякі версії приймають "today 5-y", інші — "today+5-y"
  try1 <- try(gtrendsR::gtrends(keyword = keywords, time = time_str,
                                gprop = gprop, geo = geo, category = category,
                                hl = hl, onlyInterest = TRUE),
              silent = TRUE)
  if (inherits(try1, "try-error") || is.null(try1$interest_over_time)) {
    alt <- sub("\\+", " ", time_str, fixed = TRUE)
    try2 <- gtrendsR::gtrends(keyword = keywords, time = alt,
                              gprop = gprop, geo = geo, category = category,
                              hl = hl, onlyInterest = TRUE)
    return(try2)
  }
  try1
}

to_weekly_wide <- function(res, names_vec = NULL) {
  iot <- res$interest_over_time
  stopifnot(!is.null(iot), nrow(iot) > 0)
  out <- iot %>%
    dplyr::transmute(
      date = as.Date(date),
      name = if (is.null(names_vec)) keyword else factor(keyword, levels = names_vec, labels = names_vec),
      hits = suppressWarnings(as.numeric(replace(hits, hits == "<1", "0.5")))
    ) %>%
    tidyr::pivot_wider(names_from = name, values_from = hits) %>%
    arrange(date)
  out
}

weekly_to_monthly <- function(weekly_df) {
  weekly_df %>%
    tidyr::pivot_longer(-date, names_to = "name", values_to = "hits") %>%
    mutate(ym = zoo::as.yearmon(date)) %>%
    group_by(name, ym) %>%
    summarise(index = mean(hits, na.rm = TRUE), .groups = "drop") %>%
    mutate(date = as.Date(lubridate::ymd(paste0(format(ym, "%Y-%m"), "-01")))) %>%
    select(name, date, index) %>%
    tidyr::pivot_wider(names_from = name, values_from = index) %>%
    arrange(date)
}

plot_weekly <- function(weekly_long, cols, title) {
  ggplot(weekly_long, aes(date, index, color = framework)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = cols, guide = guide_legend(override.aes = list(linewidth = 1.8))) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title = title, x = NULL, y = "Індекс 0–100") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top", panel.grid.minor = element_blank())
}

plot_monthly <- function(monthly_wide, cols, title) {
  mlong <- monthly_wide %>% tidyr::pivot_longer(-date, names_to = "framework", values_to = "index")
  year_df <- data.frame(x = seq(lubridate::floor_date(min(mlong$date), "year"),
                                lubridate::floor_date(max(mlong$date), "year"), by = "year"))
  ggplot(mlong, aes(date, index, color = framework)) +
    geom_vline(data = year_df, aes(xintercept = x), inherit.aes = FALSE,
               linetype = "dashed", linewidth = 0.4, alpha = 0.5, color = "#9AA0A6") +
    geom_line(linewidth = 1.4) +
    scale_color_manual(values = cols, guide = guide_legend(override.aes = list(linewidth = 1.8))) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title = title, x = NULL, y = "Індекс 0–100") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top", panel.grid.minor = element_blank())
}

# --- output dir ---
out_dir <- file.path(getwd(), "ts_out")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# =====================================================
# VARIANT A: UI replica (Angular Topic, React Topic, Vue term)
# =====================================================
ui_keywords <- c("Angular", "React", "Vue")  # як у твоєму скріні
res_ui <- safe_gtrends(ui_keywords, time_str = "today+5-y", geo = "", category = 31)

weekly_ui <- to_weekly_wide(res_ui, names_vec = ui_keywords)
monthly_ui <- weekly_to_monthly(weekly_ui)

# збереження CSV
readr::write_csv(weekly_ui,  file.path(out_dir, "gt_ui_5y_weekly_cat31.csv"))
readr::write_csv(monthly_ui, file.path(out_dir, "gt_ui_5y_monthly_cat31.csv"))

# палітра близька до скріну
cols_ui <- c("Angular"="#3366CC","React"="#DC3912","Vue"="#FF9900")

# графіки
p_ui_week  <- plot_weekly(weekly_ui %>% tidyr::pivot_longer(-date, names_to="framework", values_to="index"),
                          cols_ui, "Google Trends UI — cat 31, 5y (weekly)")
p_ui_month <- plot_monthly(monthly_ui, cols_ui, "Google Trends UI — cat 31, 5y (monthly avg)")

ggsave(file.path(out_dir, "gt_ui_5y_weekly_cat31.png"),  p_ui_week,  width = 12, height = 5, dpi = 300)
ggsave(file.path(out_dir, "gt_ui_5y_monthly_cat31.png"), p_ui_month, width = 12, height = 5, dpi = 300)

# =====================================================
# VARIANT B: Full 5 frameworks (Topics where possible)
# (увага: інша нормалізація 0–100, бо інший набір ключів)
# =====================================================
fw <- tibble::tribble(
  ~name,    ~keyword,
  "Angular","Angular (web framework)",
  "React",  "React (web framework)",
  "Vue",    "Vue.js",
  "Svelte", "Svelte (software)",
  "Next.js","Next.js"
)

res_full <- safe_gtrends(fw$keyword, time_str = "today+5-y", geo = "", category = 31)

weekly_full <- to_weekly_wide(res_full, names_vec = fw$keyword) %>%
  dplyr::rename_with(~fw$name[match(., fw$keyword)], -date)  # перейменуємо колонки під короткі імена
monthly_full <- weekly_to_monthly(weekly_full)

# збереження CSV
readr::write_csv(weekly_full,  file.path(out_dir, "gt_full5_5y_weekly_cat31.csv"))
readr::write_csv(monthly_full, file.path(out_dir, "gt_full5_5y_monthly_cat31.csv"))

# контрастна палітра Okabe–Ito
cols_full <- c(
  "Angular"="#0072B2", "React"="#D55E00", "Vue"="#009E73",
  "Svelte"="#CC79A7", "Next.js"="#E69F00"
)

# графіки
p_full_week  <- plot_weekly(weekly_full %>% tidyr::pivot_longer(-date, names_to="framework", values_to="index"),
                            cols_full, "Google Trends — cat 31, 5y (weekly) — 5 frameworks")
p_full_month <- plot_monthly(monthly_full, cols_full, "Google Trends — cat 31, 5y (monthly avg) — 5 frameworks")

ggsave(file.path(out_dir, "gt_full5_5y_weekly_cat31.png"),  p_full_week,  width = 12, height = 5, dpi = 300)
ggsave(file.path(out_dir, "gt_full5_5y_monthly_cat31.png"), p_full_month, width = 12, height = 5, dpi = 300)

message("Done. CSV і графіки збережено у: ", out_dir,
        "\n- UI replica: gt_ui_5y_weekly_cat31.csv/png, gt_ui_5y_monthly_cat31.csv/png",
        "\n- Full 5:     gt_full5_5y_weekly_cat31.csv/png, gt_full5_5y_monthly_cat31.csv/png")
