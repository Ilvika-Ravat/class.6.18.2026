#
#
#
#
#
#
#
#
#
#
#
#
#
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

nba_games <- read_csv("Games.csv", show_col_types = FALSE) %>%
  mutate(
    game_date = ymd_hms(gameDateTimeEst),
    year = year(game_date),
    scoreline = str_c(homeScore, awayScore, sep = "-"),
    result_label = if_else(homeScore > awayScore, "Home win", "Away win")
  )

score_counts <- nba_games %>%
  count(homeScore, awayScore, name = "count")

score_progress <- nba_games %>%
  arrange(game_date) %>%
  mutate(new_score = !duplicated(scoreline)) %>%
  mutate(
    game_number = row_number(),
    cumulative_unique = cumsum(new_score)
  )

score_by_year <- nba_games %>%
  count(year, name = "unique_scores") %>%
  arrange(year)

top_scores <- score_counts %>%
  arrange(desc(count), desc(homeScore + awayScore)) %>%
  slice_head(n = 10) %>%
  mutate(label = str_c(homeScore, "-", awayScore))

unique_score_count <- n_distinct(nba_games$scoreline)
first_year <- min(nba_games$year, na.rm = TRUE)
last_year <- max(nba_games$year, na.rm = TRUE)
#
#
#
#
#
info_plot <- tibble(
  label = c("Unique scores", "Years", "Games"),
  value = c(unique_score_count, str_c(first_year, "–", last_year), nrow(nba_games))
) %>%
  ggplot(aes(x = 1, y = label, label = value)) +
  geom_text(aes(label = label), hjust = 0, size = 7, color = "#111827", fontface = "bold") +
  geom_text(aes(y = label, label = value), hjust = 1.1, size = 8, color = "#4338ca") +
  xlim(0.75, 1.3) +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))

progress_plot <- score_progress %>%
  ggplot(aes(x = game_date, y = cumulative_unique)) +
  geom_area(fill = "#4338ca", alpha = 0.3) +
  geom_line(color = "#4338ca", linewidth = 1.4) +
  geom_point(color = "#fbbf24", size = 2.5) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "7 days") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Unique scorelines over time",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "#334155"),
    axis.line = element_line(color = "#cbd5e1"),
    panel.grid = element_line(color = "#e2e8f0"),
    panel.grid.minor = element_blank()
  )

score_heat <- score_counts %>%
  ggplot(aes(x = homeScore, y = awayScore, fill = count)) +
  geom_tile(color = "#111827", width = 0.96, height = 0.96) +
  geom_abline(intercept = 0, slope = 1, color = "white", linewidth = 1, linetype = "dashed") +
  geom_text(data = top_scores, aes(label = label), color = "white", size = 3, fontface = "bold", nudge_y = 1) +
  scale_fill_gradientn(
    colours = c("#0f172a", "#4338ca", "#f97316", "#facc15"),
    values = rescale(c(0, 1, 2, max(score_counts$count))),
    guide = guide_colorbar(barwidth = 16, barheight = 0.6),
    labels = comma
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  labs(
    title = "Home PTS vs Away PTS",
    subtitle = "Darker tiles are rarer; brighter tiles are repeated game scorelines",
    x = "Home PTS",
    y = "Away PTS",
    fill = "Games"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, margin = margin(b = 12)),
    panel.grid.major = element_line(color = "#d1d5db", size = 0.35),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

(score_heat + plot_spacer()) / (info_plot + progress_plot) +
  plot_layout(heights = c(3, 1), widths = c(3, 1))
#
#
#
#
#
top_scores %>%
  arrange(desc(count), desc(homeScore + awayScore)) %>%
  transmute(scoreline = label, games = count) %>%
  knitr::kable()
#
#
#
#
