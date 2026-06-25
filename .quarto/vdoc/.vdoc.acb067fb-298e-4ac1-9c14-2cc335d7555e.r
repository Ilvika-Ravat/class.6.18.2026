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

team_games <- nba_games %>%
  select(home = hometeamName, away = awayteamName) %>%
  pivot_longer(everything(), values_to = "team") %>%
  count(team, name = "games") %>%
  arrange(desc(games), team)

unique_score_count <- n_distinct(nba_games$scoreline)
first_year <- min(nba_games$year, na.rm = TRUE)
last_year <- max(nba_games$year, na.rm = TRUE)
#
#
#
#
#
score_counts %>%
  ggplot(aes(x = homeScore, y = awayScore, fill = count)) +
  geom_tile(color = "#1f2937", width = 0.95, height = 0.95) +
  geom_abline(intercept = 0, slope = 1, color = "white", linewidth = 0.8, linetype = "dashed") +
  scale_fill_gradientn(
    colours = c("#111827", "#4338ca", "#f97316", "#facc15"),
    values = rescale(c(0, 1, 2, max(score_counts$count))),
    guide = guide_colorbar(barwidth = 15, barheight = 0.8),
    labels = comma
  ) +
  scale_x_continuous(breaks = seq(
    min(score_counts$homeScore) - 5,
    max(score_counts$homeScore) + 5,
    by = 10
  )) +
  scale_y_continuous(breaks = seq(
    min(score_counts$awayScore) - 5,
    max(score_counts$awayScore) + 5,
    by = 10
  )) +
  labs(
    title = "NBA Scorigami: Home vs Away scorelines",
    subtitle = str_glue("{unique_score_count} unique scorelines in {first_year}–{last_year}"),
    x = "Home PTS",
    y = "Away PTS",
    fill = "Games"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14, margin = margin(b = 12)),
    panel.grid = element_line(color = "#d1d5db", size = 0.4),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )
#
#
#
#
#
score_progress %>%
  ggplot(aes(x = game_date, y = cumulative_unique)) +
  geom_line(color = "#2563eb", linewidth = 1.3) +
  geom_point(color = "#fb923c", size = 2.5) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "5 days") +
  scale_y_continuous(breaks = seq(0, unique_score_count, by = 1)) +
  labs(
    title = "Unique scorelines discovered over the playoff schedule",
    x = "Game date",
    y = "Cumulative unique scorelines"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
#
#
#
#
#
team_games %>%
  slice_max(games, n = 10) %>%
  mutate(rank = row_number()) %>%
  select(rank, team, games) %>%
  knitr::kable(
    caption = "Top 10 teams by games played in the dataset"
  )
#
#
#
#
