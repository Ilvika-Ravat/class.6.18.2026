suppressPackageStartupMessages({
  library(tidyverse)
  library(rvest)
})

page_url <- "https://en.wikipedia.org/wiki/2026_FIFA_World_Cup_squads"
output_file <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(output_file) || !nzchar(output_file)) {
  output_file <- "player_data.csv"
}

page <- read_html(page_url)

squad_tables <- page %>% html_elements("table.wikitable.plainrowheaders")

extract_birth_info <- function(birth_text) {
  birth_text <- str_squish(birth_text)

  birth_date_text <- str_extract(
    birth_text,
    "[A-Z][a-z]+\\s+\\d{1,2},\\s+\\d{4}"
  )

  age_text <- str_extract(
    birth_text,
    "(?<=aged\\s)\\d+|(?<=age\\s)\\d+"
  )

  tibble(
    date_of_birth = as.Date(birth_date_text, format = "%B %d, %Y"),
    age = as.integer(age_text)
  )
}

extract_team_table <- function(table_node) {
  team_name <- table_node %>%
    html_element(xpath = "preceding::h3[1]") %>%
    html_text2()

  table_node %>%
    html_element("tbody") %>%
    html_elements("tr.nat-fs-player") %>%
    map_dfr(function(row) {
      cells <- row %>% html_elements("th, td")
      birth_info <- extract_birth_info(cells[[4]] %>% html_text2())

      tibble(
        team = team_name,
        number = cells[[1]] %>% html_text2() %>% str_squish() %>% na_if(""),
        position = cells[[2]] %>% html_text2() %>% str_squish(),
        player = cells[[3]] %>% html_text2() %>% str_squish(),
        caps = cells[[5]] %>% html_text2() %>% str_squish() %>% as.integer(),
        goals = cells[[6]] %>% html_text2() %>% str_squish() %>% as.integer(),
        club = cells[[7]] %>% html_text2() %>% str_squish()
      ) %>%
        bind_cols(birth_info)
    })
}

player_data <- map_dfr(squad_tables, extract_team_table) %>%
  mutate(
    across(c(team, position, player, club), str_squish)
  ) %>%
  select(team, player, position, number, date_of_birth, age, caps, goals, club)

write_csv(player_data, output_file)
cat(sprintf("Wrote %d rows to %s\n", nrow(player_data), output_file))
