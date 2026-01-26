# =============================================================================
# Bounce Back - Shared Functions
# Common visualization and data processing functions for all chapters
# =============================================================================

# Load required libraries
library(tidyverse)
library(BasketballAnalyzeR)
library(ggimage)
library(cropcircles)
library(ggtext)
library(glue)
library(janitor)
library(htmltools)
library(gganimate)
library(viridis)
library(highcharter)
library(scales)

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Load the main NBA shots dataset
#' @return A data frame with all NBA shots from 2003-2024
load_all_shots <- function() {
  read.csv("data/allshots-2003-2024.csv")
}

#' Load player-specific shots
#' @param player_name The name of the player (e.g., "LeBron James")
#' @param all_shots Optional: pre-loaded all_shots data frame
#' @return A filtered data frame for the specified player
load_player_shots <- function(player_name, all_shots = NULL) {
  if (is.null(all_shots)) {
    all_shots <- load_all_shots()
  }
  all_shots %>%
    filter(namePlayer == player_name)
}

#' Load gamelogs data
#' @return A data frame with 2023-24 season gamelogs
load_gamelogs <- function() {
  read.csv("data/gamelogs_2024.csv")
}

# =============================================================================
# SHOT CHART FUNCTIONS (from LeBron chapter)
# =============================================================================

#' Create a shot chart for a specific player and game
#' @param data The shots data frame
#' @param selected_player Player name
#' @param selected_date Game date in YYYYMMDD format
#' @return A ggplot object
create_shotchart <- function(data, selected_player, selected_date) {
  # Filter the data for the specified player and date
  filtered_data <- data |>
    filter(namePlayer == selected_player) |>
    filter(dateGame == selected_date) |>
    mutate(x = (locationX / 10) - 0,
           y = (locationY / 10) - 41.75) |>
    filter(y < 0) |>
    mutate(
      court_zone = case_when(
        distanceShot <= 8 ~ "Paint",
        distanceShot > 8 & distanceShot < 23 ~ "Mid-Range",
        distanceShot >= 23 ~ "3PT",
        TRUE ~ "Other"
      )
    )

  # Create the shot chart plot
  plot <- BasketballAnalyzeR::drawNBAcourt(
    ggplot(data = filtered_data), size = 0.5, col = "grey20") +
    geom_point(aes(x = x, y = y, fill = court_zone),
               shape = 21, color = "white", size = 2.5, alpha = 0.8) +
    scale_fill_manual(
      values = c("Paint" = "#99CCFF", "Mid-Range" = "#50C878", "3PT" = "#FF9999")
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  return(plot)
}

#' Create a slope chart comparing two games
#' @param data The shots data frame
#' @param selected_player Player name
#' @param game_1 First game date
#' @param game_2 Second game date
#' @return A ggplot object
shots_game_by_game_slope_chart <- function(data, selected_player, game_1, game_2) {
  filtered_data <- data |>
    filter(namePlayer == selected_player) |>
    filter(dateGame %in% c(game_1, game_2)) |>
    mutate(x = (locationX / 10) - 0,
           y = (locationY / 10) - 41.75) |>
    filter(y < 0) |>
    mutate(
      court_zone = case_when(
        distanceShot <= 8 ~ "Paint",
        distanceShot > 8 & distanceShot < 23 ~ "Mid-Range",
        distanceShot >= 23 ~ "3PT",
        TRUE ~ "Other"
      )) |>
    group_by(dateGame, court_zone) |>
    summarize(total_shots_two_games = n(), .groups = "drop") |>
    group_by(dateGame) |>
    mutate(
      all_shots_two_games = sum(total_shots_two_games),
      percentage = (total_shots_two_games / all_shots_two_games) * 100
    ) |>
    ungroup()

  label_positions <- filtered_data |>
    group_by(court_zone) |>
    summarize(
      x_pos = mean(as.numeric(as.factor(dateGame))),
      y_pos = mean(percentage),
      .groups = "drop"
    )

  plot <- ggplot(filtered_data, aes(x = as.factor(dateGame), y = percentage,
                                     group = court_zone, color = court_zone)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_text(data = label_positions,
              aes(x = x_pos, y = y_pos, label = court_zone, hjust = 0.1, vjust = 1),
              size = 4, show.legend = FALSE) +
    scale_color_manual(
      values = c("Paint" = "#99CCFF", "Mid-Range" = "#50C878", "3PT" = "#FF9999")
    ) +
    theme_minimal()

  return(plot)
}

#' Create a line chart showing shot preferences over time
#' @param data The shots data frame
#' @param selected_player Player name
#' @param start_season Start season year
#' @param end_season End season year
#' @return A ggplot object
create_linechart <- function(data, selected_player, start_season, end_season) {
  filtered_data <- data |>
    filter(namePlayer == selected_player) |>
    filter(yearSeason >= start_season & yearSeason <= end_season) |>
    mutate(x = (locationX / 10) - 0,
           y = (locationY / 10) - 41.75) |>
    mutate(
      court_zone = case_when(
        distanceShot <= 8 ~ "Paint",
        distanceShot > 8 & distanceShot < 23 ~ "Mid-Range",
        distanceShot >= 23 ~ "3PT",
        TRUE ~ "Other"
      )
    )

  filtered_summary <- filtered_data |>
    group_by(yearSeason, court_zone) |>
    summarize(total_shots = n(), .groups = "drop") |>
    group_by(yearSeason) |>
    mutate(
      overall_shots = sum(total_shots),
      percentage = (total_shots / overall_shots) * 100
    ) |>
    ungroup()

  plot <- ggplot(filtered_summary, aes(x = yearSeason, y = percentage,
                                        color = court_zone, group = court_zone)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("Paint" = "#99CCFF", "Mid-Range" = "#50C878", "3PT" = "#FF9999")
    ) +
    theme_minimal()

  return(plot)
}

# =============================================================================
# HIGHCHARTER VISUALIZATION FUNCTIONS
# =============================================================================

#' Create an interactive line chart with Highcharter
#' @param data Data frame with columns: x, y, group (optional)
#' @param title Chart title
#' @param subtitle Chart subtitle
#' @param x_title X-axis title
#' @param y_title Y-axis title
#' @return A highchart object
hc_line_chart <- function(data, title = "", subtitle = "",
                          x_title = "", y_title = "") {
  hc <- highchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = x_title)) %>%
    hc_yAxis(title = list(text = y_title)) %>%
    hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
    hc_add_theme(hc_theme_flatdark())

  return(hc)
}

#' Create a career timeline chart
#' @param career_data Data frame with season stats
#' @param stat_column Column name of the statistic to plot
#' @param title Chart title
#' @param annotations Optional list of annotations (championships, awards, etc.)
#' @return A highchart object
create_career_timeline <- function(career_data, stat_column, title = "Career Stats",
                                   annotations = NULL) {
  hc <- highchart() %>%
    hc_chart(type = "areaspline") %>%
    hc_title(text = title) %>%
    hc_xAxis(
      categories = career_data$season,
      title = list(text = "Season")
    ) %>%
    hc_yAxis(title = list(text = stat_column)) %>%
    hc_add_series(
      name = stat_column,
      data = career_data[[stat_column]],
      color = "#e94560",
      fillOpacity = 0.3
    ) %>%
    hc_tooltip(
      pointFormat = "<b>{point.y:.1f}</b>",
      headerFormat = "<span style='font-size:10px'>{point.key}</span><br/>"
    ) %>%
    hc_add_theme(hc_theme_flatdark())

  # Add annotations if provided
  if (!is.null(annotations)) {
    hc <- hc %>%
      hc_annotations(list(
        labels = annotations
      ))
  }

  return(hc)
}

#' Create a comparison bar chart
#' @param data Data frame with categories and values
#' @param categories_col Column name for categories
#' @param values_col Column name for values
#' @param title Chart title
#' @return A highchart object
create_comparison_chart <- function(data, categories_col, values_col,
                                    title = "Comparison") {
  hc <- highchart() %>%
    hc_chart(type = "bar") %>%
    hc_title(text = title) %>%
    hc_xAxis(categories = data[[categories_col]]) %>%
    hc_yAxis(title = list(text = values_col)) %>%
    hc_add_series(
      name = values_col,
      data = data[[values_col]],
      colorByPoint = TRUE
    ) %>%
    hc_colors(c("#e94560", "#1a1a2e", "#16213e", "#0f3460", "#533483")) %>%
    hc_add_theme(hc_theme_flatdark())

  return(hc)
}

#' Create a radar chart for player attributes
#' @param data Data frame with attribute names and values
#' @param title Chart title
#' @return A highchart object
create_radar_chart <- function(data, title = "Player Attributes") {
  hc <- highchart() %>%
    hc_chart(polar = TRUE, type = "line") %>%
    hc_title(text = title) %>%
    hc_xAxis(
      categories = data$attribute,
      tickmarkPlacement = "on",
      lineWidth = 0
    ) %>%
    hc_yAxis(
      gridLineInterpolation = "polygon",
      lineWidth = 0,
      min = 0
    ) %>%
    hc_add_series(
      name = "Value",
      data = data$value,
      pointPlacement = "on",
      color = "#e94560"
    ) %>%
    hc_add_theme(hc_theme_flatdark())

  return(hc)
}

# =============================================================================
# CAREER STATS FUNCTIONS
# =============================================================================

#' Create a data frame with career statistics for historical players
#' @param player_name Player name
#' @return A data frame with career stats by season
get_career_stats <- function(player_name) {
  # This function would typically pull from an API or database

# For now, we'll define stats manually for historical players

  career_stats <- list(
    "Michael Jordan" = tibble(
      season = c("1984-85", "1985-86", "1986-87", "1987-88", "1988-89", "1989-90",
                 "1990-91", "1991-92", "1992-93", "1994-95", "1995-96", "1996-97",
                 "1997-98", "2001-02", "2002-03"),
      ppg = c(28.2, 22.7, 37.1, 35.0, 32.5, 33.6, 31.5, 30.1, 32.6, 26.9, 30.4, 29.6, 28.7, 22.9, 20.0),
      rpg = c(6.5, 3.6, 5.2, 5.5, 8.0, 6.9, 6.0, 6.4, 6.7, 6.9, 6.6, 5.9, 5.8, 5.7, 6.1),
      apg = c(5.9, 2.9, 4.6, 5.9, 8.0, 6.3, 5.5, 6.1, 5.5, 5.3, 4.3, 4.3, 3.5, 5.2, 3.8),
      championships = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0)
    ),
    "Larry Bird" = tibble(
      season = c("1979-80", "1980-81", "1981-82", "1982-83", "1983-84", "1984-85",
                 "1985-86", "1986-87", "1987-88", "1988-89", "1989-90", "1990-91", "1991-92"),
      ppg = c(21.3, 21.2, 22.9, 23.6, 24.2, 28.7, 25.8, 28.1, 29.9, 22.3, 24.3, 19.4, 20.2),
      rpg = c(10.4, 10.9, 10.9, 11.0, 10.1, 10.5, 9.8, 9.2, 9.3, 9.3, 9.5, 8.5, 9.6),
      apg = c(4.5, 5.5, 5.8, 5.8, 6.6, 6.6, 6.8, 7.6, 6.1, 7.8, 7.5, 7.2, 6.8),
      championships = c(0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0)
    ),
    "Magic Johnson" = tibble(
      season = c("1979-80", "1980-81", "1981-82", "1982-83", "1983-84", "1984-85",
                 "1985-86", "1986-87", "1987-88", "1988-89", "1989-90", "1990-91", "1995-96"),
      ppg = c(18.0, 21.6, 18.6, 16.8, 17.6, 18.3, 18.8, 23.9, 19.6, 22.5, 22.3, 19.4, 14.6),
      rpg = c(7.7, 8.6, 9.6, 8.6, 7.3, 6.2, 5.9, 6.3, 6.2, 7.9, 6.6, 7.0, 5.7),
      apg = c(7.3, 8.6, 9.5, 10.5, 13.1, 12.6, 12.6, 12.2, 11.9, 12.8, 11.5, 12.5, 6.9),
      championships = c(1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0)
    ),
    "Bill Russell" = tibble(
      season = c("1956-57", "1957-58", "1958-59", "1959-60", "1960-61", "1961-62",
                 "1962-63", "1963-64", "1964-65", "1965-66", "1966-67", "1967-68", "1968-69"),
      ppg = c(14.7, 16.6, 16.7, 18.2, 16.9, 18.9, 16.8, 15.0, 14.1, 12.9, 13.3, 12.5, 9.9),
      rpg = c(19.6, 22.7, 23.0, 24.0, 23.9, 23.6, 23.6, 24.7, 24.1, 22.8, 21.0, 18.6, 19.3),
      apg = c(1.8, 2.9, 3.2, 3.7, 3.4, 4.5, 4.9, 4.7, 5.3, 4.8, 5.8, 4.6, 4.9),
      championships = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    ),
    "Kareem Abdul-Jabbar" = tibble(
      season = c("1969-70", "1970-71", "1971-72", "1972-73", "1973-74", "1974-75",
                 "1975-76", "1976-77", "1977-78", "1978-79", "1979-80", "1980-81",
                 "1981-82", "1982-83", "1983-84", "1984-85", "1985-86", "1986-87",
                 "1987-88", "1988-89"),
      ppg = c(28.8, 31.7, 34.8, 30.2, 27.0, 30.0, 27.7, 26.2, 25.8, 23.8, 24.8, 26.2,
              23.9, 21.8, 21.5, 22.0, 23.4, 17.5, 14.6, 10.1),
      rpg = c(14.5, 16.0, 16.6, 14.5, 14.5, 14.0, 13.3, 13.3, 12.9, 12.8, 10.8, 10.3,
              8.7, 7.5, 7.3, 7.9, 6.1, 6.7, 6.0, 4.5),
      apg = c(4.1, 3.3, 4.6, 5.0, 4.8, 4.1, 5.0, 3.9, 4.3, 5.0, 4.5, 3.4,
              3.0, 2.5, 3.5, 3.2, 3.5, 2.6, 1.7, 1.0),
      championships = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0)
    ),
    "Hakeem Olajuwon" = tibble(
      season = c("1984-85", "1985-86", "1986-87", "1987-88", "1988-89", "1989-90",
                 "1990-91", "1991-92", "1992-93", "1993-94", "1994-95", "1995-96",
                 "1996-97", "1997-98", "1998-99", "1999-00", "2000-01", "2001-02"),
      ppg = c(20.6, 23.5, 23.4, 22.8, 24.8, 24.3, 21.2, 21.6, 26.1, 27.3, 27.8, 26.9,
              23.2, 16.4, 18.9, 10.3, 11.9, 7.1),
      rpg = c(11.9, 11.5, 11.4, 12.1, 13.5, 14.0, 13.8, 12.1, 13.0, 11.9, 10.8, 10.9,
              9.2, 9.8, 9.6, 6.2, 9.1, 6.0),
      bpg = c(2.7, 3.4, 3.4, 2.7, 3.4, 4.6, 3.9, 4.3, 4.2, 3.7, 3.4, 2.9,
              2.2, 1.8, 2.5, 1.5, 1.6, 1.0),
      championships = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0)
    )
  )

  if (player_name %in% names(career_stats)) {
    return(career_stats[[player_name]])
  } else {
    warning(paste("Career stats not found for", player_name))
    return(NULL)
  }
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Format large numbers with commas
#' @param x Numeric value
#' @return Formatted string
format_number <- function(x) {
  format(x, big.mark = ",", scientific = FALSE)
}

#' Calculate shot zone percentages
#' @param shots_data Data frame with shot data
#' @return Data frame with zone percentages
calculate_zone_percentages <- function(shots_data) {
  shots_data %>%
    mutate(
      court_zone = case_when(
        distanceShot <= 8 ~ "Paint",
        distanceShot > 8 & distanceShot < 23 ~ "Mid-Range",
        distanceShot >= 23 ~ "3PT",
        TRUE ~ "Other"
      )
    ) %>%
    group_by(court_zone) %>%
    summarize(
      total_shots = n(),
      made_shots = sum(isShotMade, na.rm = TRUE),
      pct = round(made_shots / total_shots * 100, 1),
      .groups = "drop"
    )
}

#' Create a consistent theme for ggplot visualizations
#' @return A ggplot theme object
theme_bounce_back <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", color = "#1a1a2e"),
      plot.subtitle = element_text(size = 12, color = "#666"),
      axis.title = element_text(size = 11, color = "#333"),
      axis.text = element_text(size = 10, color = "#666"),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3)
    )
}

# =============================================================================
# COLOR PALETTES
# =============================================================================

#' Bounce Back color palette
bounce_back_colors <- list(
  primary = "#1a1a2e",
  secondary = "#16213e",
  accent = "#e94560",
  accent_hover = "#ff6b6b",
  paint = "#99CCFF",
  midrange = "#50C878",
  three_pt = "#FF9999",
  text = "#333333",
  text_light = "#666666"
)

#' NBA team colors
nba_team_colors <- list(
  "Cleveland Cavaliers" = "#860038",
  "Miami Heat" = "#98002E",
  "Los Angeles Lakers" = "#552583",
  "Golden State Warriors" = "#1D428A",
  "Oklahoma City Thunder" = "#007AC1",
  "Brooklyn Nets" = "#000000",
  "Phoenix Suns" = "#1D1160",
  "San Antonio Spurs" = "#C4CED4",
  "Chicago Bulls" = "#CE1141",
  "Boston Celtics" = "#007A33",
  "Houston Rockets" = "#CE1141"
)
