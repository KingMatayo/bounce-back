# Load required libraries
library(tidyverse)
library(BasketballAnalyzeR)
library(nbastatR)
library(ggimage)
library(cropcircles)
library(ggtext)
library(glue)
library(janitor)
library(htmltools)
library(sysfonts)
library(plotly)

# Set a grey color for consistency in the visualization
grey <- '#818990'

# Use the Chivo font (you must install this locally)
font <- sysfonts::font_add("Chivo", "./fonts/Chivo/static/Chivo-Regular.ttf")

#lebron's shots

all_shots <- read.csv("./data/allshots-2003-2024.csv") 

lebron_shots <- all_shots |> 
  filter(namePlayer == "LeBron James")

head(lebron_shots)

# Filter for specific seasons and scale the x and y coordinates
lebron_shots_season <- lebron_shots |>
  filter(yearSeason %in% c("2004", "2024")) |>
  # Scale x and y coordinates (court dimensions adjusted manually)
  mutate(x = (locationX / 10) - 0,
         y = (locationY / 10) - 41.75) |>
  # Remove shots from beyond half-court range
  filter(y < 0) |> 
  mutate(
    # Categorize shots based on distance from the basket
    court_zone = case_when(
      distanceShot <= 8 ~ "Paint",               # Shots close to the basket
      distanceShot > 8 & distanceShot < 23 ~ "Mid-Range",  # Shots outside the paint but inside the 3PT line
      distanceShot >= 23 ~ "3PT",                # Shots beyond the 3-point line
      TRUE ~ "Other"                            # Catch-all for undefined cases
    )
  )

# Group data by dateofGame and courtzone, then calculate stats
lebron_stats <- lebron_shots_season |>
  filter(dateGame %in% c("20031029", "20231029")) |>
  group_by(nameTeam,dateGame,court_zone) |>
  summarise(total_shots = n(), .groups = 'drop') |>
  pivot_wider(names_from = court_zone, values_from = total_shots, values_fill = 0) |>
  janitor::clean_names() |>
  mutate(total = x3pt + mid_range + paint,
         x3pt_pct = round(100 * x3pt / total,1),
          mid_range_pct = round(100 * mid_range / total,1),
          paint_pct = round(100 * paint / total, 1)) %>% 
  ungroup()

# Creating the visualization
# Step 1: Data frame with player images and labels
images <- data.frame(
  nameTeam = c( "Cleveland Cavaliers", "Los Angeles Lakers"),
  date = c("October 29, 2003", "October 29, 2023"),
  label = c("Rookie LeBron", "20-Year Vet LeBron"),
  image = c("./assets/lebron_2003.png",
            "./assets/lebron_2023.png")
) |>
  left_join(lebron_stats, by = c("nameTeam" = "name_team")) |>
  mutate(text_label = glue("<span style='font-size:14px;'>**{toupper(label)}**</span><br>
                            <span style='font-size:14px;'>**{toupper(date)}**</span><br>
                           <span style='font-size:12.5px;color:grey40;'>{nameTeam} · {total} shot attempts · </span><br>
                           <span style='color:#ED254E;font-size:12.5px;'>{round(x3pt_pct,0)}% were 3PT-ers</span><br>
                           <span style='color:#ED254E;font-size:12.5px;'>{round(mid_range_pct,0)}% were Mid-Range</span><br>
                           <span style='color:#ED254E;font-size:12.5px;'>{round(paint_pct,0)}% were in the Paint</span>")
  )

# Step 2: Circle crop player images
images$cropped <- cropcircles::circle_crop(
  images = images$image, border_size = 1, border_colour = "whitesmoke"
)

# Step 3: Creating the short chart visualization

# lebron shots for the 2 key games

lebron_shots_two_games <- lebron_shots_season |> 
  filter(dateGame %in% c("20031029", "20231029"))

# the shots data and image data (below) are mixed up - come back to this later to break it down and fix it 
# Might be best to create each shot chart separately, then add the images, also separately - food for thought
shot_chart_draft <- 
  # draws the NBA half court
  BasketballAnalyzeR::drawNBAcourt(
    ggplot(data = lebron_shots_two_games), size = 0.5, col = "grey20") +
  
  # shot data
  geom_point(aes(x = x, y = y, fill = court_zone), shape = 21, color = "white", size = 2.5, alpha = 0.8) + 
  
  # backdrop image with fill to create border
  geom_image(data=images, mapping=aes(x=-20, y=6, image=cropped), color="#818990", size=0.16, asp=1/1.2) +
  
  # player image
 geom_image(data = images, aes(x = -20, y = 6, image = cropped), size = 0.15, asp = 1 / 1.2) +
  
  # text per player with name, team, and stats
  
  ggtext::geom_textbox(data = images, aes(x = -15, y = -7, label = text_label), 
                       fill = NA, box.size = NA) +
  
  # Create your own discrete scale
  scale_fill_manual(values=rev(c("#99CCFF", "#50C878", "#FF9999"))) +
  
  # For position scales, a vector of range expansion constants used to add some padding around the data to ensure that they are placed some distance away from the axes
  
  scale_y_continuous(expand=c(0.1,0.2)) +
  
  # create small multiple chart for each player across 2 columns
  
  facet_wrap(~ label, ncol = 2) +
  
  # Cartesian coordinates with fixed "aspect ratio"
  coord_equal() +
  
  # Set guides for each scale
  guides(fill = guide_legend(override.aes=list(size=5)))

shot_chart_draft
