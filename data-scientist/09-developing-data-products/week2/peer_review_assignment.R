library(tidyverse)
library(leaflet)

nba_arena_data <- read_csv("./nba_arena_data.csv")

nba_icons <- lapply(nba_arena_data$icon_url, function(url) {
  makeIcon(
    iconUrl <- url,
    iconWidth = 60, iconHeight = 40
  )
})

names(nba_icons) <- nba_arena_data$team

class(nba_icons) <- "leaflet_icon_set"

nba_arena_data %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat = ~loc_lat,
             lng = ~loc_lng,
             popup = ~paste(team, "@", arena, sep = "\n"),
             icon = ~nba_icons[team])
 