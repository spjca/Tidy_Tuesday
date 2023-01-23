# https://urbandatapalette.com/post/2021-08-tessellation-sf/



library(tidytuesdayR)
library(sf)
library(dplyr)
library(mapview)
library(tmap)


tuesdata <- tidytuesdayR::tt_load('2022-07-05')


rent <- tuesdata$rent


permits <- tuesdata$sf_permits

new <- tuesdata$new_construction

permits$lon <- as.numeric(as.character(substr(permits$location, 9, 26)),20)
permits$lat <- as.numeric(as.character(substr(permits$location, 28, 44)),20)

t <- permits %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  st_as_sf(coords = c("lat", "lon"), crs = 2326, remove = FALSE)


mapview_test_points = mapview(t, cex = 3, alpha = .5, popup = NULL)


## squares
area_fishnet_grid = st_make_grid(t, c(150, 150), what = "polygons", square = TRUE)

# To sf and add grid ID
fishnet_grid_sf = st_sf(area_fishnet_grid) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(area_fishnet_grid)))

# count number of points in each grid
# https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
fishnet_grid_sf$n_colli = lengths(st_intersects(fishnet_grid_sf, t))

# remove grid without value of 0 (i.e. no points in side that grid)
fishnet_count = filter(fishnet_grid_sf, n_colli > 0)


tmap_mode("view")

map_fishnet = tm_shape(fishnet_count) +
  tm_fill(
    col = "n_colli",
    palette = "Reds",
    style = "cont",
    title = "Number of collisions",
    id = "grid_id",
    showNA = FALSE,
    alpha = 0.5,
    popup.vars = c(
      "Number of collisions: " = "n_colli"
    ),
    popup.format = list(
      n_colli = list(format = "f", digits = 0)
    )
  ) +
  tm_borders(col = "grey40", lwd = 0.7)

map_fishnet
