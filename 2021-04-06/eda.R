# eda

# load libraries
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(tmap)

forest <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')
forest_area <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv')
brazil_loss <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')
soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')
vegetable_oil <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/vegetable_oil.csv')


set.seed(1234)

ww <- ne_countries(scale = "medium", returnclass = "sf")

ll <- ww$name %>% length

val <- sample(c("a","b","c","d"), ll, replace=T)

ww <- ww %>% mutate(value=val)


ww %>%
  left_join(forest_area, by = )
ggplot(ww) +
  geom_sf(data = forestaes(fill=value),  col = "black", lwd = 0.3 )+
  xlab(NULL) + 
  ylab(NULL) 

data <- world %>%  

urb_anim = tm_shape(world) + tm_polygons() + 
  tm_shape(urban_agglomerations) + tm_dots(size = "population_millions") +
  tm_facets(along = "year", free.coords = FALSE)