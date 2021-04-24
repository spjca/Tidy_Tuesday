
library(tidyverse)
library(scales)
library(fuzzyjoin)
library(ggthemes)
library(gganimate)

forest <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')

country_data <- forest %>%
  #filter(year == 2010) %>%
  inner_join(maps::iso3166, by = c(code = "a3"))

p <- map_data("world") %>%
  as_tibble() %>%
  filter(region != "Antarctica") %>%
  regex_left_join(country_data, by = c(region = "mapname")) %>%
  ggplot(aes(long, lat, group = group, fill = net_forest_conversion)) +
  geom_polygon(color = "black", size = .05) +
  scale_fill_gradient2(low = "red", high = "green",
                       labels = comma) +
  theme_map() +
  labs(fill = "Net forest change") +
  transition_manual(year)

animate(p)

