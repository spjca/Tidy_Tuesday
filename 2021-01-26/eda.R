# tidy tuesday week 5 analysis - plastic pollution

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-26/readme.md

library(tidyverse)
library(leaflet)
library(maps)
library(sf)

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

str(plastics)

plastics$grand_total <- plastics$grand_total %>%
                        replace_na(0)

# first let's look at the total plastic count for each company 
company_totals <- aggregate(plastics$grand_total, by = list(plastics$parent_company), FUN = sum)

# next each country's plastic totals
country_yearly_totals <- aggregate(plastics$grand_total, by = list(plastics$country, plastics$year), FUN = sum)
colnames(country_yearly_totals) <- c("ADMIN", "Year", "Total")

cyt <- plastics %>%
  group_by(country,year) %>%
  summarise(Total = sum(grand_total))

# create grand total aggregates and volunteer count columns in seperate data frames
# to be joined and read into to the leaflet map
c_2019 <- country_yearly_totals %>%
  filter(Year == "2019") %>%
  select(-Year)
colnames(c_2019) <- c("ADMIN","Total_2019")

c_2020 <- country_yearly_totals %>%
  filter(Year == "2020") %>%
  select(-Year)
colnames(c_2020) <- c("ADMIN","Total_2020")

v_2019 <- plastics %>%
  filter(year == "2019") %>%
  select(country,volunteers) %>%
  distinct()
colnames(v_2019) <- c("ADMIN","Volunteers_2019")

v_2020 <- plastics %>%
  filter(year == "2020") %>%
  group_by(country, volunteers) %>%
  select(country, volunteers) %>%
  distinct()
colnames(v_2020) <- c("ADMIN","Volunteers_2020")

# https://datahub.io/core/geo-countries#resource-countries
geojson_countries <- geojson_sf("https://datahub.io/core/geo-countries/r/countries.geojson")

# now merge the aggregated call data with the spacial data frame
m <- sp::merge(geojson_countries,c_2019, by = 'ADMIN')
m <- sp::merge(m,c_2020, by = 'ADMIN')
m <- sp::merge(m,v_2019, by = 'ADMIN')
m <- sp::merge(m,v_2020, by = 'ADMIN')

# create variable with label information pulled from geojson data
mytext <- paste(
  "Country: ", m$ADMIN, 
  "<br/> 2020 - Total Plastic: ", m$Total_2020, "&emsp;Total Volunteers: ", m$Volunteers_2020,
  "<br/> 2019 - Total Plastic: ", m$Total_2019, "&emsp;Total Volunteers: ", m$Volunteers_2019,
  sep = " "
) %>%
  lapply(htmltools::HTML)



leaflet(m) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, 
              smoothFactor = 0.3, 
              label = mytext,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))
