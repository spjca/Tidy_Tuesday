# eda 2

# libraries
library(tidyverse)
library(lubridate)
library(scales)
library(data.table)
library(hrbrthemes)
library(viridis)

# load data
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

games <- games %>%
  mutate(avg_peak_perc = parse_number(avg_peak_perc) / 100) %>%
  mutate(date = ymd(paste(year, month, 1))) %>%
  filter(date > min(date)) %>%
  mutate(month = fct_reorder(month, month(date)))

x <- games %>%
  group_by(gamename) %>%
  summarise(maxPeak = max(peak)) %>%
  arrange(desc(maxPeak) )



filtData <- games %>%
  filter(gamename == "PLAYERUNKNOWN'S BATTLEGROUNDS" |
         gamename == "Counter-Strike: Global Offensive" |
           gamename == "Dota 2" |
           gamename == "Cyberpunk 2077" |
           gamename == "Terraria" |
           gamename == "Fallout 4" |
           gamename == "Life is Strange 2" |
           gamename == "Grand Theft Auto V" |
           gamename == "HITMAN\u0099 2" |
           gamename == "World of Warships" 
            ) 


ggplot(data = filtData, 
       aes(x = date,
           y = peak,
           group = gamename, 
           color = gamename)) +
  geom_line()

# use data table for easier manipultion    
x <- data.table(games)
doomData  <- x[gamename %like% "DOOM"& peak >100]

############## doom

#doomPlot <- 
a <- ggplot(data = doomData) +
  geom_line(aes(x = date,
                y = peak,
                group = gamename,
                color = gamename)) +
  scale_y_log10(labels = comma) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 0.5
  )) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill=NA)
        ) +
  labs(x="Year", 
       y="Max Daily Players (log scale)",
       title = "Max Daily Players",
       subtitle = "Doom Franchise 2012 - 2021",
       caption="Data from Valve\nSeanPJ.com") 

##################### total war

twData  <- x[gamename %like% "Total War" & peak >100 ]
b <- ggplot(data = twData) +
  geom_line(aes(x = date,
                y = peak,
                group = gamename,
                color = gamename)) +
  scale_y_log10(labels = comma) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 0.5
  )) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill=NA)
  ) +
  labs(x="Year", 
       y="Max Daily Players (log scale)",
       title = "Max Daily Players",
       subtitle = "Total War Franchise 2012 - 2021",
       caption="Data from Valve\nSeanPJ.com")

############### gta

c <- gtaData  <- x[gamename %like% "Grand Theft" & peak >100 ]
ggplot(data = gtaData) +
  geom_line(aes(x = date,
                y = peak,
                group = gamename,
                color = gamename)) +
  scale_y_log10(labels = comma) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 0.5
  )) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill=NA)
  ) +
  labs(x="Year", 
       y="Max Daily Players (log scale)",
       title = "Max Daily Players",
       subtitle = "Grand Theft Auto Franchise 2012 - 2021",
       caption="Data from Valve\nSeanPJ.com")

################## world of


d <- woData  <- x[gamename %like% "World of" & !gamename %like% "Goo" & peak >100 ]
ggplot(data = woData) +
  geom_line(aes(x = date,
                y = peak,
                group = gamename,
                color = gamename)) +
  scale_y_log10(labels = comma) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 0.5
  )) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill=NA)
  ) +
  labs(x="Year", 
       y="Max Daily Players (log scale)",
       title = "Max Daily Players",
       subtitle = "World of War Franchise 2012 - 2021",
       caption="Data from Valve\nSeanPJ.com")

################## hitman

e <- hitmanData  <- x[gamename %like% "Hitman" & peak >100 ]
ggplot(data = hitmanData) +
  geom_line(aes(x = date,
                y = peak,
                group = gamename,
                color = gamename)) +
  scale_y_log10(labels = comma) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 0.5
  )) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill=NA)
  ) +
  labs(x="Year", 
       y="Max Daily Players (log scale)",
       title = "Max Daily Players",
       subtitle = "Hitman Franchise 2012 - 2021",
       caption="Data from Valve\nSeanPJ.com")

################## pubg

pubgData  <- x[gamename %like% "PLAYERUNKNOWN" & peak >100 ]
ggplot(data = pubgData) +
  geom_line(aes(x = date,
                y = peak,
                group = gamename,
                color = gamename)) +
  scale_y_log10(labels = comma) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 0.5
  )) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill=NA)
  ) +
  labs(x="Year", 
       y="Max Daily Players (log scale)",
       title = "Max Daily Players",
       subtitle = "PUBG 2017 - 2021",
       caption="Data from Valve\nSeanPJ.com")

################## cs

csData  <- x[gamename %like% "Counter-Strike" & peak >100 ]
f <- ggplot(data = csData) +
  geom_line(aes(x = date,
                y = peak,
                group = gamename,
                color = gamename)) +
  scale_y_log10(labels = comma) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 0.5
  )) +
  theme(legend.position="bottom",
        legend.background = element_rect(fill=NA)
  ) +
  labs(x="Year", 
       y="Max Daily Players (log scale)",
       title = "Max Daily Players",
       subtitle = "Counter-Strike Franchise 2012 - 2021",
       caption="Data from Valve\nSeanPJ.com")

## instead of rewriting everything each time, ill make a function

plotter <- function(input){
  ggplot(data = input) +
    geom_line(aes(x = date,
                  y = peak,
                  group = gamename,
                  color = gamename)) +
    scale_y_log10(labels = comma) +
    scale_fill_viridis(discrete = T) +
    theme_ipsum() + 
    theme(axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 0.5
    )) +
    theme(legend.position="bottom",
          legend.background = element_rect(fill=NA)
    ) +
    labs(x="Year", 
         y="Max Daily Players (log scale)",
         title = "Max Daily Players",
         subtitle = paste(input$gamename," Franchise 2012 - 2021"),
         #subtitle = "Franchise 2012 - 2021",
         caption="Data from Valve\nSeanPJ.com")  
} 

plotter(csData)
plotter(doomData)
# much better, but I need to fix the subtitle


#################### football manager

mgrData  <- x[gamename %like% "Football Manager" & peak >100 ]
g <- ggplot(data = mgrData) +
  geom_line(aes(x = date,
                y = peak,
                group = gamename,
                color = gamename)) +
  scale_y_log10(labels = comma) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 0.5
  )) +
  theme(legend.position="right",
        legend.background = element_rect(fill=NA)
  ) +
  labs(x="Year", 
       y="Max Daily Players (log scale)",
       title = "Max Daily Players",
       subtitle = "Football Manager Franchise 2012 - 2021",
       caption="Data from Valve\nSeanPJ.com")


#################### civilization

civData  <- x[gamename %like% "Sid Meier's Civilization" & peak >100 ]
h <- ggplot(data = civData) +
  geom_line(aes(x = date,
                y = peak,
                group = gamename,
                color = gamename)) +
  scale_y_log10(labels = comma) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 0.5
  )) +
  theme(legend.position="right",
        legend.background = element_rect(fill=NA)
  ) +
  labs(x="Year", 
       y="Max Daily Players (log scale)",
       title = "Max Daily Players",
       subtitle = "Civilization Franchise 2012 - 2021",
       caption="Data from Valve\nSeanPJ.com")


#################### nba 2k

nbaData  <- x[gamename %like% "NBA 2K" & peak >100 ]
i <- ggplot(data = nbaData) +
  geom_line(aes(x = date,
                y = peak,
                group = gamename,
                color = gamename)) +
  scale_y_log10(labels = comma) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 0.5
  )) +
  theme(legend.position="right",
        legend.background = element_rect(fill=NA)
  ) +
  labs(x="Year", 
       y="Max Daily Players (log scale)",
       title = "Max Daily Players",
       subtitle = "NBA 2K Franchise 2012 - 2021",
       caption="Data from Valve\nSeanPJ.com")


#################### borderlands

blandsData  <- x[gamename %like% "Borderlands" & peak >100 ]
j <- ggplot(data = blandsData) +
  geom_line(aes(x = date,
                y = peak,
                group = gamename,
                color = gamename)) +
  scale_y_log10(labels = comma) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 0.5
  )) +
  theme(legend.position="right",
        legend.background = element_rect(fill=NA)
  ) +
  labs(x="Year", 
       y="Max Daily Players (log scale)",
       title = "Max Daily Players",
       subtitle = "Borderlands Franchise 2012 - 2021",
       caption="Data from Valve\nSeanPJ.com")


#################### star wars

swData  <- x[gamename %like% "STAR WARS" & peak >100 ]
k <- ggplot(data = swData) +
  geom_line(aes(x = date,
                y = peak,
                group = gamename,
                color = gamename)) +
  scale_y_log10(labels = comma) +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 0.5
  )) +
  theme(legend.position="right",
        legend.background = element_rect(fill=NA)
  ) +
  labs(x="Year", 
       y="Max Daily Players (log scale)",
       title = "Max Daily Players",
       subtitle = "STAR WARS Franchise 2012 - 2021",
       caption="Data from Valve\nSeanPJ.com")
