# eda 

# libraries
library(tidyverse)
library(lubridate)
library(scales)


# load data
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')


# data investigation and cleaning 

games <- games %>%
  mutate(avg_peak_perc = parse_number(avg_peak_perc) / 100) %>%
  mutate(date = ymd(paste(year, month, 1))) %>%
  filter(date > min(date)) %>%
  mutate(month = fct_reorder(month, month(date)))
 
games %>%
  count(date) %>%
  ggplot(aes(date, n)) +
  geom_col()

games %>%
  filter(date == max(date)) %>%
  ggplot(aes(avg)) +
  geom_histogram() +
  scale_x_log10(labels = comma,
                breaks = 10 ^ seq(0, 5)) +
  labs(x = "Average # of players across Feb 2021")


# over 1200 games but how ofthen they show up varies considerably, 
# I need to trim this somehow to make it readable
# lets start with more than 100 months
keepList <-games %>% 
  group_by(gamename)  %>% 
  summarise(total = sum(n())) %>%
  filter(total>100) %>%
  arrange(desc(total))

gamesFiltered <- games %>%
              filter(gamename %in% keepList$gamename)


data <- games %>%
  count(yearMonth) 

# circular barplot

# Set a number of 'empty bar'
empty_bar <- 10

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(data))
colnames(to_add) <- colnames(data)
data <- rbind(data, to_add)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=n)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", fill=alpha("green", 0.3)) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 0) + 
  geom_text(data=label_data, aes(x=id, y=n+10, label=yearMonth, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

p
