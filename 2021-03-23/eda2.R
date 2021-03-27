# eda 2

# libraries
library(tidyverse)       # Data management
library(ggplot2)     # Graph framework
library(wordcloud)
library(ggwordcloud) # Wordcloud using ggplot
library(gganimate)   # Animation
library(tm)
library(RColorBrewer) # color palettes
library(tidytext)
library(lubridate)
library(png)
library(grid)
library(jpeg)

# load data
unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')


rc_words <- roll_calls %>%
  mutate(yr = as.integer(year(date))) %>%
  filter(!is.na(short)) %>%
  unnest_tokens(word, short) %>%
  anti_join(stop_words, by = "word") %>%
  distinct(yr, word) %>%
  add_count(word, name = "word_count") %>%
  filter(word_count >= 25)

gg <- rc_words %>%
  ggplot(aes(label = word
             , size=word_count
             , color = word_count)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 7) +
  scale_colour_gradient(low =  "blue", high = "red") +
  labs(title = "Wordcloud of UN Roll Call Votes",
        caption = "Data from United Nations\nSeanPJ.com") +
  theme_minimal() 

gg2 <- gg + transition_time(yr) +
  labs(subtitle = 'Year: {frame_time}')

animate(gg2 ,nframes = 280, fps = .5,  width = 600, height = 400,end_pause=30)
anim_save("gg_anim_wc.gif")


image_file <- "unv3.png"
image_file2 <- "unv2.jpg"
# read in the image file
img <- readPNG(image_file)
img2 <- readJPEG(image_file2)

gg3 <- rc_words %>%
  ggplot(.,aes(label = word
             , size=word_count
             , color = word_count)) +  
  annotation_custom(rasterGrob(img, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 7) +
  scale_colour_gradient(low =  "blue", high = "red") +
  labs(title = "Annual Wordcloud of UN Roll Call Vote Topics",
       caption = "Data from United Nations\nSeanPJ.com") +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=15),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = '{frame_time}') +
  transition_time(yr)

animate(gg3,
        nframes = 120, 
        fps = .5,  
        width = 800, 
        height = 500,
        end_pause=30)

anim_save("gg_anim_wc.gif")
