library(babynames)   # Data
library(dplyr)       # Data management
library(ggplot2)     # Graph framework
library(ggwordcloud) # Wordcloud using ggplot
library(gganimate)   # Animation

data(babynames)

babies <- babynames %>%
  filter(year %in% c(1915, 2015)) %>%
  group_by(name, sex, year) %>%
  summarise(n=sum(n)) %>%
  arrange(desc(n)) %>%
  group_by(year, sex) %>%
  top_n(n=5) %>%
  ungroup() %>%
  select(name, sex)
  
  
babyyears <- babynames %>%
  inner_join(babies, by=c("name","sex")) %>%
  filter(year>=1915 & year %% 5 == 0) %>%  # Keep all years if you like
  mutate(year=as.integer(year)) 


gg <- babyyears %>%
  ggplot(aes(label = name, size=n)) +
  geom_text_wordcloud() +
  theme_classic()

gg2 <- gg + transition_time(year) +
  labs(title = 'Year: {frame_time}')

animate(gg2, end_pause=30)
anim_save("gg_anim_wc.gif")