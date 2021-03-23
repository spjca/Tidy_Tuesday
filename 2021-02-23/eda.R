# libraries
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(scales)
library(dplyr)
library(gganimate)


# load data
employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')
earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

earn$yrQtr <- apply( earn[ , c("year","quarter") ] , 1 , paste , collapse = "-" )


en <- earn %>%
  filter(
    sex != 'Both Sexes' &
    race == 'All Races' & 
      (age == '16 to 24 years' | 
       age == '25 to 34 years' |
      age == '35 to 44 years' |
      age == '35 to 44 years' |
      age == '45 to 54 years' |
      age == '55 to 64 years' |
      age == '65 years and over') &
      ethnic_origin == 'All Origins'
           ) %>%
  select(sex,
         age,
         #ethnic_origin,
         yrQtr,median_weekly_earn) 


en_plot <- en %>%
  ggplot(.) +
  geom_line(aes(x=yrQtr,y=median_weekly_earn, group = age, color = age)) +
  facet_grid(~ sex)+
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  theme(axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 0.5
        )) +
  theme(legend.position="bottom") +
  labs(x="Year", y="Median $/Week Earned",
       title = "Median Weekly Earnings by Gender",
       #subtitle = "2010 - 2020",
       caption="SeanPJ.com\n Data from US Bureau of Labor Statistics") 
  
en_plot


# lets look at employment data as a divergent bar chart
# first there is an error in the minor_occupation data
employed$minor_occupation[employed$minor_occupation=="Manage-ment, business, and financial operations occupations"] <- "Management, business, and financial operations occupations"

e <- employed %>%
  filter(race_gender=='Men' | race_gender =='Women') %>%
  select(minor_occupation,race_gender,employ_n,year)  %>%  
  #flip the sign to make them back to back
  mutate(employ_n_inv= ifelse(race_gender =="Men",employ_n,employ_n*-1)) %>% 
  select(!employ_n)
  

x <- ggplot(e, aes(x=minor_occupation,y=employ_n_inv,fill=race_gender)) +
  geom_bar(stat="identity",position="identity") +
  coord_flip() +
  labs(y = "Number of Employees",
       x = "Occupation",
       title = "Number of Employees by Occupation and Gender 2015-2020",
       subtitle = "Year: {closest_state}",
       caption = "Data from US Bureau of Labor Statistics\nSeanPJ.com") + 
  theme(axis.line=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major.x = element_line( size=.1, color="grey" ),
    panel.grid.minor.x = element_line( size=.1, color="grey" ),
    plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust=-1),
    plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"),
    plot.caption =element_text(size=8, hjust=0.5, face="italic", color="black"),
    plot.background=element_blank(),
    plot.margin = margin(2,2, 2, 4, "cm")) +
  scale_y_continuous(labels = comma) 

anim <- x + transition_states(year, transition_length = 4, state_length = 1)


animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))


