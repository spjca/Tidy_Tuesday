# eda

# libraries
library(tidyverse)       # Data management
library(ggplot2)     # Graph framework
library(wordcloud)
library(ggwordcloud) # Wordcloud using ggplot
library(gganimate)   # Animation
library(tm)
library(RColorBrewer) # color palettes

# load data
unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

# animated word cloud of how resolutions change over time
# https://stackoverflow.com/questions/61132650/is-there-a-way-to-animate-a-word-cloud-in-r
# https://semba-blog.netlify.app/11/05/2019/wordclouds-plotting-with-ggwordcloud-package-in-r/

allWords <- paste(roll_calls$short, collapse = " ")

corpus <- allWords %>% 
  tm::VectorSource()%>% 
  tm::Corpus()

corpusClean <- corpus %>% 
  tm_map(FUN = content_transformer(tolower)) %>% # Convert the text to lower case
  tm_map(FUN = removeNumbers) %>% # Remove numbers
  tm_map(removeWords, stopwords("english")) %>% # Remove english common stopwords
  tm_map(removeWords, c("will", "let", "ring")) %>%   # Remove words 
  tm_map(removePunctuation) %>%   # Remove punctuations
  tm_map(stripWhitespace)   # Eliminate extra white spaces


corpusCleanTbl <- corpusClean %>% 
  tm::TermDocumentMatrix() %>% 
  as.matrix() %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>%
  dplyr::rename(word = 1, freq = 2) %>%
  dplyr::arrange(desc(freq))

# fail
ggplot(data = corpusCleanTbl, 
       aes(label = word, size = log(freq), col = as.character(freq))) + 
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      grid_size = 1, eccentricity = .9)+
  scale_size_area(max_size = 14)+
  scale_color_brewer(palette = "Paired", direction = -1)+
  theme_void()



BG <- roll_calls %>% 
  select(session, short) %>%
  group_by(session) %>%
  summarise(text = paste0(short, collapse = " ")) 


x <- BG$text %>%
  tm::VectorSource() %>% 
  tm::Corpus() %>% 
  tm_map(FUN = content_transformer(tolower)) %>% # Convert the text to lower case
  tm_map(FUN = removeNumbers) %>% # Remove numbers
  tm_map(removeWords, stopwords("english")) %>% # Remove english common stopwords
  tm_map(removeWords, c("will", "let", "ring")) %>%   # Remove words 
  tm_map(removePunctuation) %>%   # Remove punctuations
  tm_map(stripWhitespace) %>%   # Eliminate extra white spaces 
  tm::TermDocumentMatrix() %>% 
  as.matrix() %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>%
  dplyr::rename(word = 1, freq = 2) %>%
  dplyr::arrange(desc(freq))
