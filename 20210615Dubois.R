library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

#retrive info
tweets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-15/tweets.csv')

str(tweets)
glimpse(tweets)

#is date a date? No
class(tweets$datetime)
#tweets are in pdt and pst timezone
as.POSIXct.Date(tweets$datetime)

#convert to date
tweets$datetime <-as.Date(tweets$datetime)

#working out which format I want
format(tweets$datetime[5],format="%B-%Y")


tweets_dates <-tweets %>%
          group_by(datetime) %>%
          arrange(datetime) %>%
          tally() %>%
          na.omit() 


  
ggplot( data = tweets_dates, aes(x = datetime, y = n, fill ="#FA8775") )+
            geom_col() +
  #remove some of the dates, to reduce clutter
            scale_x_date(date_labels = "%B-%Y") +
            labs( title="#DuBoisChallenge Tweets 2021",
                  y = "Number of Tweets",
                  x = "Tweet Date",
                  caption = "Source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-06-15") +
                  theme(plot.title = element_text(hjust = 0.5,color="#B00000", face = "bold" ),
                        plot.caption = element_text(color="#B00000", hjust = 0),
                        axis.title.x = element_text(color ="#B00000"),
                        axis.title.y = element_text(color ="#B00000"),
                        legend.position = "none",
                        panel.background = element_blank())
ggsave("DubiosTweetsbyTime2021.png")



#text analysis
#https://www.tidytextmining.com/tidytext.html

library(tidytext)

#one line for every word in every tweet
tweet_df <-tweets %>%
  unnest_tokens(word, content) %>%
  select (word, datetime)

#r already has stop words, will try them out
data(stop_words)


#remove stop words from my df

tweet_df<- tweet_df %>%
  anti_join(stop_words)

tweet_df %>%
  count(word, sort = TRUE) 


#these tweets are pretty boring but let's visualize
tweet_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 30 & word != "https" & word !="t.co") %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = n, y = word)) +
  geom_col(fill = "#003f81") +
  xlim(0,500)+
  labs(y = NULL,
       title = "TidyTuesday Dubois Challenge Tweets",
       x = "Word Frequency")+
  theme_minimal()
ggsave("DubiosFreqWords.png")

#adding one more line
  