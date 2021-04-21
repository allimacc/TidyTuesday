library(dplyr)

#library(ggplot2)

#read file
netflix <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv ')

#figuring out what needs cleaning below
#rating_yr<-  netflix_movies_usa %>%
#  group_by(rating) %>%
#  summarise(first = min(release_year))
#rating_yr

#removing older movies because they have a different rating type
netflix_movies_usa <- netflix%>%
  filter(country == "United States" & type == "Movie"  & rating %in% c("G", "PG", "PG-13", "NC-17","R")) %>%
  select(release_year, rating, show_id, type)

usa_summary <- netflix_movies_usa %>%
  group_by(release_year, rating) %>%
  summarise(number_movies = n())

#check what I have
#usa_summary

ggplot(usa_summary, aes(x = release_year, y = number_movies, fill=rating)) +
  geom_col() +
  theme_classic()+
  labs(title = "Netflix US Movies by Release Year & Rating")



