#from https://github.com/cnicault/tidytuesday/commit/8fcd2a5be0b9047d39151c425ac49d8491673f99

install.packages(sysfonts)
remove.packages(sysfonts)

#wht does this do?
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(lubridate)
library(ggfx)
#these below don't work, so moving on 
#library(sysfonts)
#library(showtext)


#function to get a font I want

#these don't work without showtext
#font_add_google("Oswald", "oswald")
#font_add_google("Roboto Condensed", "roboto condensed")
#font_add_google("Share Tech Mono", "techmono")

#get file

netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

#choose some colors
movie_dark <- "#730E16"
movie_light <- "#D92B4B"
tv_dark <- "#593515"
tv_light <- "#D9AD77"
netflix_clr <- "#E50914"

#data prep
netflix_clean <- netflix_titles %>%
  mutate(date = mdy(date_added),
         year = year(date),
         month = month(date),
         date_ym = floor_date(date, "month")) %>%
  group_by(date_ym, type) %>%
  summarise(total = n()) %>%
  ungroup() 



#get tv specific stuff
df_tv <- netflix_clean %>%
  filter(type == "TV Show") %>%
  arrange(date_ym) %>%
  mutate(base = cumsum(total),
         base = lag(base, default = 0))


#df_tv


#get movies
df_movie <- netflix_clean %>%
  filter(type == "Movie") %>%
  arrange(date_ym) %>%
  mutate(base = cumsum(total),
         base = lag(base, default = 0)) 


years <- tibble(year = seq(2014, 2020, 1)) %>%
  mutate(date_ym = ymd(glue::glue("{year}-06-01")))

#make the plot
final <- ggplot() +
  geom_rect(data = df_movie, aes(xmin = date_ym, xmax = date_ym + months(1), ymin = 0, ymax = base), fill = movie_light, color = NA) +
  geom_rect(data = df_movie, aes(xmin = date_ym, xmax = date_ym + months(1), ymin = base, ymax = base + total), fill = movie_dark, color = NA) +
  geom_rect(data = df_tv, aes(xmin = date_ym + months(1), xmax = date_ym, ymin = 0, ymax = -base), fill = "#D9AD77", color = NA) +
  geom_rect(data = df_tv, aes(xmin = date_ym + months(1), xmax = date_ym, ymin = -base, ymax = -base - total), fill = tv_dark, color = NA)+
  geom_text(data = years, aes(x = date_ym, y = 0, label = year), , size = 5, color = "white") +
  annotate("text", x = ymd("2020-09-01"), y = -1000, label = "TV Shows",, size = 6, fontface = "bold", color = tv_dark) +
  annotate("text", x = ymd("2021-04-01"), y = -sum(df_tv$total), label = sum(df_tv$total), , size = 4, fontface = "bold", color = tv_light, vjust = 0) +
  annotate("text", x = ymd("2020-09-01"), y = 2410, label = "Movies",, size = 6, fontface = "bold", color = movie_dark) +
  annotate("text", x = ymd("2021-04-01"), y = sum(df_movie$total), label = sum(df_movie$total), , size = 4, fontface = "bold", color = movie_light, vjust = 0) +
  annotate("text", x = ymd("2017-06-01"), y = 2000, label = "NETFLIX catalogue", , size = 8, fontface = "bold", color = netflix_clr, hjust = 0) +
  annotate("text", x = ymd("2016-02-01"), y = 1500, label = str_wrap("Cumulative sum of movies and TV shows added each month. Darker colors: number of movies / TV show for the month, ligher colors: total number of movies / TV show available at that time", 80), family = "roboto condensed", size = 3, color = "white", hjust = 0, lineheight = 1) + 
  scale_x_date(limits = c(ymd("2014-01-01"), ymd("2021-09-01"))) +
  coord_flip() +
  guides(fill = FALSE) +
  labs(caption = "Visualization: Christophe Nicault | Data: Kaggle / Shivam Bansal") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey40", color = NA),
         plot.caption = element_text(, color = "white", size = 8, hjust = 0.96, margin = margin(0,0,5,0)))
 ggsave(final, file="nextflix.png", scale= 1, height=4, width=10, dpi=320, units = "in" )
 ragg::agg_png(here::here("render", paste0("netflix", "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), res = 320, width = 10, height = 4, units = "in")
final

dev.off()
