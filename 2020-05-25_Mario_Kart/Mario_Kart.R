#Project originates from tidy Tuesday
#https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/2021-05-25
#Source
#https://mkwrs.com/

library(readr)
library(dplyr)
library(ggplot2)


#load records

records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

#Wondering what record_duration is and I'll wonder again.
#Record duration in days
head(records)

head(drivers)

#what track/type combo saw biggest decrease in time with shortcut

no_shortcut<- records %>%
  filter(shortcut =='No') %>%
  select(track, type,time) %>%
  group_by(track, type) %>%
  summarise(noshortcut_record =min(time))

shortcut<- records %>%
  filter(shortcut =='Yes') %>%
  select(track, type,time) %>%
  group_by(track, type) %>%
  summarise(shortcut_record =min(time))

#put the records back together
#removing type because only three lap groups appear to have shortcuts
joined_records <- no_shortcut %>%
    inner_join( shortcut) %>%
    mutate(pct_shortcut_improvement = ((noshortcut_record-shortcut_record)/noshortcut_record)*100) %>%
    filter(pct_shortcut_improvement != 0) %>%
    select(-type) %>%
    arrange(desc(pct_shortcut_improvement))


#Visual-- inspo here--https://github.com/javendano585/TidyTuesday/blob/main/2021/Week_22/2021_05_25_tidy_tuesday.Rmd

  ggplot(joined_records) +
  geom_segment(aes(x = noshortcut_record, xend = shortcut_record, y = reorder(track, pct_shortcut_improvement), yend = reorder(track, pct_shortcut_improvement), 
                   color =track, size=.5))+
  geom_text(aes(noshortcut_record, track, label=paste(round(pct_shortcut_improvement,0),'%')), size = 3,
                hjust = 1) +
  labs(x = 'Time(seconds) with and without shortcut + percent improvement',
       y = 'Track name',
         title = 'Mario Kart Record Times',
         subtitle = '3-lap records. Comparing record time with and without shortcut',
       caption = '@JaimeAAvendano; Data from Mario Kart World Records') +
#hide the legend  
  theme(legend.position = 'none')
  
  ggsave("MarioKartShortutImprovements.png")



