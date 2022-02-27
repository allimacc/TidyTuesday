library(readr)
library(dplyr)
library(ggplot2)
library(stringr)



setwd("~/rprojects/TidyTuesday/2022-02-22")


#get file
freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')


#what am I looking at?
freedom %>% 
  filter(country == "United States of America")


#Oceania is just a mess
freedom %>%
  rename(region_name=Region_Name) %>%
  group_by(region_name, year) %>%
  summarise( total_political_rights = sum(PR), 
             total_civil_liberties =sum(CL)) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(pct_change_rights = (total_political_rights/lag(total_political_rights) -1)  ) %>%
  ggplot(aes(x=year, y=pct_change_rights, color=region_name))+
  geom_line() +
  labs(title ="Percent YOY Change in Human Rights Across Continents") +
  scale_y_continuous(label = label_percent()) +
  theme_minimal()+
  ylim(-0.15,0.15) +
  theme(axis.title.y = element_blank())



#Oceania is just a mess
freedom %>%
  rename(region_name=Region_Name) %>%
  filter(!region_name =='Oceania') %>%
  group_by(region_name, year) %>%
  summarise( total_political_rights = sum(PR), 
             total_civil_liberties =sum(CL)) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(pct_change_rights = (total_political_rights/lag(total_political_rights) -1)  ) %>%
  ggplot(aes(x=year, y=pct_change_rights, color=region_name))+
  geom_line() +
  labs(title ="Percent YOY Change in Human Rights Across Continents") +
  scale_y_continuous(label = label_percent()) +
  theme_minimal()+
  theme(axis.title.y = element_blank())


#Am I even looking at this correctly?

#What if I just look at united states
freedom %>%
  filter(country =='Tunisia') %>%
  group_by(country,  year) %>%
  summarise( total_political_rights = sum(PR), 
             total_civil_liberties =sum(CL)) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(pct_change_rights = (total_political_rights/lag(total_political_rights) -1)  ) %>%
  ggplot(aes(x=year, y=pct_change_rights, color=country))+
  geom_line() +
  labs(title ="Percent YOY Change in Human Rights in Tunisia") +
  scale_y_continuous(label = label_percent()) +
  theme_minimal()+
  theme(axis.title.y = element_blank())

#finding one I like to follow along and improve my skills
#https://github.com/cnicault/tidytuesday/commit/a87aae8ef9474ea006e725626a762366ffefed83

#parameters
col_low <- "#FA7F08"
col_high <- "#348888"
col_mid <- "#9EF8EE"
bck_clr <- "grey20"
seg_size <- 0.01

freedom %>%
  filter(year==2020) %>%
  group_by(PR, CL) %>%
  summarise(total=n())%>%
  mutate(prod = CL * PR) %>%
  ggplot(aes(x=PR, y=CL, size=total, color=prod)) +
  scale_color_gradient2(low = col_low, high = col_high, mid = col_mid, midpoint = 24) +
  geom_point() +
  geom_text(aes(label=total), size =4, fontface="bold", color ="black") +
  annotate("text", x=4, y=-.25, label = "Civil Liberites", fontface ="bold") +
  annotate("text", x =-.25, y =4, label = "Political Rights", angle = 90, fontface = "bold") +
  guides(color="none", size = "none") +
  scale_size(range= c (5,25)) +
  labs (title = "2020 Situation") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size =15, fontface = "bold"))
  
#need to set up so that lowest number is actually bigger here
freedom %>%
  mutate(CL = 8 - CL,
         PR = 8 - PR) %>%
  filter(year==2020 ) %>%
  group_by(PR, CL, Region_Name) %>%
  summarise(total=n())%>%
  mutate(prod = -CL * -PR) %>%
  ggplot(aes(x=PR, y=CL, size=total, color=prod)) +
  scale_color_gradient2(low = col_low, high = col_high, mid = col_mid, midpoint = 24) +
  geom_point() +
  facet_wrap(vars(Region_Name))+
  geom_text(aes(label=total), size =4,  color ="black") +
  #annotate("text", x=4, y=-.25, label = "Civil Liberites", fontface ="bold", size =3) +
  #annotate("text", x =-.25, y =4, label = "Political Rights", angle = 90, fontface = "bold", size =3) +
  guides(color="none", size = "none") +
  scale_size(range= c (2,10)) +
  labs (title = "2020 Situation",
        caption= "Inspiration https://github.com/cnicault/tidytuesday",
        y= "Civil Liberties", 
        x = "Political Rights") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size =15, family = "sans"))