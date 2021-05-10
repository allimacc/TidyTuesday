

library(tidyverse)
library(maps)
library(ggplot2)
library(ggmap)
library(mapdata)

install.packages(c("ggmap", "mapdata"))

water <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

water

#what countries are in here
africa_w <- water %>%
  filter(!is.na(country_name, water_source)) %>% 
  filter(country_name %in% c("Kenya")) %>% 
  filter(lat_deg < 5 & lon_deg >30 & lon_deg < 50) %>%
  rename(lat = lat_deg, long = lon_deg) %>%
  select(country_name, lat, long, water_source, install_year)


map <- map_data("world", region="Kenya")

ggplot() + geom_polygon(data = map, aes(x=long, y = lat, group = group), fill="darkseagreen2") + 
  coord_fixed(1) +
  geom_point(data=africa_w, aes(x=long, y=lat, color =water_source), size=0.7) +
  labs(title = "Water Sources in Kenya", caption = "source : water point exchange") +
  theme(legend.position = "bottom")
        
ggsave("Kenyawater.png")        
        )
  
