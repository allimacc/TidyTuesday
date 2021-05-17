#tidy tuesday project
#https://github.com/rfordatascience/tidytuesday/commit/1722a1d94c5ef19455148739864209f39cec96c5#diff-dc6d0f5f57a3373ec6142228ad1742ee6926b70ff9758b6a6b66ac7a5c6d7df1
#learning from https://github.com/andybridger/TidyTuesday/blob/main/2021w20/2021w20.R

install.packages("tigris")
#install.packages("sf")

library(dplyr)
library(readr)
library(ggplot2)
library(tigris)
library(patchwork)
library(scales)
library(sf)
#library(ggmap)
#library(maps)



#this part doesn't work and maybe this week i'll find out why
#import Roboto font
sysfonts::font_add_google('Roboto')
showtext::showtext_auto()
showtext::showtext_opts(dpi = 400)

#creating a function for later
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#get the information
broadband <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband_zip.csv')


glimpse(broadband)

#get virginia data
virginia <- broadband %>%
  filter(ST =='VA') 
  

#the last 3 numbers of the county id seem to be an ordering if the counties.
virginia$COUNTYFP <- substrRight(as.character(virginia$`COUNTY ID`), 3)
tracts_max <- tracts(state = 'VA')
ggtract_VA<-fortify(tracts_max, region = "GEOID") 
ggtract_VA<-left_join(ggtract_VA, virginia, by=c("COUNTYFP")) 

ggtract_VA
#lets try this
p1 <- ggplot() +
   geom_sf(data = ggtract_VA, aes(fill=as.numeric('BROADBAND USAGE')), colour = "#061541", size =0.25) +
   coord_sf(datum = NA) +
   scale_fill_viridis_c(limits=c(0,1), 
                        breaks=c(0,1, 0.5), labels=c("0", "100%", "50%"),
                        guide = guide_colourbar(title.position = "top"))+
   theme_minimal(base_size=10)+
   theme(plot.subtitle = element_text(size=9),
         plot.title=element_text(size=14))+
   guides(fill = guide_colorbar(title.position = "top",
                                title.hjust = 0.5,
                                reverse = FALSE,
                                barheight = unit(10,"lines"),
                                barwidth = unit(0.5, "lines")))+
   labs(fill="",
        title = "Virgina")

 
p3 <- p1   +
  plot_annotation(
    title = "Broadband Access in the States I Went to College\n",
    subtitle = "Data are the percentage of people in each county with access to fixed terrestrial broadband at speeds of\n25 Mbps/3 Mbp in 2017. The majority of people in all Massachusetts counties have access to broadband.\nIn most counties in Massachusetts, 100% of people have access to broadband. In contrast, the majority\nof people in many rural counties in Kansas do NOT have access to broadband.\n", 
    caption = '\n#TidyTuesday Week 20 | Viz: @AndyBridger | Data: The Verge & Microsoft'
  )  &
  theme(panel.background = element_rect(fill = "white", colour="white"),
        plot.background = element_rect(fill = "white", colour="white"),
        plot.title = element_text(colour = "#440154", size=20, face="bold", hjust = 0.5, family="Roboto"),
        plot.subtitle = element_text(colour = "#440154", size=12, hjust = 0.0, family="Roboto", lineheight = 1.1),
        plot.caption = element_text(colour = "#440154", size=8, face="bold", hjust = 0, family="Roboto"))
p1
 
 
 
 
 
 
 
 
 usa<-map_data("state")
 va <- usa %>% filter( region =='virginia')
 
 ggplot(ggtract_VA,aes(fill=usage))+
    geom_map(map=county, aes(map_id=subregion))+
    expand_limits(x=county$long,y=county$lat)
 

 ggplot(ggtract_VA, aes(fill=`BROADBAND USAGE` ))+
    geom_map(map = va, aes(map_id=subregion))
 
 ggplot()+
    geom_polygon(ggtract_VA,mapping= aes(x= INTPTLON, y = INTPTLAT), fill= "blue", size =0.25) +
    coord_fixed(1) 
    
