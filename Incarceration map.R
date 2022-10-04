packages <- c("tidyverse", "stringr", "censusapi", "sf", "tidycensus", "ggspatial", "tigris")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org")
}
library(tidyverse)
library(sf)
library(ggspatial)
library(tigris)
library(viridis)
library(showtext)

df <- readr::read_csv("incarceration.csv")

incarceration_pop <- df%>%
mutate(per_pop=incarcerated_20/total_population_20*1000)%>%
glimpse()

us_counties <- counties(cb = TRUE, resolution = "20m")

incarceration_counties <- left_join(us_counties, incarceration_pop, by=c("NAMELSAD"="county"))

font_add_google("Montserrat")
font_add_google("Roboto")
font_add_google("Inconsolata")
showtext_auto()

incarceration_counties %>%
  shift_geometry()%>%
  ggplot() +
  geom_sf(aes(geometry=geometry, fill=per_pop), color = NA) +
  scale_fill_viridis(direction=-1) +
  labs(title = "Number of incarcerated people per 1,000 county residents (2020)", caption = "Data: The Marshall Project - Map: Alejandra Arevalo", fill="Incarcerated People/1,000 residents")+
  theme(plot.background = element_rect(fill='transparent', color=NA),
        panel.border=element_blank(),
        panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_blank(), 
        panel.background = element_rect(fill='transparent'),
        axis.ticks = element_blank(),
        plot.margin = margin(50,10,50,10,"pt"),
        plot.title=element_text(size=25,family="Montserrat", face="bold", hjust = 0.5),
        legend.text=element_text(size=16,family="Roboto", lineheight = 25),
        plot.caption=element_text(size=16,family="Inconsolata", hjust = 0.5, vjust = 0),  
        legend.position="bottom",
        legend.title = element_blank(),
        legend.key.height= unit(10, "pt"),
        legend.key.width= unit(30, "pt"),
        legend.background = element_rect(fill='transparent', color=NA)
        )

ggsave("incarceration_map.png", bg="transparent")
