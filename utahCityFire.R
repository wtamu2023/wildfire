# generate map of Arizona and surrounding states with points for wildfire data

require(sf)
require(maps)
require(tidyverse)
require(ggplot2)
require(ggvoronoi)

# extract city data
cities <- get('world.cities')
cities <- cities[cities$country.etc == 'USA', ]

# load INFORM NIFC dataset
data <- read.csv('./inform-2023.csv',
                 na.strings = "NaN",)

# Configure date
data$FireDiscoveryDateTime <- as.Date(data$FireDiscoveryDateTime)
data$ContainmentDateTime <- as.Date(data$ContainmentDateTime)

#Subset by date
dateSep <- with(data,data[data$FireDiscoveryDateTime >= "2000-01-01" &
                            data$FireDiscoveryDateTime <= "2023-01-01"
                          & data$IncidentTypeCategory == 'WF',])

# subset by fire intensity
tinyFire <- with(dateSep,dateSep[dateSep$IncidentSize < 2.0, ])
smallFire <- with(dateSep,dateSep[dateSep$IncidentSize >= 2.0 & dateSep$IncidentSize <= 10.0, ])
mediumFire <- with(dateSep,dateSep[dateSep$IncidentSize > 10.0 & dateSep$IncidentSize <= 50.0, ])
largeFire <- with(dateSep,dateSep[dateSep$IncidentSize > 50.0 & dateSep$IncidentSize <= 100.0, ])
hugeFire <- with(dateSep,dateSep[dateSep$IncidentSize > 100.0 & dateSep$IncidentSize <= 500.0, ])

#load maps
caData <- map_data("state", region=c('California'))
azData <- map_data("state", region=c('Arizona'))
nvData <- map_data("state", region=c('Nevada'))
nmData <- map_data("state", region=c('New Mexico'))
utData <- map_data("state", region=c('Utah'))
coData <- map_data("state", region=c('Colorado'))

# assign colours per fire intensity
colorsMap <- c("< 2" = "#002060", "2 to 10" = "#2C7BB6", "10 to 50" = "#FDAE61",
            "50 to 100" = "#D73127", "> 100" = "#800000")
colors <- c("#002060", "#2C7BB6","#FDAE61",
               "#D73127", "#800000")
names =  c("< 2", "2 to 10", "10 to 50",
           "50 to 100","> 100")
# generate plot
ggplot() + 
  geom_polygon( data=azData, aes(x=long, y=lat),
                color="#000000", fill= '#ebebeb' ) +
  geom_polygon(data=nvData, aes(x=long, y=lat),
               color="#000000", fill='#f1f1f1') +
  geom_polygon(data=caData, aes(x=long, y=lat),
             color="#000000", fill='#f1f1f1') +
  geom_polygon(data=nmData, aes(x=long, y=lat),
               color="#000000", fill='#f1f1f1') +
  geom_polygon(data=utData, aes(x=long, y=lat),
               color="#000000", fill='#f1f1f1') +
  geom_polygon(data=coData, aes(x=long, y=lat),
               color="#000000", fill='#f1f1f1') +
  coord_cartesian(xlim =c(-117, -107.5), ylim = c(31, 38))+
  geom_point(data=tinyFire, aes(X,Y, color= "< 2"), alpha=0.1, size=0.85)+
  geom_point(data=smallFire, aes(X,Y, color="2 to 10"), alpha=0.1, size=0.85)+
  geom_point(data=mediumFire, aes(X,Y, color="10 to 50"), alpha=0.1, size=0.85)+
  geom_point(data=largeFire, aes(X,Y, color="50 to 100"), alpha=0.1, size=0.85)+
  geom_point(data=hugeFire, aes(X,Y, color="> 100"), alpha=0.1, size=0.85)+
  geom_point(data=cities, aes(x= long, y= lat, size=pop),
             color='#ffff33', shape=15, alpha=0.9)+
  geom_point(data=cities,aes(x=long,y=lat,size=pop+0.1),color='#000000',shape=0,alpha=0.9)+
  geom_voronoi(data=cities, aes(x=long,y=lat), geom='path', color='#000000', # Comment out to remove voronoi diagrams
               fill='#f1f1f1', alpha=0.25) +
  geom_label(data=cities,aes(x=long,y=lat,label=ifelse(pop>200000,as.character(name),NA)),
                             hjust=-.2,vjust=.1,fill='white',size=3, alpha = .5)+
  
  labs(title="Maximal Wildfire Detection Distance in Arizona and Adjacent States (2000 to 2022)",
       subtitle= 'Voronoi Tesselations Indicate Distance Travelled from City Center as Ray Until Intersects With
       Ray from Other City Source',
       caption="Wildfire Data: National Interagency Fire Center Public Fire Occurance Record (Update 13FEB2023) Map and Cities: R maps package",
       colour="Incident Size  (NIFC Arbitrary Unit)") + 
  xlab('Longitude') +
  ylab('Latitude') +
  scale_color_manual(values=colors, limits= names) +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    plot.title = element_text(size=16),
    axis.title = element_text(size=14),
    legend.title = element_text(size=14),
    legend.key = element_blank(),
    legend.text = element_text(size=12),
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    )+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size=3)),
         fill='none',label='none',size='none') 
  