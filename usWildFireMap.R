# Generate map of continental united states with wildfire points

require(mapproj)

# load inform database
data <- read.csv('./inform-2023.csv',
                 na.strings = "NaN",)

# configure date
data$FireDiscoveryDateTime <- as.Date(data$FireDiscoveryDateTime)
data$ContainmentDateTime <- as.Date(data$ContainmentDateTime)

# subset by date
dateSep <- with(data,data[data$FireDiscoveryDateTime >= "2000-01-01" &
                            data$FireDiscoveryDateTime <= "2023-01-01"
                          & data$IncidentTypeCategory == 'WF',])

#subset by fire incidence size
tinyFire <- with(dateSep,dateSep[dateSep$IncidentSize < 2.0, ])
smallFire <- with(dateSep,dateSep[dateSep$IncidentSize >= 2.0 & dateSep$IncidentSize <= 10.0, ])
mediumFire <- with(dateSep,dateSep[dateSep$IncidentSize > 10.0 & dateSep$IncidentSize <= 50.0, ])
largeFire <- with(dateSep,dateSep[dateSep$IncidentSize > 50.0 & dateSep$IncidentSize <= 100.0, ])
hugeFire <- with(dateSep,dateSep[dateSep$IncidentSize > 100.0 & dateSep$IncidentSize <= 500.0, ])

# load us national map
usData <- map_data("state")

# assign colours to fire incidence size
colorsMap <- c("< 2" = "#002060", "2 to 10" = "#2C7BB6", "10 to 50" = "#FDAE61",
               "50 to 100" = "#D73127", "> 100" = "#800000")
colors <- c("#002060", "#2C7BB6","#FDAE61",
            "#D73127", "#800000")
names =  c("< 2", "2 to 10", "10 to 50",
           "50 to 100","> 100")

# plot
ggplot() + 
  geom_polygon( data=usData, aes(x=long, y=lat, group=group),
                color="black", fill= '#f1f1f1' ) +
  coord_cartesian(xlim =c(-125, -68), ylim = c(25, 50))+
  geom_point(data=tinyFire, aes(X,Y, color= "< 2"), alpha=0.1, size=0.65)+
  geom_point(data=smallFire, aes(X,Y, color="2 to 10"), alpha=0.1, size=0.65)+
  geom_point(data=mediumFire, aes(X,Y, color="10 to 50"), alpha=0.1, size=0.65)+
  geom_point(data=largeFire, aes(X,Y, color="50 to 100"), alpha=0.1, size=0.65)+
  geom_point(data=hugeFire, aes(X,Y, color="> 100"), alpha=0.1, size=0.65)+
  
  labs(title="Wildfires in Continental US Based On Intensity (2000 to 2022)",
       caption="Wildfire Data: National Interagency Fire Center Public Fire Occurance Record (Update 13FEB2023)",
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
         fill=NA,label=NA) 
