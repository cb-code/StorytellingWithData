# | Spring 2020 | APAN S5800 | Storytelling w. Data | Columbia University SPS | chb2132 |

# | Mapping Subway Station Locations On to A Map of New York City | http://web.mta.info/developers/turnstile.html |
# | First attempts at using underlying map visualizations/images and plotting data points on top as layers from datasets |

library(sp)
library(d3heatmap)
library(heatmaply)
library(rgeos)
library(rgdal)
library(viridis)
library(mapdata)
library(maptools)
library(ggmap)
library(ggplot2)
library(ggthemes)
library(geojsonio)
library(extrafont)
library(tidyverse)
library(tidycensus)
library(RColorBrewer)
library(devtools)
library(tidytransit)
library(dplyr)
library(tidyr)
library(leaflet)

subway_entrances <- read.csv('Subway Entrances.csv')
subway_entrances <- subway_entrances[c("NAME","the_geom","LINE")]

names(subway_entrances) <- c("STATION", "COORDINATES", "LINES")

subway_entrances$STATION <- as.character(subway_entrances$STATION)
subway_entrances$COORDINATES <- as.character(subway_entrances$COORDINATES)
subway_entrances$LINES <- as.character(subway_entrances$LINES)

subway_entrances$COORDINATES <- sub("POINT [(]","",subway_entrances$COORDINATES)
subway_entrances$COORDINATES <- sub("[)]","",subway_entrances$COORDINATES)

subway_coord <- separate(data = subway_entrances, col = COORDINATES, into = c("long", "lat"), sep = " ", remove = TRUE, convert = FALSE)

subway_stations <- leaflet(subway_coord) %>%
        addTiles() %>%  
        setView(-73.957083, 40.961145, zoom = 12) %>%
        addCircles(~long, ~lat, weight = 3, radius=120, 
                   color="#0073B2", stroke = TRUE, fillOpacity = 0.8) %>% 
        addLegend("bottomleft", colors="#0073B2", labels="MTA Subway Entrances: NYC Open Data", title="NYC Subway Stations")

#display map
subway_stations

#loading the spatial data file from NYC Open Data online (geojson file)
MTA_spatial <- geojson_read('Subway Entrances.geojson', what = 'sp')

#reformatting the data file for ggplot2
MTA_map <- fortify(MTA_spatial)

#plotting/visualizing the map
MTA_plot <- ggplot() +
        geom_map(data = MTA_map, map = MTA_map, aes(x = long, y = lat, map_id = id)) +
        labs(x - NULL, y = NULL) #hide axis label
coord_quickmap()

MTA_gg <- MTA_plot + theme_tufte() + ggtitle("NYC Subway Station Map")

MTA_gg

ggsave("NYC Subway Station Map.png", MTA_gg, width=4, height=3)

#play user system interface audio prompt when script is done
system("say -v Magnus Code Complete!")
