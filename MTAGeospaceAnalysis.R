#-----------------Storytelling with Data (5800) | Spring 2020 | Columbia University | Code by CBlanchard (chb2132)-----------------#
#                                                                                                                                  #
#                 Geospatial Analysis NYC mapping for subway stations/lines with 2010 census data                                  #
#                                                                                                                                  #
#                 NYC MTA Subway Entrance and Exit Dataset (via Open Data NY on NYC.gov domain)                                    #
#                 https://data.ny.gov/Transportation/NYC-Transit-Subway-Entrance-And-Exit-Data/i9wp-a4ja                           #
#                                                                                                                                  #
#                 NYC 2010 Census Map Data (via NYC Dept. of City Planning branch of NYC Open Data/nyc.gov domain)                 #
#                 https://www1.nyc.gov/site/planning/data-maps/open-data/districts-download-metadata.page                          #
#                                                                                                                                  #
#                 NYC Neighborhood Tabulation Areas (NTA) Dataset ("same as above")                                                #
#                 https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-nynta.page                                            #
#                                                                                                                                  #
#                 NYC MTA Subway Line Data, Route Information (via Wikipedia: NYC Subway Nomenclature Page)                        #
#                 https://en.wikipedia.org/wiki/New_York_City_Subway_nomenclature                                                  #


library(sf)
library(rosm)
library(mapview)
library(magrittr)
library(ggthemes)
library(tidyverse)
library(prettymapr)
library(cartography)

setwd('~/Documents/5800/AM MTA - meet AM')

census_map_nyc <- st_read('nyct/nyct2010.shp')
nbhood_map_nyc <- st_read('nynta/nynta.shp')

census_map_nyc <- census_map_nyc %>%
     filter(BoroName != "Staten Island") %>%
     `colnames<-`(str_to_lower(colnames(census_map_nyc)))
nbhood_map_nyc <- nbhood_map_nyc %>%
     filter(BoroName != "Staten Island") %>%
     `colnames<-`(str_to_lower(colnames(nbhood_map_nyc)))

colnames(census_map_nyc)
colnames(nbhood_map_nyc)

subway_stations <- read_csv('NYC_Transit_Subway_Entrance_And_Exit_Data.csv')
station_lines <- read_csv('nyc_subway_stations_grouped.csv')

colnames(subway_stations) <- colnames(subway_stations) %>%
     str_to_lower() %>%
     str_replace_all(" ", "_")
colnames(station_lines) <- colnames(station_lines) %>%
     str_to_lower() %>%
     str_replace_all(" ", "_")

sub.line.tidy <- station_lines %>%
     transform(lines = strsplit(lines), "_") %>%
     unnest(lines) %>%
     rename(route_name = lines)

sub.ent.w.key <- subway_stations %>%
     mutate_at(vars(division:station_name), str_to_lower) %>%
     unite("stat_name", division:station_name, sep = "_") %>%
     mutate_at(vars(route1:route11), str_to_upper)

sub.ent.w.key <- sub.ent.w.key %>%
     select(-c(station_latitude:station_longitude)) %>%
     distinct() %>%
     left_join(
          sub.ent.w.key %>%
               select(stat_name:station_longitude) %>%
               distinct() %>%
               group_by(stat_name) %>%
               summarize(
                    avg_stat_lat = mean(station_latitude),
                    avg_stat_long = mean(station_longitude)
               ),
          by = "stat_name"
     )

sub.ent.sml <- sub.ent.w.key %>%

     select(stat_name, avg_stat_lat:avg_stat_long, route1:route11) %>%
     distinct() %>%
     gather("route_num", "route_name", route1:route11) %>%
     filter(!is.na(route_name)) %>%
     select(-route_num) %>%
     distinct()

#plotting a map of NYC census tract overlaid with NTA map

census_map_nyc %>%
ggplot() +
geom_sf(color = "#FF0099", size = .5) +

geom_sf(data = nbhood_map_nyc, aes(fill = boroname), color = "white") +
scale_fill_manual(values = c("#76B7B2", "#F28E2B","#4E79A7", "#E15759"), name = "Borough") +
ggtitle("Census Tracts and Neighborhood Tabulation Areas in NYC") +
theme(axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())

overlay_map <- census_map_nyc %>% st_set_crs(4326) %>% st_transform(crs=32736)

overlay_map %>%
     ggplot() +
     geom_sf(fill = NA) +
     geom_point(data = sub.ent.sml,
          aes(avg_stat_long, avg_stat_lat, color = route_name),
          size = 2,
          alpha = 0.8
     ) +
     scale_color_tableau(name = "Route") +
     ggtitle("Census Tracts and Neighborhood Tabulation Areas in NYC") +
     theme(axis.title.x = element_blank(),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.title.y = element_blank(),
           axis.text.y = element_blank(),
           axis.ticks.y = element_blank())

num.stat.by.rt.wiki %>%
     filter(route_name == "GS")
sub.ent.ada.updt %>%
     filter(route_name == "GS") %>%
     arrange(stat_name)

sub.stat.num.updt <- sub.ent.ada.updt %>%
     filter(!(route_name == "GS" & (str_detect(stat_name, "flushing") | str_detect(stat_name, "lexington"))))

num.stat.by.rt.wiki %>%
     filter(route_name == "FS")

stat.count.join %>%
     filter(route_name == "FS") %>%
     select(route_name:n)
sub.stat.num.updt %>%
     filter(route_name == "FS") %>%
     arrange(avg_stat_lat)

sub.stat.num.updt <- sub.stat.num.updt %>%
     filter(!(route_name == "FS" & (str_detect(stat_name, "irt") | str_detect(stat_name, "ind"))))
sub.stat.num.updt %>%
     filter(route_name == "FS")

sub.stat.num.updt %>%
     group_by(route_name) %>%
     count() %>%
     ungroup() %>%
     inner_join(num.stat.by.rt.wiki, by = "route_name") %>%
     filter(n != num_stations_norm) %>%
     filter(num_stations_norm == min(num_stations_norm))

sub.line.tidy %>%
     filter(route_name == "G")
sub.stat.num.updt %>%
     filter(route_name == "G") %>%
     select(avg_stat_lat, avg_stat_long) %>%
     distinct() %>%
     nrow()

sub.stat.num.updt %>%
     group_by(route_name) %>%
     count() %>%
     ungroup() %>%
     inner_join(num.stat.by.rt.wiki, by = "route_name") %>%
     filter(n != num_stations_norm & route_name != "B")

sub.stat.num.updt <- sub.stat.num.updt %>%
     filter(route_name != "D") %>%
     bind_rows(
          sub.stat.num.updt %>%
               filter(route_name == "D" & (str_detect(stat_name, "ind") | str_detect(stat_name, "bmt"))) %>%
               filter(!(
                    stat_name %in% c(
                         "bmt_broadway_34th st", "bmt_4 avenue_pacific st",
                         "bmt_brighton_atlantic av", "bmt_sea beach_new utrecht av",
                         "bmt_brighton_stillwell av")
               )) %>%
               bind_rows(
                    sub.stat.num.updt %>%
                         filter(str_detect(stat_name, "4 avenue_36")) %>%
                         mutate(route_name = "D") %>%
                         distinct()
               ) %>%
               mutate(ada = ifelse(
                    stat_name %in% c("ind_8 avenue_125th st", "ind_6 avenue_broadway-lafayette st", "bmt_west end_bay parkway"),
                    TRUE, ada))
     )

sub.stat.num.updt %>%
     filter(route_name == "D") %>%
     nrow()

sub.stat.num.updt <- sub.stat.num.updt %>%
     filter(route_name != "F") %>%
     bind_rows(
          sub.stat.num.updt %>%
               filter(route_name == "F" & (str_detect(stat_name, "ind") | str_detect(stat_name, "bmt"))) %>%
               filter(!(
                    stat_name %in% c(
                         "bmt_broadway_34th st", "bmt_canarsie_6th av",
                         "bmt_nassau_essex st", "bmt_broadway_lawrence st",
                         "bmt_4 avenue_9th st", "bmt_brighton_stillwell av",
                         "bmt_brighton_west 8th st"
                    )
               )) %>%
               mutate(ada = ifelse(
                    stat_name %in% c("ind_6 avenue_broadway-lafayette st", "ind_fulton_jay st - borough hall"),
                    TRUE, ada))
     )

sub.stat.num.updt %>%
     filter(route_name == "F") %>%
     nrow()

sub.stat.num.updt <- sub.stat.num.updt %>%
     filter(route_name != "M") %>%
     bind_rows(
          sub.stat.num.updt %>%
               filter(route_name == "M" & (str_detect(stat_name, "ind") | str_detect(stat_name, "bmt"))) %>%
               filter(!(stat_name %in% c("bmt_broadway_34th st", "bmt_canarsie_6th av", "bmt_nassau_essex st"))) %>%
               mutate(ada = ifelse(stat_name %in% c("ind_6 avenue_broadway-lafayette st"), TRUE, ada))
     )

sub.stat.num.updt %>%
     filter(route_name == "M") %>%
     nrow()

sub.stat.num.updt %>%
group_by(route_name) %>%
count() %>%
ungroup() %>%
inner_join(num.stat.by.rt.wiki, by = "route_name") %>%
filter(n != num_stations_norm & route_name != "B")

sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "1") %>%
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "1" & str_detect(stat_name, "irt")) %>%
filter(!(stat_name %in% c("irt_42nd st shuttle_times square", "irt_clark_park place"))) %>%

bind_rows(
     tibble(
          stat_name = "irt_broadway-7th ave_wtc cortlandt", ada = TRUE,
          avg_stat_lat = 40.7115, avg_stat_long = -74.012, route_name = "1")
) %>%
     mutate(ada = ifelse(
          stat_name %in% c("irt_broadway-7th ave_dyckman st", "irt_broadway-7th ave_168th st"),
          TRUE, ada))
)

sub.stat.num.updt %>%
     filter(route_name == "1") %>%
     nrow()

sub.stat.num.updt <- sub.stat.num.updt %>%
     filter(route_name != "2") %>%
     bind_rows(
          sub.stat.num.updt %>%
               filter(route_name == "2" & str_detect(stat_name, "irt")) %>%
               filter(!(
                    stat_name %in% c(
                         "irt_42nd st shuttle_times square", "irt_lexington_fulton st",
                         "irt_lexington_borough hall"
                    )
               )) %>%
               mutate(ada = ifelse(
                    stat_name %in% c(
                         "irt_white plains road_gun hill rd", "irt_white plains road_east 180th st",
                         "irt_clark_fulton st"
                    ),
                    TRUE, ada))
     )

sub.stat.num.updt %>%
     filter(route_name == "2") %>%
     nrow()

sub.stat.num.updt <- sub.stat.num.updt %>%
     filter(route_name != "3") %>%
     bind_rows(
          sub.stat.num.updt %>%
               filter(route_name == "3" & str_detect(stat_name, "irt")) %>%
               filter(!(
                    stat_name %in% c("irt_42nd st shuttle_times square", "irt_lexington_fulton st", "irt_lexington_borough hall")
               )) %>%
               mutate(ada = ifelse(stat_name %in% c("irt_clark_fulton st"), TRUE, ada))
     )

sub.stat.num.updt %>%
     filter(route_name == "3") %>%
     nrow()

sub.stat.num.updt %>%
group_by(route_name) %>%
count() %>%
ungroup() %>%
inner_join(num.stat.by.rt.wiki, by = "route_name") %>%
filter(n != num_stations_norm & route_name != "B")

sub.stat.num.updt <- sub.stat.num.updt %>%
     filter(route_name != "R") %>%
     bind_rows(
          sub.stat.num.updt %>%
               filter(route_name == "R" & (str_detect(stat_name, "bmt") | str_detect(stat_name, "ind"))) %>%
               filter(!(stat_name %in% c(
                    "ind_6 avenue_smith-9th st", "bmt_4 avenue_pacific st",
                    "bmt_brighton_atlantic av", "ind_fulton_jay st - borough hall",
                    "bmt_nassau_canal st", "bmt_canarsie_union square",
                    "ind_6 avenue_34th st", "ind_8 avenue_42nd st"
               ))) %>%
               mutate(ada = ifelse(stat_name %in% c("bmt_broadway_lawrence st", "bmt_broadway_cortlandt st"), TRUE, ada))
     )

sub.stat.num.updt %>%
     filter(route_name == "R") %>%
     nrow()

sub.stat.num.updt <- sub.stat.num.updt %>%
     filter(route_name != "N") %>%
     bind_rows(
          sub.stat.num.updt %>%
               filter(route_name == "N" & str_detect(stat_name, "bmt")) %>%
               filter(!(stat_name %in% c(
                    "bmt_brighton_stillwell av", "bmt_west end_62nd st",
                    "bmt_brighton_atlantic av", "bmt_4 avenue_pacific st",
                    "bmt_nassau_canal st", "bmt_canarsie_union square"
               ))) %>%
               bind_rows(sub.stat.num.updt %>% filter(route_name == "N" & str_detect(stat_name, "queensboro")))
     )

sub.stat.num.updt %>%
     filter(route_name == "N") %>%
     nrow()

sub.stat.num.updt <- sub.stat.num.updt %>%
     bind_rows(
          sub.stat.num.updt %>%
               filter(route_name == "N" & avg_stat_lat > 40.68367) %>%
               bind_rows(
                    sub.stat.num.updt %>%
                         filter(route_name == "R") %>%
                         filter(avg_stat_lat > 40.69410 & avg_stat_lat < 40.71952)
               ) %>%
               mutate(route_name = "W")
     )

sub.stat.num.updt %>%
     filter(route_name == "W") %>%
     nrow()

sub.stat.num.updt <- sub.stat.num.updt %>%
     filter(route_name != "Q") %>%
     bind_rows(
          sub.stat.num.updt %>%
               filter(route_name == "Q" & !str_detect(stat_name, "astoria") & str_detect(stat_name, "bmt")) %>%
               bind_rows(
                    tibble(
                         stat_name = c("ind_2 avenue_72nd st", "ind_2 avenue_86th st", "ind_2 avenue_96th st"),
                         # all are ADA = TRUE
                         ada = rep(TRUE, 3),
                         avg_stat_lat = c(40.768889, 40.777861, 40.7841),
                         avg_stat_long = c(-73.958333, -73.95175, -73.9472),
                         route_name = rep("Q", 3)
                    )
               ) %>%
               bind_rows(
                    sub.stat.num.updt %>%
                         filter(stat_name == "ind_63rd street_lexington av") %>%
                         mutate(route_name = "Q")
               ) %>%
               filter(!(stat_name %in% c(
                    "bmt_broadway_5th av", "bmt_broadway_lexington av",
                    "bmt_broadway_49th st", "bmt_canarsie_union square",
                    "bmt_nassau_canal st", "bmt_4 avenue_pacific st",
                    "bmt_brighton_atlantic av", "bmt_coney island_stillwell av",
                    "bmt_coney island_west 8th st"
               ))) %>%
               mutate(ada = ifelse(stat_name %in% c("bmt_brighton_av h", "bmt_brighton_kings highway"), TRUE, ada))
     )

#save plot output in RStudio using ggsave for image quality/attribute preservation (IDE UI images often strongly aliased, ~72 dpi)
ggsave("Census Tracts and Neighborhood Tabulation Areas in NYC.png", overlay_map_2)

#play user system interface audio prompt when script is done
system("say -v Magnus Code Complete!")
