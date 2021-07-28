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

# new column names:
colnames(census_map_nyc)
colnames(nbhood_map_nyc)

# Note: the values we are using here are not necessarily actually all subway stations--
# for purpose of clarity here, we are using the term "subway station" to mean any place
# a rider may legitimately enter and exit the subway to or from the streets of New York

subway_stations <- read_csv('NYC_Transit_Subway_Entrance_And_Exit_Data.csv')

#reading dataset of station groupings by subway line
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
     # convert the 3 columns to lowercase
     mutate_at(vars(division:station_name), str_to_lower) %>%
     # create a unique key for each station
     unite("stat_name", division:station_name, sep = "_") %>%
     # capitalize all of the route names (to fix the e issue)
     mutate_at(vars(route1:route11), str_to_upper)

# coordinates fix:
sub.ent.w.key <- sub.ent.w.key %>%
     # get rid of original coordinates:
     select(-c(station_latitude:station_longitude)) %>%
     distinct() %>%
     # join onto average coordinates:
     left_join(
          # get the average lat and average long for each station:
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
     # reformat the route columns into a long format
     gather("route_num", "route_name", route1:route11) %>%
     # get rid of the many NAs in the route column that were there due to the formatting
     filter(!is.na(route_name)) %>%
     select(-route_num) %>%
     distinct()

theme_set(theme_minimal())

#plotting a map of NYC census tract overlaid with NTA map
census_map_nyc %>%
ggplot() +
geom_sf(color = "#FF0099", size = .5) +
#NYC NTA map outline overlay in dark blue
geom_sf(data = nbhood_map_nyc, aes(fill = boroname), color = "white") +
scale_fill_manual(values = c("#76B7B2", "#F28E2B","#4E79A7", "#E15759"), name = "Borough") +
ggtitle("Census Tracts and Neighborhood Tabulation Areas in NYC") +
theme(axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())

# transform the neighborhood map to include subway entrance/exit data:
#overlay_map <- st_transform(census_map_nyc, crs = "+init=epsg:4326")

overlay_map <- census_map_nyc %>% st_set_crs(4326) %>% st_transform(crs=32736)

overlay_map %>%
     ggplot() +
     # plot the neighborhoods
     geom_sf(fill = NA) +
     # plot the subway lines
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

## By lowest number of stations
Starting slow, with the shuttle routes:
     ### Shuttles
     ```{r shuttle_corr, comment=NA}
### GS (Manhattan)
num.stat.by.rt.wiki %>%
     filter(route_name == "GS")
sub.ent.ada.updt %>%
     filter(route_name == "GS") %>%
     arrange(stat_name)
# GS has 2 stations for each of its stops, keep only the "42nd st shuttle" stops:
sub.stat.num.updt <- sub.ent.ada.updt %>%
     filter(!(route_name == "GS" & (str_detect(stat_name, "flushing") | str_detect(stat_name, "lexington"))))
### FS (Brooklyn)
# goal count:
num.stat.by.rt.wiki %>%
     filter(route_name == "FS")
# actual count:
stat.count.join %>%
     filter(route_name == "FS") %>%
     select(route_name:n)
sub.stat.num.updt %>%
     filter(route_name == "FS") %>%
     arrange(avg_stat_lat)
# problem: 3 franklin ave stations, when there should only be one
# solution: remove the ind and irt stations - those are the extras
sub.stat.num.updt <- sub.stat.num.updt %>%
     filter(!(route_name == "FS" & (str_detect(stat_name, "irt") | str_detect(stat_name, "ind"))))
sub.stat.num.updt %>%
     filter(route_name == "FS")
```
Next, based on lowest number of stops:
     ```{r after_shuttle, comment=NA}
sub.stat.num.updt %>%
     group_by(route_name) %>%
     count() %>%
     ungroup() %>%
     inner_join(num.stat.by.rt.wiki, by = "route_name") %>%
     filter(n != num_stations_norm) %>%
     filter(num_stations_norm == min(num_stations_norm))
```
The G and Z trains, both with 21 stations each, are next. I had initially wondered if maybe there are duplicates based on station lat/long pairs, and if I could get away with using the unique coordinates:
     ```{r G_check, comment=NA}
sub.line.tidy %>%
     filter(route_name == "G")
sub.stat.num.updt %>%
     filter(route_name == "G") %>%
     select(avg_stat_lat, avg_stat_long) %>%
     distinct() %>%
     nrow()
```
Unfortunately, that did not turn out to be the case. From here, I manually checked through each route station stop list, comparing them to the official list on each train's webpage.
### G train
Goal number of G train stations: 21
```{r G_train_corr, comment=NA}
sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "G") %>%
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "G" & str_detect(stat_name, "ind") & stat_name != "ind_queens boulevard_23rd st-ely av")
)
# Check:
sub.stat.num.updt %>%
filter(route_name == "G") %>%
nrow()
```
### Z train
Goal number of Z train stations: 21
```{r Z_train_corr, comment=NA}
# division info and trunk line:
sub.line.tidy %>%
filter(route_name == "Z")
sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "Z") %>%
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "Z" & str_detect(stat_name, "bmt") & stat_name != "bmt_broadway_canal st (ul)") %>%
# add Z train to the broadway junction stop in Queens (was A/C/J/L)
bind_rows(
sub.stat.num.updt %>%
filter(str_detect(stat_name, "broadway junction")) %>%
mutate(route_name = "Z") %>%
distinct()
) %>%
# add Z train to the Alabama Ave stop in Queens (was J only)
bind_rows(
sub.stat.num.updt %>%
filter(str_detect(stat_name, "alabama")) %>%
mutate(route_name = "Z")
) %>%
# add back in the jamaica center and jfk airport stops that were filtered out earlier based on the "bmt" filter
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "Z" & avg_stat_long > -73.82829)
)
)
# Check:
sub.stat.num.updt %>%
filter(route_name == "Z") %>%
nrow()
```
The station count for Z is now correct, next:
```{r after_z_and_g, comment=NA}
sub.stat.num.updt %>%
group_by(route_name) %>%
count() %>%
ungroup() %>%
inner_join(num.stat.by.rt.wiki, by = "route_name") %>%
filter(n != num_stations_norm) %>%
filter(num_stations_norm == min(num_stations_norm))
```
### 7 train
Goal number of 7 train stations: 22
```{r 7_train_corr, comment=NA}
# division and trunk line:
sub.line.tidy %>%
filter(route_name == "7")
sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "7") %>%
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "7" & str_detect(stat_name, "irt")) %>%
# eliminate station copies at times square and grand central
filter(!(
stat_name %in% c(
"irt_42nd st shuttle_times square", "irt_42nd st shuttle_grand central",
"irt_lexington_grand central-42nd st"
)
)) %>%
# convert ADA = TRUE at the court sq station (was incorrectly FALSE)
mutate(ada = ifelse(stat_name == "irt_flushing_45 rd-court house sq", TRUE, ada))
)
# Check:
sub.stat.num.updt %>%
filter(route_name == "7") %>%
nrow()
```
### E train
Goal number of E train stations: 22
```{r E_train_corr, comment=NA}
sub.line.tidy %>%
filter(route_name == "E")
sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "E") %>%
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "E" & str_detect(stat_name, "ind") & stat_name != "ind_8 avenue_chambers st") %>%
# ADA fix
mutate(ada = ifelse(stat_name == "ind_archer av_jamaica-van wyck", TRUE, ada)) %>%
# E train to Briarwood station
bind_rows(
sub.stat.num.updt %>%
filter(str_detect(stat_name, "briarwood")) %>%
mutate(route_name = "E")
)
)
# Check:
sub.stat.num.updt %>%
filter(route_name == "E") %>%
nrow()
```
Next:
```{r after_E, comment=NA}
sub.stat.num.updt %>%
group_by(route_name) %>%
count() %>%
ungroup() %>%
inner_join(num.stat.by.rt.wiki, by = "route_name") %>%
filter(n != num_stations_norm) %>%
filter(num_stations_norm == min(num_stations_norm))
```
### L train
Goal number of L train stations: 24
```{r L_train_corr, comment=NA}
# L division and line:
sub.line.tidy %>%
filter(route_name == "L")
sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "L") %>%
bind_rows(
sub.stat.num.updt %>%
# remove extra stations
filter(route_name == "L" & str_detect(stat_name, "bmt") & stat_name != "bmt_broadway_union square") %>%
# correct ADA status
mutate(ada = ifelse(stat_name == "bmt_canarsie_wilson av", TRUE, ada)) %>%
# add L train to broadway junction
bind_rows(
sub.stat.num.updt %>%
filter(str_detect(stat_name, "broadway junction") & route_name == "L")
)
)
# Check:
sub.stat.num.updt %>%
filter(route_name == "L") %>%
nrow()
```
Next:
```{r after_L, comment=NA}
sub.stat.num.updt %>%
group_by(route_name) %>%
count() %>%
ungroup() %>%
inner_join(num.stat.by.rt.wiki, by = "route_name") %>%
filter(n != num_stations_norm) %>%
filter(num_stations_norm == min(num_stations_norm))
```
### B train
The B train is a little unusual in that it makes more stops during rush hour, which is reflected in the number of "limited" service stations.
Goal number of B train stations: 37
```{r b_train_corr, comment=NA}
sub.line.tidy %>%
filter(route_name == "B")
sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "B") %>%
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "B" & (str_detect(stat_name, "bmt") | str_detect(stat_name, "ind"))) %>%
# convert non-ada stations to those that are ada = TRUE now
mutate(ada = ifelse(
stat_name %in% c(
"bmt_brighton_kings highway", "ind_6 avenue_broadway-lafayette st", "ind_8 avenue_125th st"
),
TRUE, ada)) %>%
# stations to exclude:
# atlantic ave /barclays duplicates and stops between barclays and brighton where B does not stop
filter(!(avg_stat_lat > 40.60867 & avg_stat_lat < 40.63508)) %>%
filter(!(
stat_name %in% c(
"bmt_broadway_34th st", "bmt_brighton_parkside av",
"bmt_4 avenue_pacific st", "bmt_brighton_atlantic av",
"bmt_brighton_av u", "bmt_brighton_neck rd",
"bmt_brighton_beverly rd", "bmt_brighton_cortelyou rd"
)
))
)
# Check:
sub.stat.num.updt %>%
filter(route_name == "B") %>%
nrow()
```
Next:
```{r after_B, comment=NA}
sub.stat.num.updt %>%
group_by(route_name) %>%
count() %>%
ungroup() %>%
inner_join(num.stat.by.rt.wiki, by = "route_name") %>%
filter(n != num_stations_norm & route_name != "B") %>%
filter(num_stations_norm == min(num_stations_norm))
```
### 4 train
Goal number of 4 train stations: 28 (need to exclude late night service)
```{r 4_train_corr, comment=NA}
sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "4") %>%
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "4" & str_detect(stat_name, "irt")) %>%
filter(!(
stat_name %in% c(
"irt_flushing_grand central-42nd st", "irt_42nd st shuttle_grand central",
"irt_clark_fulton st", "irt_clark_borough hall"
)
)) %>%
mutate(ada = ifelse(stat_name == "irt_lexington_fulton st", TRUE, ada))
)
# Check:
sub.stat.num.updt %>%
filter(route_name == "4") %>%
nrow()
```
## By primary trunk line
After going through one or two routes per trunk line, I realized that the other train routes along that line would have similar problems, so it would be easier to go by trunk line group, starting with the trains that have already been modified.
### J train
Goal number of J train stations: 30
```{r J_train_corr, comment=NA}
sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "J") %>%
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "J" & str_detect(stat_name, "bmt") & stat_name != "bmt_broadway_canal st (ul)") %>%
# add back in jamaica center and the jfk airport stop
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "J" & avg_stat_long > -73.82829)
) %>%
# add J train to broadway junction
bind_rows(
sub.stat.num.updt %>%
filter(str_detect(stat_name, "broadway junction") & route_name == "J")
) %>%
mutate(ada = ifelse(stat_name == "bmt_nassau_fulton st", TRUE, ada))
)
# Check:
sub.stat.num.updt %>%
filter(route_name == "J") %>%
nrow()
```
What's left?
     ```{r after_J, comment=NA}
sub.stat.num.updt %>%
     group_by(route_name) %>%
     count() %>%
     ungroup() %>%
     inner_join(num.stat.by.rt.wiki, by = "route_name") %>%
     filter(n != num_stations_norm & route_name != "B")
```
Will leave N/Q/R/W for last, but let's get into the A/C lines since the E was already visited.
### A train
Goal number of A train stations: 44
```{r A_train_corr, comment=NA}
sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "A") %>%
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "A" & str_detect(stat_name, "ind")) %>%
# remove stations where the A does not stop
filter(!(
stat_name %in% c(
"ind_8 avenue_world trade center", "ind_8 avenue_broadway-nassau",
"ind_fulton_franklin av", "ind_fulton_kingston-throop",
"ind_fulton_ralph av", "ind_fulton_rockaway av",
"ind_fulton_liberty av", "ind_fulton_van siclen av",
"ind_fulton_shepherd av"
)
)) %>%
# add in the fulton st stop in manhattan
bind_rows(
sub.stat.num.updt %>%
filter(str_detect(stat_name, "fulton st") & route_name == "4") %>%
mutate(route_name = "A")
) %>%
# ada fixes
mutate(ada = ifelse(
stat_name %in% c(
"ind_8 avenue_125th st", "ind_rockaway_far rockaway-mott av",
"ind_rockaway_aqueduct racetrack", "ind_fulton_jay st - borough hall",
"ind_fulton_utica av", "ind_liberty_lefferts blvd"
),
TRUE, ada
)) %>%
# add in rockaway beach stops that A makes during rush hour
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "H" & !(str_detect(stat_name, "broad channel"))) %>%
mutate(route_name = "A")
)
)
# Check:
sub.stat.num.updt %>%
filter(route_name == "A") %>%
nrow()
```
### C train
Goal number of C train stations: 40
```{r c_train_corr, comment=NA}
sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "C") %>%
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "C" & str_detect(stat_name, "ind")) %>%
filter(!(stat_name %in% c("ind_8 avenue_broadway-nassau", "ind_8 avenue_world trade center"))) %>%
bind_rows(
sub.stat.num.updt %>%
filter(str_detect(stat_name, "fulton st") & route_name == "A") %>%
mutate(route_name = "C")
) %>%
mutate(ada = ifelse(
stat_name %in% c("ind_8 avenue_125th st", "ind_fulton_jay st - borough hall", "ind_fulton_utica av"),
TRUE, ada))
)
# Check:
sub.stat.num.updt %>%
filter(route_name == "C") %>%
nrow()
```
### 5 train
Goal number of 5 train stations: 45
```{r 5_train_corr, comment=NA}
sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "5") %>%
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "5" & str_detect(stat_name, "irt")) %>%
filter(!(
stat_name %in% c(
"irt_clark_borough hall", "irt_clark_fulton st",
"irt_flushing_grand central-42nd st", "irt_42nd st shuttle_grand central",
"irt_white plains road_wakefield-241st st"
)
)) %>%
mutate(ada = ifelse(
stat_name %in% c(
"irt_lexington_fulton st", "irt_white plains road_east 180th st",
"irt_white plains road_gun hill rd"
),
TRUE, ada))
)
# Check
sub.stat.num.updt %>%
filter(route_name == "5") %>%
nrow()
```
### 6 train
Goal number of 6 train stations: 38
```{r 6_train_corr, comment=NA}
sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "6") %>%
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "6" & str_detect(stat_name, "irt")) %>%
filter(!(stat_name %in% c("irt_flushing_grand central-42nd st", "irt_42nd st shuttle_grand central"))) %>%
mutate(ada = ifelse(stat_name %in% c("irt_lexington_bleecker st"), TRUE, ada))
)
# Check:
sub.stat.num.updt %>%
filter(route_name == "6") %>%
nrow()
```
What's left?
     ```{r after_6, comment=NA}
sub.stat.num.updt %>%
     group_by(route_name) %>%
     count() %>%
     ungroup() %>%
     inner_join(num.stat.by.rt.wiki, by = "route_name") %>%
     filter(n != num_stations_norm & route_name != "B")
```
D/F/M next.
### D train
Goal number of D train stations: 36
```{r D_train_corr, comment=NA}
sub.stat.num.updt <- sub.stat.num.updt %>%
     filter(route_name != "D") %>%
     bind_rows(
          sub.stat.num.updt %>%
               # the D train stops are a mix of ind and bmt
               filter(route_name == "D" & (str_detect(stat_name, "ind") | str_detect(stat_name, "bmt"))) %>%
               filter(!(
                    stat_name %in% c(
                         "bmt_broadway_34th st", "bmt_4 avenue_pacific st",
                         "bmt_brighton_atlantic av", "bmt_sea beach_new utrecht av",
                         "bmt_brighton_stillwell av")
               )) %>%
               # add in the brooklyn 36th st stop:
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
# is the number of stations correct now?
sub.stat.num.updt %>%
     filter(route_name == "D") %>%
     nrow()
```
### F train
Goal number of F train stations: 45
```{r F_train_corr, comment=NA}
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
# Check:
sub.stat.num.updt %>%
     filter(route_name == "F") %>%
     nrow()
```
### M train
Goal number of M train stations: 36
```{r M_train_corr, comment=NA}
sub.stat.num.updt <- sub.stat.num.updt %>%
     filter(route_name != "M") %>%
     bind_rows(
          sub.stat.num.updt %>%
               filter(route_name == "M" & (str_detect(stat_name, "ind") | str_detect(stat_name, "bmt"))) %>%
               filter(!(stat_name %in% c("bmt_broadway_34th st", "bmt_canarsie_6th av", "bmt_nassau_essex st"))) %>%
               mutate(ada = ifelse(stat_name %in% c("ind_6 avenue_broadway-lafayette st"), TRUE, ada))
     )
# Check:
sub.stat.num.updt %>%
     filter(route_name == "M") %>%
     nrow()
```
What's left?
```{r after_M, comment=NA}
sub.stat.num.updt %>%
group_by(route_name) %>%
count() %>%
ungroup() %>%
inner_join(num.stat.by.rt.wiki, by = "route_name") %>%
filter(n != num_stations_norm & route_name != "B")
```
1/2/3 next:
### 1 train
Goal number of 1 train stations: 38
```{r 1_train_corr, comment=NA}
sub.stat.num.updt <- sub.stat.num.updt %>%
filter(route_name != "1") %>%
bind_rows(
sub.stat.num.updt %>%
filter(route_name == "1" & str_detect(stat_name, "irt")) %>%
filter(!(stat_name %in% c("irt_42nd st shuttle_times square", "irt_clark_park place"))) %>%
# add in the re-opened WTC Cortlandt station (doesn't exist in dataset, coord from wikipedia)
bind_rows(
     tibble(
          stat_name = "irt_broadway-7th ave_wtc cortlandt", ada = TRUE,
          avg_stat_lat = 40.7115, avg_stat_long = -74.012, route_name = "1")
) %>%
     mutate(ada = ifelse(
          stat_name %in% c("irt_broadway-7th ave_dyckman st", "irt_broadway-7th ave_168th st"),
          TRUE, ada))
)
# Check:
sub.stat.num.updt %>%
     filter(route_name == "1") %>%
     nrow()
```
### 2 train
Goal number of 2 train stations: 49
```{r 2_train_corr, comment=NA}
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
# Check:
sub.stat.num.updt %>%
     filter(route_name == "2") %>%
     nrow()
```
### 3 train
Goal number of 3 train stations: 34
```{r 3_train_corr, comment=NA}
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
# Check:
sub.stat.num.updt %>%
     filter(route_name == "3") %>%
     nrow()
```
What's left?
```{r after_3, comment=NA}
sub.stat.num.updt %>%
group_by(route_name) %>%
count() %>%
ungroup() %>%
inner_join(num.stat.by.rt.wiki, by = "route_name") %>%
filter(n != num_stations_norm & route_name != "B")
```
Now for the N/Q/R/W route updates that I've been avoiding:
     ### R train
     Goal number of R train stations: 45
```{r R_train_corr, comment=NA}
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
# Check:
sub.stat.num.updt %>%
     filter(route_name == "R") %>%
     nrow()
```
### N train
Goal number of N train stations: 32
```{r N_train_corr, comment=NA}
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
               # add back in queensboro plaza, only route that has irt division instead of bmt
               bind_rows(sub.stat.num.updt %>% filter(route_name == "N" & str_detect(stat_name, "queensboro")))
     )
# Check:
sub.stat.num.updt %>%
     filter(route_name == "N") %>%
     nrow()
```
### W train
The W train was introduced to replace the Q train in Astoria when the Q was rerouted up 2nd Ave in Manhattan from its original route in Queens. Unsurprisingly, since the subway entrances/exits dataset contains the old Q train route information, the W is also not included. Luckily, the W route is a mashup of the R and N routes, so those route sections can be stitched together to create the W.
Goal number of W train stations: 23
```{r W_train_corr, comment=NA}
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
# Check:
sub.stat.num.updt %>%
     filter(route_name == "W") %>%
     nrow()
```
### Q train
Last, but not least, the Q train update, which included removing the old stations stops in Queens, adding in the 2nd Ave stops that did not exist in the dataset, and removing some station stops in Brooklyn.
Goal number of Q train stations: 29
```{r Q_train_corr, comment=NA}
sub.stat.num.updt <- sub.stat.num.updt %>%
     filter(route_name != "Q") %>%
     bind_rows(
          sub.stat.num.updt %>%
               filter(route_name == "Q" & !str_detect(stat_name, "astoria") & str_detect(stat_name, "bmt")) %>%
               # add in 3 new 2nd Ave stops at 72nd St, 86th St, and 96th St
               bind_rows(
                    tibble(
                         # assign names to match existing pattern
                         stat_name = c("ind_2 avenue_72nd st", "ind_2 avenue_86th st", "ind_2 avenue_96th st"),
                         # all are ADA = TRUE
                         ada = rep(TRUE, 3),
                         # lat and long from wikipedia pages for eachs tation
                         avg_stat_lat = c(40.768889, 40.777861, 40.7841),
                         avg_stat_long = c(-73.958333, -73.95175, -73.9472),
                         # only the Q stops at these stations on a regular schedule
                         route_name = rep("Q", 3)
                    )
               ) %>%
               # add in the 63rd St, where only the F used to stop, but now the Q also stops there
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
