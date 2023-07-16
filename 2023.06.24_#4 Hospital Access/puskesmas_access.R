# libraries
library(tidyverse)
library(hereR)
library(leaflet)
library(sf)
library(mapview)
# library(sp)

# source
source("config.R")

# health facility
data <- read.csv("2023.06.24_#4 Hospital Access/hospitals.csv")

# hospital only
puskesmas <- data %>% filter(Type %in% "Puskesmas")
puskesmas_sf <- st_as_sf(x = puskesmas,
                        coords = c("Longitude", "Latitude"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# get isoline ----
## here api setup ----
set_key(here_api_key)

## build isoline ----
### isoline untuk semua hospital ----
iso_puskesmas <- isoline(
  poi = puskesmas_sf,
  range = seq(10,20,10) *60,
  range_type = "time",
  datetime <- as.POSIXct(paste0(Sys.Date()," 10:00")) 
) %>%
  mutate(name = paste0((range - 600) / 60," to ", range / 60, " mins"))

## grouping polygon yang mau dimerge
iso_puskesmas <- iso_puskesmas  %>%
  group_by(rank) %>%
  summarise()

## convert isochrone to spatial polygon ----
iso_puskesmas.sp <- as(iso_puskesmas, "Spatial")

## reverse layer order
iso_puskesmas.sp@data <- rev(iso_puskesmas.sp@data)
iso_puskesmas.sp@polygons <- rev(iso_puskesmas.sp@polygons)
iso_puskesmas.sp$name <- rev(iso_puskesmas.sp$name)
iso_puskesmas.sp@data$name <- c("10 to 20 mins","0 to 10 mins")

## Create a color palette for the first isochrone catchment areas ----
iso_all.colors <- c("#edf3fa", "#083685")
iso_all.pal <- colorFactor(iso_all.colors, iso_puskesmas.sp@data$name)

# plot ----
## semua hospital digabung ----
m <- leaflet() %>% 
  addTiles(paste0("https://{s}.tile.jawg.io/jawg-matrix/{z}/{x}/{y}{r}.png?access-token=",jawg_token)) %>%
  addPolygons(data = iso_puskesmas.sp,
              fill=TRUE,
              fillColor = ~iso_all.pal(iso_puskesmas.sp@data$name),
              fillOpacity=0.35,
              stroke=TRUE,
              color = "black",
              weight=0.5, 
              popup = iso_puskesmas.sp@data$name,
              group = "Catchment Area") %>%  
  addMarkers(puskesmas$Longitude,
             puskesmas$Latitude,
             icon = list(
               iconUrl = "2023.06.24_#4 Hospital Access/img/puskesmas_icon.png",
               iconSize = c(15,15)
             )) %>%
  setView(lat = -6.717868966247109,
          lng = 106.73797696014125,
          zoom = 12)

m

# save map ----
htmlwidgets::saveWidget(m, "temp.html", selfcontained=TRUE)
webshot2::webshot("temp.html", file="2023.06.24_#4 Hospital Access/n.m4.png", cliprect="viewport") 
