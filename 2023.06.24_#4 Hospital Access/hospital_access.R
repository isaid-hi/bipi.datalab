# libraries
library(googleway)
library(tidyverse)
library(hereR)
library(leaflet)
library(sf)

# source
source("config.R")

# health facility
data <- read.csv("2023.06.24_#4 Hospital Access/hospitals.csv")

# hospital only
hospitals <- data %>% filter(Type %in% "Rumah Sakit")
hospital_sf <- st_as_sf(x = hospitals,
         coords = c("Longitude", "Latitude"),
         crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# get isoline ----
  ## here api setup ----
  set_key(here_api_key)
  
  ## build isoline ----
    ## isoline untuk semua hospital ----
    isochrones_all <- isoline(
      poi = hospital_sf,
      range = seq(10, 30, 10) * 60,
      range_type = "time",
      datetime <- as.POSIXct(paste0(Sys.Date()," 10:00")) 
    ) %>%
      mutate(name = paste0((range - 600) / 60," to ", range / 60, " mins"))

  ## convert isochrone to spatial polygon ----
  isochrones_all.sp <- as(isochrones_all, "Spatial")

  ## reverse layer order
isochrones_rev.sp <- isochrones_all.sp
isochrones_rev.sp@data <- rev(isochrones_rev.sp@data)
isochrones_rev.sp@polygons <- rev(isochrones_rev.sp@polygons)
isochrones_rev.sp$name <- rev(isochrones_rev.sp$name)

isochrone_select <- spTransform(isochrones_rev.sp,
                                CRS("+proj=longlat +datum=WGS84 +no_defs"))

  ## Create a color palette for the first isochrone catchment areas ----
  iso_all.colors <- c("#c6dbef", "#4292c6", "#08306b")
  iso_all.pal <- colorFactor(iso_all.colors, isochrones_all.sp@data$name)
  
# plot ----
  ## semua hospital digabung ----
  leaflet() %>% 
    addProviderTiles("CartoDB.Positron", group="Greyscale") %>% 
    addPolygons(data = isochrones_rev.sp,
                fill=TRUE,
                fillColor = ~iso_all.pal(isochrones_rev.sp@data$name),
                fillOpacity=0.35,
                stroke=TRUE,
                color = "black",
                weight=0.5, 
                popup = isochrones_rev.sp@data$name,
                group = "Catchment Area") %>%  
    addCircles(hospitals$Longitude,
               hospitals$Latitude,
               radius = 5,
               opacity = 1,
               group = "Hospitals") %>%
    
    # Add legends and layer control
    addLegend("bottomleft",
              values = isochrones_rev.sp@data$name,
              pal = iso_all.pal, 
              opacity = 0.35,
              title = "Jarak waktu tempuh Rumah Sakit",
              group = "All") %>%
    addLayersControl(options = layersControlOptions(collapsed = FALSE),
                     overlayGroups = c("Catchment Area",
                                       "Hospitals"))
  