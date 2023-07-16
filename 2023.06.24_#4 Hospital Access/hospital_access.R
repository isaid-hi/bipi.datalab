# libraries
library(tidyverse)
library(hereR)
library(leaflet)
library(sf)
# library(sp)

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
    ### isoline untuk semua hospital ----
    isochrones_all <- isoline(
      poi = hospital_sf,
      range = seq(10, 30, 10) * 60,
      range_type = "time",
      datetime <- as.POSIXct(paste0(Sys.Date()," 10:00")) 
    ) %>%
      mutate(name = paste0((range - 600) / 60," to ", range / 60, " mins"))

  ## grouping polygon yang mau dimerge
  isochrones_all <- isochrones_all  %>%
    group_by(rank) %>%
    summarise()

  ## convert isochrone to spatial polygon ----
  isochrones_all.sp <- as(isochrones_all, "Spatial")

  ## reverse layer order
  isochrones_all.sp@data <- rev(isochrones_all.sp@data)
  isochrones_all.sp@polygons <- rev(isochrones_all.sp@polygons)
  isochrones_all.sp$name <- rev(isochrones_all.sp$name)
  isochrones_all.sp@data$name <- c("20 to 30 mins","10 to 20 mins","0 to 10 mins")
  
  ## Create a color palette for the first isochrone catchment areas ----
  iso_all.colors <- c("#edf3fa", "#316eb0", "#083685")
  iso_all.pal <- colorFactor(iso_all.colors, isochrones_all.sp@data$name)
  
# plot ----
  ## semua hospital digabung ----
  m <- leaflet() %>% 
    addTiles(paste0("https://{s}.tile.jawg.io/jawg-matrix/{z}/{x}/{y}{r}.png?access-token=",jawg_token)) %>%
    addPolygons(data = isochrones_all.sp,
                fill=TRUE,
                fillColor = ~iso_all.pal(isochrones_all.sp@data$name),
                fillOpacity=0.35,
                stroke=TRUE,
                color = "black",
                weight=0.5, 
                popup = isochrones_all.sp@data$name,
                group = "Catchment Area") %>%  
    addMarkers(hospitals$Longitude,
               hospitals$Latitude,
               group = "Hospitals",
               icon = list(
                 iconUrl = "2023.06.24_#4 Hospital Access/img/hospital_icon.png",
                 iconSize = c(15,21)
               )) %>%
    setView(lat = -6.6753644410483375,
            lng = 106.77892924539358,
            zoom = 13)
   
m

  ## tambah plyline sungai cisadane ----
  ### data
    cisadane <- jsonlite::fromJSON("2023.06.24_#4 Hospital Access/data/sungai_cisadane.geojson")
    cisadane2 <- jsonlite::fromJSON("2023.06.24_#4 Hospital Access/data/cisadane_stream1.geojson")
 
    # switch coord: cisadane
    coordinates_list <- cisadane$features$geometry$coordinates
    switched_coordinates <- list(coordinates_list[[2]], coordinates_list[[1]], coordinates_list[[3]])
    cisadane$features$geometry$coordinates <- switched_coordinates
    
    #buat dataframe
    cisadane <- do.call(rbind, lapply(cisadane$features$geometry$coordinates, as.data.frame))
    cisadane2 <- do.call(rbind, lapply(cisadane2$features$geometry$coordinates, as.data.frame))
    
    #gabung keduanya
    colnames(cisadane) <- c("lng","lat")
    colnames(cisadane2) <- c("lng","lat")
    m %>%
      addTiles(paste0("https://{s}.tile.jawg.io/jawg-dark/{z}/{x}/{y}{r}.png?access-token=",jawg_token)) %>%
      addPolylines(cisadane, lng = cisadane$lng, lat = cisadane$lat,
                   color = "#2EFFBD",
                   opacity = 1) %>%
      addPolylines(cisadane2, lng = cisadane2$lng, lat = cisadane2$lat,
                   color = "#2EFFBD",
                   opacity = 1)

    
das_sf <- sf::st_read("2023.06.24_#4 Hospital Access/Batas_DAS_KLHK.shp")

# -6.6753644410483375, 106.77892924539358
# save map ----
  htmlwidgets::saveWidget(m, "temp.html", selfcontained=TRUE)
  webshot2::webshot("temp.html", file="2023.06.24_#4 Hospital Access/blackspot_tajur halang.png", cliprect="viewport") 
  