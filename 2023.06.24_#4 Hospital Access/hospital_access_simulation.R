library(tidyverse)
library(leaflet)

######################
isochrones_test <- isochrones_all

isochrones_test <- isochrones_test  %>%
  group_by(rank) %>%
  summarise()

isochrones_test.sp <- as(isochrones_test, "Spatial")

## reverse layer order
isochrones_test.sp@data <- rev(isochrones_test.sp@data)
isochrones_test.sp@polygons <- rev(isochrones_test.sp@polygons)
isochrones_test.sp$name <- rev(isochrones_test.sp$name)
isochrone_select <- spTransform(isochrones_test.sp,
                                CRS("+proj=longlat +datum=WGS84 +no_defs"))
isochrones_test.sp@data$name <- c("20 to 30 mins","10 to 20 mins","0 to 10 mins")

iso_all.colors <- c("#c6dbef", "#4292c6", "#08306b")
iso_all.pal <- colorFactor(iso_all.colors, isochrones_test.sp@data$name)

leaflet() %>% 
  addProviderTiles("CartoDB.DarkMatter", group="Greyscale") %>% 
  addPolygons(data = isochrones_test.sp,
              fill=TRUE,
              fillColor = ~iso_all.pal(isochrones_test.sp@data$name),
              fillOpacity=0.35,
              stroke=TRUE,
              color = "black",
              weight=0.5, 
              popup = isochrones_test.sp@data$name,
              group = "Catchment Area") %>%  
  # addCircles(hospitals$Longitude,
  #            hospitals$Latitude,
  #            radius = 5,
  #            opacity = 1,
  #            group = "Hospitals") %>%
  addMarkers(hospitals$Longitude,
             hospitals$Latitude,
             group = "Hospitals",
             icon = list(
               iconUrl = "2023.06.24_#4 Hospital Access/hospital-icon.png",
               iconSize = c(15,21)
             )) %>%
  
  # Add legends and layer control
  addLegend("bottomleft",
            values = isochrones_test.sp@data$name,
            pal = iso_all.pal, 
            opacity = 0.35,
            title = "Jarak waktu tempuh Rumah Sakit",
            group = "All") %>%
  addLayersControl(options = layersControlOptions(collapsed = FALSE),
                   overlayGroups = c("Catchment Area",
                                     "Hospitals"))

#############
leaflet() %>% 
  addTiles(paste0("https://{s}.tile.jawg.io/jawg-matrix/{z}/{x}/{y}{r}.png?access-token=",jawg_token))

############ puskesmas
leaflet() %>% 
  addTiles(paste0("https://{s}.tile.jawg.io/jawg-matrix/{z}/{x}/{y}{r}.png?access-token=",jawg_token)) %>%
  addMarkers(puskesmas$Longitude,
             puskesmas$Latitude,
             label = puskesmas$Name,
             icon = list(
               iconUrl = "2023.06.24_#4 Hospital Access/puskesmas_icon.png",
               iconSize = c(15,15)
             ))
