## Grab polygon of Manhattan
us_sp <- getData('GADM', country='USA', level=2)
ny_sp <- us_sp[us_sp$NAME_2 %in% "New York",]

## Make raster
r <- gt_make_raster_from_polygon(polygon    = ny_sp,
                                 zoom       = 15,
                                 google_key = gcp_api)


## Javva
id_sp <- getData('GADM', country='Indonesia', level=1)
id_sp2 <- getData('GADM', country='Indonesia', level=2)
jawa_sp <- id_sp[id_sp$NAME_0 %in% c("Banten","Jakarta Raya","Jawa Barat","Jawa Tengah","Jawa Timur","Yogyakarta"),]
bogor_sp <- id_sp2[id_sp2$NAME_2 %in% "Bogor",]

## Make raster
r <- gt_make_raster_from_polygon(polygon    = bogor_sp,
                                 zoom       = 15,
                                 google_key = gcp_api)

## Map raster
traffic_pal <- colorNumeric(c("green", "orange", "red", "#660000"), 
                            1:4,
                            na.color = "transparent")

leaflet(width = "100%") %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addRasterImage(r, colors = traffic_pal, opacity = 1, method = "ngb") 
  