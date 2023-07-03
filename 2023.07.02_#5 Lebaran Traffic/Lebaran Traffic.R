# libraries
library(tidyverse)
library(hereR)
library(leaflet)
library(sf)
library(mapview)
library(raster)
library(googletraffic)

# library(geodata) # if not using raster, getData in raster soon will be removed

# source
source("config.R")

# set here key
set_key(here_api_key)

# data traffic from HERE
traffic <- flow(
  aoi = aoi[aoi$code == "LI", ],
  url_only = TRUE
)


# default example
flows <- flow(
  aoi = aoi
)

flow(aoi, min_jam_factor = 0, url_only = FALSE)

flows <- flows[order(flows$JF), ]
rownames(flows) <- NULL

if (requireNamespace("mapview", quietly = TRUE)) {
  mapview::mapview(flows,
                   zcol = "JF",
                   lwd = round(flows$JF*2),
                   layer.name = "Jam factor",
                   map.types = c("Esri.WorldTopoMap"),
                   homebutton = FALSE
  )
}

# raster method
## Grab polygon of Manhattan
library(raster)


id_sp <- getData('GADM', country='Indonesia', level=1)
id_sp2 <- getData('GADM', country='Indonesia', level=2)
jawa_sp <- id_sp[id_sp$NAME_0 %in% c("Banten","Jakarta Raya","Jawa Barat","Jawa Tengah","Jawa Timur","Yogyakarta"),]

## Make raster
r <- gt_make_raster_from_polygon(polygon    = jawa_sp,
                                 zoom       = 15,
                                 google_key = gcp_api)
# chat gpt example
url <- "https://traffic.cit.api.here.com/traffic/6.2/flow.json"
latitude <- 37.7749
longitude <- -122.4194
yesterday <- as.Date(Sys.Date()) - 1

params <- list(
  apiKey = here_api_key,
  bbox = paste(latitude - 0.1, longitude - 0.1, latitude + 0.1, longitude + 0.1, sep = ","),
  date = format(yesterday, "%Y-%m-%d"),
  time = "12:00:00",
  tz = "UTC"
)

response <- GET(url, query = params)
library(httr)
