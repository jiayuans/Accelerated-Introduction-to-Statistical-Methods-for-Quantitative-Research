library(tigris)
library(sp)
library(rgeos)
library(ggmap)

murder <- subset(crime, offense == "murder")
qmplot(lon, lat, data = murder, colour = I("red"), size = I(3), darken = .3)

geocode("boston university")

boston = tracts(state='MA', county = 'Essex County')
plot(boston)

bu <- "boston university"
qmap(bu, zoom = 14)
qmap(bu, zoom = 14, source = "osm")

set.seed(500)
df <- round(data.frame(
  x = jitter(rep(-95.36, 50), amount = .3),
  y = jitter(rep( 29.76, 50), amount = .3)
), digits = 2)
map <- get_googlemap("boston", markers = df, path = df, scale = 2)
ggmap(map, extent = "device")

qmap(bu, zoom = 14, source = "stamen", maptype = "watercolor")
qmap(bu, zoom = 14, source = "stamen", maptype = "toner")

bos <- get_map(location = "boston")
str(bos)

ggmap(bos, extent = "normal")
str(crime)

geocode("boston")
set.seed(500)
df <- round(data.frame(
  x = jitter(rep(-71.06, 50), amount = .3),
  y = jitter(rep(42.36, 50), amount = .3)
), digits = 2)
map <- get_googlemap('boston', markers = df, path = df, scale = 2)
ggmap(map, extent = 'device')



library(leaflet)
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(-71.1054, 42.3505, popup="Boston University")
m  # Print the map

leaflet() %>% addTiles() %>%
  addPopups(-71.1054,42.3505, popup="Boston University",
            options = popupOptions(closeButton = FALSE))


