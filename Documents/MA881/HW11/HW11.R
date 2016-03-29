library("maptools")
area <- readShapePoly("Bos_neighborhoods_new.shp")

library(RColorBrewer)
colors <- brewer.pal(9, "RdPu")

library(ggmap)
area.points <- fortify(area)

ggplot() +  geom_polygon(aes(x = long,                   
                             y = lat,                   
                             group = group),                 
                         color = colors[6],                   
                         fill = colors[9],               
                         data = area.points) +  labs(x = "Longitude",       y = "Latitude")



library(rjson) 
library(plyr) 

test <-fromJSON(file=url("http://api.census.gov/data/2010/sf1?key=3a29df5d1bc88291a0ffb322ae62b863dbf7efab&get=P0030001,NAME&for=county:*&in=state:25")) 
test2<-ldply(test)[-1,] 
names(test2)<-ldply(test)[1,] 
head(test2)
