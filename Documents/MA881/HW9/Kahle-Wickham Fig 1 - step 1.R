library(tigris)
library(sp)
library(rgeos)
library(ggmap)

# Dallas, TX census tracts

dfw = tracts(state='TX', county = c('Dallas', 'Tarrant'))
plot(dfw)

# Houston, TX census tracts

hou = tracts(state='TX', county = 'Harris County')
plot(hou)


# MA census tracts

ma = tracts(state='MA')
plot(ma)


# Houston, TX zip code areas

uas <- urban_areas()

dfh <- zctas(cb=T, starts_with = "77")
houston_ua <- uas[grep("Houston, TX", uas$NAME10), ]
hou_zcta <- dfh[as.vector(gIntersects(dfh, houston_ua, byid = TRUE)), ]
plot(hou_zcta)


# Boston, MA zip code areas

dfb <- zctas(cb=T, starts_with = "02")
boston_ua <- uas[grep("Boston, MA", uas$NAME10), ]
bos_zcta <- dfb[as.vector(gIntersects(dfb, boston_ua, byid = TRUE)), ]
plot(bos_zcta)

