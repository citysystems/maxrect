library(wellknown)
library(rgdal)

smc_footprints <- read_csv("E:/City Systems/sonoma/ca_06081_footprints/ca_06081_footprints.csv")

smc_footprints_test <- head(smc_footprints)

output <- lapply(smc_footprints_test$WKT,FUN=function(WKT){
  wkt2geojson(WKT,feature=FALSE)
})

writeOGR(output, dsn="smc_footprints.geojson", layer="output", driver="GeoJSON")
