#Read in Hamilton Harbour Shapefile

library('sf')
shapefile <- read_sf("data-raw/files-raw/HH_shapefile/HH_Water_Poly.shp") #st_read()


###Project shapefile
HH_shapefile_WGS84 <- st_transform(shapefile, "WGS84") #Also try "EPSG:4326"; 'EPSG:32724'


usethis::use_data(HH_shapefile_WGS84, overwrite = TRUE)
