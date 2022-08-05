## -load packages
library(sf)
library(tmap)
library(dplyr)
data(World)
# package tmap has a syntax similar to ggplot. The functions start all with tm_
tm_shape(World) +
  tm_borders()


## - explore World
#head(World)
names(World)
class(World)
dplyr::glimpse(World)


-
plot(World[1])
plot(World[,1])
plot(World[1,])
plot(World["pop_est"])



head(World[, 1:4])



class(World)
class(World$geometry)


## -take eometry out
head(sf::st_coordinates(World))
no_geom <- sf::st_drop_geometry(World)
class(no_geom)
#bounding boxes
st_bbox(World)


## -plot only south america
names(World)
unique(World$continent)
World %>%
  filter(continent == "South America") %>%
  tm_shape() +
  tm_borders()


## -create a new column for color and use it to fill
World %>%
  mutate(our_countries = if_else(iso_a3 %in% c("COL","BRA", "MEX"), "red", "grey")) %>%
  tm_shape() +
  tm_borders() +
  tm_fill(col = "our_countries") +
  tm_add_legend("fill",
                "Countries",
                col = "red")


## -create and write a shapefile
#install.packages("rnaturalearth")
#install.packages("remotes")
#remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearth)
library(rnaturalearthhires)
bra <- ne_states(country = "brazil", returnclass = "sf")
plot(bra)
dir.create("data/shapefiles", recursive = TRUE)
st_write(obj = bra, dsn = "data/shapefiles/bra.shp", delete_layer = TRUE)


## -
bra2 <- read_sf("data/shapefiles/bra.shp")
class(bra)
class(bra2)
plot(bra)
plot(bra2)


## -
library(raster)
dir.create(path = "data/raster/", recursive = TRUE)
tmax_data <- getData(name = "worldclim", var = "tmax", res = 10, path = "data/raster/")
plot(tmax_data)
is(tmax_data) #the data are a raster stack, several rasters piled
dim(tmax_data)
extent(tmax_data)
res(tmax_data)
