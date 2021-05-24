
####-->  RIVER BUFFER DRC
####-->  Case study in Democratic Republic of Congo (DRC)
####-->  April 2019                    
####-->  by Marco Pizzolato                   


#### SHORT DESCRIPTION ----

# This scrips was built for a real case study in Democratic Republic of Congo (DRC)
# in which there is the need of selecting all the villages within two buffer zones 
# of 20km and 50km around two rivers: the Sankuru and the Kasai.
# In addition the script also extract population figures from the GHSL and Facebook datasets and 
# assign these to each village


#### CLEANUP ----

# Clean the environment if you need to do so
# rm(list = ls())


#### LIBRARIES ----

library(sf)
library(dplyr)
library(tidyr)

# NB-Further libraries will be installed after, close to the section that requires them 


####  WORKING DIRECTORY ----

# Set WD automatically
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Check the wd
getwd()


#### READ RIVER FILES ----

## Loading while setting the Coordinate Reference Systems (CRS) and reprojecting
river.drc <- readRDS("./Input/rivers_drc.rds") %>% st_set_crs(.,4326) %>% st_transform(.,54009) 

## Isolate only the geometries for the two rivers of interest
river.drc.sub <- subset(river.drc, HYD_NAME == "SANKURU" | HYD_NAME == "KASAI")
## Dissolve the geometries by river to obtain two lines instead of many
river.drc.sub2 <- river.drc.sub %>% group_by(., HYD_NAME) %>% summarise() %>% ungroup()

## Plot the two rivers
plot(river.drc.sub2, main="Kasai & Sankuru", key.pos=NULL)

## Keep the environment tidy
rm(river.drc, river.drc.sub)


#### CREATE BUFFER ZONES  ----

## Create a buffer of 20km and 50km around the rivers
buffer.20km <- st_buffer(river.drc.sub2, dist = 20000, endCapStyle = "ROUND")
plot(buffer.20km, main="Buffer 20km", key.pos=NULL)
buffer.50km <- st_buffer(river.drc.sub2, dist = 50000, endCapStyle = "ROUND")
plot(buffer.50km, main="Buffer 50km", key.pos=NULL)

## Dissolve the buffers in a single polygon
buffer.20.dis <- buffer.20km %>% dplyr::mutate(.,DISS = "all") %>% group_by(., DISS) %>% summarise() %>% ungroup()
plot(buffer.20.dis, main="Buffer 20km dissolved", key.pos=NULL)
buffer.50.dis <- buffer.50km %>% dplyr::mutate(.,DISS = "all") %>% group_by(., DISS) %>% summarise() %>% ungroup()
plot(buffer.50.dis, main="Buffer 50km dissolved", key.pos=NULL)

# ## Export the buffers as shapefile - activate if needed
# buffer.20.dis %>% st_transform(.,4326) %>%st_write(., "./Output/buffer_20dis.shp")
# buffer.50.dis %>% st_transform(.,4326) %>%st_write(., "./Output/buffer_50dis.shp")


### Isolate the different buffers. 
## This is necessary to identify the villages that falling into the following areas: 
## Sankuru 50km - Sankuru 20 km - Kasai 50km - Kasai 20km
## To ensure there are not polygon overlaps it is necessary to carry out some geoprocessing operations,
## in this way the villages will uniquely belong to a single polygon.

only.kasai.50 <- subset(buffer.50km, HYD_NAME == "KASAI")
only.sankuru.50 <- buffer.50km %>% filter(., HYD_NAME == "SANKURU") %>% st_difference(.,only.kasai.50) %>% dplyr::select(., -HYD_NAME.1)
only.kasai.20 <- subset(buffer.20km, HYD_NAME == "KASAI")
only.sankuru.20 <- buffer.20km %>% filter(., HYD_NAME == "SANKURU") %>% st_difference(.,only.kasai.50) %>% dplyr::select(., -HYD_NAME.1)

# ## Export the single buffers as shapefile - activate if needed
# only.kasai.50 %>% st_transform(.,4326) %>%st_write(., "./Output/buffer_kasai_50dis.shp")
# only.sankuru.50 %>% st_transform(.,4326) %>%st_write(., "./Output/buffer_sankuru_50dis.shp")
# only.kasai.20 %>% st_transform(.,4326) %>%st_write(., "./Output/buffer_kasai_20dis.shp")
# only.sankuru.20 %>% st_transform(.,4326) %>%st_write(., "./Output/buffer_sankuru_20dis.shp")

## Keep the environment tidy
rm(buffer.20km,buffer.50km)


#### PLOT BUFFER ZONES ----

# install.packages("tmap")
library(tmap)

## Set a bounding box using the buffer sf to display all the layers entirely  
bbox.dis <- st_bbox(buffer.50.dis)

## Create all the map elements
# River map
m.riv <- tmap::tm_shape(river.drc.sub2, bbox = bbox.dis) +  # using bbox here 
  tm_lines(col = "cyan",lwd = 1, lty = "solid", alpha = 0.3)
# Buffer maps
m.only.kasai.50 <- tmap::tm_shape(only.kasai.50) +
  tm_borders(col = "#8470FF", alpha = 0.5)
m.only.sankuru.50 <- tmap::tm_shape(only.sankuru.50) +
  tm_borders(col = "#4169E1", alpha = 0.5)
m.only.kasai.20 <- tmap::tm_shape(only.kasai.20) +
  tm_borders(col = "#66CD00", alpha = 0.5)
m.only.sankuru.20 <- tmap::tm_shape(only.sankuru.20) +
  tm_borders(col = "#008B00", alpha = 0.5)

## Plot all the map elements together
m.riv + m.only.kasai.50 + m.only.sankuru.50 + m.only.kasai.20 + m.only.sankuru.20+
  tm_layout(main.title ="River & buffers", main.title.position = "center",frame = FALSE)

## Keep the environment tidy
rm(m.riv, m.only.kasai.50, m.only.sankuru.50, m.only.kasai.20, m.only.sankuru.20, bbox.dis)


#### EXTRACT VILLAGES ----

## Load the gazetteer villages dataset
gaz.drc <- readRDS("./Input/gaz_drc.rds") %>% st_set_crs(.,4326) %>% st_transform(.,54009)

## Subset only the villages which are within 50km from the rivers and
## assign to all of them the attribute "50km" in buffer
gaz.drc.50km <- gaz.drc %>% .[subset(buffer.50.dis),] %>% mutate(BUFFER = "50km", AREA = NA)

## Remove duplicate geometries contained in the subset of the village file. 6189 -> 5281
gaz.drc.50km <- st_difference(gaz.drc.50km) 

## Ensure all the points are within DRC
# Exclude all the points which are without DRC
drc.boundaries <- readRDS("./Input/boundaries_drc_admin0.rds")
gaz.drc.50km <- st_join(gaz.drc.50km,drc.boundaries) %>% subset(., adm0_name == "Democratic Republic of the Congo")

## Assign the correct sector/territory/province to the village points
# Load the sector file anbd keep only the attributes of interest
drc.sectors <- readRDS("./Input/boundaries_drc_sectors.rds") %>%
  subset(., select = c(nom_prov,nom_distri,nom_terri,nom_sec))# already in 54009 projection

# Spatial join between the villages and the sector to get the info in the attribute table
gaz.drc.50km <- st_join(gaz.drc.50km,drc.sectors)

### Calculate in which buffer the village points are

## Calculate logic vectors used to re-assign the labels
lo.only.kasai.50 <- st_intersects(gaz.drc.50km, only.kasai.50, sparse = FALSE)
lo.only.sank.50 <- st_intersects(gaz.drc.50km, only.sankuru.50, sparse = FALSE)
lo.buff.20 <- st_intersects(gaz.drc.50km, buffer.20.dis, sparse = FALSE)

## Re-assigning the labels
gaz.drc.50km <- gaz.drc.50km %>%
                mutate(AREA = replace(AREA, lo.only.kasai.50, "kasai" )) %>%
                mutate(AREA = replace(AREA, lo.only.sank.50, "sankuru" )) %>%
                mutate(BUFFER = replace(BUFFER, lo.buff.20, "20km" ))

## Check the numbers of villages within each buffer area
gaz.drc.50km %>% group_by(AREA,BUFFER) %>%  summarise(n = n()) %>% ungroup()

## Keep the environment tidy
rm(gaz.drc, drc.boundaries, drc.sectors, lo.only.kasai.50, lo.only.sank.50, lo.buff.20)


#### GET POPULATION FIGURES - GLOBAL HUMAN SETTLEMENT LAYER (GHSL) ----

# install.packages("raster")
library(raster)

## Load the population raster with resolution 1km
rast.ghsl.1km <- readRDS("./Input/rast_ghsl_1km.rds") #raster has already 54009 projection

## Aggregation of the GHSL raster from 1km x 1km to a 2km x 2km raster (sum not average)
rast.ghsl.2km <- raster::aggregate(rast.ghsl.1km,fact=2,fun=sum) 

# writeRaster(rast.ghsl.2km, filename="GHSL_2km_.tif", format="GTiff")
rast.ghsl.2km <- raster("./Output/GHSL_2km_.tif")

## Calculate the number of villages per each cell in a raster 2km x 2km with the same exact dimension of rast.ghsl.2km
# Get the raster r.points with the same dimension as rast.ghsl.2km but filled with zeros
r.points <- rast.ghsl.2km
r.points[] <- 0
# Transform the sf in a spatial dataframe compatible with functions that require sp objects
xy <- as(gaz.drc.50km, 'Spatial')
tab <- table(cellFromXY(r.points, xy))
r.points[as.numeric(names(tab))] <- tab

## Export the rasters - Activate if needed
# writeRaster(rast.ghsl.1km, filename="./Output/rast.ghsl.1km.tif", format="GTiff")
# writeRaster(rast.ghsl.2km, filename="./Output/rast.ghsl.2km.tif", format="GTiff")
# writeRaster(r.points, filename="./Output/r.points.tif", format="GTiff")

### Get the population count GHSL

## Extract the values from the population raster 
gaz.drc.val <- as.data.frame(raster::extract(rast.ghsl.2km, gaz.drc.50km, method="simple"))
# Rename the column and add it to the villages 50km dataset
colnames(gaz.drc.val) <- c("ghsl_pop_agg")
gaz.drc.50km <- cbind(gaz.drc.50km,gaz.drc.val$ghsl_pop_agg)
# Rename the column
colnames(gaz.drc.50km)[length(gaz.drc.50km )-1] <- "ghsl_totpop"

### Extract the values from the village-count raster 
gaz.drc.val2 <- as.data.frame(raster::extract(r.points, gaz.drc.50km, method="simple"))
# Rename the column and add it to the villages 50km dataset
colnames(gaz.drc.val2) <- c("vil_count")
gaz.drc.50km <- cbind(gaz.drc.50km,gaz.drc.val2$vil_count)
# Rename the column
colnames(gaz.drc.50km)[length(gaz.drc.50km )-1] <- "vilcount"

### Calculate the estimate population
## In case two or more points are within a 2km x 2km square the population count
## will be evenly divided among them. We also discard intermediate columns.
gaz.drc.50km <- gaz.drc.50km %>% mutate(.,ghsl_pop_avg = (ghsl_totpop/vilcount)) %>% subset(., select= -c(ghsl_totpop))

# Get the column as integer
gaz.drc.50km$ghsl_pop_avg <- as.integer(gaz.drc.50km$ghsl_pop_avg)

# Keep the environment tidy
rm(xy,tab,r.points,rast.ghsl.1km, gaz.drc.val, gaz.drc.val2)


#### GET POPULATION FIGURES - FACEBOOK ----

## Load the population raster with resolution 1km
# rast.fb.30m <- readRDS("./Input/rast_fb_30m.rds") #raster is in 4326 projection

rast.fb.30m <- raster::raster("D:/Google Drive/Maud/facebook_pop/population_cod_2018-10-01.tif")

### Crop the raster

## Since the raster has a very high number of cells it is better to resize and only work on the part of interest
# Get a bounding box from the buffer 50km dissolved and conver it in 4326 which is the CRS of the raster
bbox.ext <- buffer.50.dis %>% st_transform(.,4326) %>% extent(.)
# Crop the raster using the bounding box
rast.fb.crop <- raster::crop(rast.fb.30m,bbox.ext)
writeRaster(rast.fb.crop, filename="./Output/rast.fb.crop2.tiff", format="GTiff")


### Convert the raster in points

## The goal is to transfer the facebook population data to a raster equal to the GHSL 2km x 2km
## To do this the best option is to:
# (1)transform the facebook raster in points
# (2)create an empty copy of the GHSL
# (3)assign to the empty raster the sum of the values of the points within each cell 

# (1) Transform the facebook raster in points
r.fb.points <- rasterToPoints(rast.fb.crop, fun = NULL, spatial = TRUE)

## For the function used later "rasterize" it's necessary to have a sf object, while "rasterToPoints" give a sp object
## As workaround, to get a sf with the right structure ("sf" "tbl_df" "tbl" "data.frame") we will
## export the sp as shapefile reimporting it later as sf.

library(rgdal)
# Save r.fb.points results as ESRI Shapefile in working directory
writeOGR(obj=r.fb.points, 
         dsn=".", 
         layer="intermediate", 
         driver="ESRI Shapefile", 
         overwrite_layer=TRUE)

# Import the shapefile just saved as sf
r.fb.points.sf <- sf::read_sf("./intermediate.shp") %>% st_set_crs(.,4326) %>% st_transform(.,54009)
class(r.fb.points.sf)

# Delete the shapefile just created to keep the environment clean
unlink("./intermediate.shp", recursive = FALSE)


# (2)create an empty copy of the GHSL

## Get an empty raster same dimension of the GHSL 2km x 2km, cropped with a BBOX
r.2k.empty <- rast.ghsl.2km
r.2k.empty[] <- 0
# Calculate a bounding box around the 50km river buffer
bbox.ext.59009 <- buffer.50.dis %>%  extent(.)
# Crop the empty raster with the bounding box
r.2k.resiz <- raster::crop(r.2k.empty,bbox.ext.59009)

# (3)assign to the empty raster the sum of the values of the points within each cell 

rast.fb.2km = rasterize(r.fb.points.sf, r.2k.resiz, field = "p__2018", fun = sum)

## Export the rasters - Activate if needed
writeRaster(rast.fb.2km, filename="./Output/rast.fb.2km.tif", format="GTiff")

# Keep the environment tidy
rm(rast.fb.30m,bbox.ext,rast.fb.crop,r.fb.points, r.fb.points.sf, r.2k.empty,bbox.ext.59009,r.2k.resiz)


### Get the population count FACEBOOK

## Extract the values from the population raster 
gaz.drc.val <- as.data.frame(raster::extract(rast.fb.2km, gaz.drc.50km, method="simple"))
# Rename the column and add it to the villages 50km dataset
colnames(gaz.drc.val) <- c("fb_pop_agg")
gaz.drc.50km <- cbind(gaz.drc.50km,gaz.drc.val$fb_pop_agg)
# Rename the column
colnames(gaz.drc.50km)[length(gaz.drc.50km )-1] <- "fb_totpop"

## Extract the values from the village-count raster -- No need for this since we have already calculated it for the GHSL raster

### Calculate the estimate population
## In case two or more points are within a 2km x 2km square the population count
## will be evenly divided among them. We also discard intermediate columns.
gaz.drc.50km <- gaz.drc.50km %>% mutate(.,fb_pop_avg = (fb_totpop/vilcount)) %>% subset(., select= -c(fb_totpop,vilcount))

# Get the column as integer
gaz.drc.50km$fb_pop_avg <- as.integer(gaz.drc.50km$fb_pop_avg)
# Remove NAs
gaz.drc.50km$fb_pop_avg <- replace_na(gaz.drc.50km$fb_pop_avg,0)

# Keep the environment tidy
rm(xy,tab,r.points,rast.ghsl.1km, gaz.drc.val, gaz.drc.val2)


#### EXPORTS ----

## Export the shapefile with the projected coordinates 
st_write(gaz.drc.50km, "./Output/gaz.drc.50km_merc.shp")

## Export the shapefile re-projected as WGS84 spherical including the coordinates of each point
gaz.drc.50km %>% st_transform(.,4326) %>%  mutate(LONG = st_coordinates(.)[,1]) %>%  mutate(LAT = st_coordinates(.)[,2]) %>% st_write(., "./Output/gaz.drc.50km_wgs84.shp")

## Export the csv re-projected as WGS84 spherical including the coordinates of each point
gaz.drc.50km %>% st_transform(.,4326) %>%  mutate(LONG = st_coordinates(.)[,1]) %>%  mutate(LAT = st_coordinates(.)[,2]) %>% st_write(., 'Output/gaz.drc.50km.csv')



##== END OF SCRIPT ==##


