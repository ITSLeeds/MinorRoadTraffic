# 00 Aim to get the the origin data

### Clear memory
rm(list = ls())

### Load libraries
# Processing spatial objects
library(sf)
# Map view
library(tmap)
# To load road data
library(osmdata)
# To group data
library(dplyr)
# To generate plot
library(ggplot2)
#library(dismo)

#library(deldir)

tmap_mode("view")


# ################################################################# #
#### GET ROAD OSM DATA                                           ####
# ################################################################# #

### Find the bounding box(bb) and Using Overpass query to get Roads within the bb
q = opq(getbb("Leeds, UK")) %>%
  add_osm_feature(key = "highway")

### Return an OSM Overpass query as an osmdata object in sf format
osm_raw = osmdata_sf(q = q)

saveRDS(osm_raw,"Data_Leeds_2018/00_osm_raw.RDS")


# ################################################################# #
#### GET Traffic count data                                      ####
# ################################################################# #
# Traffic count points in Leeds
traffic <- read.csv(file="Data_Leeds_2018/dft_aadf_local_authority_id_63.csv", header=TRUE)


# Change the lon and lat to the 4326 crs
traffic <- st_as_sf(traffic,coords = c("longitude","latitude"),crs = 4326) 


# Filter the count points in 2018
traffic_2018 <- traffic[traffic$year == 2018, ] 

traffic_2018 <- traffic_2018[ , c("road_name","road_type","all_motor_vehicles")] 

names(traffic_2018)<-c("road_name","road_type","traffic_flow","geometry")
head(traffic_2018)
traffic_2018 <- st_transform(traffic_2018,27700)
saveRDS(traffic_2018,"Data_Leeds_2018/00_traffic_leeds_2018.RDS")

# Generate plot
traffic_2018$class <- substr(traffic_2018$road_name, 1, 1)
head(traffic_2018)
traffic_2018_plot <- traffic_2018
traffic_2018_plot$class <- as.factor(traffic_2018_plot$class)
ggplot(traffic_2018_plot, aes(x = class, y = traffic_flow)) + 
  geom_boxplot(width = 0.7) +
  scale_x_discrete(limits = c("M", "A", "B", "C", "U")) 
  # geom_dotplot(
  #  binaxis = "y", stackdir = "center",
  #  stackratio = 0.5, fill = "black",
  #  dotsize = 0.5
  #)  


# ################################################################# #
#### GET Boundary                                                ####
# ################################################################# #

# Make Polygon Around Traffic Data
bound_buf <- st_convex_hull(st_union(traffic_2018)) 

# Buffer Polygon by 1km
bound_buf <- st_buffer(bound_buf, 1000) 

# Make map
tm_shape(bound_buf) + 
  tm_fill(col = "orange", alpha = 0.3) + 
  tm_borders(col = "black", lwd = 1) +
  tm_legend()+
  qtm(traffic_2018, dots.col = "traffic_flow")


# Write data
st_write(bound_buf, "Data_Leeds_2018/00_bound_buf.gpkg", delete_dsn = TRUE)



# ################################################################# #
#### GET Economic and Demographic Data                           ####
# ################################################################# #

### Load LSOA boundary
lsoa <- read_sf("E:/R_language/Dissertation/Data/infuse_lsoa_lyr_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp")
lsoa <- lsoa[substr(lsoa$geo_code,1,1) == "E",]
st_write(lsoa, "Data_Leeds_2018/00_LSOA_England.gpkg", delete_dsn = TRUE)

lsoa_Leeds <- lsoa[bound_buf,]
qtm(bound_buf) + qtm(lsoa_Leeds, fill = NULL)

st_write(lsoa_Leeds, "Data_Leeds_2018/00_LSOA_Leeds.gpkg", delete_dsn = TRUE)
# lsoa <- readRDS("Data_Leeds_2018/LSOA_Cambridgeshire.Rds")


### Load population and car ownership original data
pop_carown <- read.csv (file="Data/PBCC_LSOA_data.csv",header = TRUE)
pop_carown <- pop_carown[pop_carown$LSOA11 %in% lsoa_Leeds$geo_code, ]
pop_carown <- pop_carown[,c("LSOA11","cars_percap_2018","pop_2018", "T2W_CarOrVan","km_CarOrVan","T2S_car")]

demo_eco <- left_join(lsoa_Leeds, pop_carown, by = c("geo_code" = "LSOA11"))
tm_shape(demo_eco) +
  tm_fill("cars_percap_2018", breaks = c(0,0.2,0.4,0.6,1,2,70))


### Load employment original data
employ <- read.csv (file = "Data_Leeds_2018/employment_Leeds_2018.csv", header = TRUE)
# employ_empt <- demo_eco[!demo_eco$geo_label %in% employ$geo_name,]
demo_eco <- left_join(demo_eco, employ, by = c("geo_code" = "mnemonic"))
tm_shape(demo_eco) +
  tm_fill("Employment", breaks = c(0,200,400,600,1000,2000,5000))                    

saveRDS(demo_eco, "Data_Leeds_2018/00_demo_eco.RDS")
