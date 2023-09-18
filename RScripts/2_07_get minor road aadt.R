# Aim: To calculate AADT for minor roads
# This script assumes the data has already been cleaned and the AADT for major roads assigned
# Rework of original file dropping the splitting an buffering stage

### Clear memory-----------------------------------------------------------
rm(list = ls())

# Load Packages -----------------------------------------------------------

#library(ggplot2)
# Process spatial objects
library(sf)
# Map view
library(tmap)
# Data frame manipulation
library(dplyr)

tmap_mode("view")

### Load Data 
bound<-st_read("Data_Leeds_2018/00_bound_buf.gpkg")
traffic_2018.minor <- readRDS("Data_Leeds_2018/02-traffic_2018.minor.RDS")
lines_major <- st_read("Data_Leeds_2018/02_lines_major.gpkg")
lsoa_Leeds <- st_read("Data_Leeds_2018/05_lsoa_Leeds.gpkg")
lines_minor <- st_read("Data_Leeds_2018/06_lines_minor.gpkg")

#################################################################
### Get minor road AADT                                       ###
#################################################################
lines_minor_buf <- st_buffer(lines_minor, 6)
lines_minor_buf <- st_join(lines_minor_buf, 
                           traffic_2018.minor[,c("traffic_flow")])
summary(is.na(lines_minor_buf$traffic_flow))
              # Check if the traffic counts number is right

lines_minor$traffic_flow <- 
  lines_minor_buf$traffic_flow[match(lines_minor$id,lines_minor_buf$id)]

qtm(lines_minor, lines.col = "traffic_flow") + 
  qtm(traffic_2018.minor, dots.col = "traffic_flow")
summary(is.na(lines_minor$traffic_flow))


### Make the plot
# Classify the lsoa by county
lsoa_Leeds$county <- substr(lsoa_Leeds$geo_label, 1, 8)
county_name <- unique(lsoa_Leeds$county)
Leeds_name <- c("Leeds 05", "Leeds 09", "Leeds 06", "Leeds 03", "Leeds 02", "Leeds 10", 
                "Leeds 01", "Leeds 08", "Leeds 00", "Leeds 07", "Leeds 11", "Leeds 04" )
#full_name <- c("Peterborough", "South Cambridgeshire", "Huntingdonshire", 
#               "East Cambridgeshre", "Cambridage", "Fenland")
lsoa_Leeds <- lsoa_Leeds[lsoa_Leeds$county %in% Leeds_name, ]
#for (i in (1:length(Leeds_name))){
#  sub_name <- Leeds_name[i]
#  for (k in (1:nrow(lsoa_Leeds)))
#  if (lsoa_Leeds$county[k] == sub_name){
#    lsoa_Leeds$county[k] <- full_name[i]
#  }
#}

# Make the spatial distribution plot
tm1 <- tm_shape(bound) +
  tm_polygons(col = "grey", alpha = 0.8, border.col = "black") +
  tm_shape(lsoa_Leeds) +
  tm_fill(col = "county", alpha = 0.8) +
  tm_legend() +
  tm_shape(lines_major) +
  tm_lines(col = "black",lwd = 2) +
  tm_shape(lines_minor) +
  tm_lines(col = "black", lwd = 1) +
  tm_shape(traffic_2018.minor) +
  tm_dots(col = "traffic_flow", shape = 21,
          size = 0.5, legend.show = TRUE) +
  tm_layout(scale(3.5),
            inner.margins =  c(0.05, 0.15 ,0.05, 0.10),
            legend.position = c(0.02, 0.05), 
            legend.width = 5,
            legend.height = 5,
            legend.text.size = 1.3,
            legend.title.size = 2
  ) +
  tm_compass(size = 5,
             position =c(0.82, 0.85)) 
 
tmap_save(tm1, filename = "Plot_Leeds_2018/07_minor_aadt.png", 
          units = "cm", width = 42, height = 29, dpi = 600)


st_write(lines_minor, "Data_Leeds_2018/07_lines_minor.gpkg")
st_write(lsoa_Leeds, "Data_Leeds_2018/07_lsoa_Leeds.gpkg", delete_dsn = TRUE)
