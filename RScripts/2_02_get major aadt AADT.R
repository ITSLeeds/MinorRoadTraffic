# 02 Get major and minor road AADT count
# Aim this script get the AADT data of the selected network and assigns
# AADT values to the corresponding roads and junctions

# ################################################################# #
#### LOAD LIBRARIES AND DEFINE CORE SETTINGS                     ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load libraries
# Process spatial objects
library(sf)
# Map view
library(tmap)
# Join data
library(dplyr)
# Nearest K tree
library(RANN)

tmap_mode("view")

# ################################################################# #
#### Load data                                                   ####
# ################################################################# #

bound<-st_read("Data_Leeds_2018/00_bound_buf.gpkg")
traffic_2018<-readRDS("Data_Leeds_2018/00_traffic_leeds_2018.RDS")
lines<-st_read("Data_Leeds_2018/01_network.gpkg")
points_junc <- st_read("Data_Leeds_2018/01_junctions.gpkg")

# ################################################################# #
#### Allocate road name to the network                          ####
# ################################################################# 
### Allocate the road type 
major_ref <- c("motorway","motorway_link","primary",
               "primary_link","trunk","trunk_link")

lines_major <- lines[lines$highway %in% major_ref, ]
lines_minor <- lines[!lines$highway %in% major_ref, ]

lines_major$road_type <- "major"
lines_minor$road_type <- "minor"
lines <- rbind(lines_major, lines_minor)

qtm(bound, fill = NULL) + qtm(lines, lines.col = "road_type")

### Get the centroid of each major lines
lines_major_cents <- st_coordinates(st_centroid(lines_major))


### Find the 30 of nearest neighbour centroids , and find potentially duplicated ones
nn = RANN::nn2(lines_major_cents, k = 30) ### k value depends on the network###

lines_major$highway[lines_major$highway == "motorway_link"] <-  "motorway"
lines_major$highway[lines_major$highway == "primary_link"] <-  "primary"
lines_major$highway[lines_major$highway == "trunk_link"] <-  "trunk"

for(k in 1:2){
  for(i in 1:nrow(lines_major)){
    if(is.na(lines_major$ref[i])){ #find the item whose ref equals to NA
      for(j in 2:30){ #starts from 2 because j=1 is the point itself
        idx <- nn$nn.idx[i,j] # allocate the row number of the nearest centroid 
        if(lines_major$highway[idx] == lines_major$highway[i]){
          if(!is.na(lines_major$ref[idx])){
            lines_major$ref[i] <- lines_major$ref[idx]
            break
          }
        }
      }
    }
  }
}


### manually check the ref and fix the unreasonable ones
lines_major$ref[lines_major$osm_id == "85032941"] <-  "A58" 
              # Based on the map to find the lost / wrong road and fix it
lines_major$ref[lines_major$ref == "B6481"] <-  "A639" 
lines_major$ref[lines_major$ref == "B6159"] <-  "A64"

tm_shape(bound) + 
  tm_fill(col = "grey", alpha = 0.6) + 
  tm_borders(col = "black", lwd = 1) +
  tm_legend() + 
  qtm(lines_major, lines.col = "ref", lines.lwd = 3) 
#qtm(traffic_2018, dots.col = "traffic_flow")

rm(nn,lines_major_cents)

# ############################################ #
####   Assign Traffic Counts to the roads   ####
# ############################################ #

### Prepare the boundary
bound_sfg <- st_cast(bound$geom, "POLYGON")
bound_sfc <- st_sfc(bound_sfg)

### Custom a Function
get.aadt.major <- function(e){
  #message(paste0("doing ",e))
  
  traffic.sub <- traffic_2018.major[traffic_2018.major$road_name == roadnames[e],]
  traffic.sub <- traffic.sub[!duplicated(traffic.sub$geometry),]
  lines.sub <- lines.nona[lines.nona$ref == roadnames[e],]

  ### need at least 2 points to make voronoi polygons, a cell consisting of
  ### all points of the plane closer to a certain seed than to any other
  if(nrow(traffic.sub) > 1){

    voronoi <- st_voronoi(st_combine(traffic.sub), envelope = bound_sfc)
    voronoi <- st_collection_extract(voronoi)
    voronoi <- st_as_sf(voronoi)
    voronoi <- st_join(voronoi, traffic.sub) #based on the geom

  }else{
    ### Make a 15000 buffer around the point
    voronoi <- st_buffer(traffic.sub, 15000)
  }
#qtm(voronoi) + qtm(lines.sub) + qtm(traffic.sub, dots.col = "traffic_flow")


  ### Find Intersections of roads with voronoi polygons
  inter <- st_intersects(lines.sub,voronoi)
  ### Get aadt and ncycle values 1:nrow(lines.sub)
  lines.sub$traffic_flow <-sapply(1:nrow(lines.sub),function(x)
    {as.numeric(round(mean(voronoi$traffic_flow[inter[[x]]])),0)})

  ### Remove Unneeded Data
  lines.sub <- as.data.frame(lines.sub)
  lines.sub <- lines.sub[,c("osm_id","traffic_flow")]

  return(lines.sub)
}

### find the road_name of AADT different from the line$ref and fix them
roadnames <- unique(traffic_2018$road_name)
roadnames.sub <- roadnames[!roadnames %in% lines$ref] 
traffic_2018$road_name[traffic_2018$road_name == "A58M"] <- "A58(M)"
traffic_2018$road_name[traffic_2018$road_name == "A64M"] <- "A64(M)"
       # check with the google map and fix the underlying wrong road name

rm(roadnames.sub)

### Separate Major and Minor Roads of the AADT Data
traffic_2018.major <- traffic_2018[traffic_2018$road_type=="Major", ]
traffic_2018.minor <- traffic_2018[traffic_2018$road_type=="Minor", ]
saveRDS(traffic_2018.minor,"Data_Leeds_2018/02-traffic_2018.minor.RDS")

### Allocate traffic counts to the major roads
roadnames <- unique(traffic_2018.major$road_name)
lines.nona <- lines_major[!is.na(lines_major$ref),] 
                  #Create a working dataset without nas
lines.nona <- lines.nona[,c("osm_id","ref")] #Dump unneeded data
res.major <- lapply(1:length(roadnames), get.aadt.major)
             #apply a function over a list or vector
res.major <- do.call("rbind",res.major) #rbind the data.frame
res.major <- res.major[!is.na(res.major$osm_id), ]


### remove any duplicates
res.major <- res.major[!duplicated(res.major$osm_id),]
res.major$traffic_flow <- as.numeric(res.major$traffic_flow)


### Join onto the original lines data
lines_major <- left_join(lines_major,res.major, by = c("osm_id" = "osm_id"))
rm(lines.nona, roadnames, res.major)

qtm(bound) + qtm(lines_major[lines_major$highway %in% 
                               c("motorway","motorway_link",
                                 "primary","primary_link",
                                 "trunk","trunk_link"),]
    , lines.lwd = 3, lines.col = "traffic_flow") + 
  qtm(traffic_2018.major, dots.col="traffic_flow")


### Manually fix the road sections with NA aadt value
road_not_in <- c("A6176", "A651")
                   # Those whose aadt is NA isn't the road in the study case 
lines_major <- lines_major[!lines_major$ref %in% road_not_in, ]

summary(is.na(lines_major$traffic_flow))

tm1 <- tm_shape(bound)+
  tm_fill(col = "grey",alpha = 0.7) +
  tm_borders(col = "black") +
  tm_shape(lines_major) +
  tm_lines(col = "traffic_flow", lwd = 3, legend.lwd.show = TRUE) +
  tm_shape(traffic_2018.major) +
  tm_dots(col = "traffic_flow", shape = 21, size = 1, legend.show = TRUE) +
  tm_layout(scale(3.5),
            inner.margins =  c(0.05, 0.15 ,0.05, 0.10),
            legend.position = c(0.05, 0.05), 
            legend.width = 5,
            legend.height = 5,
            legend.text.size = 1.3,
            legend.title.size = 2
  ) +
  tm_compass(size = 8,
             position =c(0.82, 0.85)) 

tmap_save(tm1, filename = "Plot_Leeds_2018/02_major_road_aadt.png", 
          units = "cm", width = 42, height = 29, dpi = 600)


# ################################################################# #
####   Allocate the aadt to the major and minor road junctions   ####
# ################################################################# #

### Find Junctions between minor and major roads 
minor_int <- st_intersects(points_junc, lines_minor)
major_int <- st_intersects(points_junc, lines_major)

minor_int = lengths(minor_int)
major_int = lengths(major_int)
both_int = ifelse(minor_int > 0 & major_int > 0, TRUE, FALSE)

junc_majmi = points_junc[both_int, ]

### Match Major Road AADT onto junctions
junc_majmi <- st_join(junc_majmi, lines_major[ ,"traffic_flow"])
junc_majmi <- junc_majmi[!duplicated(junc_majmi$geom), ]

tm2 <- tm_shape(bound)+
  tm_fill(col = "grey",alpha = 0.7) +
  tm_borders(col = "black") +
  tm_shape(lines) +
  tm_lines(col = "road_type", lwd = 3) +
  tm_shape(junc_majmi) +
  tm_dots(col = "traffic_flow", shape = 21, size = 1, legend.show = TRUE) +
  tm_layout(scale(3.5),
            inner.margins =  c(0.05, 0.15 ,0.05, 0.10),
            legend.position = c(0.05, 0.05), 
            legend.width = 5,
            legend.height = 5,
            legend.text.size = 1.3,
            legend.title.size = 2
  ) +
  tm_compass(size = 8,
             position =c(0.82, 0.85)) 

tmap_save(tm2, filename = "Plot_Leeds_2018/02_major_junction_aadt.png", 
          units = "cm", width = 42, height = 29, dpi = 600)


st_write(lines_minor, "Data_Leeds_2018/02_lines_minor.gpkg", delete_dsn = TRUE)
st_write(lines_major, "Data_Leeds_2018/02_lines_major.gpkg", delete_dsn = TRUE)
st_write(junc_majmi, "Data_Leeds_2018/02_junctions.gpkg", delete_dsn = TRUE)
