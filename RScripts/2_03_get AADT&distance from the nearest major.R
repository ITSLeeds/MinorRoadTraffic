# Aim: To calculate AADT for minor roads
# This script assumes the data has already been cleaned and the AADT for 
# major roads assigned
# Rework of original file dropping the splitting an buffering stage

### Clear memory
rm(list = ls())

### Load Packages -----------------------------------------------------------
# Process spatial objects
library(sf)
# Make dodgr graph
library(dodgr)
# Map view
library(tmap)
# Data frame manipulation
library(dplyr)

tmap_mode("view")

### Load Data ---------------------------------------------------------------

bound<-st_read("Data_Leeds_2018/00_bound_buf.gpkg")
lines_minor <- st_read("Data_Leeds_2018/02_lines_minor.gpkg")
lines_major <- st_read("Data_Leeds_2018/02_lines_major.gpkg")
lines <- st_read("Data_Leeds_2018/01_network.gpkg")
junc_majmi <- st_read("Data_Leeds_2018/02_junctions.gpkg")

# ################################################################# #
####                Assign AADT to minor road                    ####
# ################################################################# #

### Convert to 4326 for dodgr
lines_major <- st_transform(lines_major, 4326) 
lines_minor <- st_transform(lines_minor, 4326)
junc_majmi <- st_transform(junc_majmi, 4326)

# Remove tiny sub-graphs - these are too small to consider and often are disconnected from the road network
pregrap <- rbind(lines_minor,lines_major[names(lines_minor)])
pregrap <- weight_streetnet(pregrap, wt_profile = "motorcar")
pregrap_tab <- as.data.frame(table(pregrap$component))
pregrap_tab <- pregrap_tab[pregrap_tab$Freq != max(pregrap_tab$Freq),]
pregrap <- pregrap[pregrap$component %in% pregrap_tab$Var1,]

lines_minor <- lines_minor[!lines_minor$osm_id %in% pregrap$way_id, ]

### Get mid-point of minor roads, i.e. centroid on the line
point_minor <- as.data.frame(st_coordinates(lines_minor))
point_minor <- group_by(point_minor, L1) %>% # Based on L1 to group the junc_minor
  summarise(X = X[round(n()/2)],
            Y = Y[round(n()/2)]) 
# use the n/2 item as the group represent

### Make dodgr graph of minor roads
graph <- weight_streetnet(lines_minor, wt_profile = "motorcar")
                             # use motorcar mode to weight the minor road

graph_ids <- graph[ ,c("from_id","from_lon","from_lat","component")]
                 # focus on the certain start point rather than the end points
graph_ids <- unique(graph_ids)  

junc_majmi <- cbind(junc_majmi, st_coordinates(junc_majmi))
junc_majmi <- left_join(junc_majmi, graph_ids, by = c("X" = "from_lon",
                                                      "Y" = "from_lat"))
                            # link the start point which is the majmi junction to the data.frame
point_minor <- left_join(point_minor, graph_ids, by = c("X" = "from_lon",
                                                        "Y" = "from_lat"))
                            # link the start point which is the minor junction to the data.frame


### For each minor road , find the nearest (in time) junction
dists <- dodgr_times(graph,
                     from = junc_majmi$from_id,
                     to = point_minor$from_id,
                     shortest = FALSE)
notna <- colSums(!is.na(dists))
notna <- as.data.frame(notna)
summary(notna$notna==0)
         #The result shows 26 minor roads don't acquire any value

saveRDS(dists,"Data_Leeds_2018/03_dists_matrix.Rds")

# foo = point_minor[notna$notna==0,]
# foo = st_as_sf(foo, coords = c("X","Y"), crs = 4326)
# qtm(foo) + qtm(junc_majmi, dots.col = "red") + qtm(bound)

nearest_junc <- list()
dist_junc <- list()
for(i in 1:ncol(dists)){
  sub <- dists[,i]
  sub <- sub[!is.na(sub)]
  if(length(sub) == 0){
    nearest_junc[[i]] <- NA
    dist_junc[[i]] <- NA
  } else {
    min_name <- names(sub)[sub == min(sub, na.rm = TRUE)]
    min_dist <- min(sub, na.rm = TRUE)
            # find the minimum value and return the name of the minimum value
    if(length(min_name) == 0){
      min_name <- NA
      min_dist <- NA
            # in case that the name of the junction lost
    }
    if(length(min_name) > 1){ 
      min_name <- min_name[1]
      min_dist <- min_dist[1] 
            # consider the condition that have more than one same min value
    }
    nearest_junc[[i]] <- min_name
    dist_junc[[i]] <- min_dist
  }
  
}
nearest_junc <- unlist(nearest_junc)
dist_junc <- unlist(dist_junc)

lines_minor$nearest_junc <- nearest_junc
lines_minor$nearest_junc_dist <- dist_junc
lines_minor$major_flow <- junc_majmi$traffic_flow[match(lines_minor$nearest_junc,
                                                junc_majmi$from_id)]

# plot each road colored by the AADT on the nearest (in time) major road

lines_minor <- st_transform(lines_minor, 27700)
lines_minor <- lines_minor[!is.na(lines_minor$major_flow), ]

tm1 <- tm_shape(bound) +
  tm_polygons(col = "grey", alpha = 0.8, border.col = "black") +
  tm_legend()+
  tm_shape(lines_minor) +
  tm_lines(col = "major_flow", lwd = 3, style = "fisher") +
  tm_shape(lines_major) +
  tm_lines(col = "black") +
  tm_layout(scale(3.5),
            inner.margins =  c(0.05, 0.05 ,0.05, 0.05),
            legend.position = c(0.05, 0.05), 
            legend.width = 5,
            legend.height = 5,
            legend.text.size = 1.3,
            legend.title.size = 2
  ) +
  tm_compass(size = 8,
             position =c(0.82, 0.85)) 

tmap_save(tm1, filename = "Plot_Leeds_2018/03_major_flow.png", 
          units = "cm", width = 42, height = 29, dpi = 600)


tm2 <-tm_shape(bound) +
  tm_polygons(col = "grey", alpha = 0.8, border.col = "black") +
  tm_legend()+
  tm_shape(lines_minor) +
  tm_lines(col = "nearest_junc_dist", lwd = 3, style = "fisher") +
  tm_shape(lines_major) +
  tm_lines(col = "black") +
  tm_layout(scale(3.5),
            inner.margins =  c(0.05, 0.05 ,0.05, 0.05),
            legend.position = c(0.05, 0.05), 
            legend.width = 5,
            legend.height = 5,
            legend.text.size = 1.3,
            legend.title.size = 2
  ) +
  tm_compass(size = 8,
             position =c(0.82, 0.85)) 

tmap_save(tm2, filename = "Plot_Leeds_2018/03_major_distance.png", 
          units = "cm", width = 42, height = 29, dpi = 600)


tm3 <-tmap_arrange(tm1,tm2)

tmap_save(tm3, filename = "Plot_Leeds_2018/03_major_traffic&dist.png", 
          units = "cm", width = 41, height = 29, dpi = 600)


st_write(lines_minor, "Data_Leeds_2018/03_lines_minor.gpkg", delete_dsn = TRUE)
st_write(point_minor, "Data_Leeds_2018/03_point_minor.gpkg", delete_dsn = TRUE)
st_write(junc_majmi, "Data_Leeds_2018/03_junc_majmi.gpkg", delete_dsn = TRUE)


