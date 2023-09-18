# Aim: To calculate road centrality
# Rework of original file dropping the splitting an buffering stage

### Clear memory
rm(list = ls())

### Load Packages 
# Process spatial objects
library(sf)
# Make dodgr graph
library(dodgr)
# Data frame manipulation
library(dplyr)
# Map view
library(tmap)

tmap_mode("view")

### Load Data 
bound<-st_read("Data_Leeds_2018/00_bound_buf.gpkg")
lines_major <- st_read("Data_Leeds_2018/02_lines_major.gpkg")
lines_minor <- st_read("Data_Leeds_2018/03_lines_minor.gpkg")

# ################################################################# #
####   Make subgraphs                                            ####
# ################################################################# #

road_cut <- st_cast(lines_major$geom, "LINESTRING") 
                            # used for the bound for the subgraphs
minor_points <- st_cast(lines_minor$geom, "POINT")
minor_points <- st_transform(minor_points, 27700)


### genarete the zones split by major road
zones <- lwgeom::st_split(bound, road_cut)
                            # use major road to cut the study bound
zones <- st_collection_extract(zones)
                            # extract the zones as a data frame
zones <- st_as_sf(zones)

zones_inter <- st_contains_properly(zones, minor_points)
                  # count the number of the minor road points that within subzones 
zones$npoints <- lengths(zones_inter)
zones <- zones[zones$npoints > 5, ] 
                  # use 5 as the threshold to filter the analyse zones
colSums(st_drop_geometry(zones))
                  # the result is 149879, due to filter the zones with less than 5 points
zones$id <- 1:nrow(zones)

tm1 <- qtm(zones, fill = "id")  # plot the zones created
tmap_save(tm1, filename = "Plot_Leeds_2018/04_zones.png",)

### Free up memory
rm(road_cut, zones_inter, all_points )
gc()


# ################################################################# #
####   Calculate road centrality                                 ####
# ################################################################# #

### prepare the crs for dodgr
zones <- st_transform(zones, 4326)
#midpo_minor <- st_as_sf(midpo_minor, coords = c("X","Y"), crs = 4326,
                        #remove = FALSE)
lines_minor <- st_transform(lines_minor, 4326)
minor_points <- st_transform(minor_points,4326)

### Loop over each zone and find the centrality of the minor road 
graphs <- list()

for(i in zones$id){
  message(paste0("Doing Zone ",i))
  zone_sub <- zones[zones$id == i, ]
  zone_sub <- st_transform(st_buffer(st_transform(zone_sub, 27700), 0.0001), 4326)
  lines_sub <- lines_minor[zone_sub, , op = st_within]
  #lines_sub2 <- st_intersection(zone_sub, lines_minor)
  #midpo_sub <- midpo_minor[zone_sub, , op = st_within]
  #summary(nrow(lines_sub) == nrow(midpo_sub))
    #qtm(zone_sub, fill = NULL) + qtm(lines_sub, lines.col = "major_flow") 
  if(nrow(lines_sub) > 0){
    graph_sub <- weight_streetnet(lines_sub, wt_profile = "motorcar")
    graph_sub <- dodgr_centrality(graph_sub)
    graph_sub <- merge_directed_graph(graph_sub)
    clear_dodgr_cache()
    graph_sub <- dodgr_to_sf(graph_sub)
    graph_sub$std_centrality <- graph_sub$centrality / zone_sub$npoints
    #summary(unique(graph_sub$way_id) %in% unique(lines_sub$osm_id))
    #summary(duplicated(graph_sub$way_id))
    #print(summary(graph_sub$centrality))
    graphs[[i]] <- graph_sub
  }


}

saveRDS(graphs,"Data_Leeds_2018/04-graphs.RDS")
graphs <- do.call(rbind, graphs)
#graphs <- bind_rows(graphs)


# Plot the centrality of all minor roads
summary(!is.na(graphs$centrality))

tm2 <- qtm(bound) +
  qtm(lines_minor, line.col = "black" )+
  tm_shape(graphs) +
  tm_lines(col = "std_centrality", lwd = 2, style = "fisher") + 
  qtm(lines_major, line.col = "ref" )
tmap_save(tm2, filename = "Plot_Leeds_2018/04_centrality.png")

# ################################################################# #
####   Match Up centrality with minor road                       ####
# ################################################################# #

summary(unique(graphs$way_id) %in% unique(lines_minor$osm_id))
                      # check if all the minor road get road centrality
summary(unique(lines_minor$osm_id) %in% unique(graphs$way_id))
                      # 2645 minor roads cannot get centrality
summary(duplicated(graphs$way_id)) 
                      # some osm_ids have been split

lines_minor <- left_join(graphs[,c("way_id" ,"centrality", "std_centrality")],
                       st_drop_geometry(lines_minor[ ,c("osm_id", "highway", 
                                                        "nearest_junc_dist", 
                                                        "major_flow")]),
                       by = c("way_id" = "osm_id"))

tm3 <-tm_shape(bound) +
  tm_polygons(col = "grey", alpha = 0.8, border.col = "black") +
  tm_legend()+
  tm_shape(lines_minor) +
  tm_lines(col = "std_centrality", lwd = 3, style = "fisher") +
  tm_shape(lines_major) +
  tm_lines(col = "black") +
  tm_layout(scale(3.5),
            inner.margins =  c(0.05, 0.15 ,0.05, 0.15),
            legend.position = c(0.10, 0.05), 
            legend.width = 5,
            legend.height = 5,
            legend.text.size = 1.3,
            legend.title.size = 2
  ) +
  tm_compass(size = 8,
             position =c(0.82, 0.85)) 

tmap_save(tm3, filename = "Plot_Leeds_2018/04_std_centrality.png", 
          units = "cm", width = 42, height = 29, dpi = 600)

        
st_write(lines_minor, "Data_Leeds_2018/04_lines_minor.gpkg", delete_dsn = TRUE)





