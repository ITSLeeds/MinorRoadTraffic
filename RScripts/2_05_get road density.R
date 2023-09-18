# Aim: To calculate road density

### Clear memory
rm(list = ls())


### Load Packages 
# Process spatial objects
library(sf)
# Map view
library(tmap)
# Data frame manipulation
library(dplyr)
# Split roads
library(stplanr)

tmap_mode("view")


### Load Data 
lsoa_Leeds <- st_read("Data_Leeds_2018/00_LSOA_Leeds.gpkg")
lines_major <- st_read("Data_Leeds_2018/02_lines_major.gpkg")
lines_minor <- st_read("Data_Leeds_2018/04_lines_minor.gpkg")
bound<-st_read("Data_Leeds_2018/00_bound_buf.gpkg")


###############################################
### Find the lsoa within the bound          ###
###############################################

bound_cut <- st_cast(bound$geom, "LINESTRING") 
lines_minor <- st_transform(lines_minor, 27700)

### split the lsoa by the bound
lsoa_Leeds <- lwgeom::st_split(lsoa_Leeds, bound_cut)
lsoa_Leeds <- st_collection_extract(lsoa_Leeds) 
lsoa_Leeds <- st_as_sf(lsoa_Leeds)
                      
### filter the lsoa that are within the bound
lsoa_cent <- st_centroid(lsoa_Leeds)
cent_inter <- as.data.frame(st_intersects(bound, lsoa_cent))
lsoa_Leeds <- lsoa_Leeds[cent_inter$col.id, ]
summary(duplicated(lsoa_Leeds$geo_code))
                            # to check if there are some lsoa with more than one zone
lsoa_Leeds_dup <-lsoa_Leeds[duplicated(lsoa_Leeds$geo_code), ]
lsoa_Leeds <- lsoa_Leeds[!duplicated(lsoa_Leeds$geo_code), ]


for (i in 1:nrow(lsoa_Leeds_dup)){
  geo_code <- lsoa_Leeds_dup$geo_code[i]
  for (k in 1:nrow(lsoa_Leeds)){
    if (lsoa_Leeds$geo_code[k] == lsoa_Leeds_dup$geo_code[i]){
      lsoa_Leeds$geom[k] <- st_union(lsoa_Leeds$geom[k], lsoa_Leeds_dup$geom[i])
       #qtm(lsoa_Leeds$geom[k])
      }
   }
}


qtm(bound) + qtm(lsoa_Leeds, fill = "geo_code")
rm(bound_cut, lsoa_cent, cent_inter, lsoa_Leeds_dup)

st_write(lsoa_Leeds, "Data_Leeds_2018/05_lsoa_Leeds.gpkg", delete_dsn = TRUE)


#############################################
### Calculate road density                ###
#############################################
lsoa_Leeds$id <-1:nrow(lsoa_Leeds)
density_lsoa <- list()
for(i in lsoa_Leeds$id){
  message(paste0("Doing lsoa ",i))
  lsoa_sub <- lsoa_Leeds[i,]
  lines_sub <- st_intersection(lsoa_sub, lines_minor)

  #qtm(bound) + qtm(lsoa_sub,fill = NULL) + qtm(lines_sub, lines.col = "blue")
  
  road_length <- sum(as.numeric(st_length(lines_sub))) / 1000
  lsoa_area <- as.numeric(st_area(lsoa_sub)%>% units::set_units(km^2))
  
  res <- data.frame(id = lsoa_sub$id,
                    road_km = road_length,
                    area_km2 = lsoa_area,
                    road_density = road_length / lsoa_area)
  
  density_lsoa[[i]] <- res
}

density_lsoa <- bind_rows(density_lsoa)
summary(is.na(density_lsoa$road_density))
summary(unique(density_lsoa$id) %in% unique(lsoa_Leeds$id))
lsoa_Leeds <- left_join(lsoa_Leeds, density_lsoa, by = c("id" = "id"))

st_write(lsoa_Leeds, "Data_Leeds_2018/05_density_lsoa.gpkg", delete_dsn = TRUE)

tm1 <- tm_shape(lsoa_Leeds) + 
  tm_fill(col = "road_density") +
  tm_borders(col = "black",lwd = 1) + 
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

tmap_save(tm1, filename = "Plot_Leeds_2018/05_lsoa_road_density.png",
          units = "cm", width = 42, height = 29, dpi = 600)


#########################################################
### Allocate road density to the minor road           ###
#########################################################

#lines_cut <- st_cast(lsoa_Leeds$geom, "LINESTRING") 
#lines_minor.sub <- lines_minor[lines_cut, ,op = st_intersects]
#lines_break <- lines_minor.sub$geom
#lsoa_break <- lsoa_Leeds$geom
#lines_minor.break <- line_breakup(lines_break, lsoa_break)
#lines_minor.sub <- lwgeom::st_split(lines_minor.sub, lines_cut) 
#lines_minor.sub <- st_collection_extract(lines_minor.sub)
#lsoa_Leeds <- st_as_sf(lsoa_Leeds)


line_segment <- function(l, n_segments, segment_length = NA) {
  if (!is.na(segment_length)) {
    l_length <- as.numeric(sf::st_length(l))
    n_segments <- ceiling(l_length / segment_length)
  }
  # browser() # tests
  # first_linestring = lwgeom::st_linesubstring(x = l, from = 0, to = 0.2)
  from_to_sequence = seq(from = 0, to = 1, length.out = n_segments + 1)
  line_segment_list = lapply(seq(n_segments), function(i)
    lwgeom::st_linesubstring(
      x = l,
      from = from_to_sequence[i],
      to = from_to_sequence[i + 1]
    )
  )
  do.call(rbind, line_segment_list)
}


lines_minor$id <- 1:nrow(lines_minor)
lines_minor <- lines_minor %>%
  group_by(id) %>%
  group_split()


library(pbapply)

cl <- parallel::makeCluster(7)
parallel::clusterExport(cl, "line_segment", envir = environment())
lines_minor2 <- pblapply(lines_minor, function(x){
  line_segment(x, segment_length = 1000)
}, cl = cl)
parallel::stopCluster(cl)

lines_minor3 <- bind_rows(lines_minor2)

write_sf(lines_minor3, "Data_Leeds_2018/05_minor_roads_split_1km.gpkg")

# t1 <- Sys.time()
# lines_minor2 <- lapply(lines_minor[1:100], function(x){
#   line_segment(x, segment_length = 1000)
# })
# t2 <- Sys.time()
# difftime(t2, t1)

lines_minor3$id <- 1:nrow(lines_minor3)

#foo <- st_coordinates(lines_minor3)
#foo <- as.data.frame(foo)
#foo2 <- foo %>% 
#  group_by(L1) %>%
#  summarise(X = X[round(n()/2)],
#            Y = Y[round(n()/2)])
#midpo_minor <- st_as_sf(foo2, coords = c("X","Y"), crs = 27700)
#st_crs(midpo_minor) <- 27700

# Assign the road density of LSOA level to the midpoints of the minor roads 
midpo_minor <- line_midpoint(lines_minor3) %>% st_as_sf()
midpo_minor$id <-  1:nrow(midpo_minor)
st_write(midpo_minor, "Data_Leeds_2018/05_midpo_minor.gpkg", delete_dsn = TRUE)

midpo_minor <- st_join(midpo_minor, lsoa_Leeds[,c("geo_code","area_km2","road_km", "road_density")])

                       
qtm(lines_minor3[1:1000, ], lines.col = "blue") + 
  qtm(midpo_minor[1:1000, ])


# check how many minor roads cannot get data and fix them
summary(is.na(midpo_minor$road_density)) # all minor midpoint got data
#midpo_minor_sub <- midpo_minor[is.na(midpo_minor$road_density), c("id")]
#midpo_minor <- midpo_minor[!is.na(midpo_minor$road_density), ]
#midpo_minor_subbuf <- st_buffer(midpo_minor_sub, 8)
                              # the distance depends on the real situation
#midpo_minor_sub <- st_join(midpo_minor_subbuf, 
#                           lsoa_Leeds[,c("geo_code","area_km2","road_km", "road_density")])

#midpo_minor <- rbind(midpo_minor, midpo_minor_sub)



# Assign the road density to the minor road
lines_minor4 <- left_join(lines_minor3, st_drop_geometry(midpo_minor), by = "id")

summary(is.na(lines_minor4$road_density)) 


tm2 <- tm_shape(bound) +
  tm_polygons(col = "grey", alpha = 0.8, border.col = "black") +
  tm_legend()+
  tm_shape(lines_minor4) +
  tm_lines(col = "road_density", lwd = 3) +
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

tmap_save(tm2, filename = "Plot_Leeds_2018/05_road_density.png",
          units = "cm", width = 42, height = 29, dpi = 600)

tm3 <- tmap_arrange(tm1, tm2)
tmap_save(tm3, filename = "Plot_Leeds_2018/05_road_density_lsoa2minor.png",
          units = "cm", width = 42, height = 29, dpi = 600)

st_write(lines_minor4, "Data_Leeds_2018/05_lines_minor.gpkg", delete_dsn = TRUE)
