# Aim: To get demographic and economical data

### Clear memory
rm(list = ls())

### Load Packages 
# Process spatial objects
library(sf)
# Map view
library(tmap)

tmap_mode("view")

### Load Data 
demo_eco <- readRDS("Data_Leeds_2018/00_demo_eco.RDS")
bound<-st_read("Data_Leeds_2018/00_bound_buf.gpkg")
lines_major <- st_read("Data_Leeds_2018/02_lines_major.gpkg")
midpo_minor <- st_read("Data_Leeds_2018/05_midpo_minor.gpkg")
lines_minor <- st_read("Data_Leeds_2018/05_lines_minor.gpkg")
lsoa_Leeds <- st_read("Data_Leeds_2018/05_lsoa_Leeds.gpkg")

### Get the original area of each LSOA
demo_eco$total_km2 <- as.numeric(st_area(demo_eco)%>% units::set_units(km^2))

### Deal with the outliners in car ownership
demo_eco$cars_percap_2018[demo_eco$geo_code == "E01011325"] <- 0.489
demo_eco$cars_percap_2018[demo_eco$geo_code == "E01011363"] <- 0.489

### Allocate the LSOA data to the minor road  
midpo_minor <- st_join(midpo_minor, demo_eco[,c("geo_label", "cars_percap_2018", 
                                                "pop_2018", "Employment", "total_km2")])

# Check how many minor centroids cannot get data and fix them
summary(is.na(midpo_minor$cars_percap_2018))
                   # all centroid of minor road located in the lsoa area
#midpo_minor_sub <- midpo_minor[is.na(midpo_minor$cars_percap_2018), c("id")]
#midpo_minor <- midpo_minor[!is.na(midpo_minor$cars_percap_2018), ]
#midpo_minor_subbuf <- st_buffer(midpo_minor_sub, 8)
#midpo_minor_sub <- st_join(midpo_minor_subbuf, 
#                           demo_eco[,c("geo_label", "cars_percap_2018",
#                                       "pop_2018","Employment", "total_km2")])

#midpo_minor <- rbind(midpo_minor, midpo_minor_sub)


# Assign the data to the minor road
lines_minor$geo_label <- midpo_minor$geo_label[match(lines_minor$id,
                                                     midpo_minor$id)]
lines_minor$total_km2 <- midpo_minor$total_km2[match(lines_minor$id,
                                                     midpo_minor$id)]
lines_minor$cars_2018 <- midpo_minor$cars_percap_2018[match(lines_minor$id,
                                                            midpo_minor$id)]
lines_minor$pop_2018 <- midpo_minor$pop_2018[match(lines_minor$id,
                                                   midpo_minor$id)]
lines_minor$employ_2018 <- midpo_minor$Employment[match(lines_minor$id,
                                                       midpo_minor$id)]

# Check if there are some LSOA that don't have employment data and replace with 0
summary(is.na(lines_minor$employ_2018))
summary(is.na(demo_eco$Employment))
#summary(unique(lines_minor$geo_code[is.na(lines_minor$employ_2018)]) 
#        %in% unique(demo_eco$geo_code[is.na(demo_eco$Employment)]))
#lines_minor$employ_2018[is.na(lines_minor$employ_2018)] <- 0


### Calculate the data for each minor road
lines_minor$sub_road_km <- as.numeric(st_length(lines_minor)) / 1000
head(lines_minor)

lines_minor$km_percent <-lines_minor$sub_road_km / lines_minor$road_km
lines_minor$area_percent <- lines_minor$area_km2 / lines_minor$total_km2 

# Based on the assumption that 1km road length serve certain amount of pop and employ
lines_minor$pop <- lines_minor$km_percent * lines_minor$pop_2018 * lines_minor$area_percent
lines_minor$employ <- lines_minor$km_percent * lines_minor$employ_2018 * lines_minor$area_percent
lines_minor$cars <- lines_minor$pop * lines_minor$cars_2018
            

# Double check if the final data is correct
tm1 <- qtm(demo_eco, fill = "pop_2018") 
tm2 <- qtm(lines_minor, lines.col = "pop_2018")
tmap_arrange(tm1,tm2)


### Map the results
lsoa_Leeds$pop_2018 <- demo_eco$pop_2018[match(lsoa_Leeds$geo_code,
                                              demo_eco$geo_code)]
lsoa_Leeds$employ_2018 <- demo_eco$Employment[match(lsoa_Leeds$geo_code,
                                                   demo_eco$geo_code)]
lsoa_Leeds$employ_2018[is.na(lsoa_Leeds$employ_2018)] <- 0
lsoa_Leeds$cars_2018 <- demo_eco$cars_percap_2018[match(lsoa_Leeds$geo_code,
                                                        demo_eco$geo_code)]
# for population
tm3 <- tm_shape(lsoa_Leeds) +
  tm_polygons(col = "pop_2018", alpha = 0.8, border.col = "black") +
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
tmap_save(tm3, filename = "Plot_Leeds_2018/06_pop_lsoa.png", 
          units = "cm", width = 42, height = 29, dpi = 600)

tm4 <- tm_shape(bound) +
  tm_polygons(col = "grey", alpha = 0.8, border.col = "black") +
  tm_legend()+
  tm_shape(lines_minor) +
  tm_lines(col = "pop", lwd = 3, style = "fisher") +
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

tmap_save(tm4, filename = "Plot_Leeds_2018/06_pop_minor.png", 
          units = "cm", width = 42, height = 29, dpi = 600)

tm5 <-tmap_arrange(tm3,tm4)
tmap_save(tm5, filename = "Plot_Leeds_2018/06_pop_lsoa2minor.png", 
          units = "cm", width = 42, height = 29, dpi = 600)

# for employment 
tm6 <- tm_shape(lsoa_Leeds) +
  tm_polygons(col = "employ_2018", alpha = 0.8, border.col = "black") +
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
tmap_save(tm6, filename = "Plot_Leeds_2018/06_employ_lsoa.png", 
          units = "cm", width = 42, height = 29, dpi = 600)

tm7 <- tm_shape(bound) +
  tm_polygons(col = "grey", alpha = 0.8, border.col = "black") +
  tm_legend()+
  tm_shape(lines_minor) +
  tm_lines(col = "employ", lwd = 3, style = "fisher") +
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

tmap_save(tm7, filename = "Plot_Leeds_2018/06_employ_minor.png", 
          units = "cm", width = 42, height = 29, dpi = 600)

tm8 <-tmap_arrange(tm6,tm7)
tmap_save(tm8, filename = "Plot_Leeds_2018/06_employ_lsoa2minor.png", 
          units = "cm", width = 42, height = 29, dpi = 600)

# for carownership
tm9 <- tm_shape(lsoa_Leeds) +
  tm_polygons(col = "cars_2018", alpha = 0.8, border.col = "black") +
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
tmap_save(tm9, filename = "Plot_Leeds_2018/06_cars_lsoa.png", 
          units = "cm", width = 42, height = 29, dpi = 600)

tm10 <- tm_shape(bound) +
  tm_polygons(col = "grey", alpha = 0.8, border.col = "black") +
  tm_legend()+
  tm_shape(lines_minor) +
  tm_lines(col = "cars", lwd = 3, style = "fisher") +
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

tmap_save(tm10, filename = "Plot_Leeds_2018/06_cars_minor.png", 
          units = "cm", width = 42, height = 29, dpi = 600)

tm11 <-tmap_arrange(tm9,tm10)

tmap_save(tm11, filename = "Plot_Leeds_2018/06_cars_lsoa2minor.png", 
          units = "cm", width = 41, height = 29, dpi = 600)

st_write(lsoa_Leeds, "Data_Leeds_2018/06_lsoa_Camb.gpkg", delete_dsn = TRUE)
st_write(lines_minor, "Data_Leeds_2018/06_lines_minor.gpkg", delete_dsn = TRUE)
