# Aim: Descriptive analysis & Multicollinearity and spatial autocorrelation analysis

### Clear memory
rm(list = ls())

### Load Packages
# Data manipulation, transformation and visualization
#library(tidyverse)
# Nice tables
#library(kableExtra)
# Process spatial objects
library(sf)
# Thematic maps
library(ggplot2)
# Map view
library(tmap)
# Obtain correlation coefficients
library(corrplot)
# Assess multicollinearity
library(car)
# Spatial autocorrelation
#library(spdep)       

tmap_mode("view")

### Load data 
lines_minor <- st_read("Data_Leeds_2018/07_lines_minor.gpkg")


### Allocate the county to each minor roads
lines_minor$county <- substr(lines_minor$geo_label, 1, 8)
county_name <- unique(lines_minor$county)
Leeds_name <- c("Leeds 10", "Leeds 07", "Leeds 08", "Leeds 06", "Leeds 09",
                "Leeds 03", "Leeds 01", "Leeds 00", "Leeds 11", "Leeds 02",
                "Leeds 04","Leeds 05")
#full_name <- c("Peterborough", "South Cambridgeshire", "Huntingdonshire", 
#               "East Cambridgeshre", "Cambridage", "Fenland")
lines_minor <- lines_minor[lines_minor$county %in% Leeds_name, ]
#for (i in (1:length(Leeds_name))){
#  sub_name <- Leeds_name[i]
#  for (k in (1:nrow(lines_minor)))
#    if (lines_minor$county[k] == sub_name){
#      lines_minor$county[k] <- full_name[i]
#    }
#}


### Subset test dataset
lines_minor_test <- lines_minor[!is.na(lines_minor$traffic_flow), ]
summary(is.na(lines_minor_test))


### Descriptive analysis
summary <- list()
for (i in (1:length(Leeds_name))){
  county <- Leeds_name[i]
  sub_test <- lines_minor_test[lines_minor_test$county == county, ]
  n <- nrow(sub_test)
  min <- min(sub_test$traffic_flow)
  median <- median(sub_test$traffic_flow)
  mean <- mean(sub_test$traffic_flow)
  max <- max(sub_test$traffic_flow)
  sd <- sd(sub_test$traffic_flow)
  res <- data.frame(county = county,
                    traffic_count = n,
                    minimum_flow = min,
                    median_flow = median,
                    mean_flow = mean,
                    maximum_flow <- max,
                    standard_deviation <- sd)
  summary[[i]] <- res
}
summary <- bind_rows(summary)

write.csv(summary, "Data_Leeds_2018/08_summary.csv")


# Make the box plot
gg1 <- ggplot(lines_minor_test, aes(x = county, y = traffic_flow)) + 
  geom_boxplot(width = 0.7)

gg1


# Make the counts histogram
gg2 <- ggplot() +
  geom_histogram(aes(lines_minor_test$traffic_flow), bins = 20,
                 col = "white")

gg2


### obtain a matrix of pearson correlation coefficients
head(lines_minor_test)
col_name <- c("std_centrality", "nearest_junc_dist", "major_flow", 
              "road_density","cars_2018", "pop_2018", "employ_2018", "cars", "pop",
              "employ", "traffic_flow")
minor_cortest <- lines_minor_test[ ,col_name] 
minor_cortest <- st_set_geometry(minor_cortest, NULL)
cormat <- cor(minor_cortest, use = "complete.obs", method = "pearson")

# significance test
sig1 <- corrplot::cor.mtest(minor_cortest, conf.level = .95,) 

# create a correlogram
cor2<- corrplot::corrplot(cormat, type = "lower", 
                   method = "circle",
                   order = "original",
                   addCoef.col="black",
                   number.cex = 0.7,
                   tl.cex = 0.7,
                   cl.cex = 0.7,
                   p.mat = sig1$p, sig.level = 0.05,
                   pch.col = "black",
                   #pch.cex = 2,
                   #col = viridis::viridis(100, option = "plasma"),
                   diag = FALSE)

st_write(lines_minor, "Data_Leeds_2018/08_lines_minor.gpkg", delete_dsn = TRUE)
st_write(lines_minor_test, "Data_Leeds_2018/08_lines_minor_test.gpkg", delete_dsn = TRUE)

