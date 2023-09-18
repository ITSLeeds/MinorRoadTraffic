# Aim: Modelling
# This script assumes the data has already been cleaned and the AADT for major roads assigned
# Rework of original file dropping the splitting an buffering stage

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
# Rich color palatte
library(tmaptools) 
# Obtain correlation coefficients
library(corrplot)
# Assess multicollinearity
library(car)
# Make centroid of roads
library(stplanr)
# Make GWR model
library(GWmodel)
# Make GWR model
library(spgwr)

tmap_mode("view")


### Load and prepare data 
bound<-st_read("Data_Leeds_2018/00_bound_buf.gpkg")
lines_major <- st_read("Data_Leeds_2018/02_lines_major.gpkg")
lines_minor_test <- st_read("Data_Leeds_2018/08_lines_minor_test.gpkg")
lines_minor <- st_read("Data_Leeds_2018/08_lines_minor.gpkg")

# Logarithmic variables of test data 
#lines_minor_test$log_centrality <- log(lines_minor_test$centrality)
lines_minor_test$log_std_centrality <- log(lines_minor_test$std_centrality)
lines_minor_test$log_nearest_junc_dist <- log(lines_minor_test$nearest_junc_dist)
lines_minor_test$log_major_flow <- log(lines_minor_test$major_flow)
lines_minor_test$log_traffic_flow <- log(lines_minor_test$traffic_flow)

#summary(lines_minor_test$log_centrality)
summary(lines_minor_test$log_std_centrality)
summary(lines_minor_test$log_nearest_junc_dist)
summary(lines_minor_test$log_major_flow)

# Logarithmic variables of predict data 
#lines_minor$log_centrality <- log(lines_minor$centrality)
lines_minor$log_std_centrality <- log(lines_minor$std_centrality)
lines_minor$log_nearest_junc_dist <- log(lines_minor$nearest_junc_dist)
lines_minor$log_major_flow <- log(lines_minor$major_flow)

#summary(lines_minor$log_centrality)
summary(lines_minor$log_std_centrality)
summary(lines_minor$log_nearest_junc_dist)
summary(lines_minor$log_major_flow)

lines_minor_sub <- lines_minor[lines_minor$log_nearest_junc_dist=="-Inf",]
lines_minor <- lines_minor[lines_minor$log_nearest_junc_dist!="-Inf",]
lines_minor_sub$log_nearest_junc_dist <- 0
lines_minor <- rbind(lines_minor, lines_minor_sub)
rm(lines_minor_sub)

######################################################################
### Modelling                                                      ###
######################################################################
### Prepare the data for GWPR model
# Transform to the centroid
cents_test <- st_centroid(lines_minor_test)
cents_pred <- st_centroid(lines_minor)


# Determine the distance between observations
cents_test_sp <- as(cents_test, "Spatial")
cents_pred_sp <- as(cents_pred, "Spatial")

DM<-gw.dist(dp.locat = st_coordinates(cents_test))


### Modelling with basic variable combination 
# Determine the equation
eq1 <- traffic_flow ~ std_centrality + nearest_junc_dist + major_flow

#Fit GLM
glm1 <- glm(eq1, cents_test,family = "poisson")
summary(glm1)
glm1_results <-data.frame(1:102)
glm1_results$y <- cents_test$traffic_flow 
glm1_results$yhat <- exp(predict(glm1, newdata = cents_test))
glm1_results$residual <- glm1_results$y-glm1_results$yhat
RMSE_GLM1 <- sqrt(sum((glm1_results$residual)^2)/nrow(glm1_results))

# Determine the adaptive bandwidth
abw <- bw.ggwr(eq1,
               data = cents_test_sp,
               family = "poisson",
               approach = "AICc",
               kernel = "bisquare", 
               adaptive = TRUE,
               dMat = DM)
# Fit GWPR
gwpr1 <- ggwr.basic(eq1, 
                    data = cents_test_sp, 
                    family = "poisson",
                    bw = abw,
                    kernel = "gaussian",
                    adaptive = TRUE,
                    dMat = DM)

gwpr1

# save the results
capture.output(print(gwpr1), file = "Result_Leeds_2018/summary_gwpr1.doc")

### Modelling with variation of basic variable combination 
eq1a <- traffic_flow ~ log_std_centrality + log_nearest_junc_dist + log_major_flow

#Fit GLM
glm1a <-glm(formula = eq1a, data = lines_minor_test, family = "poisson")
summary.glm(glm1a)
glm1a_results <-data.frame(1:102)
glm1a_results$y <- cents_test$traffic_flow 
glm1a_results$yhat <- exp(predict(glm1a, newdata = cents_test))
glm1a_results$residual <- glm1a_results$y-glm1a_results$yhat
RMSE_GLM1a <- sqrt(sum((glm1a_results$residual)^2)/nrow(glm1a_results))

#Fit GWPR
gwpr1a <- ggwr.basic(eq1a, 
                     data = cents_test_sp, 
                     family = "poisson",
                     bw = abw,
                     kernel = "gaussian",
                     adaptive = TRUE,
                     dMat = DM)

gwpr1a
gwpr1a_results <- sf::st_as_sf(gwpr1a$SDF)
RMSE_GWPR1a <- sqrt(sum((gwpr1a_results$residual)^2)/nrow(gwpr1a_results))


# save the results
capture.output(print(gwpr1a), file = "Result_Leeds_2018/summary_gwpr1a.doc")


### Modelling with variable combination 2
eq2 <- traffic_flow ~ log_std_centrality + log_nearest_junc_dist + log_major_flow +
  road_density

#Fit GLM
glm2 <-glm(formula = eq2, data = lines_minor_test, family = "poisson")
summary.glm(glm2)
glm2_results <-data.frame(1:102)
glm2_results$y <- cents_test$traffic_flow 
glm2_results$yhat <- exp(predict(glm2, newdata = cents_test))
glm2_results$residual <- glm2_results$y-glm2_results$yhat
RMSE_GLM2 <- sqrt(sum((glm2_results$residual)^2)/nrow(glm2_results))

#Fit GWPR
gwpr2 <- ggwr.basic(eq2, 
                    data = cents_test_sp, 
                    family = "poisson",
                    bw = abw,
                    kernel = "gaussian",
                    adaptive = TRUE,
                    dMat = DM)

gwpr2
gwpr2_results <- sf::st_as_sf(gwpr2$SDF)
RMSE_GWPR2 <- sqrt(sum((gwpr2_results$residual)^2)/nrow(gwpr2_results))

# save the results
capture.output(print(gwpr2), file = "Result_Leeds_2018/summary_gwpr2.doc")


### modelling with variable combination 3
eq3 <- traffic_flow ~ log_std_centrality + log_nearest_junc_dist + log_major_flow +
  pop + employ

#Fit GLM
glm3 <-glm(formula = eq3, data = lines_minor_test, family = "poisson")
summary.glm(glm3)
glm3_results <-data.frame(1:102)
glm3_results$y <- cents_test$traffic_flow 
glm3_results$yhat <- exp(predict(glm3, newdata = cents_test))
glm3_results$residual <- glm3_results$y-glm3_results$yhat
RMSE_GLM3 <- sqrt(sum((glm3_results$residual)^2)/nrow(glm3_results))

#Fit GWPR
gwpr3 <- ggwr.basic(eq3, 
                    data = cents_test_sp, 
                    family = "poisson",
                    bw = abw,
                    kernel = "gaussian",
                    adaptive = TRUE,
                    dMat = DM)

gwpr3
gwpr3_results <- sf::st_as_sf(gwpr3$SDF)
RMSE_GWPR3 <- sqrt(sum((gwpr3_results$residual)^2)/nrow(gwpr3_results))

# save the results
capture.output(print(gwpr3), file = "Result_Leeds_2018/summary_gwpr3.doc")


### modelling with variable combination 4
eq4 <- traffic_flow ~ log_std_centrality + log_nearest_junc_dist + log_major_flow +
  pop + employ +cars_2018

# Fit GLM
glm4 <-glm(formula = eq4, data = lines_minor_test, family = "poisson")
summary.glm(glm4)
glm4_results <-data.frame(1:102)
glm4_results$y <- cents_test$traffic_flow 
glm4_results$yhat <- exp(predict(glm4, newdata = cents_test))
glm4_results$residual <- glm4_results$y-glm4_results$yhat
RMSE_GLM4 <- sqrt(sum((glm4_results$residual)^2)/nrow(glm4_results))

# Fit GWPR
gwpr4 <- ggwr.basic(eq4, 
                    data = cents_test_sp, 
                    family = "poisson",
                    bw = abw,
                    kernel = "gaussian",
                    adaptive = TRUE,
                    dMat = DM)

gwpr4
gwpr4_results <- sf::st_as_sf(gwpr4$SDF)
RMSE_GWPR4 <- sqrt(sum((gwpr4_results$residual)^2)/nrow(gwpr4_results))

# save the results
capture.output(print(gwpr4), file = "Result_Leeds_2018/summary_gwpr4.doc")



##################################################################
### Interpretation of the best fitted model                    ###
##################################################################
### extract the results of GWPR
summary(gwpr4_results$Intercept)
summary(gwpr4_results$Intercept_SE)
summary(gwpr4_results$log_std_centrality)
summary(gwpr4_results$log_std_centrality_SE)
summary(gwpr4_results$log_nearest_junc_dist)
summary(gwpr4_results$log_nearest_junc_dist_SE)
summary(gwpr4_results$log_major_flow)
summary(gwpr4_results$log_major_flow_SE)
summary(gwpr4_results$pop)
summary(gwpr4_results$pop_SE)
summary(gwpr4_results$employ)
summary(gwpr4_results$employ_SE)
summary(gwpr4_results$cars_2018)
summary(gwpr4_results$cars_2018_SE)

rsd = sd(gwpr4_results$residual)
gwpr4_results$stdRes <- (gwpr4_results$residual)/sd(gwpr4_results$residual)
summary(gwpr4_results$stdRes)

# Make map for the undetermined coefficients
basemap <- tm_shape(bound) +
  tm_polygons(col = "grey", alpha = 0.3, border.col = "black") +
  tm_shape(lines_major) +
  tm_lines(col = "black",lwd = 2) +
  tm_shape(lines_minor) +
  tm_lines(col = "black", lwd = 1) 

map_coef1 <-basemap + tm_shape(gwpr4_results) +
  tm_dots(col = "Intercept", size = 1, shape = 21,
          title = expression("Intercept: coef"),
          legend.show = TRUE,
          midpoint = NA) +
  tm_compass(type = "arrow", size = 8, position =c(0.82, 0.85))+
  tm_layout(inner.margins =  c(0.05, 0.05 ,0.05, 0.05),
            legend.position = c(0.02, 0.05),
            legend.text.size = 1.5,
            legend.title.size = 1.8) 
tmap_save(map_coef1, filename = "Plot_Leeds_2018/09_map_coef1.png", 
          units = "cm", width = 42, height = 29, dpi = 300)

map_coef2 <-basemap + tm_shape(gwpr4_results) +
  tm_dots(col = "log_major_flow", size = 1, shape = 21,
          title = expression("log_major_flow: coef"),
          legend.show = TRUE,
          midpoint = NA) +
  tm_compass(type = "arrow", size = 8, position =c(0.82, 0.85))+
  tm_layout(inner.margins =  c(0.05, 0.05 ,0.05, 0.05),
            legend.position = c(0.02, 0.05),
            legend.text.size = 1.5,
            legend.title.size = 1.8) 
tmap_save(map_coef2, filename = "Plot_Leeds_2018/09_map_coef2.png", 
          units = "cm", width = 42, height = 29, dpi = 300)

map_coef3 <-basemap + tm_shape(gwpr4_results) +
  tm_dots(col = "pop", size = 1, shape = 21,
          title = expression("pop: coef"),
          legend.show = TRUE,
          midpoint = NA) +
  tm_compass(type = "arrow", size = 8, position =c(0.82, 0.85))+
  tm_layout(inner.margins =  c(0.05, 0.05 ,0.05, 0.05),
            legend.position = c(0.02, 0.05),
            legend.text.size = 1.5,
            legend.title.size = 1.8) 
tmap_save(map_coef3, filename = "Plot_Leeds_2018/09_map_coef3.png", 
          units = "cm", width = 42, height = 29, dpi = 300)

map_coef4 <-basemap + tm_shape(gwpr4_results) +
  tm_dots(col = "cars_2018", size = 1, shape = 21,
          title = expression("cars_2018: coef"),
          legend.show = TRUE,
          midpoint = NA) +
  tm_compass(type = "arrow", size = 8, position =c(0.82, 0.85))+
  tm_layout(inner.margins =  c(0.05, 0.05 ,0.05, 0.05),
            legend.position = c(0.02, 0.05),
            legend.text.size = 1.5,
            legend.title.size = 1.8) 
tmap_save(map_coef4, filename = "Plot_Leeds_2018/09_map_coef4.png", 
          units = "cm", width = 42, height = 29, dpi = 300)


map_coef5 <-basemap + tm_shape(gwpr4_results) +
  tm_dots(col = "log_std_centrality", size = 1, shape = 21,
          title = expression("Std_centrality: coef"),
          legend.show = TRUE,
          midpoint = NA) +
  tm_compass(type = "arrow", size = 8, position =c(0.82, 0.85))+
  tm_layout(inner.margins =  c(0.05, 0.05 ,0.05, 0.05),
            legend.position = c(0.02, 0.05),
            legend.text.size = 1.5,
            legend.title.size = 1.8) 
tmap_save(map_coef5, filename = "Plot_Leeds_2018/09_map_coef5.png", 
          units = "cm", width = 42, height = 29, dpi = 300)


map_coef6 <-basemap + tm_shape(gwpr4_results) +
  tm_dots(col = "log_nearest_junc_dist", size = 1, shape = 21,
          title = expression("Nearest_junc_dist: coef"),
          legend.show = TRUE,
          midpoint = NA) +
  tm_compass(type = "arrow", size = 8, position =c(0.82, 0.85))+
  tm_layout(inner.margins =  c(0.05, 0.05 ,0.05, 0.05),
            legend.position = c(0.02, 0.05),
            legend.text.size = 1.5,
            legend.title.size = 1.8) 
tmap_save(map_coef6, filename = "Plot_Leeds_2018/09_map_coef6.png", 
          units = "cm", width = 42, height = 29, dpi = 300)


map_coef7 <-basemap + tm_shape(gwpr4_results) +
  tm_dots(col = "employ", size = 1, shape = 21,
          title = expression("Employ: coef"),
          legend.show = TRUE,
          midpoint = NA) +
  tm_compass(type = "arrow", size = 8, position =c(0.82, 0.85))+
  tm_layout(inner.margins =  c(0.05, 0.05 ,0.05, 0.05),
            legend.position = c(0.02, 0.05),
            legend.text.size = 1.5,
            legend.title.size = 1.8) 
tmap_save(map_coef7, filename = "Plot_Leeds_2018/09_map_coef7.png", 
          units = "cm", width = 42, height = 29, dpi = 300)

### Check significance
gwpr4_results$sig_Intercept<-ifelse(abs(gwpr4_results$Intercept_TV) >= 1.96, 
                                    "sig", "nonsig")
gwpr4_results$sig_log_std_centrality <-ifelse(abs(gwpr4_results$log_std_centrality_TV) >= 1.96, 
                                          "sig", "nonsig")
gwpr4_results$sig_log_nearest_junc_dist <- ifelse(abs(gwpr4_results$log_nearest_junc_dist_TV) >= 1.96, 
                                                  "sig", "nonsig")
gwpr4_results$sig_log_major_flow <- ifelse(abs(gwpr4_results$log_major_flow_TV) >= 1.96, 
                                           "sig", "nonsig")
gwpr4_results$sig_pop <- ifelse(abs(gwpr4_results$pop_TV) >= 1.96, "sig", "nonsig")
gwpr4_results$sig_employ <- ifelse(abs(gwpr4_results$employ_TV) >= 1.96, "sig", "nonsig")
gwpr4_results$sig_cars_2018 <- ifelse(abs(gwpr4_results$cars_2018_TV) >= 1.96, "sig", "nonsig")

table(gwpr4_results$sig_Intercept)
table(gwpr4_results$sig_log_std_centrality)
table(gwpr4_results$sig_log_nearest_junc_dist)
table(gwpr4_results$sig_log_major_flow)
table(gwpr4_results$sig_pop)
table(gwpr4_results$sig_employ)
table(gwpr4_results$sig_cars_2018)

# Make statistically significant coefs for three variables
legend_title <- expression("log_major_flow: significant")
map_sig1 <- basemap + 
  tm_shape(gwpr4_results) + 
  tm_dots(col = "sig_log_major_flow", size = 1, shape = 21,
          title = legend_title, legend.show = TRUE,
          midpoint = NA) + 
  tm_compass(type = "arrow", size = 8, position =c(0.82, 0.85))+
  tm_layout(inner.margins =  c(0.05, 0.05 ,0.05, 0.05),
            legend.position = c(0.05, 0.05),
            legend.text.size = 1.5,
            legend.title.size = 1.8) 
tmap_save(map_sig1, filename = "Plot_Leeds_2018/09_map_sig1.png", 
          units = "cm", width = 42, height = 29, dpi = 300)


legend_title <- expression("pop: significant")
map_sig2 <- basemap + 
  tm_shape(gwpr4_results) + 
  tm_dots(col = "sig_pop", size = 1, shape = 21,
          title = legend_title, legend.show = TRUE,
          midpoint = NA) + 
  tm_compass(type = "arrow", size = 8, position =c(0.82, 0.85))+
  tm_layout(inner.margins =  c(0.05, 0.05 ,0.05, 0.05),
            legend.position = c(0.05, 0.05),
            legend.text.size = 1.5,
            legend.title.size = 1.8) 
tmap_save(map_sig2, filename = "Plot_Leeds_2018/09_map_sig2.png", 
          units = "cm", width = 42, height = 29, dpi = 300)

legend_title <- expression("cars_2018: significant")
map_sig3 <- basemap + 
  tm_shape(gwpr4_results) + 
  tm_dots(col = "sig_cars_2018", size = 1, shape = 21,
          title = legend_title, legend.show = TRUE,
          midpoint = NA) + 
  tm_compass(type = "arrow", size = 8, position =c(0.82, 0.85))+
  tm_layout(inner.margins =  c(0.05, 0.05 ,0.05, 0.05),
            legend.position = c(0.05, 0.05),
            legend.text.size = 1.5,
            legend.title.size = 1.8) 
tmap_save(map_sig3, filename = "Plot_Leeds_2018/09_map_sig3.png", 
          units = "cm", width = 42, height = 29, dpi = 300) 


### Plot GWPR statistics
legend_title <- expression(" Std. Residual")
tm1 <- basemap +
  tm_shape(gwpr4_results) +
  tm_dots(col = "stdRes", 
          shape = 21,
          size = 0.8,
          midpoint = NA,
          title = legend_title) +
  tm_layout(scale(3.5),
            inner.margins =  c(0.05, 0.15 ,0.05, 0.10),
            legend.position = c(0.05, 0.05), 
            legend.width = 5,
            legend.height = 5,
            legend.text.size = 1.3,
            legend.title.size = 2) +
  tm_compass(type = "arrow", size = 6, position =c(0.82, 0.85))

tmap_save(tm1, filename = "Plot_Leeds_2018/09_stdResiduals.png", 
          units = "cm", width = 42, height = 29, dpi = 600)



##################################################################
### Predicting the results                                     ###
##################################################################

### Prepare for predicting
eq6 <- log_traffic_flow ~ log_std_centrality + log_nearest_junc_dist + log_major_flow +
  pop + employ + cars_2018

DM1<-gw.dist(dp.locat = st_coordinates(cents_test), rp.locat = st_coordinates(cents_pred))
DM2<-gw.dist(dp.locat = st_coordinates(cents_test))

# Predicting
gwr.pred<-gwr.predict(eq6, 
                      data=cents_test_sp,
                      predictdata = cents_pred_sp,
                      bw = abw,
                      kernel = "gaussian",
                      adaptive = TRUE,
                      dMat1 = DM1,
                      dMat2 = DM2)

# Summary the result
pred_results <- sf::st_as_sf(gwr.pred$SDF)
pred_results <- st_transform(pred_results, 27700)
pred_results$traffic_pred <- round(exp(pred_results$prediction))
#pred_results$prediction <- round(pred_results$prediction)
summary(pred_results$traffic_pred)
summary(cents_test$traffic_flow)
nrow(pred_results[pred_results$traffic_pred>10000, ]) #1110 prediction results greater than 10000
nrow(pred_results[pred_results$traffic_pred>20000, ]) #378 prediction results greater than 10000
nrow(pred_results[pred_results$traffic_pred>50000, ]) #126 prediction results greater than 10000
res <- st_join(cents_pred, pred_results)
res_sub <- res[, c("id", "traffic_pred")]

summary(pred_results$log_std_centrality_coef) # positive
summary(pred_results$log_nearest_junc_dist_coef) # negative
summary(pred_results$log_major_flow_coef) # some positive, some negative
summary(pred_results$pop_coef) # most negative
summary(pred_results$employ_coef) # positive
summary(pred_results$cars_2018_coef)# some positive, some negative

# Plot the prediction value
#library(dplyr)
#lines_minor <- left_join(lines_minor, st_drop_geometry(res_sub),
#                                                       by = c("id"="id"))
tmaptools::palette_explorer()
get_brewer_pal("YlOrRd", n = 13) 

map_pred <-basemap + tm_shape(pred_results) +
  #  tm_lines(col = "traffic_pred",
  #           lwd = 2) +
  tm_dots(col = "traffic_pred",
          size = 0.8,
          shape = 21,
          breaks = c(0,500,1000,1500,2000,2500,3000,10000,20000,50000,Inf),
          palette = c("#feefac","#fff391","#fecf66","#feb441","#fe9929",
                      "#f27e1b","#e0630d","#cc4c02","#ab3c03","#862e04"),
          midpoint = NA) +
  tm_layout(scale(3.5),
            inner.margins =  c(0.05, 0.15 ,0.05, 0.10),
            legend.position = c(0.05, 0.05), 
            legend.width = 5,
            legend.height = 5,
            legend.text.size = 1.3,
            legend.title.size = 2) +
  tm_compass(type = "arrow", size = 6, position =c(0.82, 0.85))

tmap_save(map_pred, filename = "Plot_Leeds_2018/09_lines_prediction.png", 
          units = "cm", width = 42, height = 29, dpi = 600)

