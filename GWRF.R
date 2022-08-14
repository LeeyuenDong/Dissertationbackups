# GWRF
# Load R packages
library(MLmetrics)
library(GWmodel)      ## GW models
library(plyr)         ## Data management
library(sp)           ## Spatial Data management
library(spdep)        ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(raster)       ## spatial data
library(grid)         ## plot
library(gridExtra)    ## Multiple plot
library(ggplot2)      #  plotting
library(tidyverse)    # data 
library(SpatialML)    # Geographically weigted regression
library(tmap)
library(fs)
library(dplyr)

#Load Data
#Create training, validation and test data
grf_data <- read.csv("C:\\Users\\asus\\Desktop\\dt\\data\\shanhu\\London_Result0706\\London_Result\\input\\london_house_price_data1.csv", header = TRUE)  %>% 
  dplyr::select(PURCHASE, FLOORSZ, TYPEDETCH, TYPETRRD, TYPEFLAT, 
                BLDPWW1, BLD60S, BLD70S, BLD80S, BLD90S, BATH2,  
                BEDS2, GARAGE1, CENTHEAT, PROF, BLDINTW, X, Y, GSS_CODE, Type)

# BLDYEAR


Londons<-dir_info(here::here("C:\\Users\\asus\\Desktop\\dt\\data\\shanhu\\London_Result0706\\London_Result\\input"))%>%
  #$ means exact match
  dplyr::filter(str_detect(path, 
                           "London_Borough_Excluding_MHW.shp$"))%>%
  dplyr::select(path)%>%
  pull()%>%
  #read in the file in
  st_read()


set.seed(1)

# prepare data
grf_test<-grf_data %>% 
  filter(Type == 3 & PURCHASE > 0)

grf_valid<-grf_data %>%
  filter(Type == 2 & PURCHASE > 0)

grf_train<-grf_data %>%
  filter(Type == 1 & PURCHASE > 0)

#Scale covariates
grf_test[, 2:16] = scale(grf_test[, 2:16])
grf_valid[, 2:16] = scale(grf_valid[, 2:16])
grf_train[, 2:16] = scale(grf_train[, 2:16])

#bwe <-grf.bw(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
#              BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + 
#             BEDS2 + GARAGE1 + CENTHEAT + PROF + BLDINTW, 
#          ntree= 500, 
#         mtry = 5,
#        coords=Coords,
#       dframe=grf_train, bw.min = 30, bw.max = 150, step = 20,
#      forests = FALSE, weighted = TRUE)

gwrf <- function(bw, ntree){

#set.seed(10)
  
Coords<-grf_train[ ,17:18]
grf.model <- grf(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                     BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + 
                     BEDS2 + GARAGE1 + CENTHEAT + PROF + BLDINTW, 
                   dframe=grf_train,
                   
                   bw= bw,             # a positive number, in the case of an "adaptive kernel" or a real in the case of a "fixed kernel".
                   ntree= ntree, 
                   mtry = 10,           # n integer referring to the number of trees to grow for each of the local random forests.
                   kernel="adaptive",  # yhe kernel to be used in the regression. Options are "adaptive" or "fixed".
                   forests = TRUE,     # a option to save and export (TRUE) or not (FALSE) all the local forests
                   importance = T,
                   coords=Coords)      # a numeric matrix or data frame of two columns giving the X,Y coordinates of the observations
  
  # Prediction
  # Creat a data-frame
FIPS.xy<-grf_test[,17:19]
#FIPS.xy2<-grf_valid[,17:19]
FIPS.xy3<-grf_train[,17:19]
FIPS.xy$PURCHASE_test<-grf_test[,1]
#FIPS.xy2$PURCHASE_valid<-grf_valid[,1]
FIPS.xy3$PURCHASE_train<-grf_train[,1]
  
  # training data
FIPS.xy3$pre_train<-predict.grf(grf.model, grf_train, x.var.name="X", y.var.name="Y", local.w=1, global.w=0)
  
  # Validation data
#FIPS.xy2$pre_valid<-predict.grf(grf.model, grf_valid, x.var.name="X", y.var.name="Y", local.w=1, global.w=0)
  # Test data
FIPS.xy$pre_test<-predict.grf(grf.model, grf_test, x.var.name="X", y.var.name="Y", local.w=1, global.w=0)
  
  #Local result
R2_train = R2_Score(FIPS.xy3$pre_train, FIPS.xy3$PURCHASE_train)
RMSE_value_train= RMSE(FIPS.xy3$pre_train, FIPS.xy3$PURCHASE_train)
cat("RF train: " , "R2:",R2_train, "   ", "RMSE:",RMSE_value_train, "    ")

R2_test = R2_Score(FIPS.xy$pre_test, FIPS.xy$PURCHASE_test)
RMSE_value_test= RMSE(FIPS.xy$pre_test, FIPS.xy$PURCHASE_test)
cat("RF test: " , "R2:",R2_test, "   ", "RMSE:",RMSE_value_test)
}

#ntree = 100
gwrf(bw= 20,ntree= 100)

gwrf(bw= 30,ntree= 100)

gwrf(bw= 50,ntree= 100)

gwrf(bw= 100,ntree= 100)

gwrf(bw= 150,ntree= 100)

gwrf(bw= 200,ntree= 100)

#ntree = 200
gwrf(bw= 20,ntree= 200)

gwrf(bw= 30,ntree= 200)

gwrf(bw= 50,ntree= 200)

gwrf(bw= 100,ntree= 200)

gwrf(bw= 150,ntree= 200)

gwrf(bw= 200,ntree= 200)

#ntree = 500
gwrf(bw= 20,ntree= 500)

gwrf(bw= 30,ntree= 500)

gwrf(bw= 50,ntree= 500)

gwrf(bw= 100,ntree= 500)

gwrf(bw= 150,ntree= 500)

gwrf(bw= 200,ntree= 500)


# bw 150 ntree 100



#Global result
rf <- function(ntree){
  
Coords<-grf_train[ ,17:18]
grf.model <- grf(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                     BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + 
                     BEDS2 + GARAGE1 + CENTHEAT + PROF + BLDINTW, 
                   dframe=grf_train,
                   
                   bw= 20,             # a positive number, in the case of an "adaptive kernel" or a real in the case of a "fixed kernel".
                   ntree= ntree, 
                   mtry = 10,           # n integer referring to the number of trees to grow for each of the local random forests.
                   kernel="adaptive",  # yhe kernel to be used in the regression. Options are "adaptive" or "fixed".
                   forests = TRUE,     # a option to save and export (TRUE) or not (FALSE) all the local forests
                   importance = T,
                   coords=Coords)      # a numeric matrix or data frame of two columns giving the X,Y coordinates of the observations
  
  # Prediction
  # Creat a data-frame
FIPS.xy<-grf_test[,17:19]
  #FIPS.xy2<-grf_valid[,17:19]
FIPS.xy3<-grf_train[,17:19]
FIPS.xy$PURCHASE_test<-grf_test[,1]
  #FIPS.xy2$PURCHASE_valid<-grf_valid[,1]
FIPS.xy3$PURCHASE_train<-grf_train[,1]
  
  # training data
FIPS.xy3$pre_train<-predict.grf(grf.model, grf_train, x.var.name="X", y.var.name="Y", local.w=0, global.w=1)
  
  # Validation data
  #FIPS.xy2$pre_valid<-predict.grf(grf.model, grf_valid, x.var.name="X", y.var.name="Y", local.w=1, global.w=0)
  # Test data
FIPS.xy$pre_test<-predict.grf(grf.model, grf_test, x.var.name="X", y.var.name="Y", local.w=0, global.w=1)
  
#Local result
R2_train = R2_Score(FIPS.xy3$pre_train, FIPS.xy3$PURCHASE_train)
RMSE_value_train= RMSE(FIPS.xy3$pre_train, FIPS.xy3$PURCHASE_train)
cat("RF train: " , "R2:",R2_train, "   ", "RMSE:",RMSE_value_train, "    ")


R2_test = R2_Score(FIPS.xy$pre_test, FIPS.xy$PURCHASE_test)
RMSE_value_test= RMSE(FIPS.xy$pre_test, FIPS.xy$PURCHASE_test)
cat("RF test: " , "R2:",R2_test, "   ", "RMSE:",RMSE_value_test)
}

rf(ntree = 100)
rf(ntree = 200)
rf(ntree = 500)





# training data
FIPS.xy3$pre_train<-predict.grf(grf.model, grf_train, x.var.name="X", y.var.name="Y", local.w=0, global.w=1)

# Validation data
FIPS.xy2$pre_valid<-predict.grf(grf.model, grf_valid, x.var.name="X", y.var.name="Y", local.w=0, global.w=1)
# Test data
FIPS.xy$pre_test<-predict.grf(grf.model, grf_test, x.var.name="X", y.var.name="Y", local.w=0, global.w=1)



cat('GWRF train RMSE:', round(sqrt(mean((FIPS.xy3$pre_train-FIPS.xy3$PURCHASE_train)^2 , na.rm = TRUE)), digits=3), '\n')
## GWRF train RMSE: 30150.39 
cat('GWRF train MAE:', round(mean(abs(FIPS.xy3$pre_train-FIPS.xy3$PURCHASE_train) , na.rm = TRUE ), digits=3), '\n')
## GWRF train MAE: 20093.52 
cat('GWRF train R2:', round(summary(lm(PURCHASE_train~pre_train,FIPS.xy3))$r.squared, digits=3), '\n')
## GWRF train R2: 0.885 
cat('GWRF Validation RMSE:', round(sqrt(mean((FIPS.xy2$pre_valid-FIPS.xy2$PURCHASE_valid)^2 , na.rm = TRUE)), digits=3), '\n')
## GWRF Validation RMSE: 49189.75
cat('GWRF Validation MAE:', round(mean(abs(FIPS.xy2$pre_valid-FIPS.xy2$PURCHASE_valid) , na.rm = TRUE ), digits=3), '\n')
## GWRF Validation MAE: 28575.57
cat('GWRF Validation R2:', round(summary(lm(PURCHASE_valid~pre_valid,FIPS.xy2))$r.squared, digits=3), '\n')
## GWRF Validation R2: 0.767
cat('GWRF Test RMSE:', round(sqrt(mean((FIPS.xy$pre_test-FIPS.xy$PURCHASE_test)^2 , na.rm = TRUE)), digits=3), '\n')
## GWRF Test RMSE: 38947.67 
cat('GWRF Test MAE:', round(mean(abs(FIPS.xy$PURCHASE_test-FIPS.xy$pre_test) , na.rm = TRUE ), digits=3), '\n')
## GWRF Test MAE: 26114.33 
cat('GWRF Test R2:', round(summary(lm(PURCHASE_test~pre_test,FIPS.xy))$r.squared, digits=3), '\n')
## GWRF Test R2: 0.725 


#Global result

# training data
FIPS.xy3$pre_train<-predict.grf(grf.model, grf_train, x.var.name="X", y.var.name="Y", local.w=0, global.w=1)

# Validation data
FIPS.xy2$pre_valid<-predict.grf(grf.model, grf_valid, x.var.name="X", y.var.name="Y", local.w=0, global.w=1)
# Test data
FIPS.xy$pre_test<-predict.grf(grf.model, grf_test, x.var.name="X", y.var.name="Y", local.w=0, global.w=1)


cat('GWRF train RMSE:', round(sqrt(mean((FIPS.xy3$pre_train-FIPS.xy3$PURCHASE_train)^2 , na.rm = TRUE)), digits=3), '\n')
## GWRF train RMSE: 30150.39 
cat('GWRF train MAE:', round(mean(abs(FIPS.xy3$pre_train-FIPS.xy3$PURCHASE_train) , na.rm = TRUE ), digits=3), '\n')
## GWRF train MAE: 20093.52 
cat('GWRF train R2:', round(summary(lm(PURCHASE_train~pre_train,FIPS.xy3))$r.squared, digits=3), '\n')
## GWRF train R2: 0.885 
cat('GWRF Validation RMSE:', round(sqrt(mean((FIPS.xy2$pre_valid-FIPS.xy2$PURCHASE_valid)^2 , na.rm = TRUE)), digits=3), '\n')
## GWRF Validation RMSE: 49189.75
cat('GWRF Validation MAE:', round(mean(abs(FIPS.xy2$pre_valid-FIPS.xy2$PURCHASE_valid) , na.rm = TRUE ), digits=3), '\n')
## GWRF Validation MAE: 28575.57
cat('GWRF Validation R2:', round(summary(lm(PURCHASE_valid~pre_valid,FIPS.xy2))$r.squared, digits=3), '\n')
## GWRF Validation R2: 0.767
cat('GWRF Test RMSE:', round(sqrt(mean((FIPS.xy$pre_test-FIPS.xy$PURCHASE_test)^2 , na.rm = TRUE)), digits=3), '\n')
## GWRF Test RMSE: 38947.67 
cat('GWRF Test MAE:', round(mean(abs(FIPS.xy$PURCHASE_test-FIPS.xy$pre_test) , na.rm = TRUE ), digits=3), '\n')
## GWRF Test MAE: 26114.33 
cat('GWRF Test R2:', round(summary(lm(PURCHASE_test~pre_test,FIPS.xy))$r.squared, digits=3), '\n')
