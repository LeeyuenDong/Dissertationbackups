options(java.parameters = "-Xmx8000m")

library(viridis)
library(gwann)
library(ggplot2)
library(plyr)
library(sp)           ## Spatial Data management
library(tidyverse)    # data 

house_price <-read.csv("C:\\Users\\asus\\Desktop\\GWANN\\london_house_price_data.csv")


x<-as.matrix(house_price[,c("FLOORSZ","PROF" ,"TYPEDETCH" ,"TYPETRRD" ,"TYPEFLAT" ,
                             "BLDPWW1", "BLD60S","BLD70S", "BLD80S" ,"BLD90S" ,"BATH2", "BEDS2",
                              "GARAGE1", "CENTHEAT", "BLDINTW")])
y<-as.numeric(house_price[,c("PURCHASE")] )
dm<-as.matrix(dist(house_price[,c("X","Y")])  )
s_test<-sample(nrow(x),0.3*nrow(x)) # indices of test samples

y_pred = y[s_test]


r<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[-s_test,-s_test],
         x_pred=x[s_test,],w_pred=dm[-s_test,s_test],
         nrHidden=100,batchSize=50,lr=0.05,
         kernel = "gaussian",
         adaptive=T,
         #norm = T,
         bandwidth = 30,
         permutations = 0,
         bwSearch="goldenSection",
         #bwMin=min(dm)/4, bwMax=max(dm)/4,
         threads=8
)
#The prediction outcome of GWANNN is a matrix form with 620*620 dimension
#The feature importance could not be printed once run the code

p<-diag(r$predictions)
y_pred = y[s_test]
y_mean = mean(y_pred)
SSR_differ = p - y_mean
SSR_square = (SSR_differ)^2
SSR = sum(SSR_square)
SSE_differ = p-y_pred
SSE_square = (SSE_differ)^2
SSE = sum(SSE_square)
SST = SSE + SSR
R_squared = SSR/SST
print(paste("R_squared: ",R_squared))

print(paste("RMSE: ",sqrt(mean((p-y[s_test])^2))))
print(paste("Iterations: ",r$iterations))
print(paste("Bandwidth: ",r$bandwidth))
#print(paste("weights: ",r$weights))
#print(paste("predictions: ",r$predictions))

#The feature importance could not be printed once run the code
#print(paste("importance: ",r$importance))

write.csv(p,'C:\\Users\\asus\\Desktop\\GWANN\\predictions_p_result.csv')
write.csv(y_pred,'C:\\Users\\asus\\Desktop\\GWANN\\predictions_y.csv')

#write.csv(r$predictions,'C:\\Users\\asus\\Desktop\\GWANN\\predictions111.csv')
#write.csv(r$importance,'C:\\Users\\asus\\Desktop\\GWANN\\importance.csv')



#train accuracy
t<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[-s_test,-s_test],
         x_pred=x[-s_test,],w_pred=dm[-s_test,-s_test],
         nrHidden=150,batchSize=30,lr=0.05,
         kernel = "gaussian",
         adaptive=T,
         permutations = 0,
         bandwidth = 20,
         bwSearch="goldenSection",
         #bwMin=min(dm)/4, bwMax=max(dm)/4,
         threads=8)

p<-diag(t$predictions)
y_train1 = y[-s_test]
y_mean = mean(y_train1)
SSR_differ = p - y_mean
SSR_square = (SSR_differ)^2
SSR = sum(SSR_square)
SSE_differ = p-y_train1
SSE_square = (SSE_differ)^2
SSE = sum(SSE_square)
SST = SSE + SSR
R_squared = SSR/SST
print(paste("R_squared: ",R_squared))


#ANN model
r<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[-s_test,-s_test],
         x_pred=x[s_test,],w_pred=dm[-s_test,s_test],
         nrHidden=100,batchSize=50,lr=0.1,
         kernel = "gaussian",
         adaptive=F,
         #norm = T,
         bandwidth = Inf,
         permutations = 0,
         bwSearch="goldenSection",
         #bwMin=min(dm)/4, bwMax=max(dm)/4,
         threads=8
)

p<-diag(r$predictions)
y_pred = y[s_test]
y_mean = mean(y_pred)
SSR_differ = p - y_mean
SSR_square = (SSR_differ)^2
SSR = sum(SSR_square)
SSE_differ = p-y_pred
SSE_square = (SSE_differ)^2
SSE = sum(SSE_square)
SST = SSE + SSR
R_squared = SSR/SST
print(paste("R_squared: ",R_squared))


#train accuracy
t<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[-s_test,-s_test],
         x_pred=x[-s_test,],w_pred=dm[-s_test,-s_test],
         nrHidden=30,batchSize=50,lr=0.1,
         kernel = "gaussian",
         adaptive=F,
         bandwidth = Inf,
         permutations = 0,
         bwSearch="goldenSection",
         #bwMin=min(dm)/4, bwMax=max(dm)/4,
         threads=8)



p<-diag(t$predictions)
y_train1 = y[-s_test]
y_mean = mean(y_train1)
SSR_differ = p - y_mean
SSR_square = (SSR_differ)^2
SSR = sum(SSR_square)
SSE_differ = p-y_train1
SSE_square = (SSE_differ)^2
SSE = sum(SSE_square)
SST = SSE + SSR
R_squared = SSR/SST
print(paste("R_squared: ",R_squared))













# plot predictions
s<-cbind( Prediction=p, house_price[s_test,c("X","Y")] )
ggplot(s,aes(lon,lat,fill=Prediction)) + geom_raster() + scale_fill_viridis() + coord_fixed()
