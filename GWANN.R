options(java.parameters = "-Xmx8000m")

library(MLmetrics)
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

y_true = y[s_test]

gwanntest <- function(bw, nrHidden, lr){
  
  
  r<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[-s_test,-s_test],
           x_pred=x[s_test,],w_pred=dm[-s_test,s_test],
           nrHidden=nrHidden,batchSize=50,lr=lr,
           kernel = "gaussian",
           adaptive=T,
           #norm = T,
           bandwidth = bw,
           permutations = 0,
           bwSearch="goldenSection",
           #bwMin=min(dm)/4, bwMax=max(dm)/4,
           threads=8
  )
  #The prediction outcome of GWANNN is a matrix form with 620*620 dimension
  #The feature importance could not be printed once run the code
  
  p<-diag(r$predictions)
  y_true = y[s_test]
  
  R2 = R2_Score(p, y_true)
  RMSE_value = RMSE(p, y_true)
  
  
  cat("R2:",R2, "   ", "RMSE:",RMSE_value)
}
######learning rate 0.1
#lr = 0.1 bw = 20
gwanntest(bw = 20, nrHidden = 30, lr = 0.1)

gwanntest(bw = 20, nrHidden = 50, lr = 0.1)

gwanntest(bw = 20, nrHidden = 100, lr = 0.1)

gwanntest(bw = 20, nrHidden = 150, lr = 0.1)

gwanntest(bw = 20, nrHidden = 200, lr = 0.1)

gwanntest(bw = 20, nrHidden = 300, lr = 0.1)

#lr = 0.1 bw = 30
gwanntest(bw = 30, nrHidden = 30, lr = 0.1)

gwanntest(bw = 30, nrHidden = 50, lr = 0.1)

gwanntest(bw = 30, nrHidden = 100, lr = 0.1)

gwanntest(bw = 30, nrHidden = 150, lr = 0.1)

gwanntest(bw = 30, nrHidden = 200, lr = 0.1)

gwanntest(bw = 30, nrHidden = 300, lr = 0.1)

#lr = 0.1 bw = 50
gwanntest(bw = 50, nrHidden = 30, lr = 0.1)

gwanntest(bw = 50, nrHidden = 50, lr = 0.1)

gwanntest(bw = 50, nrHidden = 100, lr = 0.1)

gwanntest(bw = 50, nrHidden = 150, lr = 0.1)

gwanntest(bw = 50, nrHidden = 200, lr = 0.1)

gwanntest(bw = 50, nrHidden = 300, lr = 0.1)

#lr = 0.1 bw = 100
gwanntest(bw = 100, nrHidden = 30, lr = 0.1)

gwanntest(bw = 100, nrHidden = 50, lr = 0.1)

gwanntest(bw = 100, nrHidden = 100, lr = 0.1)

gwanntest(bw = 100, nrHidden = 150, lr = 0.1)

gwanntest(bw = 100, nrHidden = 200, lr = 0.1)

gwanntest(bw = 100, nrHidden = 300, lr = 0.1)


######learning rate 0.05
#lr = 0.05 bw = 20
gwanntest(bw = 20, nrHidden = 30, lr = 0.05)

gwanntest(bw = 20, nrHidden = 50, lr = 0.05)

gwanntest(bw = 20, nrHidden = 100, lr = 0.05)

gwanntest(bw = 20, nrHidden = 150, lr = 0.05)

gwanntest(bw = 20, nrHidden = 200, lr = 0.05)

gwanntest(bw = 20, nrHidden = 300, lr = 0.05)

#lr = 0.05 bw = 30
gwanntest(bw = 30, nrHidden = 30, lr = 0.05)

gwanntest(bw = 30, nrHidden = 50, lr = 0.05)

gwanntest(bw = 30, nrHidden = 100, lr = 0.05)

gwanntest(bw = 30, nrHidden = 150, lr = 0.05)

gwanntest(bw = 30, nrHidden = 200, lr = 0.05)

gwanntest(bw = 30, nrHidden = 300, lr = 0.05)


#lr = 0.05 bw = 50
gwanntest(bw = 50, nrHidden = 30, lr = 0.05)

gwanntest(bw = 50, nrHidden = 50, lr = 0.05)

gwanntest(bw = 50, nrHidden = 100, lr = 0.05)

gwanntest(bw = 50, nrHidden = 150, lr = 0.05)

gwanntest(bw = 50, nrHidden = 200, lr = 0.05)

gwanntest(bw = 50, nrHidden = 300, lr = 0.05)

#lr = 0.05 bw = 100
gwanntest(bw = 100, nrHidden = 30, lr = 0.05)

gwanntest(bw = 100, nrHidden = 50, lr = 0.05)

gwanntest(bw = 100, nrHidden = 100, lr = 0.05)

gwanntest(bw = 100, nrHidden = 150, lr = 0.05)

gwanntest(bw = 100, nrHidden = 200, lr = 0.05)

gwanntest(bw = 100, nrHidden = 300, lr = 0.05)

# ANN model
anntest <- function(nrHidden, lr){
  
  r<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[-s_test,-s_test],
           x_pred=x[s_test,],w_pred=dm[-s_test,s_test],
           nrHidden=nrHidden,batchSize=50,lr=lr,
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
  
  
  R2 = R2_Score(p, y_pred)
  RMSE_value = RMSE(p, y_pred)
  
  
  cat("R2:",R2, "   ", "RMSE:",RMSE_value)
}
# lr = 0.1
anntest(nrHidden = 30, lr = 0.1)

anntest(nrHidden = 50, lr = 0.1)

anntest(nrHidden = 100, lr = 0.1)

anntest(nrHidden = 150, lr = 0.1)

anntest(nrHidden = 200, lr = 0.1)

anntest(nrHidden = 300, lr = 0.1)

# lr = 0.05
anntest(nrHidden = 30, lr = 0.05)

anntest(nrHidden = 50, lr = 0.05)

anntest(nrHidden = 100, lr = 0.05)

anntest(nrHidden = 150, lr = 0.05)

anntest(nrHidden = 200, lr = 0.05)

anntest(nrHidden = 300, lr = 0.05)



print(paste("RMSE: ",sqrt(mean((p-y[s_test])^2))))
print(paste("Iterations: ",r$iterations))
print(paste("Bandwidth: ",r$bandwidth))



#train accuracy
t<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[-s_test,-s_test],
         x_pred=x[-s_test,],w_pred=dm[-s_test,-s_test],
         nrHidden=200,batchSize=50,lr=0.05,
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

R2_Score(p, y_train1)
RMSE(p, y_train1)


#ANN optimal model
r<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[-s_test,-s_test],
         x_pred=x[s_test,],w_pred=dm[-s_test,s_test],
         nrHidden=30,batchSize=50,lr=0.05,
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


R2_Score(p, y_pred)
RMSE(p, y_pred)

#ANN train accuracy
t<-gwann(x_train=x[-s_test,],y_train=y[-s_test],w_train=dm[-s_test,-s_test],
         x_pred=x[-s_test,],w_pred=dm[-s_test,-s_test],
         nrHidden=30,batchSize=50,lr=0.05,
         kernel = "gaussian",
         adaptive=F,
         bandwidth = Inf,
         permutations = 0,
         bwSearch="goldenSection",
         #bwMin=min(dm)/4, bwMax=max(dm)/4,
         threads=8)



p<-diag(t$predictions)
y_train1 = y[-s_test]

R2_Score(p, y_train1)
RMSE(p, y_train1)








