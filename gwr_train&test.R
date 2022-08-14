#########################地理加权回归
#设置环境
setwd("C:\\Users\\asus\\Desktop\\2yuen")
#library packages
library(sp)
library(rgeos)
library(maptools) #读取空间数据
library(spdep)  #用于空间自相关分析
library(RColorBrewer)
library(GWmodel) #地理加权回归包
library(gstat) #空间插值
library(raster) #栅格数据处理
#import data
LNHP <- lhGDF
LN.bou <- readShapePoly("Data/LondonBorough",verbose = T,proj4string = CRS("+init=epsg:27700"))
#点数据的空间邻域（KNN）
LNHPnb <- knn2nb(knearneigh(LNHP,k=4,longlat = TRUE)) #k最近邻域
LNHPnb_s <- make.sym.nb(LNHPnb)
plot(LNHP)
plot(nb2listw(LNHPnb_s),cbind(LNHP$X,LNHP$Y),pch=20)
#or
plot(nb2listw(LNHPnb_s),coordinates(LNHP),pch=20)
###全局空间自相关
#莫兰指数
col.W <- nb2listw(LNHPnb_s,style = "W")
moi <- moran(LNHP$PURCHASE,col.W,length(LNHP$PURCHASE),Szero(col.W))
moi
###空间回归分析
DeVar <- "PURCHASE"
InDeVars <- c("FLOORSZ","TYPEDETCH","TYPEFLAT", "BLDPWW1","BLDPOSTW" ,"BLD60S","BLD70S","BLD80S",
              "BLD90S","BATH2","PROF")
model.sel <- model.selection.gwr(DeVar,InDeVars,data = LNHP,kernel = "gaussian",
                                 adaptive = TRUE,bw=10000000000000)
sorted.models <- model.sort.gwr(model.sel,numVars = length(InDeVars),
                                ruler.vector = model.sel[[2]][,2])
model.list <- sorted.models[[1]]
model.view.gwr(DeVar,InDeVars,model.list = model.list)

plot(sorted.models[[2]][,2],col="black",pch=20,lty=5,
     main="Alternative view of GWR model selection procedure",
     ylab="AICc value",xlab="Model number",type="b")
#带宽选择的自动选择
bw.gwr.1 <- bw.gwr(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S,
                   data = LNHP,approach = "AICc",kernel = "gaussian",adaptive = TRUE)
#利用上述带宽求解模型
gwr.res <- gwr.basic(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S,data = LNHP,
                     bw=bw.gwr.1,kernel = "gaussian",adaptive = TRUE)

gwr.res
#####
#set.seed(1)
ind <- sample(2,nrow(LNHP),replace = TRUE,prob = c(0.7,0.3))
train_data <- LNHP[ind==1,]
test_data <- LNHP[ind==2,]

data1 = LNHP





#带宽选择的自动选择 train
bw.gwr.train <- bw.gwr(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                         BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
                         BEDS2 + GARAGE1 + CENTHEAT + BLDINTW,
                       data = train_data,approach = "AICc",kernel = "gaussian",adaptive = TRUE)
#利用上述带宽求解模型 train
gwr.res_train <- gwr.basic(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                             BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
                             BEDS2 + GARAGE1 + CENTHEAT + BLDINTW,
                           data = train_data,
                           bw=bw.gwr.train,kernel = "gaussian",adaptive = TRUE)

gwr.res_train

#带宽选择的自动选择 test
#bw.gwr.test <- bw.gwr(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
#                        BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
#                        BEDS2 + GARAGE1 + CENTHEAT  + BLDINTW,
#                      data = test_data ,approach = "AICc",kernel = "gaussian",adaptive = TRUE)
#利用上述带宽求解模型 test
gwr.res_test <- gwr.basic(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                            BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
                            BEDS2 + GARAGE1 + CENTHEAT  + BLDINTW,
                          data = test_data,
                          bw=bw.gwr.train,kernel = "gaussian",adaptive = TRUE)

gwr.res_test

#利用上述带宽求解模型 test
gwr.pred<- gwr.predict(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                         BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
                         BEDS2 + GARAGE1 + CENTHEAT +BLDINTW,
                       data = test_data,
                       bw=bw.gwr.train,kernel = "gaussian",adaptive = TRUE)

gwr.pred

  #prediction new


#带宽选择的自动选择 train
bw.gwr.train <- bw.gwr(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                         BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + 
                         BEDS2 + GARAGE1 + CENTHEAT +  BLDINTW,
                       data = train_data,approach = "AICc",kernel = "gaussian",adaptive = TRUE)
#利用上述带宽求解模型 train
gwr.res_train <- gwr.basic(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                             BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + 
                             BEDS2 + GARAGE1 + CENTHEAT +  BLDINTW,
                           data = train_data,
                           bw=bw.gwr.train,kernel = "gaussian",adaptive = TRUE)

gwr.res_train


#利用上述带宽求解模型 test
gwr.pred<- gwr.predict(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                            BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + 
                            BEDS2 + GARAGE1 + CENTHEAT +BLDINTW,
                          data = test_data,
                          bw=bw.gwr.train,kernel = "gaussian",adaptive = TRUE)

gwr.pred




#原始模型

#带宽选择的自动选择 train
bw.gwr.train <- bw.gwr(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S,
                       data = train_data,approach = "AICc",kernel = "gaussian",adaptive = TRUE)
#利用上述带宽求解模型 train
gwr.res_train <- gwr.basic(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S,data = train_data,
                           bw=bw.gwr.train,kernel = "gaussian",adaptive = TRUE)

gwr.res_train

#带宽选择的自动选择 test
bw.gwr.test <- bw.gwr(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S,
                      data = test_data ,approach = "AICc",kernel = "gaussian",adaptive = TRUE)
#利用上述带宽求解模型 test
gwr.res_test <- gwr.basic(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S,data = test_data,
                          bw=bw.gwr.test,kernel = "gaussian",adaptive = TRUE)

gwr.res_test
