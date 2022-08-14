#########################������Ȩ�ع�
#���û���
setwd("C:\\Users\\asus\\Desktop\\2yuen")
#library packages
library(sp)
library(rgeos)
library(maptools) #��ȡ�ռ�����
library(spdep)  #���ڿռ�����ط���
library(RColorBrewer)
library(GWmodel) #������Ȩ�ع��
library(gstat) #�ռ��ֵ
library(raster) #դ�����ݴ���
#import data
LNHP <- lhGDF
LN.bou <- readShapePoly("Data/LondonBorough",verbose = T,proj4string = CRS("+init=epsg:27700"))
#�����ݵĿռ�����KNN��
LNHPnb <- knn2nb(knearneigh(LNHP,k=4,longlat = TRUE)) #k�������
LNHPnb_s <- make.sym.nb(LNHPnb)
plot(LNHP)
plot(nb2listw(LNHPnb_s),cbind(LNHP$X,LNHP$Y),pch=20)
#or
plot(nb2listw(LNHPnb_s),coordinates(LNHP),pch=20)
###ȫ�ֿռ������
#Ī��ָ��
col.W <- nb2listw(LNHPnb_s,style = "W")
moi <- moran(LNHP$PURCHASE,col.W,length(LNHP$PURCHASE),Szero(col.W))
moi
###�ռ�ع����
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
#����ѡ����Զ�ѡ��
bw.gwr.1 <- bw.gwr(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S,
                   data = LNHP,approach = "AICc",kernel = "gaussian",adaptive = TRUE)
#���������������ģ��
gwr.res <- gwr.basic(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S,data = LNHP,
                     bw=bw.gwr.1,kernel = "gaussian",adaptive = TRUE)

gwr.res
#####
#set.seed(1)
ind <- sample(2,nrow(LNHP),replace = TRUE,prob = c(0.7,0.3))
train_data <- LNHP[ind==1,]
test_data <- LNHP[ind==2,]

data1 = LNHP





#����ѡ����Զ�ѡ�� train
bw.gwr.train <- bw.gwr(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                         BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
                         BEDS2 + GARAGE1 + CENTHEAT + BLDINTW,
                       data = train_data,approach = "AICc",kernel = "gaussian",adaptive = TRUE)
#���������������ģ�� train
gwr.res_train <- gwr.basic(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                             BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
                             BEDS2 + GARAGE1 + CENTHEAT + BLDINTW,
                           data = train_data,
                           bw=bw.gwr.train,kernel = "gaussian",adaptive = TRUE)

gwr.res_train

#����ѡ����Զ�ѡ�� test
#bw.gwr.test <- bw.gwr(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
#                        BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
#                        BEDS2 + GARAGE1 + CENTHEAT  + BLDINTW,
#                      data = test_data ,approach = "AICc",kernel = "gaussian",adaptive = TRUE)
#���������������ģ�� test
gwr.res_test <- gwr.basic(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                            BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
                            BEDS2 + GARAGE1 + CENTHEAT  + BLDINTW,
                          data = test_data,
                          bw=bw.gwr.train,kernel = "gaussian",adaptive = TRUE)

gwr.res_test

#���������������ģ�� test
gwr.pred<- gwr.predict(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                         BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + PROF+
                         BEDS2 + GARAGE1 + CENTHEAT +BLDINTW,
                       data = test_data,
                       bw=bw.gwr.train,kernel = "gaussian",adaptive = TRUE)

gwr.pred

  #prediction new


#����ѡ����Զ�ѡ�� train
bw.gwr.train <- bw.gwr(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                         BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + 
                         BEDS2 + GARAGE1 + CENTHEAT +  BLDINTW,
                       data = train_data,approach = "AICc",kernel = "gaussian",adaptive = TRUE)
#���������������ģ�� train
gwr.res_train <- gwr.basic(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                             BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + 
                             BEDS2 + GARAGE1 + CENTHEAT +  BLDINTW,
                           data = train_data,
                           bw=bw.gwr.train,kernel = "gaussian",adaptive = TRUE)

gwr.res_train


#���������������ģ�� test
gwr.pred<- gwr.predict(PURCHASE ~ FLOORSZ + TYPEDETCH + TYPETRRD + TYPEFLAT +
                            BLDPWW1 + BLD60S + BLD70S + BLD80S + BLD90S + BATH2 + 
                            BEDS2 + GARAGE1 + CENTHEAT +BLDINTW,
                          data = test_data,
                          bw=bw.gwr.train,kernel = "gaussian",adaptive = TRUE)

gwr.pred




#ԭʼģ��

#����ѡ����Զ�ѡ�� train
bw.gwr.train <- bw.gwr(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S,
                       data = train_data,approach = "AICc",kernel = "gaussian",adaptive = TRUE)
#���������������ģ�� train
gwr.res_train <- gwr.basic(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S,data = train_data,
                           bw=bw.gwr.train,kernel = "gaussian",adaptive = TRUE)

gwr.res_train

#����ѡ����Զ�ѡ�� test
bw.gwr.test <- bw.gwr(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S,
                      data = test_data ,approach = "AICc",kernel = "gaussian",adaptive = TRUE)
#���������������ģ�� test
gwr.res_test <- gwr.basic(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S,data = test_data,
                          bw=bw.gwr.test,kernel = "gaussian",adaptive = TRUE)

gwr.res_test