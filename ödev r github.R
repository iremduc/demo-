library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)
library(dplyr)
library(tidyr)
library(haven)
library(readxl)
CARDATASET <- read_excel("C:/Users/Admin/Desktop/AUTOMOBILEVERIKUMESI.xlsx")
View(CARDATASET)
summary(CARDATASET)
CARDATASET$CarName<-as.factor(CARDATASET$CarName)
CARDATASET$fueltype<-as.factor(CARDATASET$fueltype)
CARDATASET$aspiration<-as.factor(CARDATASET$aspiration)
CARDATASET$doornumber<-as.factor(CARDATASET$doornumber)
CARDATASET$carbody<-as.factor(CARDATASET$carbody)
CARDATASET$drivewheel<-as.factor(CARDATASET$drivewheel)
CARDATASET$enginelocation<-as.factor(CARDATASET$enginelocation)
CARDATASET$wheelbase<-as.numeric(CARDATASET$wheelbase)
CARDATASET$carlength<-as.numeric(CARDATASET$carlength)
CARDATASET$carwidth<-as.numeric(CARDATASET$carwidth)
CARDATASET$carheight<-as.numeric(CARDATASET$carheight)
CARDATASET$curbweight<-as.numeric(CARDATASET$curbweight)
CARDATASET$enginetype<-as.factor(CARDATASET$enginetype)
CARDATASET$cylindernumber<-as.factor(CARDATASET$cylindernumber)
CARDATASET$enginesize<-as.numeric(CARDATASET$enginesize)
CARDATASET$fuelsystem<-as.factor(CARDATASET$fuelsystem)
CARDATASET$horsepower<-as.numeric(CARDATASET$horsepower)
CARDATASET$peakrpm<-as.numeric(CARDATASET$peakrpm)
CARDATASET$citympg<-as.numeric(CARDATASET$citympg)
CARDATASET$highwaympg<-as.numeric(CARDATASET$highwaympg)
CARDATASET$price<-as.numeric(CARDATASET$price)
summary(CARDATASET)
any(is.na(CARDATASET))
library(mice)
md.pattern(CARDATASET)
library(dplyr)
CARDATASET %>%
  group_by(carbody) %>%
  summarise(N = n()) 
library(dplyr)
CARDATASET %>%
  select(curbweight,carlength,carwidth,carheight) %>%
  mshapiro_test()
library(funModeling)
plot_num(CARDATASET)
shapiro.test(CARDATASET$carlength)
shapiro.test(CARDATASET$price)
shapiro.test(CARDATASET$carwidth)
shapiro.test(CARDATASET$carheight)
CARDATASET$lncarlength<-log(CARDATASET$carlength)
CARDATASET$lnprice<-log(CARDATASET$price)
CARDATASET$lncarwidth<-log(CARDATASET$carwidth)
CARDATASET$lncarheight<-log(CARDATASET$carheight) 
library(funModeling)
plot_num(CARDATASET[,22:25])
#Univariate Normality
shapiro.test(CARDATASET$lncarlength)
shapiro.test(CARDATASET$lnprice)
shapiro.test(CARDATASET$lncarwidth)
shapiro.test(CARDATASET$lncarheight) 
CARDATASET %>%
  dplyr::select(lnprice,lncarheight,lncarwidth,lncarlength) %>% 
  mshapiro_test()
#Degiskenlere Göre Ortalamalar ve Ortalama Çizimleri
#ARABA modelleri duzeyleri bazında degisken ortalamaları ve sapmalar
CARDATASET%>% 
  group_by(carbody) %>%
  summarise(across(c(lncarheight,lnprice,lncarwidth), list(mean=mean,sd=sd)))

#KAPI SAYILARI duzeyleri bazında degisken ortalamaları
CARDATASET %>% 
  group_by(doornumber) %>%
  summarise(across(c(lncarheight,lnprice,lncarwidth), list(mean=mean,sd=sd)))
library(gplots)
#CarName icin:
plotmeans(lnprice~carbody,xlab="carbody",ylab="lnprice", main="Mean Plot\nwith 95% CI",data=CARDATASET)
plotmeans(lncarheight~carbody, xlab="carbody",ylab="lncarheight", main="Mean Plot\nwith 95% CI",data=CARDATASET)
plotmeans(lncarwidth~carbody, xlab="carbody",ylab="lncarwidth", main="Mean Plot\nwith 95% CI",data=CARDATASET)

#doornumber icin:
plotmeans(lnprice~doornumber,xlab="doornumber",ylab="lnprice", main="Mean Plot\nwith 95% CI",data=CARDATASET)
plotmeans(lncarheight~doornumber, xlab="doornumber",ylab="lncarheight", main="Mean Plot\nwith 95% CI",data=CARDATASET)
plotmeans(lncarwidth~doornumber, xlab="doornumber",ylab="lncarwidth", main="Mean Plot\nwith 95% CI",data=CARDATASET)
#saçılıım matrisi
lnli<-CARDATASET[,c(22:25)]
library(PerformanceAnalytics)
chart.Correlation(CARDATASET[,c(22:25)], histogram=TRUE, pch=19)
#Örneğin carwidth değişkeni için CarName göre inceleme yapılırsa:
library(dplyr)
out<- CARDATASET %>% tibble::rownames_to_column(var="outlier") %>% group_by(carbody) %>% mutate(is_outlier=ifelse(is_outlier(lncarwidth), lncarwidth, as.numeric(NA)))
out$outlier[which(is.na(out$is_outlier))] <- as.numeric(NA)

ggplot(out, aes(y=lncarwidth, x=CarName,fill=carbody))+
  geom_boxplot() + 
  geom_text(aes(label=outlier),na.rm=TRUE,nudge_x=0.15,size=3.5)+ 
  labs(x="arabamodelleri", y = "lncarwidth")+
  scale_fill_discrete(name = "arabamodelleri")
#Outliers-Multivariate
m<-CARDATASET[,c(5,22:25)]
pc <- m %>%
  pivot_longer( c(lnprice,lncarwidth,lncarheight),names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  tukey_hsd(value ~ carbody)
pc<-pc[,c(1,2,4,3,5:9)]
pc
#install.packages("biotools")
library("biotools")
box_m(CARDATASET[, c("lnprice","lncarwidth","lncarheight")],CARDATASET$carbody) 
#MANOVA  
lncar_man <- manova(cbind(lnprice,lncarwidth,lncarheight) ~ carbody,data=CARDATASET)
summary(lncar_man, test = "Hotelling-Lawley")
summary(lncar_man, test = "Wilks")
summary(lncar_man, test = "Pillai")
summary(lncar_man, test = "Roy")
#install.packages("car")
library(car)
library(tidyverse)
CARDATASET %>% 
  pivot_longer( c(lnprice,lncarheight,lncarwidth),names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  levene_test(value ~ carbody,center=mean)
### Test of Between Subjects####Farkliligi yaratan degisken hangisi ?
summary.aov(lncar_man)
# Çoklu Karşılaştırmalar (Multiple Comparisons)
#Levene - Equal variances -Tukey
#Levene- Equal variances -Tukey
m<-CARDATASET[,c(5,22:25)]
pc <- m %>%
  pivot_longer( c(lnprice,lncarwidth,lncarheight),names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  tukey_hsd(value ~ carbody)
pc<-pc[,c(1,2,4,3,5:9)]
pc
CARDATASET%>%
  group_by(carbody,doornumber) %>%
  summarise(N = n())
summary(CARDATASET)
CARDATASET$doornumber<- as.factor(CARDATASET$doornumber)
CARDATASET$carbody<- as.factor(CARDATASET$carbody)
###Çift Yönlü Manova
library(heplots)
boxM(cbind(lnprice,lncarwidth,lncarheight) ~ carbody*doornumber, data=CARDATASET )
#MANOVA  
lncar_cift <- manova(cbind(lnprice,lncarwidth,lncarheight) ~ doornumber*carbody,data=CARDATASET)
summary(lncar_cift, test = "Pillai") #uygun olanlardan biri secilebilir
#Homogeneity of variance- Levene's Test
#install.packages("car")
library(car)
CARDATASET %>% 
  pivot_longer( c(lnprice,lncarwidth,lncarheight),names_to = "variable", values_to = "value") %>% 
  group_by(variable) %>%
  levene_test(value ~ carbody*doornumber,center=mean)
### Test of Between Subjects####Farkliligi yaratan degisken hangisi ?
summary.aov(lncar_cift)
# Çoklu Karsilastirmalar (Multiple Comparisons)
#doornumber icin- Levene- Equal variances -Tukey
m2<-CARDATASET[,c(4,5,22:25)]
pc_2 <- CARDATASET %>%
  pivot_longer( c(lnprice,lncarlength,lncarheight),names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  tukey_hsd(value ~ doornumber*carbody)
pc_2<-pc_2[,c(1,2,4,3,5:9)]             
doornumber_etk<-filter(pc_2, term=="doornumber")
doornumber_etk
#CarName  ve doornumber için Etkilesim Grafikleri (Interaction Plots) 
attach(CARDATASET)
interaction.plot(carbody,doornumber,lncarwidth, fun=mean, type="l", legend=TRUE,col=1:3, lwd=2)
interaction.plot(carbody,doornumber,lncarheight, fun=mean, type="l", legend=TRUE, col=1:3,lwd=2)
detach(CARDATASET)