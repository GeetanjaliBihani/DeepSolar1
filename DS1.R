#DS1
#DESCRIBE DATA
#Deep Solar data analysis
DS1<-read.csv("/Users/geetanjalibihani/Downloads/deepsolar_tract.csv")
head(DS1)
DS1<-DS1[,-1]


#what types of people in USA install solar panels?
#counties with highest household income have lowest solar installations
library(ggplot2)
ggplot(DS1,aes(x=average_household_income,y=solar_panel_area_divided_by_area))+geom_point(aes(color=average_household_income))+theme_classic()

#net metering years does not have any linear relation with solar panel installation
library(ggplot2)
ggplot(DS1,aes(x=(solar_panel_area_divided_by_area), y=net_metering))+geom_point(aes(color=net_metering))+theme_classic()
DS1$solar_system_count

#building different models using all variables and comparing % error
#linear regression
ds1_new<-DS1[complete.cases((DS1)),]
solarmodel<-lm(daily_solar_radiation~., data=ds1_new)
summary(solarmodel)
library(caret)
a<-varImp(solarmodel)
par(mfrow = c(2, 2))
plot(solarmodel)
library(ggplot2)
ds1_new$predicted=solarmodel$fitted.values
ds1_new$residuals=solarmodel$residuals

#plotting actual values with residuals to see how residuals are distributed in the data
ggplot(ds1_new,aes(x=daily_solar_radiation, y=predicted))+
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_point(aes(color=abs(residuals)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+
  theme_classic()

mse(ds1_new$daily_solar_radiation,ds1_new$predicted)
#random forest
library(randomForest)
ds1_new<-DS1[complete.cases((DS1)),]
#removing county and state columns because they are character columns
ds1_new<-ds1_new[,c(-6,-38)]
str(ds1_new)
#split into train and validation set
set.seed(100)
train <- sample(nrow(ds1_new), 0.7*nrow(ds1_new), replace = FALSE)
TrainSet <- ds1_new[train,]
ValidSet <- ds1_new[-train,]
summary(TrainSet)
summary(ValidSet)
str(TrainSet)
model1 <- randomForest(daily_solar_radiation ~ .,data = TrainSet, importance=TRUE)
plot(model1$oob.times)
varImpPlot(model1)
model1
TrainSet$predicted<-predict(model1, newdata= TrainSet)
TrainSet$residuals<-TrainSet$daily_solar_radiation-TrainSet$predicted

library(ggplot2)
ggplot(TrainSet,aes(x=daily_solar_radiation,y=predicted))+geom_point(aes(color=abs(residuals)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()

#predicting on validation dataset
ValidSet$predicted<-predict(model1, newdata= ValidSet)
ValidSet$residuals<-ValidSet$daily_solar_radiation-ValidSet$predicted
ggplot(ValidSet,aes(x=daily_solar_radiation,y=predicted))+geom_point(aes(color=abs(residuals)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()
library(ModelMetrics)
mse(ValidSet$daily_solar_radiation,ValidSet$predicted)
mse(TrainSet$daily_solar_radiation,TrainSet$predicted)

#running this rf model on overall ds data
library(randomForest)
ds1_new<-DS1[complete.cases((DS1)),]
#removing county and state columns because they are character columns
ds1_new<-ds1_new[,c(-6,-38)]
ds1_new$predicted<-predict(model1,newdata=ds1_new)
ds1_new$residuals<-ds1_new$daily_solar_radiation-ds1_new$predicted
ggplot(ds1_new,aes(x=daily_solar_radiation,y=predicted))+geom_point(aes(color=abs(residuals)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()
mse(ds1_new$daily_solar_radiation,ds1_new$predicted)


#ARITIFICAL NEURAL NETWORK
#begin by feature selection
#finding and removing redundant features using recursive feature elimination (we have 165 features to begin with)
ds1_new<-DS1[complete.cases((DS1)),]
ds1_new<-ds1_new[,c(-6,-38)]
str(ds1_new)
solarrad<-ds1_new$daily_solar_radiation
ds1_new$daily_solar_radiation<-NULL
ds1_new$daily_solar_radiation<-solarrad
library(mlbench)
library(caret)
library(randomForest)
model1 <- randomForest(daily_solar_radiation ~ .,data = ds1_new, importance=TRUE)
plot(model1$oob.times)
imp_vars<-as.data.frame(model1$importance)
#filter features, take the top 20 most influential features into account, as given using the random forest model for preliminary filtering
library(sqldf)
ds_20f<-sqldf('select relative_humidity,	earth_temperature,	heating_degree_days,	lon,	air_temperature,	incentive_count_nonresidential,	atmospheric_pressure,	lat,	heating_design_temperature,	elevation,	frost_days,	incentive_count_residential,	cooling_design_temperature,	cooling_degree_days,	fips,	earth_temperature_amplitude,	electricity_price_commercial,	electricity_price_overall,	tile_count_residential,	total_panel_area, daily_solar_radiation
              from ds1_new')
head(ds_20f)
data_corr<-as.data.frame(cor(ds_20f))
#filtering features whcih are highly correlated
ds_20f<-sqldf('select relative_humidity,	lon,	air_temperature,	incentive_count_nonresidential,	atmospheric_pressure,	lat,	incentive_count_residential,	cooling_design_temperature,	fips,	earth_temperature_amplitude,	electricity_price_commercial,	electricity_price_overall,	tile_count_residential,	total_panel_area, daily_solar_radiation
              from ds1_new')
head(ds_20f)

#5 layers
ds_20f<-sqldf('select relative_humidity,	lon,	air_temperature,	incentive_count_nonresidential,	atmospheric_pressure,	lat,	incentive_count_residential,	cooling_design_temperature,	fips,	earth_temperature_amplitude,	electricity_price_commercial,	electricity_price_overall,	tile_count_residential,	total_panel_area, daily_solar_radiation
              from ds1_new')
head(ds_20f)
train <- sample(nrow(ds_20f), 0.7*nrow(ds_20f), replace = FALSE)
TrainSet <- ds_20f[train,]
ValidSet <- ds_20f[-train,]
library("neuralnet")
net.ds <- neuralnet(daily_solar_radiation~.,TrainSet, hidden=5, threshold=0.01)
net.results <- compute(net.ds, ds_20f)
ds_20f$predicted<-net.results$net.result
ds_20f$residuals<-ds_20f$daily_solar_radiation-ds_20f$predicted
library(ggplot2)
g1<-ggplot(ds_20f,aes(x=daily_solar_radiation,y=predicted))+geom_point(aes(color=abs(residuals)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()
mse(ds_20f$daily_solar_radiation,ds_20f$predicted)

#10 layers
ds_20f<-sqldf('select relative_humidity,	lon,	air_temperature,	incentive_count_nonresidential,	atmospheric_pressure,	lat,	incentive_count_residential,	cooling_design_temperature,	fips,	earth_temperature_amplitude,	electricity_price_commercial,	electricity_price_overall,	tile_count_residential,	total_panel_area, daily_solar_radiation
              from ds1_new')
head(ds_20f)
train <- sample(nrow(ds_20f), 0.7*nrow(ds_20f), replace = FALSE)
TrainSet <- ds_20f[train,]
ValidSet <- ds_20f[-train,]
library("neuralnet")
net.ds <- neuralnet(daily_solar_radiation~.,TrainSet, hidden=10, threshold=0.01)
net.results <- compute(net.ds, ds_20f)
ds_20f$predicted<-net.results$net.result
ds_20f$residuals<-ds_20f$daily_solar_radiation-ds_20f$predicted
library(ggplot2)
g2<-ggplot(ds_20f,aes(x=daily_solar_radiation,y=predicted))+geom_point(aes(color=abs(residuals)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()
mse(ds_20f$daily_solar_radiation,ds_20f$predicted)

#20 layers
ds_20f<-sqldf('select relative_humidity,	lon,	air_temperature,	incentive_count_nonresidential,	atmospheric_pressure,	lat,	incentive_count_residential,	cooling_design_temperature,	fips,	earth_temperature_amplitude,	electricity_price_commercial,	electricity_price_overall,	tile_count_residential,	total_panel_area, daily_solar_radiation
              from ds1_new')
head(ds_20f)
train <- sample(nrow(ds_20f), 0.7*nrow(ds_20f), replace = FALSE)
TrainSet <- ds_20f[train,]
ValidSet <- ds_20f[-train,]
library("neuralnet")
net.ds <- neuralnet(daily_solar_radiation~.,TrainSet, hidden=20, threshold=0.01)
net.results <- compute(net.ds, ds_20f)
ds_20f$predicted<-net.results$net.result
ds_20f$residuals<-ds_20f$daily_solar_radiation-ds_20f$predicted
library(ggplot2)
g3<-ggplot(ds_20f,aes(x=daily_solar_radiation,y=predicted))+geom_point(aes(color=abs(residuals)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()
mse(ds_20f$daily_solar_radiation,ds_20f$predicted)
library(ggpubr)
figure <- ggarrange(g1, g2, g3,
                    labels = c("5 layers", "10 layers", "20 layers"),
                    ncol = 1, nrow = 3)
figure
#Neural network with all features
#5 layers
train <- sample(nrow(ds1_new), 0.7*nrow(ds1_new), replace = FALSE)
ds1_new$predicted<-NULL
TrainSet <- ds1_new[train,]
ValidSet <- ds1_new[-train,]
library("neuralnet")
net.ds <- neuralnet(daily_solar_radiation~.,TrainSet, hidden=5, threshold=0.01)
net.results <- compute(net.ds, ds1_new)
ds1_new$predicted<-net.results$net.result
ds1_new$residuals<-ds1_new$daily_solar_radiation-ds1_new$predicted
library(ModelMetrics)
library(ggplot2)
g1<-ggplot(ds1_new,aes(x=daily_solar_radiation,y=predicted))+geom_point(aes(color=abs(residuals)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()
mse(ds1_new$daily_solar_radiation,ds1_new$predicted)

#10 layers
train <- sample(nrow(ds1_new), 0.7*nrow(ds1_new), replace = FALSE)
ds1_new$predicted<-NULL
TrainSet <- ds1_new[train,]
ValidSet <- ds1_new[-train,]
library("neuralnet")
net.ds <- neuralnet(daily_solar_radiation~.,TrainSet, hidden=10, threshold=0.01)
net.results <- compute(net.ds, ds1_new)
ds1_new$predicted<-net.results$net.result
ds1_new$residuals<-ds1_new$daily_solar_radiation-ds1_new$predicted
library(ModelMetrics)
library(ggplot2)
g2<-ggplot(ds1_new,aes(x=daily_solar_radiation,y=predicted))+geom_point(aes(color=abs(residuals)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()
mse(ds1_new$daily_solar_radiation,ds1_new$predicted)

#20 layers
train <- sample(nrow(ds1_new), 0.7*nrow(ds1_new), replace = FALSE)
ds1_new$predicted<-NULL
TrainSet <- ds1_new[train,]
ValidSet <- ds1_new[-train,]
library("neuralnet")
net.ds <- neuralnet(daily_solar_radiation~.,TrainSet, hidden=20, threshold=0.01)
net.results <- compute(net.ds, ds1_new)
ds1_new$predicted<-net.results$net.result
ds1_new$residuals<-ds1_new$daily_solar_radiation-ds1_new$predicted
library(ModelMetrics)
library(ggplot2)
g3<-ggplot(ds1_new,aes(x=daily_solar_radiation,y=predicted))+geom_point(aes(color=abs(residuals)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()
mse(ds1_new$daily_solar_radiation,ds1_new$predicted)
library(ggpubr)
figure <- ggarrange(g1, g2, g3,
                    labels = c("5 layers", "10 layers", "20 layers"),
                    ncol = 1, nrow = 3)
figure

