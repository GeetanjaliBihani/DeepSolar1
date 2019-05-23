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

#building different models using all variables and comparing % error
#linear regression
#all features
ds1_new<-DS1[complete.cases((DS1)),]
solarmodel1<-lm(daily_solar_radiation~., data=ds1_new)
summary(solarmodel)
library(ggplot2)
ds1_new$predicted1=solarmodel1$fitted.values
ds1_new$residuals1=solarmodel1$residuals

#selected features
library(caret)
a<-as.data.frame(varImp(solarmodel))
solarmodel2<-lm(daily_solar_radiation~wind_speed+heating_design_temperature+earth_temperature_amplitude+relative_humidity+voting_2016_dem_win+frost_days+cooling_design_temperature+cooling_degree_days+air_temperature+lon+housing_unit_median_value+lat+voting_2016_gop_percentage+earth_temperature, data=ds1_new)
library(ggplot2)
ds1_new$predicted2=solarmodel2$fitted.values
ds1_new$residuals2=solarmodel2$residuals

#plotting actual values with residuals to see how residuals are distributed in the data
lr1<-ggplot(ds1_new,aes(x=daily_solar_radiation, y=predicted1))+
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_point(aes(color=abs(residuals1)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+
  theme_classic()
lr2<-ggplot(ds1_new,aes(x=daily_solar_radiation, y=predicted2))+
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_point(aes(color=abs(residuals2)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+
  theme_classic()

library(ggpubr)
figure <- ggarrange(lr1, lr2,
                    labels = c("LR(All features) ", "LR(15 features)"),
                    ncol = 1, nrow = 2)
figure

mse(ds1_new$daily_solar_radiation,ds1_new$predicted1)
mse(ds1_new$daily_solar_radiation,ds1_new$predicted2)
cor(ds1_new$daily_solar_radiation,ds1_new$predicted1)
cor(ds1_new$daily_solar_radiation,ds1_new$predicted2)

library(plyr)
par(mfrow=c(1,2))
boxplot(ds1_new$residuals1)
boxplot(ds1_new$residuals2)

nrow(as.data.frame(boxplot.stats(ds1_new$residuals1)$out))
nrow(as.data.frame(boxplot.stats(ds1_new$residuals2)$out))


#Random forest
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

#all features
model1 <- randomForest(daily_solar_radiation ~ .,data = TrainSet, importance=TRUE)

#filtering features
varImpPlot(model1)


model1 <- randomForest(daily_solar_radiation ~ .,data = TrainSet, importance=TRUE)

model2 <- randomForest(daily_solar_radiation ~ relative_humidity+total_panel_area_residential+total_panel_area+tile_count+solar_panel_area_per_capita+solar_panel_area_divided_by_area+solar_system_count_residential+number_of_solar_system_per_household+tile_count_residential+wind_speed+solar_system_count+race_asian_rate+race_asian+race_other+total_panel_area_nonresidential,data = TrainSet, importance=TRUE)
model3 <- randomForest(daily_solar_radiation ~ relative_humidity+total_panel_area_residential+total_panel_area+tile_count,data = TrainSet, importance=TRUE)
model4 <- randomForest(daily_solar_radiation ~ relative_humidity+total_panel_area_residential+total_panel_area+tile_count+solar_panel_area_per_capita,data = TrainSet, importance=TRUE)
model5 <- randomForest(daily_solar_radiation ~ relative_humidity+total_panel_area_residential+total_panel_area+tile_count+solar_panel_area_per_capita+solar_panel_area_divided_by_area,data = TrainSet, importance=TRUE)

#predicting on validation dataset
ValidSet$predicted1<-predict(model1, newdata= ValidSet)
ValidSet$residuals1<-ValidSet$daily_solar_radiation-ValidSet$predicted1
ValidSet$predicted2<-predict(model2, newdata= ValidSet)
ValidSet$residuals2<-ValidSet$daily_solar_radiation-ValidSet$predicted2
ValidSet$predicted3<-predict(model3, newdata= ValidSet)
ValidSet$residuals3<-ValidSet$daily_solar_radiation-ValidSet$predicted3
ValidSet$predicted4<-predict(model4, newdata= ValidSet)
ValidSet$residuals4<-ValidSet$daily_solar_radiation-ValidSet$predicted4
ValidSet$predicted5<-predict(model5, newdata= ValidSet)
ValidSet$residuals5<-ValidSet$daily_solar_radiation-ValidSet$predicted5


rf1<-ggplot(ValidSet,aes(x=daily_solar_radiation,y=predicted1))+geom_point(aes(color=abs(residuals1)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()
rf2<-ggplot(ValidSet,aes(x=daily_solar_radiation,y=predicted2))+geom_point(aes(color=abs(residuals2)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()
rf3<-ggplot(ValidSet,aes(x=daily_solar_radiation,y=predicted3))+geom_point(aes(color=abs(residuals3)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()
rf4<-ggplot(ValidSet,aes(x=daily_solar_radiation,y=predicted4))+geom_point(aes(color=abs(residuals4)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()
rf5<-ggplot(ValidSet,aes(x=daily_solar_radiation,y=predicted5))+geom_point(aes(color=abs(residuals5)))+scale_color_gradient2(low = "White", mid = "Yellow", high = "Red")+theme_classic()

library(ggpubr)
figure <- ggarrange(rf1, rf2,
                    labels = c("RF (All features)", "RF (15 features)"),
                    ncol = 1, nrow = 2)
figure

library(ModelMetrics)
mse(ValidSet$daily_solar_radiation,ValidSet$predicted1)
mse(ValidSet$daily_solar_radiation,ValidSet$predicted2)
cor(ValidSet$daily_solar_radiation,ValidSet$predicted1)
cor(ValidSet$daily_solar_radiation,ValidSet$predicted2)

