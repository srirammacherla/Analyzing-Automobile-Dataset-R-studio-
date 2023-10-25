datacsv <- read.csv("~/Downloads/datacsv.csv")
View(datacsv)

cars <-datacsv <- read.csv("~/Downloads/datacsv.csv")
names(cars) <- c("mpg","cylinders","displacement","horsepower","weight","acceleration","model_year","origin","car_name")
str(cars)

cars$horsepower <- as.numeric(cars$horsepower)
cars$horsepower[cars$horsepower=="?"] <- NA
cars$cylinders <- as.numeric(cars$cylinders)
kable(head(cars),format ="pandoc", caption ="Data Preview")

library(tidyverse)

new_data <- select(cars,mpg,displacement,horsepower,weight,acceleration)
new_data <- na.omit(new_data)
kable(summary(new_data), format="pandoc", caption = "Summery Statistics")

cars.first <- new_data[1:300,]
cars.last <- new_data[301:398,]

#mpg vs displacement

Linear_Reg.displace <- lm(mpg~displacement, data=cars.first)
summary(Linear_Reg.displace)

par(mfrow=c(2,2))
residual <- Linear_Reg.displace$residuals
plot(cars.first$mpg~residual,lwd=3, col="blue",main="mpg vs residual", xlab="residual",ylab = "mpg")
grid(NA, 5, lwd = 2,col = "darkgray")

hist(residual,prob=T,breaks=20,main="HISTOGRAM OF ACCELERATION RESIDUALS",xlab="Residuals")
lines(density(residual),col="red",lwd=3)


#mpg vs horsepower

Linear_Reg.horse_power <- lm(mpg~horsepower, data=cars.first)
summary(Linear_Reg.horse_power)

par(mfrow=c(2,2))
residual <- Linear_Reg.horse_power$residuals
plot(cars.first$mpg~residual,lwd=3, col="blue",main="mpg vs residual", xlab="residual",ylab = "mpg")
grid(NA, 5, lwd = 2,col = "darkgray")

hist(residual,prob=T,breaks=20,main="HISTOGRAM OF ACCELERATION RESIDUALS",xlab="Residuals")
lines(density(residual),col="red",lwd=3)

#mpg vs acceleration

Linear_Reg.acc <- lm(mpg~acceleration, data=cars.first)
summary(Linear_Reg.acc)

par(mfrow=c(2,2))
residual <- Linear_Reg.acc$residuals
plot(cars.first$mpg ~ residual, lwd = 3, col = "blue", main = "mpg vs residual", xlab = "residual", ylab = "mpg")
grid(NA, 5, lwd = 2,col = "darkgray")

hist(residual,prob=T,breaks=20,main="HISTOGRAM OF ACCELERATION RESIDUALS",xlab="Residuals")
lines(density(residual), col = "red", lwd = 3)




#mpg vs weight


Linear_Reg.wei <- lm(mpg~weight, data=cars.first)
summary(Linear_Reg.wei)

par(mfrow=c(2,2))
residual <- Linear_Reg.wei$residuals
plot(cars.first$mpg~residual,lwd=3, col="blue",main="mpg vs residual", xlab="residual",ylab = "mpg")
grid(NA, 5, lwd = 2,col = "darkgray")

hist(residual,prob=T,breaks=20,main="HISTOGRAM OF ACCELERATION RESIDUALS",xlab="Residuals")
lines(density(residual),col="red",lwd=3)


#predicted values with remaining values
library(dplyr)
library(Metrics)

predicted_displacement <- Linear_Reg.displace %>% predict(cars.last)
data.frame (R2 = R2(predicted_displacement, cars.last$mpg),
            RMSE = RMSE(predicted_displacement, cars.last$mpg),
            MAE = MAE(predicted_displacement, cars.last$mpg))
predictions_error <- RMSE(predicted_displacement, cars.last$mpg)/mean(cars.last$mpg)
compare_dis <- as.data.frame(cbind(cars.last$mpg,predicted_displacement),row=FALSE)
names(compare_dis) <- c("observ","predi_dis")
kable(head(compare_dis),format="pandoc", caption = "predicted values of mpg - displacement model")

predicted_horsepower <- Linear_Reg.horse_power %>% predict(cars.last)
data.frame( R2 = R2(predicted_horsepower, cars.last$mpg),
            RMSE = RMSE(predicted_horsepower, cars.last$mpg),
            MAE = MAE(predicted_horsepower, cars.last$mpg))
predictions_error <- RMSE(predicted_horsepower, cars.last$mpg)/mean(cars.last$mpg)
compare_hors <- as.data.frame(cbind(cars.last$mpg,predicted_horsepower),row=FALSE)
names(compare_hors) <- c("observ","predi_dis")
kable(head(compare_hors),format="pandoc", caption = "predicted values of mpg - horsepower model")


predicted_weight <- Linear_Reg.wei %>% predict(cars.last)
data.frame( R2 = R2(predicted_weight, cars.last$mpg),
            RMSE = RMSE(predicted_weight, cars.last$mpg),
            MAE = MAE(predicted_weight, cars.last$mpg))
predictions_error <- RMSE(predicted_weight, cars.last$mpg)/mean(cars.last$mpg)
compare_wei <- as.data.frame(cbind(cars.last$mpg,predicted_weight),row=FALSE)
names(compare_wei) <- c("observ","predi_dis")
kable(head(compare_wei),format="pandoc", caption = "predicted values of mpg - weight model")


predicted_acceleration <- Linear_Reg.acc %>% predict(cars.last)
data.frame( R2 = R2(predicted_acceleration, cars.last$mpg),
            RMSE = RMSE(predicted_acceleration, cars.last$mpg),
            MAE = MAE(predicted_acceleration, cars.last$mpg))
predictions_error <- RMSE(predicted_acceleration, cars.last$mpg)/mean(cars.last$mpg)
compare_acc <- as.data.frame(cbind(cars.last$mpg,predicted_acceleration),row=FALSE)
names(compare_acc) <- c("observ","predi_dis")
kable(head(compare_acc),format="pandoc", caption = "predicted values of mpg acceleration model")

