library(R.utils)
library(crayon)
library(car)
library("lmtest")
library(ggplot2)
library(GGally)
library(caret)
library(olsrr)


# reading file
car_pricing = read.csv2("CarPrice_Assignment.csv", sep = ",", dec = ".")

str(car_pricing)

summary(car_pricing)

# analyze car price spread and distribution
# distribution plot
hist(car_pricing$price, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "price",
     main = "Car Price Distibution Plot")
lines(density(car_pricing$price), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

#box plot of spread
boxplot(car_pricing$price, main = "Car Price Spread",
        ylab = "Price",
        col = "blue3",
        border = "black",
        notch = TRUE)

# analyze count of categorical variables
# fuel type
ggplot(car_pricing,aes(fueltype, fill=fueltype)) + geom_bar()
# aspiration
ggplot(car_pricing,aes(aspiration, fill=aspiration)) + geom_bar()
# door number
ggplot(car_pricing,aes(doornumber, fill=doornumber)) + geom_bar()
# car body
ggplot(car_pricing,aes(carbody, fill=carbody)) + geom_bar()
# drive wheel
ggplot(car_pricing,aes(drivewheel, fill=drivewheel)) + geom_bar()
# enginelocation
ggplot(car_pricing,aes(enginelocation, fill=enginelocation)) + geom_bar()
# enginetype
ggplot(car_pricing,aes(enginetype, fill=enginetype)) + geom_bar()
# cylinder number
ggplot(car_pricing,aes(cylindernumber, fill=cylindernumber)) + geom_bar()
# fuel system
ggplot(car_pricing,aes(fuelsystem, fill=fuelsystem)) + geom_bar()

# clean and tidy categorical factors to their numerical values
## Clean and tidy num.of.doors
car_pricing$doornumber <- as.character(car_pricing$doornumber)
car_pricing$doornumber[car_pricing$doornumber == 'four'] <- 4
car_pricing$doornumber[car_pricing$doornumber == 'two'] <- 2
car_pricing$doornumber <- as.integer(car_pricing$doornumber)

## Clean and tidy cylindernumber
car_pricing$cylindernumber <- as.character(car_pricing$cylindernumber)
car_pricing$cylindernumber[car_pricing$cylindernumber == 'eight'] <- 8
car_pricing$cylindernumber[car_pricing$cylindernumber == 'five'] <- 5
car_pricing$cylindernumber[car_pricing$cylindernumber == 'four'] <- 4
car_pricing$cylindernumber[car_pricing$cylindernumber == 'six'] <- 6
car_pricing$cylindernumber[car_pricing$cylindernumber == 'three'] <- 3
car_pricing$cylindernumber[car_pricing$cylindernumber == 'twelve'] <- 12
car_pricing$cylindernumber[car_pricing$cylindernumber == 'two'] <- 2
car_pricing$cylindernumber <- as.integer(car_pricing$cylindernumber)

# description for our data using pairs
pairs(price~doornumber + cylindernumber + wheelbase + carlength + carwidth + carheight + curbweight + enginesize + boreratio + stroke + compressionratio + horsepower + peakrpm + citympg + highwaympg, data=car_pricing)
# description for our data using correaltion
ggcorr(subset(car_pricing, select = c('doornumber', 'wheelbase', 'carlength', 'carwidth', 'carheight','curbweight','cylindernumber','enginesize', 'boreratio', 'stroke', 'compressionratio', 'horsepower', 'peakrpm', 'citympg', 'highwaympg','price')), label = TRUE, label_size = 2.9, hjust = 1, layout.exp = 2)

# check for linearRegression
doorNumberCor <- cor.test(car_pricing$price, car_pricing$doornumber)
wheelBaseCor <- cor.test(car_pricing$price, car_pricing$wheelbase)
carLengthCor <- cor.test(car_pricing$price, car_pricing$carlength)
carWidthCor <- cor.test(car_pricing$price, car_pricing$carwidth)
carHeightCor <- cor.test(car_pricing$price, car_pricing$carheight)
curbWeightCor <- cor.test(car_pricing$price, car_pricing$curbweight)
cylinderNumberCor <- cor.test(car_pricing$price, car_pricing$cylindernumber)
engineSizeCor <- cor.test(car_pricing$price, car_pricing$enginesize)
boreratioCor <- cor.test(car_pricing$price, car_pricing$boreratio)
strokeCor <- cor.test(car_pricing$price, car_pricing$stroke)
compressionCor <- cor.test(car_pricing$price, car_pricing$compressionratio)
horseCor <- cor.test(car_pricing$price, car_pricing$horsepower)
peakRpmCor <- cor.test(car_pricing$price, car_pricing$peakrpm)
cityMpgCor <- cor.test(car_pricing$price, car_pricing$citympg)
highWayMpgCor <- cor.test(car_pricing$price, car_pricing$highwaympg)
test_correlation <- function(corVar, nameVar) {
  return(ifelse((corVar["estimate"] > 0.5 | corVar["estimate"] < -0.5 ) & isZero(corVar$p.value), green(paste("There is a linear relation with ",nameVar, "\n")), red(paste("There is no linear relation with ", nameVar, "\n"))))
}
cat(test_correlation(doorNumberCor, "DoorNumber"))
cat(test_correlation(wheelBaseCor, "WheelBase"))
cat(test_correlation(carLengthCor, "CarLength"))
cat(test_correlation(carWidthCor, "CarWidth"))
cat(test_correlation(carHeightCor, "CarHeight"))
cat(test_correlation(curbWeightCor, "CurbWeight"))
cat(test_correlation(cylinderNumberCor, "cylinderNumber"))
cat(test_correlation(engineSizeCor, "EngineSize"))
cat(test_correlation(boreratioCor, "BoreRation"))
cat(test_correlation(strokeCor, "Stroke"))
cat(test_correlation(compressionCor, "Compression"))
cat(test_correlation(horseCor, "HorsePower"))
cat(test_correlation(peakRpmCor, "PeakRPM"))
cat(test_correlation(cityMpgCor, "CityMPG"))
cat(test_correlation(highWayMpgCor, "HighWayMPG"))

# eliminating categorical variables
car_pricing <- subset(car_pricing, select = c('doornumber', 'wheelbase', 'carlength', 
                      'carwidth', 'carheight','curbweight','cylindernumber','enginesize', 
                      'boreratio', 'stroke', 'compressionratio', 'horsepower', 'peakrpm', 
                      'citympg', 'highwaympg','price'))

# split to test and training dataset
set.seed(123)
samplesize <- round(0.8 * nrow(car_pricing), 0)
index <- sample(seq_len(nrow(car_pricing)), size = samplesize)

data_train <- car_pricing[index, ]
data_test <- car_pricing[-index, ]

# calculer le modele de regression
reg <- lm(price ~ ., data = data_train)
summary(reg)
# supprimer la redendance
vif(reg)
# remove city mpg
car_pricing1 <- subset(car_pricing, select = c('doornumber', 'wheelbase', 'carlength', 
                                              'carwidth', 'carheight','curbweight','cylindernumber','enginesize', 
                                              'boreratio', 'stroke', 'compressionratio', 'horsepower', 'peakrpm', 
                                              'highwaympg','price'))
set.seed(123)
samplesize <- round(0.8 * nrow(car_pricing1), 0)
index <- sample(seq_len(nrow(car_pricing1)), size = samplesize)

data_train1 <- car_pricing1[index, ]
data_test1 <- car_pricing1[-index, ]

reg1 = lm(price~ . , data=data_train1)
summary(reg1)
vif(reg1)
# remove engine size
car_pricing2 <- subset(car_pricing, select = c('doornumber', 'wheelbase', 'carlength', 'carwidth', 
                                               'carheight','curbweight','cylindernumber', 'boreratio', 'stroke', 
                                               'compressionratio', 'horsepower', 'peakrpm', 'highwaympg','price'))
set.seed(123)
samplesize <- round(0.8 * nrow(car_pricing2), 0)
index <- sample(seq_len(nrow(car_pricing2)), size = samplesize)

data_train2 <- car_pricing2[index, ]
data_test2 <- car_pricing2[-index, ]

reg2 = lm(price~ . , data=data_train2)
summary(reg2)
vif(reg2)
# remove curb weight
car_pricing3 <- subset(car_pricing, select = c('doornumber', 'wheelbase', 'carlength', 'carwidth', 
                                               'carheight','cylindernumber', 'boreratio', 'stroke', 
                                               'compressionratio', 'horsepower', 'peakrpm', 'highwaympg','price'))
set.seed(123)
samplesize <- round(0.8 * nrow(car_pricing3), 0)
index <- sample(seq_len(nrow(car_pricing3)), size = samplesize)

data_train3 <- car_pricing3[index, ]
data_test3 <- car_pricing3[-index, ]

reg3 = lm(price~ . , data=data_train3)
summary(reg3)
vif(reg3)

# Backward tracing
step(reg3, trace = TRUE, direction = "backward")

reg4 = lm(price~wheelbase + carwidth + cylindernumber + boreratio + compressionratio + 
            horsepower + highwaympg, data=data_train3)
summary(reg4)

# Forward Tracing With constant regression
step(lm(price~1, data=data_train3), scope=~.+doornumber + wheelbase + carlength + carwidth + carheight + 
       cylindernumber + boreratio + stroke + compressionratio + 
       horsepower + peakrpm + highwaympg, direction="forward", trace = TRUE)

reg5 = lm(price ~ horsepower + carwidth + cylindernumber + 
            wheelbase + compressionratio + highwaympg + boreratio, data=data_train3)
summary(reg5)

# removing the non significant variables
reg6 = lm(price ~ horsepower + cylindernumber + 
            wheelbase + compressionratio + highwaympg, data=data_train3)
summary(reg6)

# verification of outliers
plot(reg6$fitted.values, rstudent(reg6), xlab="Valeurs Prédites", ylab="Résidus studentisées")
abline(h=c(2,-2), col="red")

plot(cooks.distance(reg6), type="h", ylab="Distance de cook")
par(mfrow=c(1,2))

#removing outliers
data_train3 = data_train3[-c(6),]

reg7 = lm(price ~ horsepower + cylindernumber + 
            wheelbase + compressionratio + highwaympg, data=data_train3)
summary(reg7)

#model performance
lm_pred <- predict(reg7, newdata = data_test3 %>% select(-price))

# RMSE of train dataset
RMSE(pred = reg7$fitted.values, obs = data_train3$price)

# RMSE of test dataset
RMSE(pred = lm_pred, obs = data_test3$price)

#linearty test
resact <- data.frame(residual = reg7$residuals, fitted = reg7$fitted.values)

resact %>% ggplot(aes(fitted, residual)) + geom_point() + geom_smooth() + geom_hline(aes(yintercept = 0)) + 
  theme(panel.grid = element_blank(), panel.background = element_blank())

# normality test
hist(rstudent(reg7),prob=TRUE,main="Distribution des résidus",xlab="residuals")
lines(density(rstudent(reg7)),col="red")

qqnorm(rstudent(reg7),datax=TRUE,main="Q-Q plot des résidus")
qqline(rstudent(reg7),datax=TRUE,col="red")

shapiro.test(rstudent(reg7))

# Heteroscedasticity Test
plot(reg7$fitted.values,rstudent(reg7),main = "Heteroscedasticity",xlab="Valeurs prédites",ylab = "Valeurs résiduelles") 
abline(h=0,col="red")

bptest(reg7)

# autocorrelation test
dwtest(reg7)
