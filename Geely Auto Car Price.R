################### Car Price Analysis ###################
###################      Geely Auto      ###################

#Install packages
library(ggplot2)
library(Rcpp)
library(car)
library(MASS)
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)

#Importing dataset
car <- read.csv('CarPrice_Assignment.csv', header = T)

### ========== DATA UNDERSTANDING ========== ###
str(car)
summary(factor(car$symboling))
summary(factor(car$fueltype))
summary(factor(car$aspiration))
summary(factor(car$doornumber))
summary(factor(car$carbody))
summary(factor(car$drivewheel))
summary(factor(car$enginelocation))
summary(factor(car$enginetype))
summary(factor(car$cylindernumber))
summary(factor(car$fuelsystem))

### ========== DATA CLEANING & DATA PREPARATION ========== ###

#Checking number of NA values
sapply(car, function(x) sum(is.na(x)))  #No NA's

#Spell check
car1 <- car
car1 <- separate(car, CarName, into = c("CarBrand", "CarModel"), sep = ' ', extra = 'merge') 
car1$CarModel[which(is.na(car1$CarModel))] <- 'NA'
sum(is.na(car1$CarModel))
sum(is.na(car1$CarBrand))
summary(factor(car1$CarBrand))    #Spell mistakes
car1$CarBrand[which(car1$CarBrand == 'alfa-romero')] <- 'alfa-romeo'
car1$CarBrand[which(car1$CarBrand == 'maxda')] <- 'mazda'
car1$CarBrand[which(car1$CarBrand == 'Nissan')] <- 'nissan'
car1$CarBrand[which(car1$CarBrand == 'porcshce')] <- 'porsche'
car1$CarBrand[which(car1$CarBrand == 'toyouta')] <- 'toyota'
car1$CarBrand[which(car1$CarBrand == 'vokswagen')] <- 'volkswagen'
car1$CarBrand[which(car1$CarBrand == 'vw')] <- 'volkswagen'

summary(factor(car1$CarModel))
car1$CarModel[which(car1$CarModel == '100 ls')] <- '100ls'

# Check and fix Outliers
quantile(car1$wheelbase, seq(0,1,0.01))
quantile(car1$carlength, seq(0,1,0.01))
quantile(car1$carwidth, seq(0,1,0.01))
quantile(car1$carheight, seq(0,1,0.01))
quantile(car1$curbweight, seq(0,1,0.01))
quantile(car1$boreratio, seq(0,1,0.01))
quantile(car1$stroke, seq(0,1,0.01))

quantile(car1$compressionratio, seq(0,1,0.01))  ##
car1$compressionratio[which(car1$compressionratio > 10.9400)] <- 10.9400

quantile(car1$horsepower, seq(0,1,0.01)) ##
car1$horsepower[which(car1$horsepower > 207.00)] <- 207.00

quantile(car1$peakrpm, seq(0,1,0.01))
quantile(car1$citympg, seq(0,1,0.01))
quantile(car1$highwaympg, seq(0,1,0.01))
# Outliers were found in compressionratio and horsepower 

#Bivariate Analysis of Price and Categorical variables to understand dependencies
ggplot(car1, aes(factor(fueltype), price)) + geom_point(alpha = 0.3) + theme_light()
ggplot(car1, aes(factor(CarBrand), price)) + geom_point(alpha = 0.3) + theme_light()
ggplot(car1, aes(factor(symboling), price)) + geom_point(alpha = 0.3) + theme_light()
ggplot(car1, aes(factor(doornumber), price)) + geom_point(alpha = 0.3) + theme_light()
ggplot(car1, aes(factor(carbody), price)) + geom_point(alpha = 0.3) + theme_light()
ggplot(car1, aes(factor(drivewheel), price)) + geom_point(alpha = 0.3) + theme_light()
ggplot(car1, aes(factor(enginelocation), price)) + geom_point(alpha = 0.3) + theme_light()
ggplot(car1, aes(factor(enginetype), price)) + geom_point(alpha = 0.3) + theme_light()
ggplot(car1, aes(factor(cylindernumber), price)) + geom_point(alpha = 0.3) + theme_light()
ggplot(car1, aes(factor(fuelsystem), price)) + geom_point(alpha = 0.3) + theme_light()

# Treating Categorical Variables with 2 levels
levels(car1$fueltype) <- c(1,0)
car1$fueltype <- as.numeric(levels(car1$fueltype))[car1$fueltype]

levels(car1$aspiration) <- c(1,0)
car1$aspiration <- as.numeric(levels(car1$aspiration))[car1$aspiration]

levels(car1$doornumber) <- c(1,0)
car1$doornumber <- as.numeric(levels(car1$doornumber))[car1$doornumber]

levels(car1$enginelocation) <- c(1,0)
car1$enginelocation <- as.numeric(levels(car1$enginelocation))[car1$enginelocation]

# Treating Categorical Variables with >2 levels by creating dummies
# CarBrand
dummy1 <- data.frame(model.matrix(~CarBrand, data = car1))
dummy1 <- dummy1[,-1]
car_1 <- cbind(car1[,-3], dummy1)
#Carbody
dummy2 <- data.frame(model.matrix(~carbody, car1))
dummy2 <- dummy2[,-1]
car_1 <- cbind(car_1[,-7], dummy2)
#drivewheel
dummy3 <- data.frame(model.matrix(~drivewheel, car1))
dummy3 <- dummy3[,-1]
car_1 <- cbind(car_1[,-7], dummy3)
#enginetype
dummy4 <- data.frame(model.matrix(~enginetype, car1))
dummy4 <- dummy4[,-1]
car_1 <- cbind(car_1[,-13], dummy4)
#cylindernumber
dummy5 <- data.frame(model.matrix(~cylindernumber, car1))
dummy5 <- dummy5[,-1]
car_1 <- cbind(car_1[,-13], dummy5)
#CarModel
dummy6 <- data.frame(model.matrix(~CarModel, car1))
dummy6 <- dummy6[,-1]
car_1 <- cbind(car_1[,-3], dummy6)
#fuelsystem
dummy7 <- data.frame(model.matrix(~fuelsystem, car1))
dummy7 <- dummy7[,-1]
car_1 <- cbind(car_1[,-12], dummy7)
#symboling
car1$symboling <- as.factor(car1$symboling)  #Since they are numeric values
dummy8 <- data.frame(model.matrix(~symboling, car1))
dummy8 <- dummy8[,-1]
car_1 <- cbind(car_1[,-2], dummy8)  #symboling.1 is symboling = -1

# Creating Training and Testing Data set using Sampling
set.seed(12345)
trainindices= sample(1:nrow(car_1), 0.7*nrow(car_1))
# generate the train data set
train = car_1[trainindices,]
#Similarly store the rest of the observations into "test".
test = car_1[-trainindices,]


### ========== DATA MODELING & EVALUATION ========== ###

model1 <- lm(formula = price~., train)
summary(model1)   

#Using Step AIC method to reduce variables which are not significant, since we have >200 variables
step1 <- stepAIC(model1, direction = 'both')

#Taking off all the dummy variables of carmodel 
model2 <- lm(price ~ CarBrandbmw + compressionratio + peakrpm + CarBrandmitsubishi + 
               CarBrandpeugeot + CarBrandplymouth + CarBrandrenault + CarBrandsubaru + 
               CarBrandtoyota + CarBrandvolkswagen + fueltype + enginelocation + wheelbase + 
               carlength + carwidth + curbweight + cylindernumberfour + 
               symboling.1 + symboling1 + symboling2 + aspiration + fuelsystem2bbl + 
               fuelsystemidi + fuelsystemmpfi + boreratio + highwaympg + CarBrandchevrolet + 
               CarBrandhonda + CarBrandaudi + CarBrandbuick + CarBranddodge + CarBrandisuzu + 
               CarBrandisuzu + CarBrandjaguar + CarBrandmazda + CarBrandmercury + CarBrandnissan + 
               CarBrandvolvo + CarBrandsaab + CarBrandporsche + carbodyhardtop + carbodysedan + 
               carbodywagon + carbodyhatchback + enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcv + 
               enginetyperotor + cylindernumbersix + cylindernumbertwelve, train)
summary(model2)  
step2 <- stepAIC(model2, direction = 'both')

#Building 3rd model using variables in step2
model3 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
               CarBrandpeugeot + CarBrandplymouth + CarBrandrenault + CarBrandsubaru + 
               CarBrandtoyota + CarBrandvolkswagen + fueltype + enginelocation + wheelbase + 
               carwidth + curbweight + 
               symboling.1 + symboling1 + symboling2 + aspiration + fuelsystem2bbl + 
               fuelsystemidi + fuelsystemmpfi + boreratio + highwaympg + 
               CarBrandaudi + CarBrandbuick + CarBranddodge + CarBrandisuzu + 
               CarBrandjaguar + CarBrandmazda + CarBrandmercury + CarBrandnissan + 
               CarBrandporsche + carbodyhardtop + carbodysedan + 
               carbodywagon + carbodyhatchback + enginetypeohc + enginetypeohcv + 
               enginetyperotor + cylindernumbertwelve, train)
summary(model3)
step3 <- stepAIC(model3, direction = 'both')

#4th Model
model4 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
               CarBrandpeugeot + CarBrandplymouth + CarBrandrenault + CarBrandsubaru + 
               CarBrandtoyota + CarBrandvolkswagen + fueltype + enginelocation + wheelbase + 
               carwidth + curbweight + 
               symboling.1 + symboling1 + fuelsystem2bbl + 
               fuelsystemidi + fuelsystemmpfi + highwaympg + 
               CarBrandbuick + CarBranddodge + 
               CarBrandjaguar + CarBrandmazda + CarBrandmercury + CarBrandnissan + 
               CarBrandporsche + carbodysedan + 
               carbodywagon + carbodyhatchback + enginetypeohc + 
               enginetyperotor + cylindernumbertwelve, train)
summary(model4)
step4 <- stepAIC(model4, direction = 'both')

#Eliminate fuelsystemidi and CarBrandmercury
model5 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
               CarBrandpeugeot + CarBrandplymouth + CarBrandrenault + CarBrandsubaru + 
               CarBrandtoyota + CarBrandvolkswagen + fueltype + enginelocation + wheelbase + 
               carwidth + curbweight + 
               symboling.1 + symboling1 + fuelsystem2bbl + 
               fuelsystemmpfi + highwaympg + 
               CarBrandbuick + CarBranddodge + 
               CarBrandjaguar + CarBrandmazda + CarBrandnissan + 
               CarBrandporsche + carbodysedan + 
               carbodywagon + carbodyhatchback + enginetypeohc + 
               enginetyperotor + cylindernumbertwelve, train)
summary(model5)
vif(model5)

#Let's now eliminate 1 variable at time by looking at p and VIF values
#Remove highwaympg
model6 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
               CarBrandpeugeot + CarBrandplymouth + CarBrandrenault + CarBrandsubaru + 
               CarBrandtoyota + CarBrandvolkswagen + fueltype + enginelocation + wheelbase + 
               carwidth + curbweight + 
               symboling.1 + symboling1 + fuelsystem2bbl + 
               fuelsystemmpfi + 
               CarBrandbuick + CarBranddodge + 
               CarBrandjaguar + CarBrandmazda + CarBrandnissan + 
               CarBrandporsche + carbodysedan + 
               carbodywagon + carbodyhatchback + enginetypeohc + 
               enginetyperotor + cylindernumbertwelve, train)
summary(model6)
vif(model6)

#Correlation check for car dimensions(wheelbase, carwidth) and weight
cor(train$curbweight, train$carwidth)
cor(train$carwidth, train$wheelbase)
cor(train$curbweight, train$wheelbase)
#since all 3 are highly correlated, will consider only one which gives better r-squared value

model7 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
               CarBrandpeugeot + CarBrandplymouth + CarBrandrenault + CarBrandsubaru + 
               CarBrandtoyota + CarBrandvolkswagen + fueltype + enginelocation + 
               carwidth + 
               symboling.1 + symboling1 + fuelsystem2bbl + 
               fuelsystemmpfi + 
               CarBrandbuick + CarBranddodge + 
               CarBrandjaguar + CarBrandmazda + CarBrandnissan + 
               CarBrandporsche + carbodysedan + 
               carbodywagon + carbodyhatchback + enginetypeohc + 
               enginetyperotor + cylindernumbertwelve, train)
#Consideting curbweight (Car weigth) since it gives best r squared value from other 2 variables
summary(model7)
vif(model7)

#Removing fuelsystemmpfi
model8 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
               CarBrandpeugeot + CarBrandplymouth + CarBrandrenault + CarBrandsubaru + 
               CarBrandtoyota + CarBrandvolkswagen + fueltype + enginelocation + 
               curbweight + 
               symboling.1 + symboling1 + fuelsystem2bbl + 
               CarBrandbuick + CarBranddodge + 
               CarBrandjaguar + CarBrandmazda + CarBrandnissan + 
               CarBrandporsche + carbodysedan + 
               carbodywagon + carbodyhatchback + enginetypeohc + 
               enginetyperotor + cylindernumbertwelve, train)
summary(model8)
vif(model8)

#Removing fuelsystem2bbl
model9 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
               CarBrandpeugeot + CarBrandplymouth + CarBrandrenault + CarBrandsubaru + 
               CarBrandtoyota + CarBrandvolkswagen + fueltype + enginelocation + 
               curbweight + 
               symboling.1 + symboling1 + 
               CarBrandbuick + CarBranddodge + 
               CarBrandjaguar + CarBrandmazda + CarBrandnissan + 
               CarBrandporsche + carbodysedan + 
               carbodywagon + carbodyhatchback + enginetypeohc + 
               enginetyperotor + cylindernumbertwelve, train)
summary(model9)
vif(model9)

#Removing fueltype
model10 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandrenault + CarBrandsubaru + 
                CarBrandtoyota + CarBrandvolkswagen + enginelocation + curbweight + 
                symboling.1 + symboling1 + CarBrandbuick + CarBranddodge + 
                CarBrandjaguar + CarBrandmazda + CarBrandnissan + CarBrandporsche + carbodysedan + 
                carbodywagon + carbodyhatchback + enginetypeohc + enginetyperotor + 
                cylindernumbertwelve, train)
summary(model10)
vif(model10)

#Removing cylindernumbertwelve
model11 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandrenault + CarBrandsubaru + 
                CarBrandtoyota + CarBrandvolkswagen + enginelocation + curbweight + 
                symboling.1 + symboling1 + CarBrandbuick + CarBranddodge + 
                CarBrandjaguar + CarBrandmazda + CarBrandnissan + CarBrandporsche + carbodysedan + 
                carbodywagon + carbodyhatchback + enginetypeohc + enginetyperotor, train)
summary(model11)
vif(model11)

#Removing symboling.1 - highest p value
model12 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandrenault + CarBrandsubaru + 
                CarBrandtoyota + CarBrandvolkswagen + enginelocation + curbweight + symboling1 + CarBrandbuick + CarBranddodge + 
                CarBrandjaguar + CarBrandmazda + CarBrandnissan + CarBrandporsche + carbodysedan + 
                carbodywagon + carbodyhatchback + enginetypeohc + enginetyperotor, train)
summary(model12)
vif(model12)

#Removing CarBrandvolkswagen
model13 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandrenault + CarBrandsubaru + 
                CarBrandtoyota + enginelocation + curbweight + symboling1 + CarBrandbuick + CarBranddodge + 
                CarBrandjaguar + CarBrandmazda + CarBrandnissan + CarBrandporsche + carbodysedan + 
                carbodywagon + carbodyhatchback + enginetypeohc + enginetyperotor, train)
summary(model13)
vif(model13)

#Removing CarBrandmazda
model14 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandrenault + CarBrandsubaru + 
                CarBrandtoyota + enginelocation + curbweight + symboling1 + CarBrandbuick + CarBranddodge + 
                CarBrandjaguar + CarBrandnissan + CarBrandporsche + carbodysedan + 
                carbodywagon + carbodyhatchback + enginetypeohc + enginetyperotor, train)
summary(model14)
vif(model14)

#Removing CarBrandrenault
model15 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandsubaru + 
                CarBrandtoyota + enginelocation + curbweight + symboling1 + CarBrandbuick + CarBranddodge + 
                CarBrandjaguar + CarBrandnissan + CarBrandporsche + carbodysedan + 
                carbodywagon + carbodyhatchback + enginetypeohc + enginetyperotor, train)
summary(model15)
vif(model15)

#Remove enginetyperotor
model16 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandplymouth + CarBrandsubaru + 
                CarBrandtoyota + enginelocation + curbweight + symboling1 + CarBrandbuick + CarBranddodge + 
                CarBrandjaguar + CarBrandnissan + CarBrandporsche + carbodysedan + 
                carbodywagon + carbodyhatchback + enginetypeohc, train)
summary(model16)
vif(model16)

#Removing CarBrandplymouth
model17 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandsubaru + 
                CarBrandtoyota + enginelocation + curbweight + symboling1 + CarBrandbuick + CarBranddodge + 
                CarBrandjaguar + CarBrandnissan + CarBrandporsche + carbodysedan + 
                carbodywagon + carbodyhatchback + enginetypeohc, train)
summary(model17)
vif(model17)

#Removing symboling1
model18 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandsubaru + 
                CarBrandtoyota + enginelocation + curbweight + CarBrandbuick + CarBranddodge + 
                CarBrandjaguar + CarBrandnissan + CarBrandporsche + carbodysedan + 
                carbodywagon + carbodyhatchback + enginetypeohc, train)
summary(model18)
vif(model18)

#Removing CarBranddodge
model19 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandsubaru + 
                CarBrandtoyota + enginelocation + curbweight + CarBrandbuick + 
                CarBrandjaguar + CarBrandnissan + CarBrandporsche + carbodysedan + 
                carbodywagon + carbodyhatchback + enginetypeohc, train)
summary(model19)
vif(model19)

#Removing carbodysedan
model20 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandsubaru + 
                CarBrandtoyota + enginelocation + curbweight + CarBrandbuick + 
                CarBrandjaguar + CarBrandnissan + CarBrandporsche + 
                carbodywagon + carbodyhatchback + enginetypeohc, train)
summary(model20)
vif(model20)

#Removing carbodyhatchback
model20 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandsubaru + 
                CarBrandtoyota + enginelocation + curbweight + CarBrandbuick + 
                CarBrandjaguar + CarBrandnissan + CarBrandporsche + 
                carbodywagon + enginetypeohc, train)
summary(model20)
vif(model20)

#Removing CarBrandnissan
model21 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandsubaru + 
                CarBrandtoyota + enginelocation + curbweight + CarBrandbuick + 
                CarBrandjaguar + CarBrandporsche + 
                carbodywagon + enginetypeohc, train)
summary(model21)
vif(model21)

#Removing CarBrandtoyota
model22 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandsubaru + 
                enginelocation + curbweight + CarBrandbuick + 
                CarBrandjaguar + CarBrandporsche + 
                carbodywagon + enginetypeohc, train)
summary(model22)
vif(model22)

#Removing carbodywagon
model23 <- lm(price ~ CarBrandbmw + compressionratio + CarBrandmitsubishi + 
                CarBrandpeugeot + CarBrandsubaru + 
                enginelocation + curbweight + CarBrandbuick + 
                CarBrandjaguar + CarBrandporsche + 
                enginetypeohc, train)
summary(model23)
vif(model23)

#Removing CarBrandmitsubishi
model24 <- lm(price ~ CarBrandbmw + compressionratio + 
                CarBrandpeugeot + CarBrandsubaru + 
                enginelocation + curbweight + CarBrandbuick + 
                CarBrandjaguar + CarBrandporsche + 
                enginetypeohc, train)
summary(model24)
vif(model24)

#Removing compressionratio
model25 <- lm(price ~ CarBrandbmw + 
                CarBrandpeugeot + CarBrandsubaru + 
                enginelocation + curbweight + CarBrandbuick + 
                CarBrandjaguar + CarBrandporsche + 
                enginetypeohc, train)
summary(model25)
vif(model25)

#Removing CarBrandsubaru
model26 <- lm(price ~ CarBrandbmw + 
                CarBrandpeugeot + 
                enginelocation + curbweight + CarBrandbuick + 
                CarBrandjaguar + CarBrandporsche + enginetypeohc, train)
summary(model26)
vif(model26)

#Removing CarBrandporsche
model27 <- lm(price ~ CarBrandbmw + 
                CarBrandpeugeot + 
                enginelocation + curbweight + CarBrandbuick + 
                CarBrandjaguar + enginetypeohc, train)
summary(model27)
vif(model27)

#Removing CarBrandpeugeot
model28 <- lm(price ~ CarBrandbmw +
                enginelocation + curbweight + CarBrandbuick + 
                CarBrandjaguar + enginetypeohc, train)
summary(model28)
vif(model28)

#Removing enginetypeohc
model29 <- lm(price ~ CarBrandbmw + enginelocation + curbweight + 
                CarBrandbuick + CarBrandjaguar, train)
summary(model29)
vif(model29)
# Multiple R-squared:88.39% ~ 88% as approximately same as	Adj R-squared:87.96% ~ 88%,
# we can take this as final model


### ========== MODEL VALIDATION ========== ###
# Predict car price in testing dataset
predict1 <- predict(model29, test[,-19])
test$test_price <- predict1

# Calculate correlation
r <- cor(test$price, test$test_price)
#Calculating car squared
rsquared <- r^2
rsquared   # R sqaured % from testing data = 84.79%
#Difference between R sqaured of Training and Testing dataset is less than 4%


#Plotting observations (Actual Price and Predicted Price)
ggplot(test, aes(test$car_ID, test$price)) + geom_line(aes(colour = 'black')) + 
  scale_x_continuous(name = 'Car ID', breaks = seq(1, 205, 10), limits = c(1, 205)) +
  scale_y_continuous(name = 'Price', breaks = seq(0, 42000, 10000), limits = c(0, 42000)) +
  geom_line(aes(test$car_ID, test$test_price, colour = 'blue')) + theme_light()

#Error plots (Difference between Actual price and Predicted price)
test$error <- test$price - test$test_price
ggplot(test, aes(car_ID, error)) + geom_line() + 
  scale_x_continuous(name = 'Car ID', breaks = seq(1, 205, 5), limits = c(1, 205)) + 
  scale_y_continuous(name = 'Error', breaks = seq(-8000, 15000, 1000), limits = c(-8000, 15000)) + theme_bw() + 
  geom_hline(yintercept = 0, colour = 'brown') + theme_light()


# Identified 5 variables as most significant for predicting the car price
#####           Car Brand - BMW 
#####           Engine Location
#####           Car Weigth (curbweight)
#####           Car Brand - Buick
#####           Car Brand - Jaguar
