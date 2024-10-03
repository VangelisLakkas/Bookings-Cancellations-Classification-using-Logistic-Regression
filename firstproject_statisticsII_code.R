install.packages("readxl")
require(foreign)
require(xlsx)
library(readxl)
require(readxl)
library(lubridate)
require(ggplot2)
library(glmnet)
library(Matrix)
library(car)
library(aod)



##Data Importing
bookings_df <-read_excel('project2.xlsx')


sum(is.na(bookings_df))
anyNA.data.frame(bookings_df)


bookings_df$date.of.reservation <- ifelse(grepl("/", bookings_df$date.of.reservation),
                                          as.Date(bookings_df$date.of.reservation, format = "%m/%d/%Y"),
                                          as.Date(as.numeric(bookings_df$date.of.reservation), origin = "1899-12-30"))

bookings_df$date.of.reservation <- as.Date(bookings_df$date.of.reservation)
head(bookings_df$date.of.reservation)
length(unique(bookings_df$date.of.reservation))


class(bookings_df$date.of.reservation)

sum(is.na(bookings_df))
##After the conversion of the date of reservation column 2 NAs were produced since the number of NAs is very small compared to 
## the number of the observations I will use na.omit method to get rid of the NAs
bookings_df <- na.omit(bookings_df)


## Booking_ID is a unique identifier for each booking and not a feature that helps me 
##explain the cancelations of the bookings so I will remove it from the dataframe

bookings_df <- bookings_df[,-1]
bookings_df$month_of_reservation <- month(bookings_df$date.of.reservation)

bookings_df$average.price <- as.numeric(bookings_df$average.price)
bookings_df$number.of.adults <- as.numeric(bookings_df$number.of.adults)
bookings_df$number.of.children <- as.numeric(bookings_df$number.of.children)
bookings_df$number.of.weekend.nights <- as.numeric(bookings_df$number.of.weekend.nights)
bookings_df$number.of.week.nights <- as.numeric(bookings_df$number.of.week.nights)
bookings_df$special.requests <- as.numeric(bookings_df$special.requests)


bookings_df$type.of.meal <- as.factor(bookings_df$type.of.meal)
bookings_df$room.type <- as.factor(bookings_df$room.type)
bookings_df$market.segment.type <- as.factor(bookings_df$market.segment.type)
levels(bookings_df$market.segment.type)
bookings_df$car.parking.space <- as.factor(bookings_df$car.parking.space)
bookings_df$repeated <- as.factor(bookings_df$repeated)
bookings_df$month_of_reservation <- as.factor(bookings_df$month_of_reservation)


bookings_df$booking.status <- as.factor(bookings_df$booking.status)
levels(bookings_df$booking.status)

n <- nrow(bookings_df)
n


##Fitting the null model
mconstant <- glm(booking.status~1, data = bookings_df, family = "binomial"(link="logit"))
summary(mconstant)

mfull <-  glm(booking.status~., data = bookings_df, family = "binomial"(link="logit"))
summary(mfull)

step(mfull, trace = TRUE, direction = 'both', k=log(n))


lmatrix1 <- model.matrix(mfull)[,-1]
Lmatrix <- as.matrix(bookings_df[,c(1:15,17)])

fit_lasso <- glmnet(Lmatrix,bookings_df$booking.status, alpha=1, family="binomial")

plot(fit_lasso, label = TRUE)
plot(fit_lasso, xvar = "lambda", label = TRUE)

plot(lasso.cv)
coef(lasso.cv, s = lasso.cv$lambda.min)	# min is somewhat sensitive to specific run
coef(lasso.cv, s = lasso.cv$lambda.1se)	# the most regularized model

m1 <- glm(booking.status~number.of.weekend.nights+number.of.week.nights+car.parking.space+repeated+average.price+special.requests, data = bookings_df, family = "binomial"(link="logit"))
summary(m1)

print(logLik(m1));print(logLik(mconstant))

step(m1, trace=TRUE, direction = 'backward', k=log(n))

m2 <- glm(booking.status~number.of.weekend.nights+lead.time+car.parking.space+average.price+special.requests, data = bookings_df, family = "binomial"(link="logit"))
summary(m2)
print(logLik(m2))
print(logLik(mconstant))
with(m2, pchisq(deviance, df.residual, lower.tail = FALSE)) #Good fit, Deviance of m2 follows chi-square distribution 

mcfadden_pseudo_R2 <- 1 - (logLik(m2)/logLik(mconstant))
mcfadden_pseudo_R2

step(m2, trace=TRUE, direction = 'backward', k=log(n)) ##the implementation of step-wise method with the BIC criterion doesn't give us a model with less covariates
## so I will continue with m2 model

probit_m2 <- glm(booking.status~number.of.weekend.nights+lead.time+car.parking.space+average.price+special.requests, data = bookings_df, family = "binomial"(link="probit"))
summary(probit_m2)

# Calculate Cox-Snell pseudo-R^2 for m2
Cox_Snell_R2 <- 1 - exp((2/nobs(m2)) * (logLik(mconstant) - logLik(m2)))

# Calculate Nagelkerke pseudo-R^2 for m2
Nagelkerke_R2 <- Cox_Snell_R2 / (1 - exp((2/nobs(m2)) * logLik(mconstant)))

print(Cox_Snell_R2)
print(Nagelkerke_R2)
print(logLik(m2)/logLik(mconstant))

## Checking for multicolinearity
vif(m2)  ## All the variables have a VIF value near to 1 so there is no issue with multicollinearity


##################################
###### Plotting Residuals ########
##################################

##Pearson residuals
pearson_residuals <- residuals(m2, type = "pearson")

fitted_values <- predict(m2, type = "response")
par(mfrow = c(1, 1))

par(mfrow = c(2,2))

plot(bookings_df$number.of.weekend.nights,pearson_residuals, main = "Pearson Residuals vs weekend nights", xlab = "number.of.weekend.nights",col = 'purple')
plot(bookings_df$car.parking.space,pearson_residuals, main = "Pearson Residuals vs parking spacr", xlab = "car parking space",col = 'purple')
plot(bookings_df$lead.time, pearson_residuals, main = "Pearson Residuals vs Lead Time", xlab = "Lead Time",col = 'purple')
plot(bookings_df$average.price, pearson_residuals, main = "Pearson Residuals vs Average Price", xlab = "Price",col = 'purple')
plot(bookings_df$special.requests, pearson_residuals, main = "Pearson Residuals vs Special Requests", xlab = "Number of Special Requests",col = 'purple')


## Deviance Residuals
deviance_residuals <- residuals(m2, type = "deviance")

plot(bookings_df$number.of.weekend.nights,deviance_residuals, main = "Deviance Residuals vs Weekend Nights", xlab = "number.of.weekend.nights",col = 'darkgreen')
plot(bookings_df$car.parking.space,deviance_residuals, main = "Deviance Residuals vs Parking", xlab = "car parking space",col = 'darkgreen')
plot(bookings_df$lead.time, deviance_residuals, main = "Deviance Residuals vs Lead Time", xlab = "Lead Time",col = 'darkgreen')
plot(bookings_df$average.price, deviance_residuals, main = "Deviance Residuals vs Average Price", xlab = "Price",col = 'darkgreen')
plot(bookings_df$special.requests, deviance_residuals, main = "Deviance Residuals vs Special Requests", xlab = "Number of Special Requests",col = 'darkgreen')

####################################################
#### m2 is my final logistic regression model ######
####################################################

####################################################
###### Plots for EDA and pairwise-comparisons ######
####################################################

#Univariate analysis for categorical variables
par(mfrow=c(1,2))
table(bookings_df$booking.status)
barplot(table(bookings_df$booking.status), col="lightblue", main = "Booking Status Barplot", ylim = c(0,1600))
barplot(table(bookings_df$car.parking.space), col="lightblue", main = "Parking Barplot", ylim = c(0,2800))
table(bookings_df$car.parking.space)
barplot(table(bookings_df$room.type), col="lightblue", main = "Room Barplot", ylim = c(0,2000))
table(bookings_df$market.segment.type)
barplot(table(bookings_df$market.segment.type), col="lightblue", main = "Market Segment Barplot", ylim = c(0,2000), las=2, cex.names=0.9)


#Univariate analysis for numeric variables
hist(bookings_df$number.of.adults, breaks = 3,  main = "Histogram of adults staying", xlab = "number of adults")
hist(bookings_df$number.of.weekend.nights,breaks = 5, main = "Histogram of weekend nights", xlab = "number of weekend nights")
hist(bookings_df$number.of.children)

hist(bookings_df$lead.time,main = "Histogram of lead time", xlab = "days before the arrival date", ylim=c(0,1200))
hist(bookings_df$average.price, main = "Histogram of average price", xlab = "price", ylim=c(0,1200))


#response variable against numeric variables plots 
boxplot(bookings_df$number.of.weekend.nights~bookings_df$booking.status, col="darkred", main="Weekend nights per booking status", xlab = "Booking Status", ylab = "Weekend Nights")
boxplot(bookings_df$number.of.week.nights~bookings_df$booking.status, col="darkred", main="Week nights per booking status", xlab = "Booking Status", ylab = "Week Nights")
boxplot(bookings_df$special.requests~bookings_df$booking.status)
boxplot(bookings_df$average.price~bookings_df$booking.status, col="darkred", main="Average price per booking status", xlab = "Booking Status", ylab = "Average Price")
boxplot(bookings_df$lead.time~bookings_df$booking.status, col="darkred", main="Lead Time per booking status", xlab = "Booking Status", ylab = "Lead Time")

par(mfrow=c(1,1))
#response variable against categorical variables plots 
table(bookings_df$booking.status, bookings_df$car.parking.space)
barplot(table(bookings_df$booking.status, bookings_df$car.parking.space),  ylim = c(0,2800), col = c(8,3), xlab ="Parking Availability", main = "Booking Status per Parking Space", legend.text = levels(bookings_df$booking.status))
barplot(table(bookings_df$booking.status, bookings_df$room.type))
table(bookings_df$booking.status, bookings_df$room.type)
barplot(table(bookings_df$booking.status, bookings_df$market.segment.type), ylim = c(0,1400), col = c(8,3), xlab ="Market Segment", main = "Booking Status per Market Segment", legend.text = levels(bookings_df$booking.status))
table(bookings_df$booking.status, bookings_df$market.segment.type)
barplot(table(bookings_df$booking.status, bookings_df$month_of_reservation), ylim = c(0,250), col = c(8,3), xlab ="Month", main = "Booking Status per Month", legend.text = levels(bookings_df$booking.status))



m3 <- glm(booking.status ~ number.of.weekend.nights + lead.time + car.parking.space + average.price + special.requests + market.segment.type, 
                       data = bookings_df, 
                       family = binomial(link = "logit"))
summary(m3)



1 - exp((2/nobs(m3)) * (logLik(mconstant) - logLik(m3)))
(1 - exp((2/nobs(m3)) * (logLik(mconstant) - logLik(m3)))) / (1 - exp((2/nobs(m3)) * logLik(mconstant)))

mprobit <- glm(booking.status ~ number.of.weekend.nights + lead.time + car.parking.space + average.price + special.requests, 
                     data = bookings_df, 
                     family = binomial(link = "probit"))
summary(mprobit)
summary(m2)
