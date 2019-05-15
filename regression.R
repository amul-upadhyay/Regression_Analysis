# Demo uses the dataset 'womwn'
# Containing height and weight of 15 women

simple_linear_model<- lm(weight ~ height, data = women)

simple_linear_model


# weight = -87.52 + 3.45 * height

plot(women$height,
     women$weight,
     xlab = "Height (inches)",
     ylab = "ylab (lbs)",
     main = "Description")

abline(simple_linear_model)


summary(simple_linear_model)

# Residual provide a quick view of distribution of residuals,
# which by definition have a mean zero
# therefore a median could not be far from zero, and the min and max
# should be roughly equal im absolute zero
# Residual Standard Error (RSE), R square and f-statistic are metrics
#that are used to check
#the standrad error (SE) defines the accuracy of beta coefficients
# for a given beta coefficient, the SE reflects how the coefficient
# varies under repeat sampling. It can be usd to compute CI and t-statistics

# The correlation coefficient measures the level of association between
# 2 variables and ranges from -1 (perfect neg correlation)
# to +1 (per pos correlation)
# A value close to 0 = weak relationship
# A low correlation (-0.2 < x < 0.2) indicates that a lot of the variation
# of the outcome (y) against predictor (x) is unexplained and 
# we should then look for better predictor variables


cor(women$height, women$weight)

confint(simple_linear_model)

install.packages('CARS')
library(CARS)

head(CARS)


scatter.smooth(x = cars$speed,
               y = cars$dist,
               main = "Distance ~ speed",
               xlab = "CAr speed",
               ylab = "stopping distance")


# Box plot will show the outliers in data

par(mfrow = c(1, 2)) # Divide graph area into 2 cals

boxplot(cars$speed,
        main = "Speed",
        sub = paste0("Outlier rows : ",boxplot.stats(cars$speed)$out))



boxplot(cars$dist,
        main = "Distance",
        sub = paste0("Outlier rows : ",boxplot.stats(cars$dist)$out))


# Skewness function to examine normality

install.packages("e1071")
library(e1071)

# Density plot for speed
# skewness < -1 or > 1 = highly skewed
# -1 to 0.5 and 0.5 to 1 moderate skewness
# -0.5 to 0.5 = approx symetic


plot(density(cars$speed),
     main = "Density",
     ylab = "Frequency",
     sub = paste("Skewness :", round(e1071::skewness(cars$speed), 2)))


# Fill the are within the density plot to red

polygon(density(cars$speed), col = "red")


# Density plot for dis

plot(density(cars$dist),
     main = "Density",
     ylab = "Frequency",
     sub = paste("Skewness :", round(e1071::skewness(cars$dist), 2)))


# Fill the are within the density plot to red

polygon(density(cars$dist), col = "red")

cor(cars$speed, cars$dist)


# Build the model on full data

linear_model <- lm(dist ~ speed, data = cars)
linear_model
summary(linear_model)


set.seed(200)

# choose a random sample from 1:all records in cars dataset, 80% of rows

random_sample <- sample(1:nrow(cars), 0.8 * nrow(cars))

# model training data

training_data <- cars[random_sample,]

# model testing data

testing_data <- cars[-random_sample,]


training_data

testing_data

nrow(training_data)
nrow(testing_data)

# Build the model on training data

lr_model <- lm(dist ~ speed, data = training_data)

summary(lr_model)

distance_predicted <- predict(lr_model, testing_data)

distance_predicted

actual_predicted <- data.frame(cbind(actuals = testing_data$dist, predicted = distance_predicted))


head(actual_predicted)

correlation_accuracy <- cor(actual_predicted)

correlation_accuracy


# min -max accuracy

min_max_accuracy <- mean(apply(actual_predicted, 1, min)/ 
                                 apply(actual_predicted, 1, max))

# MAPE

mape <- mean(abs((actual_predicted$predicted - actual_predicted$actuals))/
                     actual_predicted$actuals)

# min_max_accuracy higer the better
# mape lower the better

min_max_accuracy
mape






































































