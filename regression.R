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











































