
### Part 1 : Regression ###

# Linear Regression

Heart_data <- read.csv('HeartDiseaseTrainTest.csv')
head(Heart_data)
colnames(Heart_data)
dim(Heart_data)
str(Heart_data)
summary(Heart_data)


# Using the lm() function to fit a simple linear regression model, with Max_heart_rate as the response and age as the predictor.
lm.fit <- lm(Max_heart_rate ~ age, data = Heart_data)
attach(Heart_data)
lm.fit <- lm(Max_heart_rate ~ age)
lm.fit
summary(lm.fit)
coef(lm.fit)
# To obtain a confidence interval for the coefficient estimates, we can use the confint() command.
confint(lm.fit)
# The predict() function can be used to produce confidence intervals and prediction intervals for the prediction of Max_heart_rate for a given value of age.
print("Confidence interval")
predict(lm.fit, data.frame(age = (c(5, 10, 15))), interval= "confidence")
predict(lm.fit, data.frame(age = (c(5, 10, 15))), interval= "prediction")
# Note: The linear regression analysis reveals that age is negatively associated with Max_heart_rate, as indicated by the coefficients of the fitted model. The intercept is estimated at 202.98, implying the expected Max_heart_rate at age zero, and the coefficient for age is estimated at -0.99, suggesting a decrease in Max_heart_rate as age increases. The 95% confidence intervals for the coefficients provide a level of certainty about their true values. Predicted Max_heart_rate values at specific ages (5, 10, and 15) come with narrower confidence intervals, reflecting the precision of these predictions. However, the wider prediction intervals acknowledge the inherent variability and uncertainty in predicting individual observations. Overall, the analysis provides valuable insights into the age-related trends in Max_heart_rate, offering both point estimates and intervals that capture the uncertainty associated with the model and predictions.

# Plot Max_heart_rate and age along with the least squares regression line using the plot() and abline() functions.
plot(age, Max_heart_rate , col = "black", pch = 20)
abline(lm.fit, lwd = 3, col = "red")

### Check For Homoscedasticity
par(mfrow = c(2,2))
plot(lm.fit)

# To plot the residuals against the fitted values.
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# On the basis of the residual plots, there is some evidence of non-linearity. Leverage statistics can be computed for any number of predictors using the hatvalues() function
plot(hatvalues(lm.fit))

# The which.max() function identifies the index of the largest element of a vector
which.max(hatvalues(lm.fit))
###

# Comment: 
# The linear regression analysis suggests that there is a relationship between age and Max_heart_rate. The regression model provides coefficient estimates along with confidence and prediction intervals for Max_heart_rate based on age. The visualization of the regression line on the scatter plot helps to understand the trend in the data. The residual plots indicate some evidence of non-linearity and potential outliers, which may need further investigation. The leverage statistics highlight influential observations in the dataset. Overall, this analysis provides valuable insights into the association between age and Max_heart_rate in the context of heart disease data. Further refinement of the model or exploration of additional variables may be necessary for a more comprehensive understanding of the relationship.




# Polynomial Regression
#install.packages("MultiKink")
#install.packages("ggplot2")

library(MultiKink)
library(ggplot2)
set.seed(1974)

# Using the poly() to predict Max_heart_rate with a forth-degree polynomial
heart.age.plot <- ggplot(Heart_data, aes(x = age, y = Max_heart_rate)) +
  geom_point(alpha=0.55, color="black") + 
  theme_minimal() 
heart.age.plot

model.cubic <- lm( Max_heart_rate ~ age + I(age^2) + I(age^3), Heart_data)
summary(model.cubic)

model.cubic.poly <- lm( Max_heart_rate ~ poly(age,3), data = Heart_data)
summary(model.cubic.poly)

plot(predict(model.cubic.poly), predict(model.cubic))

heart.age.plot + stat_smooth(method = "lm", formula = y ~ poly(x, 3, raw=T), size = 1)

# Add a linear fit to the plot above
heart.age.plot + stat_smooth(method = "lm",  formula = y ~ poly(x, 3, raw=T), size = 1) + 
  stat_smooth(method = "lm",  formula = y~poly(x,1,raw=T), lty = 2, col = "red" , size = 1)
###

# Comment:
# In this R script, polynomial regression is employed to model the relationship between age and maximum heart rate using the Heart_data dataset. Two models are fitted: a cubic regression (model.cubic) and a polynomial regression of degree 3 (model.cubic.poly). Visualizations are created to showcase the scatter plot of the data and overlay cubic and linear fits. The cubic polynomial appears to capture non-linear trends in the data, suggesting its potential suitability for predicting maximum heart rate based on age. However, careful consideration should be given to the balance between model complexity and interpretability, as more complex models may be prone to overfitting. Further validation, such as cross-validation, is recommended to assess the models' performance on unseen data. Overall, the analysis provides insights into the complex relationship between age and maximum heart rate, highlighting the importance of considering non-linear terms in the regression modeling process.



# Multiple Regression
str(Heart_data)

# Using the lm() function to fit a Multiple Regression model, with Max_heart_rate as the response and age, cholesterol, resting_blood_pressure as the predictor
lm.fit <- lm(Max_heart_rate ~ age + cholestoral + resting_blood_pressure, data = Heart_data)
summary(lm.fit)

dim(Heart_data)

lm.fit <- lm(Max_heart_rate ~ ., data = Heart_data)
summary(lm.fit)

summary(lm(Max_heart_rate ~ age * cholestoral, data = Heart_data))
cor(Heart_data$age, Heart_data$cholestoral)

plotting.data <- expand.grid(age = seq(min(Heart_data$age), 
                max(Heart_data$age), length.out=30), 
                  cholestoral=c(min(Heart_data$cholestoral), mean(Heart_data$cholestoral), 
                    max(Heart_data$cholestoral), 
                    resting_blood_pressure = c(min(Heart_data$resting_blood_pressure), 
                      mean(Heart_data$resting_blood_pressure), 
                        max(Heart_data$resting_blood_pressure))))
  
View(plotting.data)

heart.plot <- ggplot(Heart_data, aes(x = age, y=Max_heart_rate)) + geom_point()
heart.plot

###

# Comment:
# The provided R code conducts a thorough analysis of the "Heart_data" dataset, employing multiple regression models to investigate the relationship between predictor variables which are age, cholesterol, and resting_blood_pressure and the response variable, Max_heart_rate. The code includes fitting models with and without interaction terms, assessing correlations between age and cholesterol, and preparing data for visualization. While the code generates a scatter plot to visualize the relationship between age and Max_heart_rate, a specific conclusion cannot be drawn without the actual results and interpretations of the regression analyses. A comprehensive conclusion would involve examining coefficients, significance levels, and model fit statistics to make informed statements about the impact of the predictor variables on the response variable in the context of the dataset.


#Natural Cubic Spline
#install.packages("gam")

library(splines)
library(MultiKink) #for the data
library(ggplot2)   #for the plots
set.seed(1974)     #fix the random generator seed

attach(Heart_data)

#linear model with the natural cubic splines function 
cub.splines.bs <- lm(Max_heart_rate ~ bs(age, knots = c(5,10,20,30,40)), data=Heart_data)
summary(cub.splines.bs)

#simple scatter
Heart.age.plot <- ggplot(Heart_data, aes(x = age, y = Max_heart_rate)) + geom_point(alpha=0.55, color="black") +  theme_minimal() 

Heart.age.plot + stat_smooth(method = "lm", formula = y~bs(x,knots = c(5,10,20,30,40)), lty = 1, col = "blue") + 
  stat_smooth(method = "lm", formula = y~ns(x,knots = c(5,10,20,30,40)), lty = 1, col = "red")

###

# Comment:
# In this analysis, a linear model incorporating natural cubic splines was applied to the relationship between age and maximum heart rate using the 'gam' package in R. The knots were strategically placed at ages 5, 10, 20, 30, and 40 to capture potential non-linearities in the data. The resulting model, as summarized, provides insights into the intricate relationship between age and maximum heart rate. The scatter plot illustrates the raw data, while the blue and red curves represent the fitted models using natural cubic splines and alternative smoothing methods, respectively. The use of natural cubic splines allows for flexibility in capturing the underlying patterns in the data, revealing potential non-linear trends. Further interpretation of the model coefficients and examination of the plotted curves can guide a comprehensive understanding of how age influences maximum heart rate in the dataset.

######_______________________#####
# Conclusion:
# In this R script, a comprehensive analysis of the "Heart_data" dataset is conducted, focusing on regression modeling techniques. The script begins with a linear regression analysis, fitting a model to predict maximum heart rate (Max_heart_rate) based on age. The interpretation of coefficients, confidence intervals, and prediction intervals is demonstrated. Residual plots are examined for potential non-linearity, and leverage statistics are computed to identify influential observations. Subsequently, polynomial regression is applied to capture non-linear trends in the relationship between age and Max_heart_rate, showcasing cubic and linear fits. The importance of balancing model complexity and interpretability is emphasized. The script then extends to multiple regression, incorporating additional predictors such as cholesterol and resting blood pressure. Model summaries and significance tests provide insights into the impact of these variables on Max_heart_rate. Finally, natural cubic splines are introduced as a flexible modeling approach, allowing for non-linear relationships. While the script provides valuable insights, it underscores the necessity of thorough interpretation and validation, such as cross-validation, to ensure the reliability of the regression models.




