#----------------------------------------------------
# Econometrics and Statistical Models - Group Project
#----------------------------------------------------

# Members :
# CRETIN Lucie - PARIS Emma - SZEPEK Maria Sofie - VINCENOT Amandine

#----------------
# 1 - Import data 
#----------------

library(readr)
insurance <- read_delim("C:/Users/01892059/Documents/TBS/M2/Cours Communs/Group Project Anne/insurance.csv",
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
insurance <- data.frame(insurance)
View(insurance)

# Y will be "charges"
# X = potentially 6 variables 


#-------------------
# 2 - Data structure
#-------------------

str(insurance)
# We can see there are 4 quantitative variables and 3 qualitative variables

# Transform qualitative variables as factor
insurance$sex = as.factor(insurance$sex)
insurance$smoker = as.factor(insurance$smoker)
insurance$region = as.factor(insurance$region)

# Check if it has been successfully transform
str(insurance)


#-----------------------------------------------------------
# 3 - Data cleaning : Check for missing values and outliers
#-----------------------------------------------------------

# Check for missing values
summary(insurance)
sum(is.na(insurance))
# There is no missing values

# Check outliers for quantitative variables 

attach(insurance) # To avoid using "insurance$"

par(mfrow=c(1,4))

# Check if the variable charges is normally distributed with histogram
hist(charges, main = "Charges")
boxplot(charges, main = "Charges")
insurance$logcharges = log(insurance$charges)
hist(logcharges, main = "Logcharges")
boxplot(logcharges, main = "Logcharges")
# Replace a left hand side by a normal distribution : this is a little bit better

par(mfrow=c(1,1))

# Boxplots
par(mfrow=c(1,4))

boxplot(age, main = "Age")
boxplot(bmi, main = "BMI")
boxplot(children, main = "Children")

# Remove the outliers for BMI
Q1_bmi <- quantile(insurance$bmi, .25)
Q3_bmi <- quantile(insurance$bmi, .75)
IQR_bmi <- IQR(insurance$bmi)
insurance_clean <- subset(insurance, insurance$bmi > (Q1_bmi - 1.5*IQR_bmi) 
                          & insurance$bmi < (Q3_bmi + 1.5*IQR_bmi))
dim(insurance_clean)
boxplot(insurance_clean$bmi, main = "BMI w/o outliers")

# Alternative to remove the outliers for BMI :
# Q_bmi <- quantile(insurance$bmi, probs=c(.25, .75), na.rm = FALSE)
# IQR_bmi <- IQR(insurance$bmi)
# Low_bmi <- Q_bmi[1]-1.5*IQR_bmi # Lower Range
# Up_bmi <-  Q_bmi[2]+1.5*IQR_bmi # Upper Range  
# insurance_clean <- subset(insurance, insurance$bmi > Low_bmi 
#                           & insurance$bmi < Up_bmi)
# boxplot(insurance_clean$bmi, main = "BMI w/o outliers")

par(mfrow=c(1,1))

# Compare before and after removing outliers
summary(insurance)
summary(insurance_clean)


#-------------------------------------------------------
# 4 - Descriptive statistics and graph for each variable
#-------------------------------------------------------

# Graph for quantitative variables

attach(insurance_clean) # To avoid using "insurance_clean$"

# New Histograms
par(mfrow=c(1,4))
hist(age, main = "Age") 
hist(bmi, main = "BMI")
hist(children, main = "Children")
hist(logcharges, main = "Logcharges")
par(mfrow=c(1,1))

# New Boxplots
boxplot(age, main = "Age")
boxplot(bmi, main = "BMI")
boxplot(children, main = "Children")
boxplot(logcharges, main = "Logcharges")

# Graph for qualitative variables 

par(mfrow=c(1,2))

table(sex)
prop.table(table(sex))
barplot(table(sex), main = "Sex")
plot(insurance_clean$logcharges~insurance_clean$sex)

table(smoker)
prop.table(table(smoker))
barplot(table(smoker), main = "Smoker")
plot(insurance_clean$logcharges~insurance_clean$smoker)

table(region)
prop.table(table(region))
barplot(table(region), main = "Region")
plot(insurance_clean$logcharges~insurance_clean$region)

par(mfrow=c(1,1))


#----------------------------------------------------------------------
# 5 - Check correlations between variables with the scatter plot matrix
#----------------------------------------------------------------------

# Scatter plot matrix to see correlations between variables

install.packages('datarium')
install.packages('lattice')

library(datarium)
library(lattice)

splom(~insurance_clean[c(1:6,8)], groups=NULL,data=insurance_clean,axis.line.tck=0,axis.text.alpha=0)
cor(insurance_clean[,c(1,3,4,8)]) 
pairs(insurance_clean[,c(1:6,8)])

# There is a little correlation between age and logcharges but it isn't a huge issue

#---------------------------------------
# 6 - Simple linear regression with age
#---------------------------------------


#Link between logcharges and age
#------

#Graph
attach(insurance_clean)
par(mfrow=c(1,1))
plot(logcharges~age)
# Seems to be an influence of age on logcharges. 

# Model estimation 
slm.fit = lm(logcharges~age)
summary(slm.fit)


# Multiple R-squared:  0.4567,	Adjusted R-squared:  0.4562. This model explains 45,6% of the logcharges variations. 
# All the p-value are less than 5%, so all the coefficient are significant. 
# So we can conclude that the coefficient beta_1 is significantly different from 0 and we can interpret its value.
# beta_1 = 7,43 >0 so an increase by 1 unit of age impacts logcharges by an increase of 7,43. 


plot(age, logcharges, pch="+")
abline(slm.fit, col="red")





# Residual analysis
#-----

#Normality of residuals 
#Qqplot 
par(mfrow=c(1,1))
qqnorm(residuals(slm.fit))
qqline(residuals(slm.fit))
# Not really a Normal distribution. 

#Shapiro-Wilks test : 
#H0 : the distribution is normal. 
#H1 : the distribution is not normal. 
shapiro.test(residuals(slm.fit))
#pvalue = 2.297e-08 <<< 5% so we reject H0. 
#The distribition cannot be considered as normal. 
#There must be potential outliers...
hist(residuals(slm.fit))


#Homoscedasticity 
par(mfrow=c(1,2))
plot(predict(slm.fit), residuals(slm.fit))
plot(predict(slm.fit), rstudent(slm.fit))
par(mfrow=c(1,1))

#--------------------------------
# 7 - Multiple linearregression V1
#--------------------------------

attach(insurance_clean)

# Model estimation 
fit1 = lm(logcharges~. -charges, data=insurance_clean) 
summary(fit1)


# Multiple R-squared:  0.7669,	Adjusted R-squared:  0.7655   -> we have not a very good model.  
# Predictors : by looking at the pvalue (Pr(>|t|)), not all are significant (need to remove them). 
# The non-significant predictor is regionnorthwest. It means that we have to remove region. 
# We will remove it after the VIF analysis. 


# Variance inflation factor (VIF)
# ----
library("car")
vif(fit1)
# The larger the VIF is, the more correlated the variables are. 
# VIF provides the link btw the variable we are considering and all the others. 
# In our case, all VIF are really close to 1, so there is no problem of colinearity. 
# We can continue with all predictors and apply the stepwise selection. 

#------------------
# Residual analysis
#------------------

#Normality of residuals 
#Qqplot 
qqnorm(residuals(fit1))
qqline(residuals(fit1))
# It's not a normal distribution. 

#Shapiro-Wilks test : 
#H0 : the distribution is normal. 
#H1 : the distribution is not normal. 
shapiro.test(residuals(fit1))  
# p-value < 2.2e-168 <<< 5% so we reject H0. 
# The distribition cannot be considered as normal. 
# There must be potential outliers...
hist(residuals(fit1))


#Homoscedasticity 
par(mfrow=c(1,2))
plot(predict(fit1), residuals(fit1))
plot(predict(fit1), rstudent(fit1)) #Divided the residuals by their standard deviation. 
par(mfrow=c(1,1))



# Variables stepwise selection 
# ----

#We remove first the least significant variable, which is region. 
fit2 = update(fit1,~. -region)
summary(fit2)
# All the variables are now significant.  



#----------------------
# 8 - Model validation
#----------------------

# Multiple R-squared:  0.7638,	Adjusted R-squared:  0.7629 -> the model is not really good, 
# It was better with region. 


#------------------
# Residual analysis
#------------------

#Normality of residuals 
#Qqplot 
qqnorm(residuals(fit2))
qqline(residuals(fit2))
# It's not a normal distribution. 

#Shapiro-Wilks test : 
#H0 : the distribution is normal. 
#H1 : the distribution is not normal. 
shapiro.test(residuals(fit2))  
# p-value < 2.2e-168 <<< 5% so we reject H0. 
# The distribition cannot be considered as normal. 
# There must be potential outliers...
hist(residuals(fit2))


#Homoscedasticity 
par(mfrow=c(1,2))
plot(predict(fit2), residuals(fit2))
plot(predict(fit2), rstudent(fit2)) #Divided the residuals by their standard deviation. 
par(mfrow=c(1,1))



#----------------------------
# 9 - Correction of the model V2
#----------------------------

attach(insurance_clean)

# We want to see if there are outliers in our data
plot(insurance_clean$age, insurance_clean$logcharges)
plot(insurance_clean$children, insurance_clean$logcharges)
plot(insurance_clean$bmi, insurance_clean$logcharges)
# With the plot of age and logcharges, we can see that
# it seems to be 2 types of populations depending on the age.

# We choose to split our dataset into 2 sample : one with logcharges below 9,5 and one with logcharges 
# above 9,5. Since most of the points are below 9,5, we will use this sample to fit our model.
insurance_sample = insurance_clean[!(insurance_clean$logcharges >= 9.5),]
str(insurance_sample)
plot(insurance_sample$age, insurance_sample$logcharges)
# We have removed 413 values. 


# New estimation of the model with the new sample. 
#-----

new_model = lm(insurance_clean$logcharges~. -charges, data=insurance_clean) 
summary(new_model)
# Multiple R-squared:  0.9097,	Adjusted R-squared:  0.9089 
# We should remove BMI. 

# Removing BMI
new_model1 = lm(insurance_clean$logcharges~. -charges -bmi, data=insurance_clean) 
summary(new_model1)
# All the coefficients are significant. 


# Residual analysis for model validation 
#-----

#Normality of residuals 
#Qqplot 
qqnorm(residuals(new_model1))
qqline(residuals(new_model1))
# It's not a normal distribution. 

#Shapiro-Wilks test : 
#H0 : the distribution is normal. 
#H1 : the distribution is not normal. 
shapiro.test(residuals(new_model1))  
# p-value < 2.2e-16 <<< 5% so we reject H0. 
# The distribition cannot be considered as normal. 



#Homoscedasticity 
par(mfrow=c(1,2))
plot(predict(new_model1), residuals(new_model1))
plot(predict(new_model1), rstudent(new_model1))
par(mfrow=c(1,1))
# Model not validate. 



#----------------------------
# 10 - Correction of the model V3
#----------------------------

insurance_clean$bmi_classification[insurance_clean$bmi < 18.4] <- "Underweight"
insurance_clean$bmi_classification[insurance_clean$bmi >= 18.5 & insurance_clean$bmi <= 24.9] <- "Normal"
insurance_clean$bmi_classification[insurance_clean$bmi >= 25 & insurance_clean$bmi <= 29.9] <- "Overweight"
insurance_clean$bmi_classification[insurance_clean$bmi > 30] <- "Obese"
insurance_clean$bmi_classification <- as.factor(insurance_clean$bmi_classification)
insurance_clean$children <- as.factor(insurance_clean$children)
insurance_clean$age <- as.numeric(insurance_clean$age)

fit3 = lm(logcharges ~ age + sex + children + smoker + region 
               + bmi_classification + age*sex + age*children + age*smoker,
               data=insurance_clean) 
summary(fit3)

insurance_sample$bmi_classification[insurance_sample$bmi < 18.4] <- "Underweight"
insurance_sample$bmi_classification[insurance_sample$bmi >= 18.5 & insurance_sample$bmi <= 24.9] <- "Normal"
insurance_sample$bmi_classification[insurance_sample$bmi >= 25 & insurance_sample$bmi <= 29.9] <- "Overweight"
insurance_sample$bmi_classification[insurance_sample$bmi > 30] <- "Obese"
insurance_sample$bmi_classification <- as.factor(insurance_sample$bmi_classification)
insurance_sample$children <- as.factor(insurance_sample$children)
insurance_sample$age <- as.numeric(insurance_sample$age)



# Targetedly observe potential interaction effects within sample
fit4prep = lm(logcharges ~ age + sex + children + smoker + region + bmi 
              + age*sex + age*children + age*smoker + age*bmi
              + sex*children + sex*smoker + sex*bmi
              + children*smoker + children*bmi
              + smoker*bmi, 
              data=insurance_sample)
summary(fit4prep)

# Keep significant terms and interaction terms:
fit4 = lm(logcharges ~ age + sex + children + smoker + region
          + age*sex + age*children,
          data=insurance_sample) 
summary(fit4)
# Multiple R-squared:  0.9239,	Adjusted R-squared:  0.9224
# All the variables are relevant



# An investigation of the predictive power of this model is carried out:
#'''''''''''''''''''''''''''''''''''''''''''''
# Application of the predictive accuracy tools
#'''''''''''''''''''''''''''''''''''''''''''''

#Sample the dataset
set.seed(1)
row.number <- sample(1:nrow(insurance_sample), 0.8*nrow(insurance_sample))
train = insurance_sample[row.number,]
test = insurance_sample[-row.number,]

# Estimate the linear fit on the training set
fit4_0.8 <- lm(logcharges ~ age + sex + children + smoker + region
               + age*sex + age*children,
               data=insurance_sample) 
summary(fit4_0.8)

# Compute RMSE, MAPE
pred0.8 <- predict(fit4_0.8,newdata=test) # data=test to predict 
err0.8 <- pred0.8-test$logcharges
rmse <- sqrt(mean(err0.8^2))
mape <- mean(abs(err0.8/test$logcharges))
c(RMSE=rmse,mape=mape,R2=summary(fit4_0.8)$r.squared) # to print the 3 parameters

plot(test$logcharges,pred0.8) # plot of predicted values against test values

#'''''''''''''''''''''''''''''''''
# LOOCV method
#'''''''''''''''''''''''''''''''''

install.packages("boot")
library("boot")
glm.fit <- glm(logcharges ~ age + sex + children + smoker + region
              + age*sex + age*children,
              data=insurance_sample) 
cv.err <- cv.glm(insurance_sample,glm.fit)
cv.err$delta[1]  # to print the cross-validation statistics



#----------------------------
# 11 - Correction of the model V3 with polynomial
#----------------------------


# model without polynomial transformation age^2 using I(X^2)
fit_sample_without_poly <-lm(logcharges ~ age + children + sex + smoker 
                             + region, data = insurance_sample)
summary(fit_sample_without_poly)


# polynomial transformation age^2 using I(X^2)
fit5 <-lm(logcharges ~ age + I(age^2) + children + sex + smoker 
          + region, data = insurance_sample)
summary(fit5)
#R^2 = 0.9158
# bmi removed


# ANOVA test between the 2 nested models

anova(fit_sample_without_poly, fit5)
# Ho: no significant change between the 2 models
# H1: there is a significant difference
# -> p-value < 0.05 -> there is a significant difference

# Compare various nested models 

fit5_poly3 <- lm(logcharges ~ age + I(age^3) + children + sex + smoker 
                 + region, data = insurance_sample)

anova(fit_sample_without_poly, fit5, fit5_poly3)

# The best model seems to be model 2 since pvalues after model 2 are larger
# than 5%

#----- 


# An investigation of the predictive power of this model is carried out using : 
#'''''''''''''''''''''''''''''''''
# LOOCV method
#'''''''''''''''''''''''''''''''''

install.packages("boot")
library("boot")
glm.fit <- glm(logcharges ~ age + I(age^2) + children + sex + smoker + region
               , data = insurance_sample)
cv.err <- cv.glm(insurance_sample,glm.fit)
cv.err$delta[1]  # to print the cross-validation statistics



###########################################
# Including relevant interaction effects: 
###########################################
fit6 <-lm(logcharges ~ age + I(age^2) + children + sex + smoker + region
          + age*sex + age*children
          , data = insurance_sample)
summary(fit6)
# R^2 = 0.9287
# age*smoker + sex*smoker + children*smoker + smoker*bmi with NA values



# Residual analysis for model validation 
#-----

#Normality of residuals 
#Qqplot 
qqnorm(residuals(fit6))
qqline(residuals(fit6))
# It's not a normal distribution. 

#Shapiro-Wilks test : 
#H0 : the distribution is normal. 
#H1 : the distribution is not normal. 
shapiro.test(residuals(fit1))
shapiro.test(residuals(fit2))  
shapiro.test(residuals(fit3))  
shapiro.test(residuals(fit4))  
shapiro.test(residuals(fit5))  
shapiro.test(residuals(fit6))  

# p-value < 2.2e-16 <<< 5% so we reject H0. 
# The distribition cannot be considered as normal. 

#Homoscedasticity 
par(mfrow=c(1,2))
plot(predict(fit6), residuals(fit6))
plot(predict(fit6), rstudent(fit6)) #Divided the residuals by their standard deviation. 
par(mfrow=c(1,1))



# An investigation of the predictive power of this model is nevertheless carried out:
#'''''''''''''''''''''''''''''''''''''''''''''
# Application of the predictive accuracy tools
#'''''''''''''''''''''''''''''''''''''''''''''
# LOOCV method
#'''''''''''''''''''''''''''''''''

install.packages("boot")
library("boot")
glm.fit <- glm(logcharges ~ age + I(age^2) + children + sex + smoker + region
                  + age*sex + age*children
                  , data = insurance_sample)
cv.err <- cv.glm(insurance_sample,glm.fit)
cv.err$delta[1]  # to print the cross-validation statistics




#-------------------------------------------------
# 12 - GAM extension to multiple linear regression
#-------------------------------------------------

library("splines") 
library("gam")


attach(insurance_clean)

# GAM using quartiles for the knots
gam1 = lm(logcharges~ns(age, knots=c(26, 39, 51))+ns(bmi, knots=c(25, 30.10, 33.82))
          +ns(children, knots=c(1, 1.084, 2))+
            +region+smoker+sex, data=insurance_clean)
summary(gam1)

par(mfrow=c(2,3))
plot.Gam(gam1, se=TRUE, col="red")

# We remove BMI because pvalue>5%
gam2 = lm(logcharges~ns(age, knots=c(26, 39, 51))+ns(children, knots=c(1, 1.084, 2))+
            +region+smoker+sex, data=insurance_clean)
summary(gam2)

par(mfrow=c(2,3))
plot.Gam(gam2, se=TRUE, col="red")


# Try to keep age normal (without knots)
gam3 = lm(logcharges~age+ns(children, knots=c(1, 1.084, 2))+
            +region+smoker+sex, data=insurance_clean)
summary(gam3)

par(mfrow=c(2,3))
plot.Gam(gam3, se=TRUE, col="red")

# DF = 4 for children 
gam4 = lm(logcharges~age+ns(children, df=4)+
            +region+smoker+sex, data=insurance_clean)
summary(gam4)

par(mfrow=c(2,3))
plot.Gam(gam4, se=TRUE, col="red")

# Better that we can have 
# Multiple R-squared:  0.7102,	Adjusted R-squared:  0.708 
# on garde le gam avec le plus grand R2 mais on dit que la regression multiple est mieux m?me si on peut pas la valider.


# GAM with smoothing splines: parameter determination using restricted marginal likelihood (REML)

gam7  <- gam(logcharges ~ s(age) + s(age, by=smoker) + s(bmi, by=smoker) 
             + smoker +children , data=insurance_clean, method="REML")
gam.check(gam7)
coef(gam7)
summary(gam7)



