---
  title: "Predict Customer Churn"

getwd()
setwd("E:/Nilesh/Custemor Churn")
df <- read.csv('Telco-Customer-Churn.csv')

sum(is.na(df))
# From summary command data contain total 11 Vlaues of NA 
summary(df)

# 11 Vlues are present in df$TotalCharges
sum(is.na(df$TotalCharges))

# Plot the histogram and see how the data is distributed
hist(df$TotalCharges , col= "red")
boxplot(df$TotalCharges)

#install.packages('psych')
library(psych)
describe(df$TotalCharges)

# We have checked with hist for data distribution & Boxplot for outlier detection 
# & decide NA Values should replace with the MEAN
df$TotalCharges[is.na(df$TotalCharges)] <- round(mean(df$TotalCharges, na.rm = TRUE))

# Now the NA VAlues Shows as "0" in data set
sum(is.na(df$TotalCharges))
sum(is.na(df))

# Now the NA VAlues Shows as "0" in data set
df1 <- complete.cases(df)
df1

head(df)
str(df)


# Replace col [10:15] with following factors
#install.packages("plyr")
library(plyr)
cols_recode1 <- c(10:15)
for(i in 1:ncol(df[,cols_recode1])) 
  {
  df[,cols_recode1][,i] <- as.factor(mapvalues
 (df[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

head(df)
str(df)

# Also map some other values in df$MultipleLines
df$MultipleLines <- as.factor(mapvalues(df$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

# Inspect the tenure column for convertion into Group factor
hist(df$tenure , col = "green")
min(df$tenure)
max(df$tenure)
plot(df$tenure, col="yellow")
describe(df$tenure)

# Define group_tenure for the tenure colomn and convert into tenure_group with sapply command
group_tenure <- function(tenure)
  {
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

# with saaply and as.factor command add one extra column in data set
df$tenure_group <- sapply(df$tenure,group_tenure)
df$tenure_group <- as.factor(df$tenure_group)

plot(df$tenure_group, col = "yellow")
summary(df$tenure_group)

# Also map some other values in df$SeniorCitizen
df$SeniorCitizen <- as.factor(mapvalues(df$SeniorCitizen, 
                                        from=c("0","1"),
                                        to=c("No", "Yes")))
plot(df$SeniorCitizen, col="red")
str(df)

# Remove below two column as we dont need them in model building
df$customerID <- NULL
df$tenure <- NULL

###################################################################################
## Exploratory data analysis and feature selection
## Find numerical variables
## Calculate the correlation matrix
numeric.var <- sapply(df, is.numeric)
corr.matrix <- cor(df[,numeric.var])
corr.matrix

#install.packages("corrplot")
library(corrplot)
corrplot(corr.matrix, main="Correlation Plot for Numeric Variables", method="number")

# Monthly Charges and Total Charges are correlated, so avoid Multicollinearity remove Total charges
df$TotalCharges <- NULL

#install.packages("ggplot2")
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
## Bar plots of categorical variables

p1 <- ggplot(df, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal() 
p2 <- ggplot(df, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(df, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(df, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)


p5 <- ggplot(df, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(df, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(df, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(df, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)


p9 <- ggplot(df, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p10 <- ggplot(df, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p11 <- ggplot(df, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p12 <- ggplot(df, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2)


p13 <- ggplot(df, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p14 <- ggplot(df, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p15 <- ggplot(df, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p16 <- ggplot(df, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p17 <- ggplot(df, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("Tenure Group") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, p17, ncol=2)

###########################################################################

#All categorical variables have a reasonable broad distribution, therefore, 
#all of them will be kept for the further analysis.

###########################################################################

## Logistic Regression Model Building
# Split the data into training and testing sets.
set.seed(1234)
library(caTools)

# Spliting the data in training and testing of data
split <- sample.split(df, SplitRatio = 0.80)
split

# Devide data into traning and testing
training <- subset(df,split == TRUE)
testing <- subset(df,split == FALSE)
# Check the dimensions for training & testing
dim(training)
dim(testing)


# Build the logistic Regression line with glm() function 
model1 <- glm(Churn ~. ,family=binomial(link="logit"),data=training)
summary(model1)

# After summary visualization check the most significant variable for model building
# try to remove insignificant one and check value of Residual deviance and AIC score
model2 <- glm(Churn ~ SeniorCitizen + MultipleLines + OnlineSecurity + Contract + PaymentMethod
              + tenure_group,family=binomial(link="logit"),data=training)
summary(model2)

# AIC value increases with selected variables try other variables also
model3 <- glm(Churn ~ SeniorCitizen + MultipleLines + OnlineSecurity + Contract + PaymentMethod
              + tenure_group + PaperlessBilling,family=binomial(link="logit"),data=training)
summary(model3)

library(texreg)
# Comparison Table for AIC Values
screenreg(list(model1,model2,model3))

# as compare summary of model1,2 & 3 for Model1 AIC value is low so will go with model1
# done the anova test for variable significance 
anova(model1, test="Chisq")

# PaymentMethod and Dependents seem to improve the model less even though they all have low p-values.
#######################################################################################################

## Assessing the predictive ability of the model

predmodel <- predict(model1)
predmodel

# Response Values of Confusion Matrix
res1 <- predict(model1,training,type = "response")
(table(ActualValue = training$Churn,PredictValue = res>0.5))

# Response Values of Confusion Matrix
res2 <- predict(model1,training,type = "response")
(table(ActualValue = training$Churn,PredictValue = res>0.3))

library(caret)
#install.packages("summarytools")
library(summarytools)
ctable2 <- as.table(matrix(c(3130, 955, 357, 199), nrow = 2, byrow = TRUE))
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")

# For each unit increase in Monthly Charge, there is a 2.4% decrease in the l
# ikelihood of a customer's churning.

exp(cbind(OR=coef(model1), confint(model1)))

#install.packages('partykit')
library(partykit)
tree <- ctree(Churn~Contract+tenure_group+PaperlessBilling, training)
plot(tree, type='simple')

pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)
