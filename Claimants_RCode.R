setwd("C:/ML/Claimants LOR")
data <- read.csv("claimants.csv", header = TRUE)

# Removing the first column which is is an Index
colnames(data)
x <- data[,-1] 


plot(x$CLMINSUR,pred1)

model <- glm(ATTORNEY~.,data=x,family = "binomial")

# To calculate the odds ratio manually we are going to take exp of coef(model)
exp(coef(model))

# Confusion matrix table 
prob <- predict(model,x,type="response")
summary(model)

# Confusion matrix considering the threshold value as 0.5 
confusion<-table(prob>0.5,x$ATTORNEY)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy 
# 70.62


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

 # Creating new column to store the above values
x[,"prob"] <- prob
x[,"pred_values"] <- pred_values
x[,"yes_no"] <- yes_no

View(x[,c(1,7:9)])

table(x$ATTORNEY,x$pred_values)

# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity



# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve, better the model 
# We will use ROC curve for any classification technique not only for logistic

library(ROCR)
rocrpred<-prediction(prob,x$ATTORNEY)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
