View(bank.full)
colnames(bank.full)[which(names(bank.full) == "y")] <- "term_deposit"
bank.full <- as.data.frame(bank.full)
class(bank.full)
# Preparing a linear regression 
attach(bank.full)
linearmodel <- lm(term_deposit~.,data=bank.full)
pred1 <- predict(linearmodel,bank.full)
pred1
plot(age,pred1)

# The output of sigmoid function lies in between 0-1
model <- glm(term_deposit~.,data=bank.full,family = "binomial")

# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))
# Confusion matrix table 
prob <- predict(model,bank.full,type="response")
prob
summary(model)#### AIC::21648

confusion<-table(prob>0.5,bank.full$term_deposit)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy##### 0.901

# Creating empty vectors based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
bank.full[,"prob"] <- prob
bank.full[,"pred_values"] <- pred_values
bank.full[,"yes_no"] <- yes_no

# View table

table(bank.full$term_deposit,bank.full$pred_values)

##ROC Curve
install.packages("ROCR")
library(ROCR)
predterm<-prediction(prob,bank.full$term_deposit)
rocrpredterm<-performance(predterm,'tpr','fpr')
plot(rocrpredterm)

###getting treshhold values

rocr.cutoff <- data.frame(cut_off = rocrpredterm@alpha.values[[1]],fpr=rocrpredterm@x.values,tpr=rocrpredterm@y.values)
colnames(rocr.cutoff) <- c("cut_off","FPR","TPR")
View(rocr.cutoff)

library(dplyr)
rocr.cutoff$cut_off <- round(rocr.cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr.cutoff <- arrange(rocr.cutoff,desc(TPR))
View(rocr.cutoff)
