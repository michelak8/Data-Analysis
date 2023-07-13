#### LOGISTIC REGRESSION ####

Bankdata_train$loan = ifelse(Bankdata_train$loan == "yes",1,0)
Bankdata_test$loan = ifelse(Bankdata_test$loan == "yes",1,0)

#### ASSIGN LOGISTIC MODEL TO 'BANKmodel2' ####
BANKmodel2 <- glm(loan ~ ., data = Bankdata_train, family = "binomial")
summary(BANKmodel2)


#### REDUCE PARAMETERS OF 'BANKmodel2' using STEPWISE REGRESSION ####
BANKmodel2_step <- step(BANKmodel2, k = log(nrow(Bankdata_train)))
summary(BANKmodel2_step)


#### COMPARE THE TWO LOGISTIC MODELS USING BIC FOR BEST FIT TO TRAINING DATA ####
BIC(BANKmodel2)
BIC(BANKmodel2_step)

BF <- exp((29903.83-29854.53)/2)

BF = 50740994965
#THEREFORE BANKmodel2 IS 5.07 x 10^11 times more likely to predict the outcome variable "loan".


#### RUN DEVIANCE TEST TO EXPLAIN VARIANCE IN OUTCOME VARIABLE #### 
1-BANKmodel2_step$deviance/BANKmodel2_step$null.deviance
Deviance = 0.07233688

# ACCESS LOG ODDS ####
summary(BANKmodel2_step)

#Find the exponent of coefficients(probabilities/percentages) 

#JOBS
exp(BANKmodel2_step$coefficients[3])
1.23-1
# For clients who are Entrepreneurs, there is a 23% increase in the likelihood of the client having a personal loan.

exp(BANKmodel2_step$coefficients[4])
1-0.59
#for clients who are housemaids there is a 41% decrease in the likelihood of the client having a personal loan

exp(BANKmodel2_step$coefficients[5])
1-0.77
#for clients who are management, there is a 23% decrease in the likelihood of the client having a personal loan

exp(BANKmodel2_step$coefficients[6])
1-0.83
#for clients who are retired, there is a 17% decrease in the likelihood of the client having a personal loan

exp(BANKmodel2_step$coefficients[7])
1-0.77
#for clients who are self employed, there is a 23% decrease in the likelihood of the client having a personal loan

exp(BANKmodel2_step$coefficients[9])
1-0.088
#for clients who are students, there is a 91% decrease in the likelihood of the client having a personal loan

exp(BANKmodel2_step$coefficients[11])
1-0.39
#for clients who are Unemployed, there is a 61% decrease in the likelihood of the client having a personal loan

exp(BANKmodel2_step$coefficients[12])
1-0.09
#for clients who are unknown there is a 91% decrease in the likelihood of the client having a personal loan


#MARITAL

exp(BANKmodel2_step$coefficients[14])
1-0.79
#for clients who are self employed there is a 21% decrease in the likelihood of the client having a personal loan

##EDUCATION

exp(BANKmodel2_step$coefficients[15])
1.306607-1
#for clients who are education secondary there is a 31% increase in the likelihood of the client having a personal loan

exp(BANKmodel2_step$coefficients[17])
1-0.4866
#for clients who are education unknown there is a 51% decrease in the likelihood of the client having a personal loan



exp(BANKmodel2_step$coefficients[18])
2.45-1
#for clients who are defaultyes there is a 145% increase in the likelihood of the client having a personal loan


exp(BANKmodel2_step$coefficients[19])
1-0.999
#for clients who are education unknown there is a % crease in the likelihood of the client having a personal loan

exp(BANKmodel2_step$coefficients[20])
1-0.795
#for clients who are contacttelephone there is a 21% decrease in the likelihood of the client having a personal loan


exp(BANKmodel2_step$coefficients[21])
1.209-1
#for clients who are contactunknown there is a 21% increase in the likelihood of the client having a personal loan


exp(BANKmodel2_step$coefficients[22])
1-0.9923
#for clients who are day there is a 0.7% decrease in the likelihood of the client having a personal loan
  
exp(BANKmodel2_step$coefficients[23])
1-0.81
#for clients who are monthaug there is a 19% decrease in the likelihood of the client having a personal loan
  
exp(BANKmodel2_step$coefficients[26])
1.457-1
#for clients who are monthjan there is a 46% increase in the likelihood of the client having a personal loan  

exp(BANKmodel2_step$coefficients[27])
2.94-1
#for clients who are monthjul there is a 194% increase in the likelihood of the client having a personal loan


exp(BANKmodel2_step$coefficients[29])
1-0.57
#for clients who are monthmar there is a 43% decrease in the likelihood of the client having a personal loan

exp(BANKmodel2_step$coefficients[31])
1.8-1
#for clients who are monthnov there is an 80% increase in the likelihood of the client having a personal loan

exp(BANKmodel2_step$coefficients[33])
1-0.61
#for clients who are monthsep there is a 39% decrease in the likelihood of the client having a personal loan

exp(BANKmodel2_step$coefficients[35])
1-0.46
#for clients who are poutcomesuccess there is a 54% decrease in the likelihood of the client having a personal loan

exp(BANKmodel2_step$coefficients[36])
1-0.88
#for clients who are poutcomeunknown there is a 12% decrease in the likelihood of the client having a personal loan

exp(BANKmodel2_step$coefficients[37])
1-0.68
#for clients who are yyes there is a 32% decrease in the likelihood of the client having a personal loan




#### RUNNING PREDICTION FROM STEPWISE MODEL ON TEST DATA ####
pred <- predict(BANKmodel2_step, Bankdata_test, type = "response")
head(pred)
# predictions are the percentage of the ov value.

#Confusion Matrix
pred <- ifelse(pred > 0.5, 1,0)
print(confusionMatrix(as.factor(pred),as.factor(Bankdata_test$loan), positive = "1"))

library(verification)
pred <- predict(BANKmodel2_step , Bankdata_test, type = "response")
roc.plot(Bankdata_test$loan == 1, pred)
#Roc plot show 0.35 as best threshold for
#maximising sensitivity and specificity

head(pred)

#Make 0s and 1s (no default and default)
pred <- ifelse(pred > 0.35, 1, 0)

#Finally get the confusion matrix
print(confusionMatrix(as.factor(pred), as.factor(testData$default), positive = "1"))
