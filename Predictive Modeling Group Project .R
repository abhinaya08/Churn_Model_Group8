#Predictive Modeling Group Project Work
#Logistic Regression Exploratory Analysis 

rm(list=ls())

library(glmnet)

data = read.csv('Telecom_customer churn.csv')
head(data)
str(data)
summary(data)

#Omit missing values 
na.strings = c('')
telecom = na.omit(data)

head(telecom)
str(telecom)
summary(telecom)
dim(telecom)

logMedVal = log(telecom$churn)

#determine train and test data sets 

n=dim(telecom)
ind = sample(1:n, size=0.2*nrow(telecom))
train = telecom[-ind, ]
test = telecom[ind, ]

#Create Y variable 
Y=train$churn

dim(train)
dim(test)
#check 

#determine variable correlation 
nums = unlist(lapply(telecom, is.numeric))
cor.data = cor(telecom[,nums], method=c("pearson", "kendall", "spearman"))
head(cor.data)
write.csv(cor.data, "correlation_numeric.csv")

pairs(train, col=train$churn)

#run logistic regression
XXtd = model.matrix(~.*income*totcalls*months, data=data.frame(scale(td)))[,-1]
tdframe = data.frame(logMedVal, XXtd)

glm.fit <- glm(churn ~ eqpdays+ months+ change_mou + hnd_price + ovrrev_Mean + totmrc_Mean, data=train, family='binomial')
summary(glm.fit)

eta = predict(glm.fit) 
pyx = predict(glm.fit,type='response')


##Determine accuracy of the model
pred=predict(glm.fit, newdata=test, type = 'response')
accuracy = table(pred>.5, test[,'churn'])
sum(diag(accuracy))/sum(accuracy)

pred=predict(glm.fit, newdata=test)
confusionMatrix(data=pred, test$churn)

