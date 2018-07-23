#==group assignment

getwd()
df <- read.csv("C:/Users/abhin/Documents/Summer classes/Predictive_models/Assignment/telecom-customer/Telecom_customer churn.csv")
head(df)
str(df)
summary(df)

#==omitting all missing values
df1 <- na.omit(df)
head(df1)
str(df1)
dim(df1)

#==running correlation matrix
nums <- unlist(lapply(df1, is.numeric))  
cor.df <- cor(df1[,nums], method = c("pearson", "kendall", "spearman"))
head(cor.df)
write.csv(cor.df,"correlation_numeric.csv")

table(df1$churn,df1[,i])
19643/(21519+19643) #47%

#=======plotting all variables
i=90 #factor
i=1 #numeric
for (i in 1:dim(df1)[2]){
  x= df1$churn
  if(is.numeric(df1[,i])){ #take average here n plot
    t <- aggregate(df1[,i] ~ x,df1,FUN=mean)
    #t2 <- aggregate(df1[,i] ~ x,df1,FUN=count)
    #barplot(t[,2], names.arg=c("0 Churn", "1 Churn"),space =0.5,ylab=colnames(df1[i]))
    jpeg(file =paste0(getwd(),"/Assignment/telecom-customer/plots/churn_vs_",colnames(df1[i]),".jpg"))
    #plot(x,df1[,i],ylab=colnames(df1[i]),xlab="Churn",pch=19,col="darkgray")
    plot(t[,1],t[,2],ylab=colnames(df1[i]),xlab="Churn",pch=19,col="red",type="o")
    
    #plot(df1[,i],x,xlab=colnames(df1[i]),ylab="Churn",pch=19,col="darkgray")
    #lines(t[,1],t[,2],col=2,lwd=2)
    dev.off()
  }
  else{ #count n plot
    counts <- table(x, df1[,i])
    #t <- aggregate( x ~ df1[,i],df1,FUN=sum)
    jpeg(file =paste0(getwd(),"/Assignment/telecom-customer/plots/churn_vs_",colnames(df1[i]),".jpg"))
    barplot(counts,
            xlab=colnames(df1[i]), col=c("darkblue","red"),
            legend = rownames(counts), beside=TRUE)
    #plot(t[,1],t[,2],ylab=colnames(df1[i]),xlab="Churn",pch=19,col="darkgray")
    dev.off()
  }
}

#===var chi-sq code
i=102
p.val <- matrix(0,dim(df1)[2],3)
for(i in 1:dim(df1)[2]){
  if(i==86) next
  aux <- chisq.test(df1[,i],df1$churn)
  p.val[i,1] <- names(df1)[i]
  p.val[i,2] <- aux$p.value
  p.val[i,3] <- aux$statistic
  print(i)
}
p.val <- as.data.frame(p.val)
names(p.val) <- c("Variable name","P value","Intercept")
write.csv(p.val,"C:/Users/abhin/Documents/Summer classes/R assignment/chi_sq_output.csv",row.names = F)


#======Trees algorithm
#install.packages("randomForest")
#library(randomForest)
library(tree)

set.seed(100)
df1$midwest_y <- ifelse(df1$area =="MIDWEST AREA",1,0)
df1$dualband_y <- ifelse(df1$dualband == "Y",1,0)
df1$asl_flag_y <-ifelse(df1$asl_flag=="Y",1,0)
train <- sample(nrow(df1), 0.7*nrow(df1), replace = FALSE)
TrainSet <- df1[train,]
ValidSet <- df1[-train,]

summary(TrainSet)
summary(ValidSet)

equ <- "churn ~ hnd_price + eqpdays + ovrrev_Mean + roam_Mean"
temp = tree(formula(equ),data=TrainSet,mindev=.0001)
#model1 <- randomForest(formula(equ), data = TrainSet, 
#                       ntree = 500,mtry = 6, importance = TRUE)
names(temp)
temp$weights
temp
churn.tree=prune.tree(temp,best=7)
cat('pruned tree size: /n')
print(length(unique(churn.tree$where)))

#--------------------------------------------------
#plot the tree and the fits.

#plot the tree
plot(churn.tree,type="uniform")
text(churn.tree,col="blue",label=c("yval"),cex=.8)
summary(churn.tree)
#plot data with fit
churn.fit = predict(churn.tree) #get training fitted values
class(churn.fit)
View(churn.fit)

#=====================================rpart
library(rpart)
equ <- "churn ~ eqpdays+ months+ change_mou+hnd_price+ ovrrev_Mean + totmrc_Mean"
big.tree = rpart(formula(equ),method="anova",data=TrainSet,
                 control=rpart.control(minsplit=5,cp=.0005))
summary(big.tree)
names(big.tree)
big.tree$variable.importance
pred <- predict(big.tree) #predicted values for rpart

bestcp=big.tree$cptable[which.min(big.tree$cptable[,"xerror"]),"CP"]

pred_y <- ifelse(pred <= 0.5,0,1)
table(TrainSet$churn,pred_y)
#write.csv(cbind(TrainSet,pred),"check.csv")
pred <- predict(big.tree,newdata = ValidSet) #predicted values for rpart
pred_y <- ifelse(pred <= 0.5,0,1)
table(ValidSet$churn,pred_y)


#==================== random forest
library(randomForest)
n = nrow(TrainSet)
ntreev = c(10,500,5000)
nset = length(ntreev)
fmat = matrix(0,n,nset)
equ <- "churn ~ eqpdays+ months+ change_mou+hnd_price+ ovrrev_Mean + totmrc_Mean"
for(i in 1:nset) {
  #cat('doing Boston rf: ',i,'\n')
  rffit = randomForest(formula(equ),data=TrainSet,ntree=ntreev[i],maxnodes=15)
  fmat[,i] = predict(rffit)
}

#plot oob error using last fitted rffit which has the largest ntree.
par(mfrow=c(1,1))
plot(rffit)

#choosing n = 500
equ <- "churn ~ eqpdays+ months+ change_mou+hnd_price+ ovrrev_Mean + totmrc_Mean"
rffit = randomForest(formula(equ),data=TrainSet,ntree=500,maxnodes=15)
#names(rffit)
rffit$importance
#summary(rffit)
pred <- predict(rffit) #predicted values for randomforest
pred_y <- ifelse(pred <= 0.5,0,1)
table(TrainSet$churn,pred_y)
pred <- predict(rffit,newdata = ValidSet) #predicted values for rpart
pred_y <- ifelse(pred <= 0.5,0,1)
table(ValidSet$churn,pred_y)


#=====================boosting
library(gbm) #boost package
#fit boosting for various number of trees
set.seed(99)
n = nrow(TrainSet)
ntreev = c(5,20,100)
nset = length(ntreev)
fmat = matrix(0,n,nset)
equ <- "churn ~ eqpdays+ months+ change_mou+hnd_price+ ovrrev_Mean + totmrc_Mean"
for(i in 1:nset) {
  cat('doing Boston boost: ',i,'\n')
  boostfit = gbm(formula(equ),data=TrainSet,distribution='gaussian',
                 interaction.depth=2,n.trees=ntreev[i],shrinkage=.2)
  fmat[,i] = predict(boostfit,n.trees=ntreev[i])
}

#choosing ntree=500
boostfit = gbm(formula(equ),data=TrainSet,distribution='gaussian',
               interaction.depth=2,n.trees=500,shrinkage=.2)
pred <- predict(boostfit,n.trees = 500) #predicted values for randomforest
pred_y <- ifelse(pred <= 0.5,0,1)
table(TrainSet$churn,pred_y)
pred <- predict(boostfit,newdata = ValidSet,n.trees = 500) #predicted values for rpart
pred_y <- ifelse(pred <= 0.5,0,1)
table(ValidSet$churn,pred_y)


#=======================kcross validation WIP
n = dim(df1)[1]

kcv = 10
n0 = round(n/kcv,0)

out_error_rate = matrix(0,kcv,n0)

used = NULL
set = 1:n
#j=1
#i=1
for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = df1[-val,]
  test_i = df1[val,]
  
  for(i in 1:n0){
    
    big.tree = rpart(formula(equ),method="anova",data=train_i,
                     control=rpart.control(minsplit=5,cp=.0005))
    pred <- predict(big.tree,newdata = test_i) #predicted values for rpart
    pred_y <- ifelse(pred <= 0.5,0,1)
    t <- table(test_i$churn,pred_y)
    
    aux = t[2,2]/(t[1,2]+t[2,2])
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
}


mMSE = apply(out_MSE,2,mean)
par(mfrow=c(1,1))
plot(log(1/(1:100)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="California Housing (knn)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/100)+0.4,sqrt(mMSE[100]),"k=100")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")


