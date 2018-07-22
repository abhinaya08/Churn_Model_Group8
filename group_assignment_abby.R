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
#==plotting all variables
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


#======random forest variable
#install.packages("randomForest")
#library(randomForest)
library(tree)

set.seed(100)
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
cat('pruned tree size: \n')
print(length(unique(boston.tree$where)))

#--------------------------------------------------
#plot the tree and the fits.

#plot the tree
plot(churn.tree,type="uniform")
text(churn.tree,col="blue",label=c("yval"),cex=.8)

#plot data with fit
churn.fit = predict(churn.tree) #get training fitted values


#====rpart
library(rpart)
big.tree = rpart(formula(equ),method="anova",data=TrainSet,
                 control=rpart.control(minsplit=5,cp=.0005))

