library(kknn)
library(class)

df <- read.csv("Telecom_customer churn.csv")

df1 <- na.omit(df)
churn <- df1$churn 

n=dim(df1)[1]
ind = sample(1:n,(.80*n))

Y = churn[ind]
Telecom = df1[ind,]

test = Telecom
train = Telecom

########CROSS VALIDATION########

n = dim(Telecom)[1]

kcv = 10
n0 = round(n/kcv,0) 

out_MSE = matrix(0,kcv,100) 

used = NULL 
set = 1:n 
j=1
i=1
for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)} 
  if(n0>=length(set)){val=set} 
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for(i in 1:100){
    
    near = kknn(churn ~ eqpdays + months + change_mou + hnd_price+ ovrrev_Mean + totmrc_Mean,
                train_i,test_i,k=i,kernel = "rectangular") 
    aux = mean((test_i[,1]-near$fitted)^2) 
    
    out_MSE[j,i] = aux
  }

  used = union(used,val)  
  set = (1:n)[-used] 
  
  cat(j,'\n') 
}

##evaluating how good each k is##
mMSE = apply(out_MSE,2,mean) 
par(mfrow=c(1,1))
plot(log(1/(1:100)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="Telecom Dataset (knn)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/100)+0.4,sqrt(mMSE[100]),"k=100")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")

