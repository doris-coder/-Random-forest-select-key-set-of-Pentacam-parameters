###########machine learning
all=read.csv("analysis_file_new.csv")

####mean imputation(parallel)########
colnames(all)[1:10]
e=c()
for (i in 2:ncol(all)){
  a=sum(is.na(all[,i]))
  if (a>0){
    e=c(e,i)
  }
}
#102
all1=all
for(i in 2:ncol(all)){
  all1[is.na(all1[,i]), i]=round(mean(all1[,i], na.rm = TRUE),2)
}

e=c()
for (i in 2:ncol(all)){
  a=sum(is.na(all1[,i]))
  if (a>0){
    e=c(e,i)
  }
}
#0
#all1$sex=as.numeric(all1$sex)
#######train and test
smp_size <- floor(0.7 * nrow(all1))
set.seed(123)
train_ind <- sample(seq_len(nrow(all1)), size = smp_size)

train <- all1[train_ind, ]
test <- all1[-train_ind, ]


######importance ranking
require(caret)
library(randomForest)
library(e1071)
colnames(train)[1:10]
train1=train
set.seed(123)
ctrl<-trainControl(method = "cv",number =10)
rf<-train(group~., data =train1, 
          method = "rf", trControl = ctrl, metric='Accuracy')
varImp(rf)
importance=data.frame(varImp(rf)$importance)
importance$Vars<-row.names(importance)
#write.csv(importance,"importance_mean.csv",na="",row.names = F)

imp=read.csv("importance_mean.csv")
imp=imp[with(imp,order(-Overall)),]
imp$Vars[1:20]
rf_cv_mean=data.frame("number"=0,"accuracy"=0,stringsAsFactors=T)
control <- trainControl(method="cv", number=10)
max=0
p=0
for (i in c(2:10,20,50,100,150,200,250,300,350,400,450,500,550,600,650,700,750,800,811)){
  p=p+1
  print(i)
  a=imp[1:i,]
  list=c()
  for (j in 1:ncol(train1)){
    parameter=colnames(train1)[j]
    if (parameter %in% a$Vars){
      list=c(list,j)
    }
  }
  TrainSetm=train1[,list]
  TrainSetn=cbind(TrainSetm,train1$group)
  colnames(TrainSetn)[i+1]="group"
  set.seed(1234)
  rf<- train(group~., data=TrainSetn, method="rf", metric='Accuracy',trControl=control)
  #print(rf$finalModel)
  #print(rf$finalModel$cptable)
  accuracy=max(rf$results$Accuracy)
  rf_cv_mean<- rbind(rf_cv_mean, data.frame("number"=i, "accuracy" = accuracy))
  #flush.console()
  #plot(rf_cv_mean[-1,]$number[1:p],rf_cv_mean[-1,]$accuracy[1:p],type='b',ylim=(0:1),las=2,cex.axis=0.4, ylab="Accuracy", main="Random forest")
  #lines(rf_cv_mean[-1,]$number[1:p],rf_cv_mean[-1,]$accuracy[1:p],type="o", col="red", lwd=2, pch=19)
  #Sys.sleep(.09)
  if (accuracy>max){
    max=accuracy
    print(max)
  }
}
rf_cv_mean1=rf_cv_mean[-1,]
rf_cv_mean1=unique(rf_cv_mean1)
#write.csv(all1,"data_mean_imputation.csv",row.names = F,na="")
#write.csv(rf_cv_mean1,"result_mean_imputation.csv",row.names = F,na="")

################error analysis#########
###############top 6 + one by one
imp=read.csv("importance_mean.csv")
imp=imp[with(imp,order(-Overall)),]
imp$Vars[1:20]
train1=train
a=imp[c(1:20),]
list=c()
for (j in 1:ncol(train1)){
  parameter=colnames(train1)[j]
  if (parameter %in% a$Vars){
    list=c(list,j)
  }
}


TrainSetm=train1[,list]
TrainSetn=cbind(TrainSetm,train1$group)
colnames(TrainSetn)
colnames(TrainSetn)[21]="group"


train1=TrainSetn
control <- trainControl(method="cv", number=10)
max=0
p=0
for (i in c(7:20)){
  p=p+1
  print(i)
  a=imp[c(1:6,i),]
  list=c()
  for (j in 1:ncol(train1)){
    parameter=colnames(train1)[j]
    if (parameter %in% a$Vars){
      list=c(list,j)
    }
  }
  TrainSetm=train1[,list]
  TrainSetn=cbind(TrainSetm,train1$group)
  colnames(TrainSetn)[8]="group"
  set.seed(1234)
  rf<- train(group~., data=TrainSetn, method="rf", metric='Accuracy',trControl=control)
  #print(rf$finalModel)
  #print(rf$finalModel$cptable)
  accuracy=max(rf$results$Accuracy)
  #rf_c<- rbind(rf_cv, data.frame("number"=i, "accuracy" = accuracy))
  if (accuracy>max){
    max=accuracy
    print(max)
  }
}

train1=train
p=0
a=imp[c(1:6,16),]
list=c()
for (j in 1:ncol(train1)){
  parameter=colnames(train1)[j]
  if (parameter %in% a$Vars){
    list=c(list,j)
  }
}

TrainSetm=train1[,list]
TrainSetn=cbind(TrainSetm,train1$group)
colnames(TrainSetn)
colnames(TrainSetn)[8]="group"

control <- trainControl(method="cv", number=10,savePredictions = 'final')
set.seed(1234)
rf<- train(group~., data=TrainSetn, method="rf", metric='Accuracy',trControl=control)
rf
med_error=rf$pred
med_error1=med_error[med_error$pred!=med_error$obs,]
mean=med_error1$rowIndex

TrainSetn=TrainSetn[-c(46,106,181),]
rfoutput=data.frame("combination"="0","accuracy"=0,"sensitivity"=0,"specificity"=0,"presicion"=0,stringsAsFactors=T)
ctrl<-trainControl(method = "cv",number =10,summaryFunction = multiClassSummary)
n=1
for (i in 2:7){
  maxacc=0
  predictors=0
  a=combn(1:7,i)
  for (j in 1:ncol(a)){
    n=n+1
    b=a[,j]
    p=paste(colnames(TrainSetn[,b])[1])
    for (m in 2:length(b)){
      p=paste(p,"+",colnames(TrainSetn[,b])[m])
    }
    q=paste("group","~",p)
    formula <- as.formula(q)
    set.seed(7)
    rfCVFit<-train(formula, data =TrainSetn, 
                   method = "rf", trControl = ctrl, metric='Accuracy')
    #t=table(predict(mlpCVFit),train_processed$group)
    #accuracy=round((t[2,2]+t[1,1])/101,2)
    accuracy=max(rfCVFit$results$Accuracy)
    index=which(rfCVFit$results$Accuracy==accuracy)
    sensitivity=(rfCVFit$results$Sensitivity)[index]
    specificity=(rfCVFit$results$Specificity)[index]
    precision=(rfCVFit$results$Precision)[index]
    rfoutput <- rbind(rfoutput, data.frame("combination" = toString(colnames(TrainSetn[,b])), "accuracy" = accuracy,"sensitivity"=sensitivity,"specificity"=specificity,"presicion"=precision))
    #flush.console()
    #plot(rfoutput[-1,]$combination[1:n],rfoutput[-1,]$accuracy[1:n],type='b',ylim=(0:1),las=2,cex.axis=0.4, ylab="Accuracy of Cross-Validation", main="Support Vector Machines with Linear Kernel")
    #lines(rfoutput[-1,]$combination[1:n],rfoutput[-1,]$accuracy[1:n],type="o", col="yellow", lwd=2, pch=19)
    Sys.sleep(.09)
    if (accuracy>maxacc){
      maxacc=accuracy
      predictors=b
    }
  }
  print(toString(colnames(TrainSetn[,predictors])))
  print(maxacc)
  print(sensitivity)
  print(specificity)
}


# summary_ecc_f, chamber_eccsph, index_is_value
ctrl<-trainControl(method = "cv",number =10,summaryFunction = multiClassSummary)
set.seed(7)
rfCVFit<-train(group~summary_ecc_f+index_is_value+chamber_eccsph, data =TrainSetn, 
               method = "rf", trControl = ctrl, metric='Accuracy',ntree =5)
rfCVFit

a=predict(rfCVFit,rbind(test,train))
table(a,rbind(test,train)$group)

############clinical measure###########
########VA conversion
data3=read.csv("clinical data.csv")
l=read.csv("VA conversion.csv")
l$index
data3$bcva
l=l[,-2]
data4=merge(data3,l,by.x="bcva",by.y="index",all=F)
colnames(data4)
data5=data4[,-c(1,5)]

####statistical analysis
library(caret)
colnames(data5)
summary(data5)

data5$summary_ecc_f=as.numeric(paste(data5$summary_ecc_f))
data5$chamber_eccsph=as.numeric(paste(data5$chamber_eccsph))
data5$is_value=as.numeric(paste(data5$is_value))
summary(data5)
case2=data5[data5$group=="Case",]
control2=data5[data5$group!="Case",]

mean(control2$se)
mean(control2$logmar)
sd(control2$se)
sd(control2$logmar)

mean(case2$se)
mean(case2$logmar)
sd(case2$se)
sd(case2$logmar)

a=rbind(case2$se,control2$se)
skewness(a,na.rm = T)
wilcox.test(case2$se,control2$se,correct = "FALSE")

a=rbind(case2$logmar,control2$logmar)
skewness(a,na.rm = T)
wilcox.test(case2$logmar,control2$logmar,correct = "FALSE")

set.seed(7)
rfCVFit<-train(group~summary_ecc_f+chamber_eccsph+is_value+se+logmar, data =data5, 
               method = "rf", trControl = ctrl, metric='Accuracy')
rfCVFit
