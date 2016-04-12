myData=read.csv("AngleClosure.csv",na.strings=".")
##part1 clean the data
tobeRemoved=c(which(names(myData)=="EYE"),
              which(names(myData)=="GENDER"),
              which(names(myData)=="ETHNIC"))
myData=myData[,-tobeRemoved]
misCol=which(apply(myData,1,function(xx){sum(is.na(xx))})>=1)
myData=myData[-misCol,]
L=dim(myData)[1]

##part2 develop Prediction Models
##omit those variables for building prediction models
TBREMOVED=c(which(names(myData)=="HGT"),
            which(names(myData)=="WT"),
            which(names(myData)=="ASPH"),
            which(names(myData)=="ACYL"),
            which(names(myData)=="SE"),
            which(names(myData)=="AXL"),
            which(names(myData)=="CACD"),
            which(names(myData)=="AGE"),
            which(names(myData)=="CCT.OD"),
            which(names(myData)=="PCCURV_mm"))

myData=myData[,-TBREMOVED]
name=names(myData)
responce=myData$ANGLE.CLOSURE

predictor=as.matrix(myData[,-12])
head(predictor)

myData=data.frame(x=predictor,y=responce)
attach(myData)

nFolds=10
nIter=25

#####################################################

##10-fold cross-validation with support vector machine when kernel is linear,###
###where the turning parameter is only the cost####

library(e1071)
library(pROC)

AUClist=NULL

for (j in seq(0.01,5.01,0.5)){
  AUC=NULL
  for (i in 1:nIter){
    testset=sample(length(responce))[1:round(length(responce)/nFolds)]
    testx=predictor[testset,]
    testy=responce[testset]
    trainx=predictor[-testset,]
    trainy=responce[-testset]
    
    svmfit=svm(trainy~temp,kernel="linear",cost=j,probability=TRUE)
    temp=testx
    predicty=attr(predict(svmfit,temp,probability=TRUE),
                  "probabilities")[,1]
    
    rocfit=roc(as.numeric(testy),as.numeric(predicty))
    AUC=c(AUC,auc(rocfit))
  }
  print(j)
  AUCVALUE=mean(AUC)
  AUClist=c(AUClist,AUCVALUE)
}

dev.new(width=1.2*3,height=2.5)
par(mai=c(0.5,0.5,0.3,0.05),cex=0.8)
plot(seq(0.01,5.01,0.5),AUClist,type="l",
     axes=FALSE,xlab="",ylab="",main="SVM, Linear Kernel")
box()
axis(1,padj=-0.5)
title(xlab="Cost",line=1.65)
axis(2,padj=0.5)
title(ylab="AUC",line=2)

bestmodel=which(AUClist==max(AUClist))
bestlinear=min(seq(0.01,5.01,0.5)[bestmodel])
max(AUClist)
bestlinear

#Output#
#> max(AUClist)
#[1] 0.9660359
#> bestlinear
#[1] 3.51

#####################################################

##svm with radial kernel,using gamma and cost as turning parameter##

AUCmatrix2=NULL
count=1

for (j in c(0.1,0.5,1,2)){
  for(r in c(0.005,0.01,0.05,0.1)){
    AUC=NULL
    for (i in 1:nIter){
      
      testset=sample(length(responce))[1:round(length(responce)/nFolds)]
      testx=predictor[testset,]
      testy=responce[testset]
      trainx=predictor[-testset,]
      trainy=responce[-testset]
      
      temp2=trainx
      svmfit=svm(trainy~temp2,kernel="radial",
                 cost=j,gamma=r,probability=TRUE)
      temp2=testx
      predicty=attr(predict(svmfit,temp2,probability=TRUE),
                    "probabilities")[,1]
      rocfit=roc(as.numeric(testy),as.numeric(as.factor(predicty)))
      AUC=c(AUC,auc(rocfit))
    }
    
    print(c(j,r))
    
    AUCVALUE=mean(AUC)
    AUCmatrix2=cbind(AUCmatrix2,c(AUCVALUE,j,r))
  }
}

dev.new(width=1.2*3,height=2.5)
par(mai=c(0.5,0.5,0.3,0.1),cex=0.8)
image(unique(AUCmatrix2[2,]),unique(AUCmatrix2[3,]),
      matrix(AUCmatrix2[1,],
             length(unique(AUCmatrix2[2,])),
             length(unique(AUCmatrix2[3,]))),
      axes=FALSE,xlab="",ylab="",main="SVM, Radial Kernel")
box()
axis(1,padj=-0.5)
title(xlab="Cost",line=1.65)
axis(2,padj=0.5)
title(ylab="Gamma",line=2)

bestmodel=which(AUCmatrix2[1,]==max(AUCmatrix2[1,]))
AUCmatrix2[,bestmodel]

#Output#
#[1] 0.962859 2.000000 0.010000

############# 2,next we use boosting###############

install.packages("gbm")
library(gbm)
##turning parameter we interested in are n.trees and shrinkage##
AUCmatrix.Boosting=NULL
count=1

for (NumTrees in c(500,1000,2000)){
  for(r in c(0.001,0.01,0.05,0.1,0.2,0.5)){
    AUC=NULL
    for (i in 1:nIter){
      
      testset=sample(length(responce))[1:round(length(responce)/nFolds)]
      testx=predictor[testset,]
      testy=as.numeric(responce[testset]=="YES")
      trainx=predictor[-testset,]
      trainy=as.numeric(responce[-testset]=="YES")
      
      boosting.fit=gbm.fit(y=trainy,x=trainx,
                           distribution="bernoulli",n.trees=NumTrees,shrinkage = r,
                           verbose=FALSE)
      predicty=as.matrix(predict(boosting.fit,testx,n.trees=NumTrees))
      rocfit=roc(testy,predicty)
      AUC=c(AUC,auc(rocfit))
    }
    AUCVALUE=mean(AUC)
    AUCmatrix.Boosting=cbind(AUCmatrix.Boosting,c(AUCVALUE,NumTrees,r))
    print(c(NumTrees,r))
  }
}

dev.new(width=1.2*3,height=2.5)
par(mai=c(0.5,0.5,0.3,0.15),cex=0.8)
image(unique(AUCmatrix.Boosting[2,]),unique(AUCmatrix.Boosting[3,]),
      matrix(AUCmatrix.Boosting[1,],
             length(unique(AUCmatrix.Boosting[2,])),
             length(unique(AUCmatrix.Boosting[3,]))),
      axes=FALSE,xlab="",ylab="",main="Gradient Boosted")
box()
axis(1,padj=-0.5)
title(xlab="Trees",line=1.65)
axis(2,padj=0.5)
title(ylab="Shrinkage",line=2)

bestmodel=which(AUCmatrix.Boosting[1,]==max(AUCmatrix.Boosting[1,]))
AUCmatrix.Boosting[,bestmodel]

#Output#
#[1]   0.9594978 500.0000000   0.0100000

#### 3,next we use randomforest#####

install.packages("randomForest")
library(randomForest)

##where the turn parameter "mtry",number of predictors to be chosen as split candidate in each split##

AUCmatrix.RF=NULL
count=1

for(m in c(2,3,4,5,6,7,8,9,10,11)){
  AUC=NULL
  for (i in 1:nIter){
    
    testset=sample(length(responce))[1:round(length(responce)/nFolds)]
    testx=predictor[testset,]
    testy=as.numeric(responce[testset]=="YES")
    trainx=predictor[-testset,]
    trainy=as.numeric(responce[-testset]=="YES")
    
    RF.fit=randomForest(y=trainy,x=trainx,n.trees=5000,mtry=m)
    predicty=as.matrix(predict(RF.fit,testx))
    
    rocfit=roc(as.numeric(testy)-1,as.numeric(as.factor(predicty))-1)
    AUC=c(AUC,auc(rocfit))
  }
  AUCVALUE=mean(AUC)
  AUCmatrix.RF=cbind(AUCmatrix.RF,c(AUCVALUE,m))
  print(m)
}

dev.new(width=1.2*3,height=2.5)
par(mai=c(0.5,0.5,0.3,0.05),cex=0.8)
plot(AUCmatrix.RF[2,],AUCmatrix.RF[1,],type="l",
     axes=FALSE,xlab="",ylab="",main="Random Forest")
box()
axis(1,padj=-0.5)
title(xlab="mTry",line=1.65)
axis(2,padj=0.5)
title(ylab="AUC",line=2)

bestmodel=which(AUCmatrix.RF[1,]==max(AUCmatrix.RF[1,]))
AUCmatrix.RF[,bestmodel]

#Output#
#[1] 0.955753 3.000000

### 4,next we explore a relatively easies model "KNN"##

library(class)

##the turning parameter is K 
AUCmatrix.KNN=NULL
count=1

for(kN in seq(10,500,10)){
  AUC=NULL
  for (i in 1:nIter){
    
    testset=sample(length(responce))[1:round(length(responce)/nFolds)]
    testx=predictor[testset,]
    testy=as.numeric(responce[testset]=="YES")
    trainx=predictor[-testset,]
    trainy=as.numeric(responce[-testset]=="YES")
    
    KNN.fit=attr(knn(trainx,testx,trainy,k=kN,prob=TRUE),"prob")   
    
    rocfit=roc(as.numeric(testy)-1,as.numeric(KNN.fit))
    AUC=c(AUC,auc(rocfit))
  }
  AUCVALUE=mean(AUC)
  AUCmatrix.KNN=cbind(AUCmatrix.KNN,c(AUCVALUE,kN))
  print(kN)
}

dev.new(width=1.2*3,height=2.5)
par(mai=c(0.5,0.5,0.3,0.05),cex=0.8)
plot(AUCmatrix.KNN[2,],AUCmatrix.KNN[1,],type="l",
     axes=FALSE,xlab="",ylab="",main="KNN")
box()
axis(1,padj=-0.5)
title(xlab="nN",line=1.65)
axis(2,padj=0.5)
title(ylab="AUC",line=2)

bestmodel=which(AUCmatrix.KNN[1,]==max(AUCmatrix.KNN[1,]))
AUCmatrix.KNN[,bestmodel]

#Output#
[1]   0.9059938 410.0000000

#### 5,logistic regression using AIC instead of cross-validation####

AUC=NULL
for (i in 1:nIter){
  
  testset=sample(length(responce))[1:round(length(responce)/nFolds)]
  testx=predictor[testset,]
  testy=as.numeric(responce[testset]=="YES")
  trainx=predictor[-testset,]
  trainy=as.numeric(responce[-testset]=="YES")
  
  fit=step(glm(trainy~1,data=data.frame(trainx),family="binomial"),
           scope=trainy~AOD750+TISA750+IT750+IT2000+ITCM+
             IAREA+ICURV+ACW_mm+ACA+ACV+LENSVAULT)
  
  rocfit=roc(testy,predict(fit,newdata=data.frame(testx)))
  AUC=c(AUC,auc(rocfit))
}
AUCVALUE=mean(AUC)
AUCVALUE

#[1] 0.9574318

#######stacking##########

muVector=NULL
Yvector=NULL

for (i in 1:nIter){
  
  mu=NULL
  Y=NULL
  
  testset=sample(length(responce))[1:round(length(responce)/nFolds)]
  testx=predictor[testset,]
  testy=as.numeric(responce[testset]=="YES")
  trainx=predictor[-testset,]
  trainy=as.numeric(responce[-testset]=="YES")
  
  ##KNN
  kN=410
  KNN.fit.u=1-attr(knn(trainx,testx,trainy,k=kN,prob=TRUE),"prob") 
  
  ##GLM
  fit=step(glm(trainy~1,data=data.frame(trainx),family="binomial"),
           scope=trainy~AOD750+TISA750+IT750+IT2000+ITCM+
             IAREA+ICURV+ACW_mm+ACA+ACV+LENSVAULT)
  predict.glm.u=predict(fit,newdata=data.frame(testx),type="response")
  
  ##RANDOMFOREST
  m=3
  RF.fit=randomForest(y=trainy,x=trainx,n.trees=5000,mtry=m)
  predict.RF.u=as.matrix(predict(RF.fit,testx))
  
  ##BOOSTING
  NumTrees=500
  r=0.01
  boosting.fit=gbm.fit(y=trainy,x=trainx,
                       distribution="bernoulli",n.trees=NumTrees,shrinkage = r,
                       verbose=FALSE)
  predict.boosting.u=as.matrix(predict(boosting.fit,testx,n.trees=NumTrees))
  predict.boosting.u=exp(predict.boosting.u)/(1+exp(predict.boosting.u))
  
  ###SVM
  j=3.51
  temp=trainx
  svmfit=svm(factor(trainy)~temp,kernel="linear",cost=j,probability=TRUE)
  temp=testx
  predict.svm.u=attr(predict(svmfit,temp,probability=TRUE),
                     "probabilities")[,1]
  
  mu=cbind(KNN.fit.u,predict.glm.u,predict.RF.u,predict.boosting.u,predict.svm.u)
  muVector=rbind(muVector,mu)
  Y=as.numeric(responce[testset]=="YES")
  Yvector=c(Yvector,Y)
  
}

### unconstrained stack model
beta1=solve((t(muVector)%*%muVector))%*%t(muVector)%*%Yvector
predY=muVector%*%beta1
roc(Yvector,predY)

### constrained stack model
library(quadprog)
amatrix=cbind(matrix(1,5,1),diag(1,5,5))

s <- solve.QP( t(muVector) %*% muVector, t(Yvector) %*% muVector, amatrix, c(1,0,0,0,0,0),meq=1)
beta2=s$solution
predY=muVector%*%beta2
roc(Yvector,predY)


#####validation######
case=read.csv("AngleClosure_ValidationCases.csv",header=TRUE)
control=read.csv("AngleClosure_ValidationControls.csv",header=TRUE)

temp=data.frame(matrix(NA,dim(case)[1],0))
temp$AOD750=apply(cbind(case$rAOD750,case$lAOD750),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
})       
temp$TISA750=apply(cbind(case$rTISA750,case$lTISA750),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
}) 
temp$IT750=apply(cbind(case$rIT750,case$lIT750),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
}) 
temp$IT2000=apply(cbind(case$rIT2000,case$lIT2000),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
})
temp$ITCM=apply(cbind(case$rITCM,case$lITCM),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
})
temp$IAREA=apply(cbind(case$rIAREA,case$lIAREA),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
})
temp$ICURV=apply(cbind(case$rICURV,case$lICURV),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
})
temp$ACW_mm=case$ACWmm
temp$ACA=case$ACA
temp$ACV=case$ACV
temp$LENSVAULT=case$LENSVAULT
case=temp

temp=data.frame(matrix(NA,dim(control)[1],0))
temp$AOD750=apply(cbind(control$rAOD750,control$lAOD750),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
})       
temp$TISA750=apply(cbind(control$rTISA750,control$lTISA750),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
}) 
temp$IT750=apply(cbind(control$rIT750,control$lIT750),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
}) 
temp$IT2000=apply(cbind(control$rIT2000,control$lIT2000),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
})
temp$ITCM=apply(cbind(control$rITCM,control$lITCM),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
})
temp$IAREA=apply(cbind(control$rIAREA,control$lIAREA),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
})
temp$ICURV=apply(cbind(control$rICURV,control$lICURV),1,function(xx){
  if(!is.na(xx[1])){
    return(xx[1])
  }else{
    return(xx[2])
  }
})
temp$ACW_mm=control$ACW.mm.
temp$ACA=control$ACA
temp$ACV=control$ACV
temp$LENSVAULT=control$LENSVAULT
control=temp

case=na.omit(case)
positive=dim(case)[1]

control=na.omit(control)
negative=dim(control)[1]

validation=rbind(case,control)
ANGLE.CLOSURE1=rep(c(1,0),c(positive,negative))
validation=cbind(validation,ANGLE.CLOSURE1)
colnames(validation)[12]="ANGLE.CLOSURE"
Yvalid=as.matrix(validation[12])
testX=as.matrix(validation[1:11])

##KNN 0.955###
KNN.u=1-attr(knn(predictor,testX,responce,k=kN,prob=TRUE),"prob")
dev.new(width=1.2*3,height=1.2*3)
par(mai=c(0.5,0.5,0.3,0.05),cex=0.8)
plot(roc(as.numeric(Yvalid),KNN.u),main="KNN",print.auc=TRUE)

##GLM 0.954##
fit=step(glm(responce~1,data=data.frame(predictor),family="binomial"),
         scope=trainy~AOD750+TISA750+IT750+IT2000+ITCM+
           IAREA+ICURV+ACW_mm+ACA+ACV+LENSVAULT)
predict.u=predict(fit,newdata=data.frame(testX),type="response")
dev.new(width=1.2*3,height=1.2*3)
par(mai=c(0.5,0.5,0.3,0.05),cex=0.8)
plot(roc(as.numeric(Yvalid),predict.u),main="GLM",print.auc=TRUE)

##RANDOMFOREST 0.959###
RF.fit=randomForest(y=as.numeric(responce=="YES"),x=predictor,n.trees=5000,mtry=m)
predict.RF.u=as.matrix(predict(RF.fit,testX))
dev.new(width=1.2*3,height=1.2*3)
par(mai=c(0.5,0.5,0.3,0.05),cex=0.8)
plot(roc(as.numeric(Yvalid),predict.RF.u),main="RF",print.auc=TRUE)


##BOOSTING 0.959###
boosting.fit=gbm.fit(y=as.numeric(responce=="YES"),x=predictor,
                     distribution="bernoulli",n.trees=NumTrees,shrinkage = r,
                     verbose=FALSE)
predict.boosting.u=as.matrix(predict(boosting.fit,testX,n.trees=NumTrees))
predict.boosting.u=exp(predict.boosting.u)/(1+exp(predict.boosting.u))
dev.new(width=1.2*3,height=1.2*3)
par(mai=c(0.5,0.5,0.3,0.05),cex=0.8)
plot(roc(as.numeric(Yvalid),predict.boosting.u),main="Boosted",print.auc=TRUE)

###SVM 0.953##
temp=predictor
svmfit=svm(factor(as.numeric(responce=="YES"))~temp,kernel="linear",cost=j,probability=TRUE)
temp=testX
predict.svm.u=attr(predict(svmfit,temp,probability=TRUE),
                   "probabilities")[,1]
dev.new(width=1.2*3,height=1.2*3)
par(mai=c(0.5,0.5,0.3,0.05),cex=0.8)
plot(roc(as.numeric(Yvalid),predict.svm.u),main="SVM",print.auc=TRUE)

#0.959 unconstrained##
mu1=cbind(KNN.u,predict.u,predict.RF.u,predict.boosting.u,predict.svm.u)
predY=mu1%*%beta1
dev.new(width=1.2*3,height=1.2*3)
par(mai=c(0.5,0.5,0.3,0.05),cex=0.8)
plot(roc(as.numeric(Yvalid),predY),main="unconstrained",print.auc=TRUE)


#0.959 constrained
mu2=cbind(KNN.u,predict.u,predict.RF.u,predict.boosting.u,predict.svm.u)
predY=mu2%*%beta2
dev.new(width=1.2*3,height=1.2*3)
par(mai=c(0.5,0.5,0.3,0.05),cex=0.8)
plot(roc(as.numeric(Yvalid),predY),main="constrained",print.auc=TRUE)



