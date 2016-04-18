rm(list=ls())
library(e1071)
library(randomForest)
library(nnet)
library(ada)
library(kknn)
library(pROC)
library(lattice)
library(quadprog)
data = read.csv("cleandata.csv")[,-1]
myy = data[,1,drop = F]
myx = data.matrix(data[,-1])
n =dim(myx)[1]
p =dim(myx)[2]

##start CV
Niter = 100
kfold = 10
## parameter ##
cost = 1
gamma = 0.003981072

ntree=750

size=6
decay=0.5

nu=0.1
  
parak=99
distance=1

stack.res = array(NA,c(Niter, round(n/kfold),5))
stack.y.res = array(NA,c(Niter, round(n/kfold),1))
for(j in 1:Niter){
    testID = sample(1:n,round(n/kfold))
  
    print(j)
    ##svm
    model = svm(myx[-testID,],myy[-testID,],cost = cost,gamma = gamma, probability = T)
    yhat = predict(model,myx[testID,],probability = T)
    stack.res[j,,1] = attr(yhat,"probabilities")[,1]
    
    ##randomforest
    model = randomForest(myx[-testID,],myy[-testID,],ntree  = ntree, probability = T)
    rf_yhat = predict(model,myx[testID,],type = "prob")
    stack.res[j,,2] = rf_yhat[,2]

    ##Neural Network
    model = nnet(ANGLE.CLOSURE~.,data = data[-testID,], size = size,decay = decay )
    nn_yhat = predict(model,data[testID,],type = "raw")
    stack.res[j,,3] = nn_yhat
    ##Boosting 
    model = ada(myx[-testID,],myy[-testID,] ,nu=nu)
    b_yhat = predict(model,as.data.frame(myx[testID,]),type = "prob")
    stack.res[j,,4] = b_yhat[,2]
    ## K Nearest Neighbour
    model = kknn(ANGLE.CLOSURE~.,train = data[-testID,],test = data[testID,], k = parak,distance = distance )
    k_yhat = model$prob
    stack.res[j,,5] = k_yhat[,2]
    stack.y.res[j,,1] = data[testID,1]
}

stack_pres = matrix(0,dim(stack.res)[2]*dim(stack.res)[1],dim(stack.res)[3])
stack_ys = matrix(NA,dim(stack.y.res)[2]*dim(stack.y.res)[1],1)
for(ii in 1:dim(stack.res)[1]){
  for(jj in 1:dim(stack.res)[2]){
    for(kk in 1:dim(stack.res)[3]){
      stack_pres[( (ii-1)*dim(stack.res)[2]+jj),kk] = stack.res[ii,jj,kk]         
    }
    stack_ys[(ii-1)*dim(stack.y.res)[2]+jj,1] = stack.y.res[ii,jj,1]
  }
}

Dmat = t(stack_pres)%*%stack_pres
dvec = t(stack_ys)%*%stack_pres
Amat <- cbind(rep(1,5), diag(5))
bvec <- c(1,rep(0,5))

weightsConstrained = as.numeric(solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)$solution)
weightsUnConstrained=solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)$unconstrained.solution

weightsConstrained
weightsUnConstrained

dput(stack_pres,"stack_pres")
dput(stack_ys,"stack_ys")
dput(weightsUnConstrained,"weightsUnConstrained")
dput(weightsConstrained,"weightsConstrained")


##read data
weightsUnConstrained =dget(weightsUnConstrained)
weightsConstrained = dget(weightsConstrained)

mytest = dget("mytest.r")
data = read.csv("cleandata.csv")[,-1]
myy = data[,1,drop = F]
myx = data.matrix(data[,-1])


##svm
model = svm(myx,myy[,],cost = cost,gamma = gamma, probability = T)
yhat = predict(model,mytest[,-1],probability = T)
yhat = attr(yhat,"probabilities")[,1]

##randomforest
model = randomForest(myx[,],myy[,],ntree  = ntree, probability = T)
rf_yhat = predict(model,mytest[,],type = "prob")[,2]

##Neural Network
model = nnet(ANGLE.CLOSURE~.,data = data[,], size = size,decay = decay )
nn_yhat = predict(model,mytest[,],type = "raw")
##Boosting 
model = ada(myx[,],myy[,] ,nu=nu)
b_yhat = predict(model,as.data.frame(mytest[,]),type = "prob")[,2]
## K Nearest Neighbour
model = kknn(ANGLE.CLOSURE~.,train = data[,],test = mytest[,], k = parak,distance = distance )
k_yhat = model$prob[,2]
(cbind(yhat,rf_yhat,nn_yhat,b_yhat,k_yhat))

#weightsConstrained=c(0.4391,0.1338,-5.111e-18,4.004e-02,0.3871)
#weightsUnConstrained=c(0.4459,0.1128,-0.03761,0.1118,0.3923)

#weightsConstrained=c(0.2778,0,0.2677,0.1662,0.2883)
#weightsUnConstrained=c(1.0330,4.4781,1.0558,1.2125,1.0136)
  
yhat = cbind(yhat,rf_yhat,nn_yhat,b_yhat,k_yhat) %*% matrix(weightsConstrained,,1)
roc = roc(mytest[,1], yhat[,1])
auc(roc)## 0.9628
plot(roc)

yhat = cbind(yhat,rf_yhat,nn_yhat,b_yhat,k_yhat) %*% matrix(weightsUnConstrained,,1)
roc = roc(mytest[,1], yhat[,1])
auc(roc)## 0.9236
plot(roc)
