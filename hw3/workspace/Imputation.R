setwd("/Users/huangge/GoogleCloudDrive/6740/hw3/workspace")
mydata = read.csv("AngleClosure.csv")
mydata = as.matrix(mydata[,-1])

##delete EYE, GENDER, and ETHNIC
##Omit the variables HGT, WT, ASPH, ACYL, SE, AXL, CACD, AGE, CCT.OD, and PCCURV_mm
rml = c("EYE", "GENDER", "ETHNIC")
rml = c(rml,"HGT", "WT", "ASPH", "ACYL", "SE", "AXL", "CACD", "AGE", "CCT.OD", "PCCURV_mm")
data = mydata[,!colnames(mydata) %in% rml]

## delete rows of the dataset which have any missing values.
remove_index_set = apply(data,1,function(xx){
  return(sum(is.na(xx))>0)
})
data = data[!remove_index_set,]



##move response to first column
response = c("ANGLE.CLOSURE")
myy = data[,response,drop=FALSE]
myx = data[,!colnames(data) %in% response]
data = cbind(myy,myx)

write.csv(file="cleandata.csv", x=data)

## impute test data
##TODO
cbind(findIndex('l',control),findIndex('r',control))
findIndex = function(side,data){
  r = matrix(NA,dim(myx)[2],1 )
  for(ii in 1:length(r) ){
    
    cur = colnames(myx)[ii]
    cur_index =  which(colnames(data)==cur) 
    if(length(cur_index)  > 0 )  {
      r[ii,1]  =   cur_index
    }
    cur = paste(side,cur,sep = "")
    cur_index =  which(colnames(data)==cur) 
    if(length( cur_index )  > 0 )  {
      r[ii,1]   =   cur_index
    }
  }
  return(r)
}

## use input column index ##
case = read.csv("AngleClosure_ValidationCases.csv")
control = read.csv("AngleClosure_ValidationControls.csv")

myCasesNewL = c(7,9,11,12,13,14,15,30,31,32,36)
myCasesNewR = c(19,21, 23:27,30:32,36)
myControlsNewR <- c(18,20,22,23,24,25,26,29,30,31,35)
myControlsNewL <- c(6,8,10,11,12,13,14,29,30,31,35)

remove_index_set = apply(case[,myCasesNewR],1,function(xx){
  return(sum(is.na(xx))>0)
})
caseR = case[!remove_index_set,]

remove_index_set = apply(case[remove_index_set,myCasesNewL],1,function(xx){
  return(sum(is.na(xx))>0)
})
caseL = case[!remove_index_set,]
case = rbind(caseR,caseL)

remove_index_set = apply(control[,myControlsNewR],1,function(xx){
  return(sum(is.na(xx))>0)
})
controlR = control[!remove_index_set,]

remove_index_set = apply(control[remove_index_set,myControlsNewL],1,function(xx){
  return(sum(is.na(xx))>0)
})
controlL = control[!remove_index_set,]
control = rbind(controlR,controlL)


colnames(case)[30]  = "ACW_mm"
colnames(control)[29] = "ACW_mm"

mycase = cbind(rep("YES",dim(case)[1]  ),case[,myCasesNewR] ) 
mycontrol = cbind(rep("NO", dim(control)[1] ),control[,myControlsNewR]   )
for(ii in 1:dim(mycase)[2] ){
  if(ii==1){
    colnames(mycase)[1] = colnames(myy)[ii]
    colnames(mycontrol)[1] = colnames(myy)[ii]
  }else{
    colnames(mycase)[ii] = colnames(myx)[ii-1]
    colnames(mycontrol)[ii] = colnames(myx)[ii-1]  
  }
}
mytest = rbind(   mycase,mycontrol )


dput(mytest,"mytest.r")
