setwd("/Users/huangge/GoogleCloudDrive/6740/hw3/workspace")
mydata = read.csv("AngleClosure.csv")
case = read.csv("AngleClosure_ValidationCases.csv")
control = read.csv("AngleClosure_ValidationControls.csv")
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
?read.csv()

##move response to first column
response = c("ANGLE.CLOSURE")
myy = data[,response,drop=FALSE]
myx = data[,!colnames(data) %in% response]
data = cbind(myy,myx)

write.csv(file="cleandata.csv", x=data)


cbind(findIndex('l',control),findIndex('r',control))

## impute test data
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


