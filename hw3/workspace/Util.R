errorrate = function(pre, actual){
  
  compare = pre-actual
  res = 0;
  for(ii in 1:length(pre)){
    res = res+compare[ii]*compare[ii]
  }
  return(res/length(pre) )
  
}