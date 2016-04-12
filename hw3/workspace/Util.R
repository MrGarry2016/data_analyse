errorrate = function(pre, actual){
  
  compare = pre==actual
  return(1- sum(compare)/length(pre))
  
}