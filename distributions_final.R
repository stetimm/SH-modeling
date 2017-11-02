#define distributions and densities
G_H = function(x){
  #output = x^3 
  output = pbeta(x,5,2)
  #output = x^6
  #output = x^8
  return(output)
}
G_L = function(x){
  #output = x^2
  output = pbeta(x,5,4)
  #output = x^5
  #output = x^5
  return(output)
}
F_H = function(x){
  #output = x
  #output = x^4
  output = pbeta(x,3,4)
  #output = x^4
  return(output)
}
F_L = function(x){
  #output = sqrt(x)
  output = pbeta(x,3,6)
  #output = x^3
  
  return(output)
}

#densities
g_H = function(x){
  #output = 3*x^2 
  output = dbeta(x,5,2)
  #output = 6*x^5
  return(output)
}
g_L = function(x){
  #output = 2*x
  output = dbeta(x,5,4)
  #output = 5*x^4
  return(output)
}
f_H = function(x){
  #output = 1
  output = dbeta(x,3,4)
  #output = 4*x^3
  return(output)
}
f_L = function(x){
  #output = 1/sqrt(x)
  output = dbeta(x,3,6)
  #output = 2*x
  return(output)
}

