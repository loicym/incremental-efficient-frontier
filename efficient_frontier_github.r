#define a function that takes as argument a matrix of returns (MRet) and a range (sequence) of target expected returns (rangeMu)
efficient_frontier = function(MRet,rangeMu)  
{
  #get the row (uM) and column (pM) dimensions of the matrix of returns
  uM = dim(MRet)[1]
  pM = dim(MRet)[2]
  
  #compute the portfolio's individual stocks expected returns
  expRet = colMeans(MRet)
  
  #compute the sample var-covar matrix
  Omega = var(MRet)
  
  
  #define a vector of constraints (the weights of the portfolio must sum to one)
  unityVec = rep(1,pM)
  
  #define a matrix of constraints (weights sum to one and variance will match a target level of expected returns)
  A = rbind(expRet,unityVec)
  
  #get the length of the target range (sequence)
  n = length(rangeMu)
  #define an empty variance vector
  myVar = rep(NA,n)
  #define an empty matrix of weights for each stock at each level of target expected return
  myWeights = matrix(data=NA,nrow=n,ncol=pM)
  
  #loop over the target expected returns range and compute the corresponding variances and weights
  for(i in 1:n)
  {
    b = matrix(data=c(rangeMu[i],1),nrow=2)
    myVar[i] = t(b)%*%solve(A%*%solve(Omega)%*%t(A))%*%b
    myWeights[i,] = solve(Omega)%*%t(A)%*%solve(A%*%solve(Omega)%*%t(A))%*%b
  }
  
  #transform the computed variances in standard deviation
  mySd = myVar^0.5
  #return the standard deviation vector
  return(mySd)
}