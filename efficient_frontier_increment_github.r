#define a function that takes as argument a matrix of returns (MRet)
efficient_frontier_increment = function(MRet)
{
  #get the column (pM) dimension of the matrix of returns
  pM = dim(MRet)[2]
  #hard code the sequence vector of target expected returns (can be passed as argument)
  rangeMu = seq(-0.01,0.05,0.001)
  #compute the standard deviation vector of the portfolio composed of the first nine stocks of the matrix
  mySd = efficient_frontier(MRet[,1:9],rangeMu)
  
  #get a new plot
  dev.new()
  #plot the efficient frontier of the portfolio composed of the first nine stocks
  plot(mySd*100,rangeMu*100,xlab="volatility (%)",ylab="expected return (%)",t='l',xlim=c(0,max(mySd*100)),ylim=c(-1,5))
  #enjoy the chart for two seconds
  Sys.sleep(2)
  #loop over the rest of the stocks and increment the efficient frontier with the next additional stock
  for(j in 10:pM)
  {
    #recompute the efficient frontier
    mySd = efficient_frontier(MRet[,1:j],rangeMu)
    #add a line of the more efficient frontier on the chart
    lines(mySd*100,rangeMu*100,col='red')
    #define how long you want to wait before the next one
    Sys.sleep(0.1)
  }
}