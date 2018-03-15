cor(marital, housing)

bankingPlot <- ggplot(numVals, aes(age, fill = housing)) + geom_histogram(
#  fill="lightgray", 
  aes(y=..density..),
  #color="black",
  binwidth = .9)

funcHisto <- function(numValue, targetVariable){
  bankingPlot <- ggplot(numVals, aes(numValue, fill = targetVariable)) 
  + geom_histogram(
    #  fill="lightgray", 
   # aes(y=..density..),
    #color="black",
    binwidth = .7)
  return(bankingPlot)
}

funcHisto(age, marital)


bankingPlot
