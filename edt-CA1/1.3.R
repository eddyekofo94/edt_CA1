catValues <-sapply(banking, class)

housing.clean <- housing[!is.na(housing)]
marital.clean <- marital[!is.na(marital)]

bankingPlotBar <- ggplot(banking, aes(job, fill = housing)) 


bankingPlotBar+ geom_bar( width=.5)


funcBarChart <- function(catValue, targetVariable){
  bankingPlotBar <- ggplot(banking, aes(catValue, fill = targetVariable)) 
  + geom_bar(
    #  fill="lightgray", 
    # aes(y=..density..),
    #color="black",
    binwidth = .7)
  return(bankingPlotBar)
}

funcBarChart(age, marital)