

outlier_values <- boxplot.stats(age)$out  # outlier values.
boxplot(age, main="Age", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.9)

outliersPlot <-ggplot(banking, aes(x = housing, y=age)) 

outliersPlot + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +  geom_jitter(width = 0.2) + coord_flip()

hist(age)
qqline(age.clean)

sd(age)

z_test <- function(x, mu, s, n, tailed){
  z <- (x-mu)/(s/sqrt(n))
  if(tailed == 1) return(cat('Z-score - ', z, '\np-value - ', pnorm(-abs(z))))
  if(tailed == 2) return(cat('Z-score - ', z, '\np-value - ', 2 * pnorm(-abs(z))))
}