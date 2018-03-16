

outlier_values <- boxplot.stats(age)$out  # outlier values.
boxplot(age, main="Age", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.9)

outliersPlot <-ggplot(banking, aes(x = housing, y=age)) 

outliersPlot + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +  geom_jitter(width = 0.2) + coord_flip()

hist(age)
qqline(age.clean)

sd(age)

z_test <- function(x,tailed){
  z <- (x-mean(x))/sd(x)
  if(tailed == 1) return(cat('Z-score - ', z, '\np-value - ', pnorm(-abs(z))))
  if(tailed == 2) return(cat('Z-score - ', z, '\np-value - ', 2 * pnorm(-abs(z))))
  if(tailed != 1 | tailed != 2) return('can omly be one or two tailed')
}

z_test(duration,1)

summary(z_test(age,1))

quantile(age, 0.25)
quantile(age, 0.75)

summary(age)

IQR(age)

fivenum(age)
