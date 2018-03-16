head(banking, 2)

# Data cleaan up
y <- ifelse(y =='no', 0,1)

loan <- ifelse(loan =='no', 0,1)

rm(loan)

banking$loan <- ifelse(loan =='no', 0,1)


pairs.panels(numVals[1:10], pch=20, main="Banking Data")
# select columns 1-4

plot(age, duration)

# the default correlation/ numVals is all the numeric values from my dataset
cor(numVals) 

cor(numVals, method = "spearman")

with(banking, cor(age, euribor3m, method = "kendall"))
cor(numVals, method = "kendall")

corPlot <- ggplot(banking, aes(x=age,y=euribor3m)) + geom_point(position= position_jitter(w=0.1, h=0))

corPlot + ylab('Amount') + xlab('Age Value')

corResult <- cor(numVals, method = "kendall")

corResult<-round(corResult,2)
write.csv(corResult, "output.csv")



rcorr(numVals, type = c("pearson","spearman"))


res <- rcorr(as.matrix(numVals))

res$r

fit <- lm(euribor3m~nr.employed, data = numVals)

plot(euribor3m, nr.employed, main = "Scatterplot: Euribor3m & Nr.emploed")

lines(euribor3m, nr.employed, col="blue")


corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.set = 45)





