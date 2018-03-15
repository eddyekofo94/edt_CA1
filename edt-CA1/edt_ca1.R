#banking <- read.table(file.choose(), header = T, sep= ",")
banking <- read.csv(file.choose(), stringsAsFactors = FALSE)

initialFunc <- function(){
  banking <- read.csv(file.choose(), stringsAsFactors = FALSE)
  attach(banking)
  library(easypackages)
  # https://stackoverflow.com/questions/8175912/load-multiple-packages-at-once
  libraries("easypackages", "e1071", "psych", "PerformanceAnalytics", "ggplot2")
  summary(banking)
}

initialFunc()

banking<-banking[complete.cases(banking),]

install.packages("ggplot2")

library(ggplot2)
#know what type of data it is
#len=continous, supp=nominal, dose=categorical/ordinal
ToothGrowth
summary(ToothGrowth)
libraries(e1071, psych)

install.packages("easypackages")

rm(cons.price)

typeof(age)

levels(age)

#Checks the name of the variables
variable.names(banking)

is.na(banking)

# b. Percentage of missing values in the data.
# Percentage of the missing values each column
colMeans(is.na(banking))
# Or
apply(banking, 2, function(col)sum(is.na(col))/length(col))

# c. Max, min, mean, mode, median standard deviation.
summary(numVals)
# Mean of missing values
mean(is.na(banking))

colMax <- function(data) sapply(data, max)

# maximum of age = 88
max(banking$age, na.rm = TRUE)

# min age = 18
min(banking$age, na.rm = TRUE)

# mean of the duration
mean(banking$duration, na.rm = TRUE)

# mean of the age
mean(banking$age, na.rm = TRUE)

# standard deviation
sd(banking$pdays)


# d. The type of distribution that the numeric attributes seem to follow (e.g. normal).
# NROW(na.omit(banking))
numberOfRows<-nrow(banking) # number of rows

numericValues<-sapply(banking, is.numeric)  # et all the numeric values


numericValues
numVals<-banking[, numericValues] # standard subsetting

banking[, numericValues] # standard subsetting


meanNumericBanking<-mean(numVals, na.rm = T)
# ageMean<-mean(banking$age, na.rm = TRUE) #age mea

# histogram
#Create a histogram
#Set up the plot area
par(mfrow=c(1,1))

numericHisto <- function(numVal){
  result <- hist(numVal,
                  col = "blue",
                 freq = F,
                  border = "black",
                  main="Histogram of banking numeric value"
  )
  # Add line
  lines(density(numVal), col="red")
  
  return(result)
}

age.clean <-age[!is.na(age)]

numericHisto(age.clean)

ageHist <- hist(banking$age,
                
                freq = FALSE,
     col = "blue",
     xlab = "Age",
     xlim= c(15,90),
     las = 1,
     border = "black",
     main="Histogram of age"
     )
lines(density(age.clean), col="red", lwd=3)


durationHist <- hist(banking$duration,
                col = "blue",
                ylab = "Num of customers",
                xlab = "Age",
                border = "black",
                main="Histogram of age"
)

skewness(banking$cons.price.idx)

cons.price.clean <-cons.price[!is.na(cons.price)]

hist(cons.price.clean, freq = F)
lines(density(cons.price.clean), col="red")

kurtosis(age.clean)

skewness(banking$age)                # apply the skewness function 

shapiro.test(age.clean)


qqplot(age)

normality <- function(numIn){
qqnorm(numIn)
qqline(numIn, col="red", lwd=3)

}

normality(age)

pairs.panels(banking[1:4])  # select columns 1-4



