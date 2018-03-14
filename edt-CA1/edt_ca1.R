#banking <- read.table(file.choose(), header = T, sep= ",")
banking <- read.csv(file.choose(), stringsAsFactors = FALSE)

rm(myglobals)

typeof(banking)

#Checks the name of the variables
variable.names(banking)

is.na(banking)

# b. Percentage of missing values in the data.
# Percentage of the missing values each column
colMeans(is.na(banking))
# Or
apply(banking, 2, function(col)sum(is.na(col))/length(col))

# c. Max, min, mean, mode, median standard deviation.
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

numVals<-banking[, numericValues] # standard subsetting

banking[, numericValues] # standard subsetting


meanNumericBanking<-mean(numVals, na.rm = T)
# ageMean<-mean(banking$age, na.rm = TRUE) #age mean

banking <- gsub(",", "", banking)   # remove comma

banking <-as.numeric(banking)

# histogram
#Create a histogram
#Set up the plot area
par(mfrow=c(1,1))

hist(banking$age,
     col = "blue",
     ylab = "Num of patients"
     )

hist(banking$age,
     breaks=30,
     xlim= c(10,90),
     col="blue",
     border="black",
#     ylim=c(0,400),
     xlab="Age of patient",
     ylab="Age",
     maim="Histogram of age")


hist(numVals)

hist(banking)
natlog.weightlbs 
banking.norm<-rnorm(numberOfRows,)



