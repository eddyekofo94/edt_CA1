banking <- read.table(file.choose(), header = T, sep= ",")

rm(myglobals)

typeof(banking)

#Checks the name of the variables
variable.names(banking)

is.na(banking)

# Percentage of the missing values each column
colMeans(is.na(banking))
# Or
apply(banking, 2, function(col)sum(is.na(col))/length(col))

# Mean of missing values
mean(is.na(banking))


