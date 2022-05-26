#Reading csv
data <- read.csv("C:/Users/skris/Downloads/bank_transactions.csv")

#Compactly display the internal structure of an R object
str(data)

#Get or set the names of an object.
names(data)

#printing the first 6 rows
head(data)

#summary of data
summary(data)

#using bar graph to show gender distribution
gender_table = table(data$CustGender)
barplot(gender_table, main="Gender Comparison",ylab = "Count",
        xlab = "CustGender", col = rainbow(2), legend = rownames(gender_table))

#pie chart to show gender distribution
pct <- round(gender_table/sum(gender_table)*100)
lbs <- paste(c("T","F","M")," ", pct, "%", sep = " ")
library(plotrix)
pie3D(gender_table, labels = lbs, main = "Gender Comparison")

#coverting DoB into age
data$newAge=as.numeric(data$Age)


#histogram to show frequency of customer ages
hist(data$newAge, col = "purple", main = "Age Distribution",
     xlab = "data$newAge", ylab = "Frequency", labels = TRUE)

#histogram to show annual income
hist(data$CustAccountBalance, col = "blue", main = "Account Balance",
     xlab = "Account Balance Class", ylab = "Frequency", labels = TRUE)

#density plot for annual income
plot(density(data$CustAccountBalance), col = "yellow", 
     main = "Account Balance: Density Plot", xlab = "Account Balance Class", 
     ylab = "Density")
polygon(density(data$CustAccountBalance),
        col="#ccff66")
