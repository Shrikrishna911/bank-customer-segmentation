#Reading csv
bank_data <- read.csv("C:/Users/skris/Downloads/Bank_data.csv")

#Compactly display the internal structure of an R object
str(bank_data)

#Get or set the names of an object.
names(bank_data)

#printing the first 6 rows
head(bank_data)

#summary of data
summary(bank_data)

#handling missing values
is.na(bank_data)
sum(is.na(bank_data))

gender_table = table(bank_data$Gender)

#pie chart to show gender distribution
lbs <- paste(c(" F","M"), sep = " ")
library(plotrix)
pie(gender_table, labels = lbs, main = "Gender Comparison",col = rainbow(2))

#histogram to show frequency of customer ages
hist(bank_data$Age, col = "purple", main = "Age Distribution",
     xlab = "bank_data$Age", ylab = "Frequency", labels = TRUE)

#density plot for annual income
plot(density(bank_data$Annual_Salary), col = "yellow", 
     main = "Annual Salary: Density Plot", xlab = "Annual Salary Class", 
     ylab = "Density")
polygon(density(bank_data$Annual_Salary),
        col="#ccff66")

#box plot- Credit score
boxplot(bank_data$CreditScore, horizontal=TRUE, col="#990000",
        main="Credit Score")


library(purrr)
#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- bank_data[,10:11]
wss <- sapply(1:k.max, function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#k=4 as optimal cluster
k4<-kmeans(customer_data[,10:11],4,iter.max=100,nstart=50,algorithm="Lloyd")
k4

#to visualize the optimal number of clusters
library(NbClust)
library(factoextra)

#Visualizing the Clustering Results using the First Two Principle Components
pcclust=prcomp(bank_data[,10:11],scale=FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]

set.seed(1)
ggplot(bank_data, aes(x =Annual_Salary, y = CreditScore)) + 
  geom_point(stat = "identity", aes(color = as.factor(k4$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")) +
  ggtitle("Segments of Bank Customers", subtitle = "Using K-means Clustering")

