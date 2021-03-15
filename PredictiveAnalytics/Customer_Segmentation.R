# https://www.datasciencecentral.com/profiles/blogs/find-marketing-clusters-in-20-minutes-in-r

setwd("C:/Users/Administrator/Dropbox/Programming/DataClassification")

# Load skmeans library
# install.packages("skmeans")
library(skmeans)

# Load the data into variable customerdata. You will need to use na.omit function
# to remove all rows with empty values otherwise this could cause errors.
# User as.matrix to convert data type to matrix as you need that for using
# skmeans. For more info on skmeans type "?skmeans"
customerdata <- as.matrix(na.omit(read.csv("small_customer.csv")))

# Save only the columns of interest 1,26,27,28,29,30 to another variable "customers.shop"
# which we will use for clustering
customers.shop <- customerdata[,c(1,26,27,28,29,30)]

# We are going to use only columns 26 to 30 for clustering customers. Clustering using
# customer_id does not make sense so in the next two steps we are going to
# make customer_id into row names and then strip the column from the "customers.shop" dataset
rownames(customers.shop) <- customerdata[,1]
customers.shop <- customers.shop[,-1]

# Perform clustering on customers.shop dataset and save results to
# customer.clusters.amount_purchased. We will split into 5 clusters.
customer.clusters.amount_purchased <- skmeans(customers.shop, 5, method="genetic")

# Aggregate results by mean to analyse how much on average customers in different clusters
# shopped from the 5 different shops
customerdata.aggregate.amount_purchased <- aggregate(customers.shop, by = list(customer.clusters.amount_purchased$cluster), mean)

# Create padding to add legend to barplot added in the step after this step
par(xpd=T, mar=par()$mar+c(0,0,4,4))

# Create barplot of the aggregated results using barplot() on transposed form
# of the "customerdata.aggregate.amount_purchased" dataset
barplot(t(customerdata.aggregate.amount_purchased[,-1]), main="amount_purchased_by_cluster", ylab="Total",
        col=rainbow(5), space=0.1, cex.axis=0.8, las=1,
        names.arg=c("Cluster1","Cluster2","Cluster3","Cluster4","Cluster5"), cex=0.8)

# Add the legend to interpret the results
legend(4.5, 7200, names(customerdata.aggregate.amount_purchased[,-1]), cex=0.8, fill=rainbow(5))