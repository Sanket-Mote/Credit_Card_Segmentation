
#Clearing R Environment
rm(list = ls())

#Setting working directory
setwd("D:/Edwisor Data Science/Final Project 3 Submission")

#Checking current working directory
getwd()

#Importing Libraries

library(Matrix)
library(pROC)
library(DMwR)
library(ggplot2)
library(dplyr)
library(NbClust)

#Loading our dataset
df_credit = read.csv("credit-card-data.csv", header = TRUE, na.strings = c(' ','','NA'))

#Checking Structure
str(df_credit)

#Dimension
dim(df_credit)
# Observations -> 8950
# Variables -> 18

### Missing Value Analysis ###

missing_val = data.frame(apply(df_credit, 2, function(x){sum(is.na(x))}))
missing_val$Variable_name = row.names(missing_val)
row.names(missing_val) = NULL

names(missing_val)[1] = 'Missing_Count'

#Arranging in Descending Order
missing_val = missing_val[order(-missing_val$Missing_Count),]

#Rearranging the columns
missing_val = missing_val[,c(2,1)]

## Here we can see that there are 2 variables with missing values ##

df_credit$CREDIT_LIMIT[21]
#2000

# setting above 2 values as NA so that we can verify results after imputation
df_credit$CREDIT_LIMIT[21] = NA

#Imputing Credit Limit using Mean
df_credit$CREDIT_LIMIT[is.na(df_credit$CREDIT_LIMIT)] = mean(df_credit$CREDIT_LIMIT, na.rm = T)
df_credit$CREDIT_LIMIT[21]
#4494.728

#Imputing Credit Limit using Median
df_credit$CREDIT_LIMIT[is.na(df_credit$CREDIT_LIMIT)] = median(df_credit$CREDIT_LIMIT, na.rm = T)
df_credit$CREDIT_LIMIT[21]
#3000

# Since mean value seems more promising we proceed our imputation using mean

df_credit$MINIMUM_PAYMENTS[21]
#13557.3

#Setting random any 2 variables as null to cross verify after imputation
df_credit$MINIMUM_PAYMENTS[21] = NA

#Creating backup
df_credit_bkp = df_credit

#Imputing Minimum Payments using Mean
df_credit$MINIMUM_PAYMENTS[is.na(df_credit$MINIMUM_PAYMENTS)] = mean(df_credit$MINIMUM_PAYMENTS, na.rm = T)
df_credit$MINIMUM_PAYMENTS[21]
#862.7368

#Imputing Minimum Payments using Median
df_credit$MINIMUM_PAYMENTS[is.na(df_credit$MINIMUM_PAYMENTS)] = median(df_credit$MINIMUM_PAYMENTS, na.rm = T)
df_credit$MINIMUM_PAYMENTS[21]
#312.1673

# Since mean value seems more promising we proceed our imputation using mean

#Checking data post imputation
data.frame(apply(df_credit, 2, function(x){sum(is.na(x))}))

#Resetting manuallychanged values
df_credit$MINIMUM_PAYMENTS[21] = 13557.3
df_credit$CREDIT_LIMIT[21] = 2000

### Deriving Intelligent KPIs from Customer Data

# 1. Monthly Average Purchase and Cash Advance Amount

#Monthly Average Purchase
df_credit$Monthly_Avg_Purchase = df_credit$PURCHASES / df_credit$TENURE
df_credit$Monthly_Avg_Purchase[which(!is.finite(df_credit$Monthly_Avg_Purchase))] = 0
df_credit$Monthly_Avg_Purchase[is.na(df_credit$Monthly_Avg_Purchase)] = 0

#Cash Advance Amount
df_credit$Monthly_Cash_Adv = df_credit$CASH_ADVANCE / df_credit$TENURE
df_credit$Monthly_Cash_Adv[which(!is.finite(df_credit$Monthly_Cash_Adv))] = 0
df_credit$Monthly_Cash_Adv[is.na(df_credit$Monthly_Cash_Adv)] = 0

# 2. Customer Purchase Habits
Purchase <- function(df_credit){
  if((df_credit$ONEOFF_PURCHASES == 0) & (df_credit$INSTALLMENTS_PURCHASES == 0))
    return("No Purchases")
  if((df_credit$ONEOFF_PURCHASES > 0) & (df_credit$INSTALLMENTS_PURCHASES == 0))
    return("One Off Purchases")
  if((df_credit$ONEOFF_PURCHASES == 0) & (df_credit$INSTALLMENTS_PURCHASES > 0))
    return("Installment Purchases")
  if((df_credit$ONEOFF_PURCHASES > 0) & (df_credit$INSTALLMENTS_PURCHASES > 0))
    return("Both Purchases")
}

df_credit$Purchase_Type[df_credit$ONEOFF_PURCHASES == 0 & df_credit$INSTALLMENTS_PURCHASES == 0] = "No Purchases"
df_credit$Purchase_Type[df_credit$ONEOFF_PURCHASES > 0 & df_credit$INSTALLMENTS_PURCHASES == 0] = "One Off Purchases"
df_credit$Purchase_Type[df_credit$ONEOFF_PURCHASES == 0 & df_credit$INSTALLMENTS_PURCHASES > 0] = "Installment Purchases"
df_credit$Purchase_Type[df_credit$ONEOFF_PURCHASES > 0 & df_credit$INSTALLMENTS_PURCHASES > 0] = "Both Purchases"

unique(df_credit$Purchase_Type)

# 3. Average Amount Per Purchase

#Average Amount Per Purchase
df_credit$Average_Purchase_Size = df_credit$PURCHASES / df_credit$PURCHASES_TRX
df_credit$Average_Purchase_Size[which(!is.finite(df_credit$Average_Purchase_Size))] <- 0
df_credit$Average_Purchase_Size[is.na(df_credit$Average_Purchase_Size)] = 0

# 4. Cash Advance Transaction
df_credit$Cash_Adv_Trx_Size = df_credit$CASH_ADVANCE / df_credit$CASH_ADVANCE_TRX
df_credit$Cash_Adv_Trx_Size[is.na(df_credit$Cash_Adv_Trx_Size)] = 0
df_credit$Cash_Adv_Trx_Size[which(!is.finite(df_credit$Cash_Adv_Trx_Size))] <- 0

# 5. Limit Usage (Balance to Credit Limit Ratio)
df_credit$Limit_Usage = df_credit$BALANCE / df_credit$CREDIT_LIMIT * 100

#6. Payments To Minimum Payments Ratio
# Setting all customers whose payment is 0 as 0 
df_credit$MINIMUM_PAYMENTS[df_credit$PAYMENTS == 0] = 0

df_credit$Min_Payment_Ratio = df_credit$PAYMENTS / df_credit$MINIMUM_PAYMENTS *100
df_credit$Min_Payment_Ratio[which(!is.finite(df_credit$Min_Payment_Ratio))] <- 0
df_credit$Min_Payment_Ratio[is.na(df_credit$Min_Payment_Ratio)] = 0

#Exporting Data with KPI
write.csv(df_credit, "R_Processed_KPI.csv")

### Insights from New Derived KPIs

################ INSIGHT 1

Purchase_Ratio = aggregate(df_credit$PURCHASES, by=list(df_credit$Purchase_Type), FUN=mean)
#Both Purchases  723.1683
#Installment Purchases 1324.5195
#No Purchases 1007.6712
#One Off Purchases  549.8208

Purchase_Ratio$percent = Purchase_Ratio$x / nrow(df_credit) * 100

barplot(Purchase_Ratio$percent, main = "Purchase Type Distribution", 
        col = c("lightblue"),
        xlab = "Purchase Type", 
        ylab = "Percent of customers")

#Insight 1: 25.33% customers are doing both type of Purchases (One off or Installment Purchase), 
#while approximately 6% are doing Installment Purchases and 8.79% are doing one off purchases

############### INSIGHT 2

Minimum_Payment_Ratio = aggregate(df_credit$Min_Payment_Ratio, by=list(df_credit$Purchase_Type), FUN=mean)

Minimum_Payment_Ratio$percent = Minimum_Payment_Ratio$x / nrow(df_credit) * 100

barplot(Minimum_Payment_Ratio$percent, main = "Purchase Type Minimum Payment", 
        col = c("lightblue"),
        xlab = "Purchase Type", 
        ylab = "Minimum Payment Ratio")

#Insight 2: Customers taking Installments are making minimum payments various those who are not doing 
#one off purchase and installment purchase are taking cash advances and making minimum payments

############### INSIGHT 3

Monthly_Cash_Advance = aggregate(df_credit$Cash_Adv_Trx_Size, by=list(df_credit$Purchase_Type), FUN=mean)

Monthly_Cash_Advance$percent = Monthly_Cash_Advance$x / nrow(df_credit) * 100

barplot(Monthly_Cash_Advance$percent, main = "Monthly Cash Advance Ratio", 
        col = c("lightblue"),
        xlab = "Purchase Type", 
        ylab = "Monthly Cash Advance")

#Insight 3: Customers doing no transactions are taking more cash advances

############### INSIGHT 4

plot(df_credit$CASH_ADVANCE_TRX, df_credit$PURCHASES, pch = 19, cex =1.5,
     col = "#cc0000", main = "Cash advance vs Purchases", xlab = "Cash Advance", ylab = "Purchases")

#Insight 4: Customers who have done atleast 25 transactions have done atleast 1 Cash Advance Transaction

#Modelling

#Standardize data
df_credit_new = data.frame(scale(df_credit_new))

#memory.limit(size=512.0)
#Extract number of clusters to build
#NBClust_res = NbClust(df_credit_new, min.nc = 4, max.nc = 7, method = "kmeans")

#Barplot to analyse the optimum clusters
#barplot(table(NBClust_res$Best.n[1,]), xlab = "No of Clusters", ylab = "Number of Criteria", 
#        main = "No of Clusters")

#K-Mean Clustering

#building clusters using k-means clustering 
cluster_four <- kmeans(df_credit_new,4)
cluster_five <- kmeans(df_credit_new,5)
cluster_six <- kmeans(df_credit_new,6)

credit_new<-cbind(df_credit_new,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
View(credit_new)

df_credit_new$CUST_ID = NULL
df_credit_new$Purchase_Type = NULL

#4_Clusters
kmeans_Model_4 = kmeans(df_credit_new, 4, nstart=10)

#Summarize output
kmeans_Model_4

# Insights for 4 Clusters 
# Cluster 0:
# In first cluster customers are utilizing lowest credit limit amongst other clusters and are mostly transacting in installment purchases
# Cluster 1:
# These set of customers are mostly doing one off purchases followed with cash advance transactions
# Cluster 2:
# These set of customers are doing maximum average purchases and are utilizing maximum credit limit. Basically this are our high spending customers and are involved in both type of transactions one off and installments
# Cluster 3:
# These customers are making cash advance transactions mostly and are not transacting much in other metods we can give them low interest offers in order increase our revenue in future

#5_Clusters
kmeans_Model_5 = kmeans(df_credit_new, 5, nstart=10)

#Summarize output
kmeans_Model_5

# Insights for 5 Clusters 
# Cluster 0:
# Customers are mostly making One off purchases and no installment purchases
# Cluster 1:
# These set of customers are making installment purchases and making minimum payments towards the complete payment in billing cycle
# Cluster 2:
# These customers are doing Cash advance transactions and are not transacting much in other categories
# Cluster 3:
# These customers are highest in doing average monthly transactions and are using maximum credit limit among other groups
# Cluster 4:
# These set of customers are doing both transactions and cash advances we can say this cluster might be a combination of cluster 2 and cluster 3.

#6_Clusters
kmeans_Model_6 = kmeans(df_credit_new, 5, nstart=10)

#Summarize output
kmeans_Model_6

# Insights for 6 Clusters 
# Cluster 0:
# Customers in first cluster are making both type (One off & Installment) payments
# Cluster 1:
# These set of customers are making no purchases but making cash advances
# Cluster 2:
# These customers are lowest in terms of credit limit utilization and most of the payments are installment purchases
# Cluster 3:
# These customers are similar to cluster 0 and makes all types of payments in less propotion compared to cluster 0.
# Cluster 4:
# These set of customers are doing One off Purchases and cash advances on a larger scale.
# Cluster 5:
# These customers are making cash advance followed with both type of transactions


