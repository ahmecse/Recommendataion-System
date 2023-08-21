rm(list=ls())

#set working directory

setwd("C:\\Users\\ahme1\\Downloads\\Compressed\\clusteval_0.1\\clusteval")

#import data
cereals_data <- read.csv("Cereals.csv", header = T, sep = ",")
cereals_data

#Data Pre-processing., ====
#...check names, structure,summary stats etc.,====
str(cereals_data)
summary(cereals_data)
names(cereals_data)
dim(cereals_data)
head(cereals_data)

#...change row-names from numbers to real names====
row.names(cereals_data)
rownames(cereals_data) <- cereals_data$name
row.names(cereals_data)
cereals_data$name <- NULL #remove name column from table

#...change datatypes of factor columns====
cat_cols=c("protein","fat","shelf")
cereals_data[,cat_cols] <- data.frame(sapply(cereals_data[,cat_cols], 
                                             as.factor))
str(cereals_data)

#...look for NA's====
sum(is.na(cereals_data))
sapply(cereals_data, function(x) sum(is.na(x)))
rowSums(is.na(cereals_data))#
#Cream_of_Wheat_(Quick) p48
#Quaker_Oatmeal c67 s1
#Almond_Delight p155

#manual imputation
cereals_m_data<-cereals_data
cereals_m_data["Cream_of_Wheat_(Quick)","potass"] <- 77
cereals_m_data["Quaker_Oatmeal", "carbo"] <- 21
cereals_m_data["Quaker_Oatmeal", "sugars"] <- 0
cereals_m_data["Almond_Delight", "potass"] <- 0

#...impute the NA's with real time data===
#install.packages("devtools")
#devtools::install_version('DMwR', '0.4.1')
#library("DMwR")
library(DMwR)
cereals_data_imputed <- centralImputation(cereals_data)

#Na omitted data
cereals_omit_data<-cereals_data
cereals_omit_data= na.omit(cereals_omit_data)
sum(is.na(cereals_omit_data))

#zero imputed data
cereals_zero_imp_data <- cereals_data
cereals_zero_imp_data[is.na(cereals_zero_imp_data)]<- 0


#...scaling the only numeric columns data==== 
cereals_scale_data <- data.frame(scale(cereals_data_imputed[,-c(2,3,10)], 
                                       center = T, scale=T))
str(cereals_scale_data)

cereals_scale_data_m <- data.frame(scale(cereals_m_data[,-c(2,3,10)], 
                                         center = T, scale=T))
cereals_scale_data_zero<- data.frame(scale(cereals_zero_imp_data[,-c(2,3,10)], 
                                           center = T, scale=T))
cereals_scale_data_omit <- data.frame(scale(cereals_omit_data[,-c(2,3,10)], 
                                            center =T, scale=T))


#Clustering the data====
#..Hierarchical clustering====

#.....distance matrix====

distance <- dist(cereals_scale_data, method="euclidean")
distance1 <- dist(cereals_scale_data_omit, method="euclidean")

class(distance) #distance matrix is different from normal matrix. 

#if we pass normal matrix it will pass error like below
#Error in if (is.na(n) || n > 65536L) stop("size cannot be NA nor exceed 65536") : 
#  missing value where TRUE/FALSE needed

#.....Heirarchical cluster analysis====
fit<- hclust(distance, method = "ward.D")
fit1<- hclust(distance, method="single")
fit2 <- hclust(distance, method="average")
fit3 <- hclust(distance, method="complete")

fit_omit<- hclust(distance1, method="ward.D")
fit1_omit<- hclust(distance1, method="single")

#.....cluster analysis plots of different methods====

#ward.D
x11(width=10, height=6, pointsize=12) 
par(mfrow=c(1,1))
plot(fit, main='ward.D')

x11(width=10, height=6, pointsize=12) 
par(mfrow=c(1,1))
plot(fit_omit, main='ward.D')


#single
x11(width=10, height=6, pointsize=12) 
par(mfrow=c(1,1))
plot(fit1, main="single")



#average
x11(width=10, height=6, pointsize=12) 
par(mfrow=c(1,1))
plot(fit2, leaflab = "textlike", main = "Average", xlab = "")


#complete
x11(width=10, height=6, pointsize=12) 
par(mfrow=c(1,1))
plot(fit3, main="Complete")
dev.off()

#.....finding optimum number of clusters====
library(factoextra)
fviz_nbclust(cereals_scale_data, hcut, method = "wss")
fviz_nbclust(cereals_scale_data_omit, hcut, method = "wss",k.max = 40)


#.....h-clusters creation====
groups <- cutree(fit, k=7)# cut tree into 7 clusters  groups
groups_omit <- cutree(fit_omit, k=12)

#.....clustering dendogram tree plot====
x11(width=10, height=6, pointsize=12) 
par(mfrow=c(1,1))
plot(fit, main='ward.D')
rect.hclust(fit, border="red", k=7)

#https://stackoverflow.com/questions/7227008/retrieving-members-from-a-cluster-using-r
#we will get the following error if we dont write one after another(plot and the rect.hclust) 
#Error in rect(m[which[n]] + 0.66, par("usr")[3L], m[which[n] + 1] + 0.33, : plot.new has not been called yet

x11(width=10, height=6, pointsize=12) 
par(mfrow=c(1,1))
plot(fit_omit, main='ward.D')
rect.hclust(fit_omit, border="red", k=12)

#.....write group number column to the dataframe====

cereals_data_hcluster <- data.frame(cereals_scale_data, groups)  
cereals_data_hcluster_omit <- data.frame(cereals_scale_data_omit, groups_omit)  

#..K- Means Clustering====
#.....K-Means Cluster Analysis with k = 5====

set.seed(1234)
kfit<- kmeans(cereals_scale_data, centers=5)

#.....model study====
kfit 
kfit$withinss
kfit$betweenss/kfit$totss #goodness of of the classification (ratio should near to 1)
#reference taken from
#https://stats.stackexchange.com/questions/82776/what-does-total-ss-and-between-ss-mean-in-k-means-clustering


#.....visualization of kmeans partitioned clusters====
library(factoextra)
fviz_cluster(kfit, cereals_scale_data)
# #.....append cluster label to the actual data frame
# cereals_kmclust_data <- data.frame(cereals_scale_data, kfit$cluster)
# head(cereals_kmclust_data)

#.....finding optimal number of clusters====
library(factoextra)
fviz_nbclust(cereals_scale_data, kmeans, method="wss", k.max = 20)
fviz_nbclust(cereals_scale_data, kmeans, method="silhouette", 
             print.summary = F, k.max = 25)
fviz_nbclust(cereals_scale_data, kmeans, method="gap_stat")


#According to above visualizations we can generalize k= 8
#.....final 8 cluster solution====

final_fit_kmeans <- kmeans(cereals_scale_data, 8) 
final_fit_kmeans
fviz_cluster(final_fit_kmeans, cereals_scale_data)


#.....append cluster label to the actual data frame====
cereals_kmclust_data <-data.frame(cereals_scale_data, 
                                  final_fit_kmeans$cluster)
head(cereals_kmclust_data)

#..K-means Stability check====
#.....Data preperation for checking====

set.seed(12345)
index<- (sample(nrow(cereals_scale_data),.70*nrow(cereals_scale_data))) #collect sample 70% rows index
stcheck_data<- cereals_scale_data[index,] #save sample in another dataset with collected indexes
stcheck_fit<- kmeans(stcheck_data, 8) #kmeans cluster with finalized k
stcheck_data$final_fit_kmeans.cluster <-stcheck_fit$cluster #assign new clusers numbers

#assigning actual and new cluster numbers to 2 different groups
Actual_cluster <- cereals_kmclust_data$final_fit_kmeans.cluster[index]
New_cluster <- stcheck_data$final_fit_kmeans.cluster

#.....Similarity measurement measure1====
library(fossil)
stabilitycheck <- adj.rand.index(Actual_cluster, New_cluster)
stabilitycheck

library(clusteval)
Stabindex <- cluster_similarity(Actual_cluster, New_cluster, 
                                similarity = "jaccard", 
                                method="independence")
Stabindex 
cluster_similarity(Actual_cluster,New_cluster,similarity = "rand")

#identifing the similar cereals based on clusters
centers_kmeans_cimp=data.frame(final_fit_kmeans$centers)
sapply(centers_kmeans_cimp,function(x) which.max(x))
final_fit_kmeans$centers
#K-means of na omited data========================================================
set.seed(1024)
kfit_omit<- kmeans(cereals_scale_data_omit, centers=5)
kfit_omit

library(factoextra)
fviz_nbclust(cereals_scale_data_omit, kmeans, method="wss")
fviz_nbclust(cereals_scale_data_omit, kmeans, method="silhouette", print.summary = F)

final_fit_kmeans_omit <- kmeans(cereals_scale_data_omit, 8)
final_fit_kmeans_omit
fviz_cluster(final_fit_kmeans_omit, cereals_scale_data_omit)

cereals_kmclust_data_omit <-data.frame(cereals_scale_data_omit, final_fit_kmeans_omit$cluster)

#omitted data preperation for check
index_omit<- (sample(nrow(cereals_scale_data_omit),.80*nrow(cereals_scale_data_omit))) #collect sample 70% rows index
stcheck_data_omit<- cereals_scale_data_omit[index_omit,] #save sample in another dataset with collected indexes
stcheck_fit_omit<- kmeans(stcheck_data_omit, 8) #kmeans cluster with finalized k
stcheck_data_omit$final_fit_kmeans_omit.cluster <-stcheck_fit_omit$cluster #assign new clusers numbers

#assigning actual and new cluster numbers to 2 different groups
Actual_cluster_omit <- cereals_kmclust_data_omit$final_fit_kmeans_omit.cluster[index_omit]
New_cluster_omit <- stcheck_data_omit$final_fit_kmeans_omit.cluster

#stability check on manual data
stabilitycheck_omit <- adj.rand.index(Actual_cluster_omit, New_cluster_omit)
length(Actual_cluster_omit)
length(New_cluster_omit)
stabilitycheck_omit

Stabindex_omit <- cluster_similarity(Actual_cluster_omit, New_cluster_omit, similarity = "jaccard", method="independence")
Stabindex_omit

cluster_similarity(Actual_cluster_omit,New_cluster_omit,similarity = "rand")

centers_kmeans_omit=data.frame(final_fit_kmeans_omit$centers)
sapply(centers_kmeans_omit,function(x) which.max(x))
final_fit_kmeans_omit$centers


#K-means of na manually imputed data========================================================
set.seed(1024)
kfit_m<- kmeans(cereals_scale_data_m, centers=5)
kfit_m

fviz_nbclust(cereals_scale_data_m, kmeans, method="wss",k.max = 30)
fviz_nbclust(cereals_scale_data_m, kmeans, method="silhouette", print.summary = F, k.max = 30)

final_fit_kmeans_m <- kmeans(cereals_scale_data_m, 8)
final_fit_kmeans_m
fviz_cluster(final_fit_kmeans_m, cereals_scale_data_m)

cereals_kmclust_data_m <-data.frame(cereals_scale_data_m, final_fit_kmeans_m$cluster)

#manual data preperation for check
index_m<- (sample(nrow(cereals_scale_data_m),.80*nrow(cereals_scale_data_m))) #collect sample 70% rows index
stcheck_data_m<- cereals_scale_data_m[index_m,] #save sample in another dataset with collected indexes
stcheck_fit_m<- kmeans(stcheck_data_m, 8) #kmeans cluster with finalized k
stcheck_data_m$final_fit_kmeans_m.cluster <-stcheck_fit_m$cluster #assign new clusers numbers

#assigning actual and new cluster numbers to 2 different groups
Actual_cluster_m <- cereals_kmclust_data_m$final_fit_kmeans_m.cluster[index_m]
New_cluster_m <- stcheck_data_m$final_fit_kmeans_m.cluster

#stability check on manual data
stabilitycheck_m <- adj.rand.index(Actual_cluster_m, New_cluster_m)

length(Actual_cluster_m)
length(New_cluster_m)
stabilitycheck_m

#install.packages("clusteval")
#library(clusteval)
Stabindex_m <- cluster_similarity(Actual_cluster_m, New_cluster_m, similarity = "jaccard", method="independence")
Stabindex_m

cluster_similarity(Actual_cluster_m,New_cluster_m,similarity = "rand")

centers_kmeans_m=data.frame(final_fit_kmeans_m$centers)
sapply(centers_kmeans_m,function(x) which.max(x))
final_fit_kmeans_m$centers


