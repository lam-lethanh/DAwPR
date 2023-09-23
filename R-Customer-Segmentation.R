#load data->remove column not numeric->check null->scale data>-detect K- run cluster-visualize



#install.packages() to install library if it is not installed

library(Hmisc)
library(cluster)
library(ggplot2)
library(factoextra)
library(dplyr)
library(ggstatsplot)
library(VIM)

#put dataset in the same folder so we dont need to have a path. 
data<-read.csv("Cust_Segmentation.csv")
str(data)
#======================


#Remove columns not numeric
data<-select(data,-Address,-Defaulted,-Customer.Id)
head(data)




# 'aggr' plots the amount of missing/imputed values in each column
aggr(data)
data <- na.omit(data) 


#Describe data and identify the outlier
describe(data)


#Identify outlier in education
boxplot(data$Edu, plot=FALSE)$out
outliers <- boxplot(data$Edu, plot=FALSE)$out
#remove
data<- data[-which(data$Edu %in% outliers),]

#remove outlier in YearEmployed
boxplot(data$Years.Employed, plot=FALSE)$out
outliers <- boxplot(data$Years.Employed, plot=FALSE)$out
data<- data[-which(data$Years.Employed %in% outliers),]
#remove outlier in Income
boxplot(data$Income, plot=FALSE)$out
outliers <- boxplot(data$Income, plot=FALSE)$out
data<- data[-which(data$Income %in% outliers),]
#remove outlier in CardDebt
boxplot(data$Card.Debt, plot=FALSE)$out
outliers <- boxplot(data$Card.Debt, plot=FALSE)$out
data<- data[-which(data$Card.Debt %in% outliers),]
#remove outlier in OtherDebt
boxplot(data$Other.Debt, plot=FALSE)$out
outliers <- boxplot(data$Other.Debt, plot=FALSE)$out
data<- data[-which(data$Other.Debt %in% outliers),]
#remove outlier in DebtIncomeRatio
boxplot(data$DebtIncomeRatio, plot=FALSE)$out
outliers <- boxplot(data$DebtIncomeRatio, plot=FALSE)$out
data<- data[-which(data$DebtIncomeRatio %in% outliers),]


#Scale data
df <- scale(data)


#Determine K

# Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


#===============================================================
#evaluate 3 method

system.time({
  # Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
 geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
  })

system.time({
  # Silhouette method
  fviz_nbclust(df, kmeans, method = "silhouette")+
    labs(subtitle = "Silhouette method")
})
system.time({
  #gap statistic method
  set.seed(123)
  fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
    labs(subtitle = "Gap statistic method")
})


pam.res3 <- pam(data, 3,  metric = "euclidean", stand = FALSE)
pam.res2 <- pam(data, 2,  metric = "euclidean", stand = FALSE)
fviz_silhouette(pam.res2, palette = "jco", ggtheme = theme_classic())
fviz_silhouette(pam.res3, palette = "jco", ggtheme = theme_classic())

#===============================================================


set.seed(123)
km.res <- kmeans(df, 3, nstart = 25)
km.res$centers



km.res$size

#draw pie chart to visualize kmean size
percent <- round(km.res$size/nrow(data)*100,2)
percent
color = c("red","blue","green","yellow")
pie(percent,labels =  paste0(percent, "%"),radius = 1, main = "ratio of each cluster", clockwise = TRUE,
    col = color)
legend("topright", legend = c("cluster 1","cluster 2","cluster 3"),
       fill = color)
fviz_cluster(km.res ,data = df)

#Labeling each data point
dd <- cbind(data, cluster = km.res$cluster)
head(dd)

#Show attributes of each cluster
aggregate(data, by=list(cluster=km.res$cluster), mean)





 