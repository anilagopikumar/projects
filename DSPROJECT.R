# TO IMPORT THE DATA SET
library(readr)
Wine_R<-read_csv("C:/users/anila/Downloads/Wine.R.csv")
View(Wine_R)
# TO REMOVE THE FIRST COLUMN FROM THE DATA SET
Wine_R<-subset(Wine_R,select = -c(1))
View(Wine_R)
summary(Wine_R)
# VISUALIZATION
# ALCOHOL
hist(Wine_R$Alcohol,main = "alcohol histogram",col = "blue")
boxplot(Wine_R$Alcohol,main = "alcohol boxplot",horizontal = T,col = "blue")
# MALIC ACID
hist(Wine_R$Malic,main = "malic acid histogram",col="blue")
boxplot(Wine_R$Malic,main = "malic acid boxplot",horizontal = T,col = "blue")
unique(boxplot(Wine_R$Malic)$out)
# ASH
hist(Wine_R$Ash,main = "ASH histogram",col = "blue")
boxplot(Wine_R$Ash,main = "ASH boxplot",horizontal = T,col = "blue")
unique(boxplot(Wine_R$Ash)$out)
# ALCALINITY
hist(Wine_R$Alcalinity,main = "alcalinity histogram",col = "blue")
boxplot(Wine_R$Alcalinity,main = "alcalinity boxplot",horizontal = T,col = "blue")
unique(boxplot(Wine_R$Alcalinity)$out)
# MAGNESIUM
hist(Wine_R$Magnesium,main = "magnesium histogram",col="blue")
boxplot(Wine_R$Magnesium,main="magnesium boxplot",horizontal = T,col="blue")
unique(boxplot(Wine_R$Magnesium)$out)
# PHENOLS
hist(Wine_R$Phenols,main = "phenols histogram",col="blue")
boxplot(Wine_R$Phenols,main = "phenols boxplot",horizontal = T, col= "blue")
unique(boxplot(Wine_R$Phenols)$out)
# FLAVANOIDS
hist(Wine_R$Flavanoids,main = "flavanoids histogram",col="blue")
boxplot(Wine_R$Flavanoids,main = "flavanoids boxplot",horizontal = T,col = "blue")
# NON FLAVANOIDS
hist(Wine_R$Nonflavanoids,main = "non flavanoids histogram",col = "blue")
boxplot(Wine_R$Nonflavanoids,main = "non flavanoids boxplot",horizontal = T,col = "blue")
# PROANTHOCYANIN
hist(Wine_R$Proanthocyanins,main = "proanthocyanin histogram",col = "blue")
boxplot(Wine_R$Proanthocyanins,main = "proanthocyanin boxplot",horizontal = T,col = "blue")
unique(boxplot(Wine_R$Proanthocyanins)$out)
# COLOR
hist(Wine_R$Color,main = "color histogram",col = "blue")
boxplot(Wine_R$Color,main = " color boxplot",horizontal = T,col = "blue")
unique(boxplot(Wine_R$Color)$out)
# HUE
hist(Wine_R$Hue,main = "hue histogram",col = "blue")
boxplot(Wine_R$Hue,main = "hue boxplot",horizontal = T,col = "blue")
unique(boxplot(Wine_R$Hue)$out)
# DILUTION
hist(Wine_R$Dilution,main = "dilution histogram",col="blue")
boxplot(Wine_R$Dilution,main = "dilution boxplot",horizontal = T, col = "blue")
# PROLINE
hist(Wine_R$Proline,main = "proline histogram",col="blue")
boxplot(Wine_R$Proline,main = "proline boxplot",horizontal = T,col = "blue")
# NORMALIZING THE DATA
data.train<-scale(Wine_R)
View(data.train)
data.train<-subset(data.train,select = -c(2))
View(data.train)
summary(data.train)
# KMEANS CLUSTERING
wss=(nrow(data.train)-1)*sum(apply(data.train,2,var)) 
for(i in 2:9) wss[i]=sum(kmeans(data.train,centers = i)$withinss)
plot(1:9,wss,type = "b",xlab = "no of clusters",ylab = "within group of sum of squares")
title(sub = " K-MEANS clustering scree plot")  
install.packages("animation")
library(animation)
km<-kmeans.ani(data.train,4)
km$centers
final1<-data.frame(data.train,km$cluster)
final1
head(final1)
final2<-final1[,c(ncol(final1),1:(ncol(final1)-1))]
aggregate(data.train[,0:11],by=list(km$cluster),FUN=mean)
final2
View(final2)
# H CLUSTERING
d<-dist(final1,method = "euclidean")
h.fit<-hclust(d,method = "single")
plot(h.fit)
groups<-cutree(h.fit,k=4)
groups
d<-dist(final1,method = "euclidean")
h.fit<-hclust(d,method = "complete")
plot(h.fit)
groups<-cutree(h.fit,k=4)
groups
rect.hclust(h.fit,k=4,border = "red")
table(final1[,1],groups)
#PRINCIPAL COMPONENT ANALYSIS
View(final2)
help("princomp")
cor(final2)
pcaobj<-princomp(final1,cor = TRUE,scores = TRUE,covmat = NULL)
summary(pcaobj)
loadings(pcaobj)
plot(pcaobj)
biplot(pcaobj)
final1<-cbind(final1,pcaobj$scores[,1:9])
View(final1)
summary(final1)
