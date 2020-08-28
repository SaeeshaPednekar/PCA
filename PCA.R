wine<-read.csv("F:/Excelr/Assignments/dataset/clustering/wine.csv")
attach(wine)
?princomp
pcaobj<-princomp(wine,cor=FALSE,scores = TRUE,covmat = NULL)
summary(pcaobj)
loadings(pcaobj)
pcaobj$loadings


sum(pcaobj$loadings[,1]**2)

sum(pcaobj$loadings[,6]**2)
sum(pcaobj$loadings[,8]**2)
sum(pcaobj$loadings[,4]**2)


plot(pcaobj$scores[,2],pcaobj$scores[,1])
cor(pcaobj$scores[,2],pcaobj$scores[,1])

plot(pcaobj$scores[,5],pcaobj$scores[,8])
cor(pcaobj$scores[,5],pcaobj$scores[,8])


plot(pcaobj)
cor(pcaobj$scores)

pcaobj$scores[,1:3]


wine<-cbind(wine,pcaobj$scores[,1:3])
View(wine)



clus_data<-wine[,15:17]
normalized_data<-scale(clus_data)
dist_1<-dist(normalized_data,method = "euclidean")


fit1<-hclust(dist_1,method = "complete")

fit2<-hclust(dist_1,method = "average")

fit3<-hclust(dist_1,method = "centroid")

fit3<-hclust(dist_1,method = "ward.D2")
plot(fit1,hang=-1)

plot(fit2,hang=-1)

plot(fit3,hang=-1)

rect.hclust(fit1,k=15,border = "violet")

group<-cutree(fit1,15)
member<-as.matrix(group)


model<-cbind(member,wine)
View(model)

View(aggregate(model[,-c(15:18)],by=list(member),FUN = mean))


#

wine<-read.csv("F:/Excelr/Assignments/dataset/clustering/wine.csv")
attach(wine)


library(kselection)

library(doParallel)
registerDoParallel(cores = 4)
k<-kselection(normalized_data,parallel = TRUE,k_threshold = 0.95,max_centers = 15)
k


normalized_data<-scale(wine[,15:17])
twss<-NULL
for (i in 1:15)
  twss[i]=sum(kmeans(normalized_data,centers=i)$tot.withinss)

plot(1:15,twss,type="b",xlab ="No. of clusters", ylab="within group of sum of squares")




fitt<-kmeans(normalized_data,15)
str(fitt)
final_model<-data.frame(fitt$cluster,wine[1:14])
View(final_model)
aggregate(wine[,1:14], by=list(fitt$cluster), FUN=mean)






