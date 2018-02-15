##clustering
#k means
k1<-kmeans(Psi,5)
attributes(k1)
k1$size
k1$cluster ##cluster number of each value
k1$withinss ##variance inside each cluster

bss=sum(rowSums(k1$centers^2)*k1$size) #between cluster sum of squares
wss=sum(k1$withinss) #within cluster sum of squares
tss=sum(rowSums(Psi^2))
ib1=bss/(bss+wss)*100 #decomposition of inertia

#hierarchial clustering
idist=dist(Psi)
h1=hclust(idist,method = "ward")
plot(h1,labels=FALSE)
nc=8 #number of clusters
c1<-cutree(h1,nc)
table(c1)
df1 = data.frame(Status=dd$Status, mca$ind$coord[,1:2], cluster=as.factor(c1)) #prepare data for ggplot

# visualize clusters using the first two factorial coordinates
ggplot(data=df1, aes(x=Dim.1, y=Dim.2)) + 
  geom_hline(yintercept=0, colour="gray65") +
  geom_vline(xintercept=0, colour="gray65") +
  geom_point(aes(colour=cluster), alpha=0.5) +
  labs(x="Dim 1", y="Dim 2") +
  ##opts(title="MCA plot with clusters of individuals")
  
  # centers of gravity of the clusters
  cog = aggregate(as.data.frame(Psi), list(c1), mean)[,-1]
Bss = sum(rowSums(cog^2) * table(c1))
Ib4 = 100 * Bss / Tss