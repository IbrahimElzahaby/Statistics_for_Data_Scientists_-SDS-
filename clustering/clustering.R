## Part one: analysis of simulated data
# Simulate data
install.packages('jpeg')
set.seed(2)
x = matrix(rnorm(50*2), ncol = 2)
x[1:25,1] = x[1:25,1] + 3
x[1:25,2] = x[1:25,2] + -4
plot(x[,1],x[,2])


# Kmeans clustering - two clusters
km.out <- kmeans(x,centers = 2, nstart = 20) # centers argument specifies number of clusters
# Plot
plot(x[,1],x[,2], pch = 21, bg = c("red","green")[km.out$cluster])


# Kmeans clustering - three clusters
km.out <- kmeans(x,centers = 3, nstart = 20) # centers argument specifies number of clusters
# Plot
plot(x[,1],x[,2], pch = 21, bg = c("red","green","blue")[km.out$cluster])




## Part two: analysis of yeast data
source("PCA.R") # Alternatively load library ChemometricsWithR
load("6_1 Exercise 1 Yeast Data.RData")
nrow(data_yeast) 
ncol(data_yeast) 


# Autoscale data and apply PCA
yeast_AS <- scale(data_yeast, center = TRUE, scale = TRUE)
rownames(yeast_AS) <- tp_yeast # For showing object names in PCA scores plot
# PCA
pca.out <- PCA(yeast_AS)
# Score plot PC1 vs PC2
par(mfrow = c(1,1))
scoreplot(pca.out, show.names = TRUE) # Each time point is indicated by a number in the plot
lines(pca.out$scores[,1],pca.out$scores[,2])
loadingplot(pca.out, min.length = 0)

# Transpose autoscaled data matrix
yeast_AS_new <- t(yeast_AS)
dim(yeast_AS_new)




# Comparing correlations and distances in autoscaled data matrix
# Lets focus on distances between the first gene and all other genes
distances <- correlations <- vector(length = nrow(yeast_AS_new))
for (i in 1:nrow(yeast_AS_new)){
  distances[i] <- dist(rbind(yeast_AS_new[1,],yeast_AS_new[i,]))^2
  correlations[i] <- 1- cor(yeast_AS_new[1,], yeast_AS_new[i,])
}
plot(distances,correlations,pch = 19, xlab = "Squared Euclidean distance", ylab = "1 - correlation")


# Evaluate total within-cluster variation for K = 2:15
nclus=15
wss <- vector(length=nclus)
for (i in 2:nclus) wss[i] <- kmeans(yeast_AS_new, centers=i, nstart=20)$tot.withinss
plot(2:nclus, wss[2:nclus], type="b", lwd = 2, xlab="Number of Clusters K", ylab="Total within-cluster sum of squares", main = "Scree plot")


#Exercise 2. Image compression
# install.packages("jpeg") # if required
library(jpeg)

# Load image
img <- readJPEG("6_1 Exercise 2 image.jpg")
dim(img)


# Plot 
plot.new()
rasterImage(img, 0,0,1,1)


# Convert to matrix
img_matrix <- cbind(c(img[,,1]), c(img[,,2]),c(img[,,3]))
dim(img_matrix)

# Kmeans
set.seed(10)
kMeans <- kmeans(img_matrix, centers = 8, nstart = 20, iter.max = 10)
round(kMeans$centers*255) # #RGB intensities are in 0 - 1 range, but can be set to 0 - 255 integer range by multiplying with 255
# Depending on the number of clusters that is chosen the kmeans function functions returns an error. 
# This can be circumvented by increasing the number of iterations and / or using a slightly different algorithm, e.g. 
# kmeans(img_matrix, centers = 8, nstart = 20, iter.max = 1000, algorithm = "Lloyd") 
# The lloyd algorithm is the kmeans algorithm that was explained in the lecture. 
# By default R uses a smarter algorithm by Hartigan and Wong, which contains clever optimizations which reduce the computational cost of kmeans.


# Plot compressed image
kColours <- kMeans$centers[kMeans$cluster,] 
img_new <- img
img_new[,,1] <- matrix(kColours[,1],ncol = 256)
img_new[,,2] <- matrix(kColours[,2],ncol = 256)
img_new[,,3] <- matrix(kColours[,3],ncol = 256)
plot.new()
rasterImage(img_new, 0,0,1,1)


# set x and y coordinates of 5 data points
data <- matrix(c(1,2,3,1,1.5,4,5,7,10,11),ncol = 2)

# Compute Euclidean distances between data points
dist_mat <- dist(data)
dist_mat

# Plot data points
plot(data[,1],data[,2], type = "n", xlab = "x1", ylab = "x2")
text(data[,1],data[,2], labels = seq(1:5), cex = 2)


# Hierarchical clustering
hc.single <- hclust(dist(data), method = "single")
# Plot dendrogram
plot(hc.single)
# Note that the height of the dendrogram corresponds to the Euclidean distances which we computed earlier.


# Visualize aspects of single, complete and average linkage
# Example 1: chaining effect single linkage
# Generate data
set.seed(123)
x1 = matrix(rnorm(250*2), ncol = 2)
#x_outlier <- matrix(rnorm(5*2), ncol = 2)/4 
#x_outlier <- matrix(runif(5*2), ncol = 2)
#x_outlier[,1] <- x_outlier[,1] + 2
x_outlier <- cbind(seq(0,7, by = 0.5), rep(0,15))
x2 = matrix(rnorm(50*2), ncol = 2)/2
x <- rbind(x2,x1,x_outlier)
x[1:50,1] = x[1:50,1] + 7
# Cluster data  
hc.single <- hclust(dist(x), method = "single")
hc.average <- hclust(dist(x), method = "average")
hc.complete <- hclust(dist(x), method = "complete")
par(mfrow = c(2,2))
plot(x,main = "Raw data")
plot(x, col = c("red","blue")[cutree(hc.single,2)],main = "Single linkage")
plot(x, col = c("red","blue")[cutree(hc.average,2)],main = "Average linkage")
plot(x, col = c("red","blue")[cutree(hc.complete,2)],main = "Complete linkage")

# Example 2: group-breaking in complete linkage
# Generate data
set.seed(123)
x1 = matrix(rnorm(250*2), ncol = 2)
x2 = matrix(rnorm(25*2), ncol = 2)/2
x <- rbind(x2,x1)
x[1:25,1] = x[1:25,1] + 4
label <- c(rep(1,25),rep(2,250))
# Cluster data  
hc.single <- hclust(dist(x), method = "single")
hc.average <- hclust(dist(x), method = "average")
hc.complete <- hclust(dist(x), method = "complete")
par(mfrow = c(2,2))
plot(x, col = c("red","blue")[label],main = "True grouping")
plot(x, col = c("red","blue")[cutree(hc.single,2)],main = "Single linkage")
plot(x, col = c("red","blue")[cutree(hc.average,2)],main = "Average linkage")
plot(x, col = c("red","blue")[cutree(hc.complete,2)],main = "Complete linkage")



install.packages("dendextend")

load("6_1 Exercise 3 Lettuce data.Rdata")

# Remove QC samples (see exercise 2 of previous practical)
data_noQC <- data[-which(Labels %in% c("QC")),]
Labels_noQC <- Labels[-which(Labels %in% c("QC"))]
sample_names_noQC <- sample_names[-which(Labels %in% c("QC"))]
rownames(data_noQC) <- sample_names_noQC # to visualize sample names in scores plot later on


# Autoscale
data_AS <- scale(data_noQC, center = TRUE, scale = TRUE)
#rownames(data_AS) <- sample_names_noQC

# Hierarchical clustering of lettuce accessions
hc.average <- hclust(dist(data_AS), method = "average")

# Plot - species indicated by colours
library(dendextend)
dend <- as.dendrogram(hc.average)
cols <- c("red","green","blue","purple","cyan")
labels_colors(dend) <- cols[as.numeric(as.factor(Labels_noQC))][order.dendrogram(dend)]
plot(dend, cex = 0.25)


# Hierarchical clustering of metabolites
set.seed(10)
met_idx <- sample(1:2026,25)
data_select <- data_AS[,met_idx]
data_select <- t(data_select) # Transpose to put columns in rows
distCor <- function(x) as.dist(1-cor(t(x)))  # 1 minus pearson correlation
hc.out <- hclust(distCor(data_select), method = "average")
plot(hc.out)



install.packages("gplots")

# Construct heatmap
hcl_row <- hclust(dist(data_AS), method = "average") # Cluster rows (lettuce accessions)
row_dend <- as.dendrogram(hcl_row)
hcl_col <- hclust(distCor(t(data_AS)), method = "average") # Cluster columns (metabolites)
col_dend <- as.dendrogram(hcl_col)
# Plot: 
library(gplots)
zlim <- c(-3,3)# restrict color scale to specific range
data_scale <- pmin(pmax(data_AS, zlim[1]), zlim[2]) 
heatmap.2(t(data_scale), col=greenred(75),
          trace = "none", symm = FALSE, scale = "none",
          Rowv = col_dend, Colv = row_dend,
          labRow = "", labCol = ""
          , lhei=c(2,24), lwid=c(2,12), keysize=0.5, key.par = list(cex=0.25))  # Adjust size of margins

