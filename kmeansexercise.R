# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust"))
library(cluster)
library(rattle.data)
library(NbClust)

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

dfscaled <- scale(wine[,-1])

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(dfscaled)

# Exercise 2:
#   * How many clusters does this method suggest?
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works

# Simply by observation it looks like there's a significant shift after the 3rd bend or "number of clusters".
# The decrease starts to lessen after that 3rd bend which to me might mean that 3 is a good number of clusters to use.
#---

#This method works by looking at the "sum of squared differences" between groups.  By taking the difference between each sample 
#and the mean of its group, and then summing the squares of each one of the differences.  The smaller this value the closer the 
#value should be to its group's mean.  The larger the value, the further the value should be from its group's mean.
#---

#It looks like the beginning of the function calculates the sum of square via a for loop.  
#Then goes through an array to sum the within-class sum of squares.  
#---

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(dfscaled, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

# This method suggests 3 clusters, it's very apparent in the barplot.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

set.seed(666)
fit.km <- kmeans(dfscaled, centers = 3, nstart = 3)
fit.km$size

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

dfscaledtable <- table(fit.km$cluster, wine$Type)
dfscaledtable

dfscaledDF <- as.data.frame(dfscaledtable)


#Relatively good clustering considering the vast majority of points fall within proper clusters.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(dfscaled, clus = fit.km$cluster)

# This plot shows three distinct clusters.  There is a small amount of data points that appear to fall within two of the cluster outlines.
# Using clusplot states that "the two components explain 55.41% of the point variability"

library(factoextra)
library(RColorBrewer)

clusterplot <- fviz_cluster(fit.km, dfscaled, repel = TRUE) +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  scale_color_brewer(palette = "YlOrRd", direction = -1) +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "K-Means - Cluster Plot")

clusterplot
