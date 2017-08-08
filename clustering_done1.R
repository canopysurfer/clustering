# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

##> wine$Type= NULL
##> wine2 =scale(wine)
## NOt sure what this scale is for

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

## what does it mean by within-groups sums of squares?

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)

## Ans: 
wssplot <- function(wine2, nc=15, seed=1234){
 wss <- (nrow(wine2)-1)*sum(apply(wine2,2,var))
for (i in 2:nc){
set.seed(seed)
wss[i] <- sum(kmeans(wine2, centers=i)$withinss)}
plot(1:nc, wss, type="b", xlab="Number of Clusters",
+ ylab="Within groups sum of squares")}

## wss plot(wine2)

# Exercise 2:
#   * How many clusters does this method suggest?
## Ans: This method suggests 3 clusters. Does the bend tell us the cluster to choose? 
## (see image named: wssplot homework1)

#   * Why does this method work? What's the intuition behind it?
## Ans: Scaling it makes it easier to see (unsure)

#   * Look at the code for wssplot() and figure out how it works
##  ans: it seems to be a plot function that creates data into 15 clusters which is assigned by the system.other than that,
## it is hard to understand 

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")
## This produces a bar plot with x axis lablled as number of clusters, y axis labels. 
## The bar plot shows that cluster 3 has 15 number of criteria (possibly the max as set in the plot)

# Exercise 3: How many clusters does this method suggest?
## This method also suggests 3


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

# fit.km <- kmeans( ... )

## Ans:
## convert to matrix and then to vector
## > wineMatrix = as.matrix(wine2)
## > wineVector = as.vector(wineMatrix)
## > set.seed(1)
## >fit.km = kmeans(wineVector, centers=3, iter.max=1000)
## >str(fit.km)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

## Ans
## table(fit.km$cluster)
## I got clusters 1,2,3 with data  912, 812, 590 respectively.
## How do I know if clustering is good? 

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

#clusplot( ... )

##Ans (Unsure how to use this faunction even after reading Help)
##> clusplot(fit.km)
#Error in clusplot.default(fit.km) : x is not numeric
##> clusplot(fit.km$cluster)
#Error in mkCheckX(x, diss) : x is not a data matrix
