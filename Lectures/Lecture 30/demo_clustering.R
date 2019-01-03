# We will start with loading the tidyverse 
library(tidyverse)

#### K-means: the first example ####

# We begin with simulating a simple data set with two clusters.
set.seed(11192018)
sim_data <- tibble(x = rnorm(50), y = rnorm(50))
sim_data[1:25, 1] <- sim_data[1:25, 1] + 3
sim_data[1:25, 2] <- sim_data[1:25, 2] - 4

# Note: the fist cluster has 25 observation with mean vector (3, -4)

# The k-means can be done with function kmeans(). For clustering with k = 2:
km_out <- kmeans(sim_data, centers = 2, nstart = 20)

# To view the estimated clusters
km_out
km_out$cluster

# Visualise the data
ggplot(sim_data, aes(x, y)) + 
  geom_point(color = km_out$cluster, shape = km_out$cluster)

# Checkpoint: What if we perform kmeans with 3 clusters? Visualise your clustering results with 3 clusters.

#### Hierarchical Clustering ####

# We will perform the clustering on the simulated data set.
# Note: the function dist() is used to compute the Euclidean distance, since the function hclust() requires to have the distance matrix.
hc_complete <- hclust(dist(sim_data), method = "complete")

# Average or single linkage is also simple to execute
hc_average <- hclust(dist(sim_data), method = "average")
hc_single <- hclust(dist(sim_data), method = "single")

# We can plot the dendrogram by simply calling the plot() function
plot(hc_complete)

# Checkpoint: View the dendrogram with different linkage methods. Will all of them give us similar clusters if we want 2 clusters?




# At the checkpoint, you mostly did by eye-balling the cutoff line. The function cutree() will determine the cluster labels for each observation associated with a given cut of the dendrogram.
cutree(hc_complete, 2)
cutree(hc_average, 2)
cutree(hc_single, 2)

# Visualise the clusters using hierarchical clustering 
ggplot(sim_data, aes(x, y)) +
  geom_point(shape = cutree(hc_complete, 2), size = 2)

# Checkpoint: What if we would like to have 4 clusters? Use different linkage methods, visualise the results and comment on the results.