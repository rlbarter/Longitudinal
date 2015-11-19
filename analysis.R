##### Analysis: clustering and then fit the model within each cluster.
library(ggplot2)
library(dplyr)
library(cluster)
load("MEPS_clean.RData")

##################### Vertical Shifting #####################
# plot a sample of the expenses:
index <- sample(unique(meps_clean$id), 1000)
ggplot(filter(meps_clean, id %in% index)) + geom_line(aes(x = as.numeric(period), y = log(1 + ER_expense), col = id))
# note that I imagine that we are going to find something like the following clusters:
        # (1) those who have constant expenses
        # (2) those who have one-off expenses
        # (3) those who have decreasing expenses 
        # (4) those who have increasing expenses


# isolate the mean expense for each individual
colnames(meps_clean)
meps_first <- meps_clean %>% 
  group_by(id) %>% 
  filter(period == 1) %>% 
  mutate(ER_mean_expense = mean(ER_expense))

# subtract the mean expense for the subject from the other expenses
meps_clean_mean <- inner_join(dplyr::select(meps_first, id, ER_mean_expense), meps_clean, by = "id")
meps_shifted <- meps_clean_mean
meps_shifted <- meps_shifted %>% mutate(ER_shifted_expense = ER_expense - ER_mean_expense)

# new variables
meps_second <- meps_shifted %>% 
  group_by(id) %>% 
  mutate(ER_change_expense1 = max(diff(ER_shifted_expense))) %>%
  mutate(ER_sd_expense = sd(ER_shifted_expense)) %>%
  mutate(ER_shifted_mean = mean(ER_shifted_expense[1:5])) %>%
  mutate(period4 = ER_shifted_expense[4]) %>%
  mutate(period3 = ER_shifted_expense[3])

meps_new <- meps_second
save(meps_new, file="MEPS_new.RData")


# look at the shifted trajectories
ggplot(filter(meps_new, id %in% index)) + geom_line(aes(x = as.numeric(period), y = ER_shifted_expense, col = id))

##################### Clustering #####################
index <- sample(unique(meps_clean$id),500)

### K-means-clustering
kmeans <- kmeans(dplyr::select(meps_new,ER_sd_expense,ER_shifted_mean), centers = 5)
# plot the clusters
meps_new_clustered <- meps_new
meps_new_clustered$cluster <- factor(kmeans$cluster)
ggplot(filter(meps_new_clustered, id %in% index)) + geom_line(aes(x = as.numeric(period), y = ER_shifted_expense, group = id, col = cluster))
ggplot(meps_new_clustered) + geom_line(aes(x = as.numeric(period), y = ER_shifted_expense, group = id, col = cluster))

### Clara
clara <- clara(dplyr::select(meps_new,ER_change_expense1, ER_sd_expense, ER_shifted_mean),k=5)
# plot the clusters
meps_new_clustered2 <- meps_new
meps_new_clustered2$cluster <- factor(clara$clustering)
ggplot(filter(meps_new_clustered2, id %in% index)) + geom_line(aes(x = as.numeric(period), y = ER_shifted_expense, group=id, col = cluster))
ggplot(meps_new_clustered2) + geom_line(aes(x = as.numeric(period), y = ER_shifted_expense, group = id, col = cluster))

################ KML (K-means for Longitudinal Data) #############
library(kml3d)
library(kml)
library(reshape)

### let's just consider ER_shifted_expense only for testing kml method
new <- as.data.frame(meps_new)
new2 <- as.matrix(new$ER_shifted_expense)
id <- unique(new$id)
period <- unique(new$period)

### changed into wide format
wide <- matrix(NA,nrow=length(id),ncol=length(period))
colnames(wide) <- period
wide[,1] = id

for(i in 1:length(id)){
  for(j in 1:length(period)){
    wide[i,j] = new2[5*i+j-5]
  }
}

View(wide)

cld = clusterLongData(wide,idAll = id)
kml(cld,nbClusters=5,toPlot='both')

# haven't figured out how to cluster data cleary enough..


