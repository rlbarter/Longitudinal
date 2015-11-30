##### Analysis: clustering and then fit the model within each cluster.
library(ggplot2)
library(dplyr)
library(reshape2)
library(cluster)
load("MEPS_clean.RData")

##################### Visualize data #####################
# plot a sample of the expenses:
index <- sample(unique(meps_clean$id), 1000)
ggplot(filter(meps_clean, id %in% index)) + geom_line(aes(x = as.numeric(period), y = log(1 + ER_expense), col = id))
# note that I imagine that we are going to find something like the following clusters:
        # (1) those who have constant expenses
        # (2) those who have one-off expenses
        # (3) those who have decreasing expenses 
        # (4) those who have increasing expenses




##################### Clustering on unshifted data: does not work #####################
index <- sample(unique(meps_clean$id),500)

# convert to wide-form
meps_wide <-  dcast(meps_clean, id ~ period, value.var = "ER_expense")

# remove those who didn't visit the ER
no_visits <- which(apply(meps_wide[,-1], 1, function(x) sum(x == 0)) == 5)
meps_wide_visited <- meps_wide[-no_visits,]

# do clustering
kmeans <- kmeans(meps_wide_visited, centers = 6)
# convert back to long form
meps_new_clustered <- meps_wide_visited
meps_new_clustered <- melt(meps_new_clustered)
meps_new_clustered <- meps_new_clustered %>% arrange(id)
colnames(meps_new_clustered) <- c("id","period","ER_expense")
meps_new_clustered$cluster <- rep(kmeans$cluster, each = 5)

# plot clusters
ggplot(filter(meps_new_clustered, id %in% index)) + geom_line(aes(x = as.numeric(period), y = ER_expense, group = id, col = factor(cluster))) 



########################## Clustering on shifted data: works really well! ################

set.seed(123)
# change to wide-form
meps_wide <- dcast(meps_clean, id ~ period, value.var = "ER_expense")
meps_wide <- as.data.frame(sapply(meps_wide, as.numeric))
# remove id variable
rownames(meps_wide) <- meps_wide$id
meps_wide <- meps_wide[,-1]
# subtract the mean:
meps_wide_shifted <- meps_wide - apply(meps_wide, 1, mean)


# remove all individuals who have no expenses and cluster the rest
no_expenses <- which(apply(meps_wide_shifted, 1, function(x) sum(x == 0)) == 5)
meps_wide_shifted_exp <- meps_wide_shifted[-no_expenses, ]


# perform clustering
width <- c()
for(i in 2:15) {
  kmeans <- kmeans(meps_wide_shifted_exp, centers = i)
  width[i] <- summary(silhouette(kmeans$cluster, dist(meps_wide_shifted_exp)))$avg.width
}
names(width) <- 2:15
plot(width)


kmeans <- kmeans(meps_wide_shifted_exp, centers = 6)

# convert back to long-form
meps_new_clustered_shifted <- meps_wide_shifted_exp
meps_new_clustered_shifted$id <- rownames(meps_new_clustered_shifted)
meps_new_clustered_shifted <- melt(meps_new_clustered_shifted, id = "id")
colnames(meps_new_clustered_shifted) <- c("id", "period", "ER_shifted_expense")
meps_new_clustered_shifted <- meps_new_clustered_shifted %>% arrange(id)
meps_new_clustered_shifted$cluster <- rep(kmeans$cluster, each = 5)
meps_new_clustered_shifted$ER_expense <- meps_new_clustered$ER_expense

# plot clusters by looking at shifted expenses
index <- sample(unique(meps_new_clustered_shifted$id), 100)
ggplot(filter(meps_new_clustered_shifted, id %in% index)) + geom_line(aes(x = as.numeric(period), y = ER_shifted_expense, group = id, col = factor(cluster)))

# plot clusters by looking at unshifted expenses
index <- sample(unique(meps_new_clustered_shifted$id), 200)
ggplot(filter(meps_new_clustered_shifted, id %in% index)) + geom_line(aes(x = as.numeric(period), y = ER_expense, group = id, col = factor(cluster)))

# average over cluster
meps_clustered_mean <- meps_new_clustered_shifted %>% group_by(cluster, period) %>% summarize(mean_ER_expense = mean(ER_expense))
ggplot(meps_clustered_mean) + 
  geom_line(aes(x = as.numeric(period), y = mean_ER_expense, col = factor(cluster)), size = 4) +
  scale_x_continuous(name = "period") +
  scale_y_continuous(name = "mean ER expense")
# so we are getting each of the peaks as well as one cluster that has very small expenses


##################### Analyzing the data by cluster ##############

meps_clean$period <- factor(meps_clean$period)
meps_clustered <- inner_join(meps_new_clustered_shifted, meps_clean, by = c("id","period"))
meps_clustered <- meps_clustered %>% select(id, period, cluster, ER_expense.x, dwelling, age, region, sex, asian, black, white, hispanic, married, education_years, health_status, mental_health_status, pregnant, employed, income, insured, smoker2, ER_visits)
colnames(meps_clustered) <- c("id", "period", "cluster", "ER_expense", "dwelling", "age", "region", "sex", "asian", "black", "white", "hispanic", "married", "education_years", "health_status", "mental_health_status", "pregnant", "employed", "income", "insured", "smoker2", "ER_visits")

# there are smokers and non-smokers in each cluster! Yay!
meps_clustered %>% group_by(cluster, smoker2) %>% summarize(n = n())

# plot expenses averaged over cluster for both smokers and no-smokers
meps_clustered_mean <- meps_clustered %>% group_by(cluster, period, smoker2) %>% summarize(mean_ER_expense = mean(ER_expense))
ggplot(meps_clustered_mean) + 
  geom_line(aes(x = as.numeric(period), y = mean_ER_expense, col = factor(smoker2)), size = 4) +
  scale_x_continuous(name = "period") +
  scale_y_continuous(name = "mean ER expense") + 
  facet_wrap(~cluster)
# so we are getting each of the peaks as well as one cluster that has very small expenses

save(meps_clustered,file="meps_clustered.RData")


### previous attempts
# 
# 
# ### let's just consider ER_shifted_expense only for testing kml method
# new <- as.data.frame(meps_new)
# new2 <- as.matrix(new$ER_shifted_expense)
# id <- unique(new$id)
# period <- unique(new$period)
# 
# ### changed into wide format
# wide <- matrix(NA,nrow=length(id),ncol=length(period))
# colnames(wide) <- period
# wide[,1] = id
# 
# for(i in 1:length(id)){
#   for(j in 1:length(period)){
#     wide[i,j] = new2[5*i+j-5]
#   }
# }
# 
# View(wide)
# 
# cld = clusterLongData(wide,idAll = id)
# kml(cld,nbClusters=5,toPlot='both')
# 
# # haven't figured out how to cluster data cleary enough..
