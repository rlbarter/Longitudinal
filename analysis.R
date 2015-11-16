##### Analysis: clustering and then fit the model within each cluster.
library(ggplot2)
library(dplyr)
load("MEPS_clean.RData")


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


# look at the shifted trajectories
ggplot(filter(meps_shifted, id %in% index)) + geom_line(aes(x = as.numeric(period), y = ER_shifted_expense, col = id))
# use k-means using the period and ER_shifted_expense variables
kmeans <- kmeans(dplyr::select(meps_shifted, period, ER_shifted_expense), centers = 3)

# plot the clusters
meps_shifted_clustered <- meps_shifted
meps_shifted_clustered$cluster <- factor(kmeans$cluster)
index <- sample(unique(meps_clean$id), 200)

ggplot(filter(meps_shifted_clustered, id %in% index)) + geom_line(aes(x = as.numeric(period), y = ER_expense, group = id, col = cluster))
# doesn't seem to work yet!
