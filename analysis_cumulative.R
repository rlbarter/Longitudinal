##### Analysis: clustering and then fit the model within each cluster.
library(ggplot2)
library(dplyr)
library(reshape2)
library(cluster)
load("MEPS_clean.RData")


# calculate cumulative expenses
meps_cum <- meps_clean %>% group_by(id) %>% mutate(cum_ER_expense = cumsum(ER_expense))


##################### Visualize data #####################
# plot a sample of the expenses:

index <- sample(unique(meps_cum$id),500)

# convert to wide-form
meps_wide <-  dcast(meps_cum, id ~ period, value.var = "cum_ER_expense")


# convert back to long form
meps_new_clustered <- meps_wide
meps_new_clustered <- melt(meps_new_clustered)
meps_new_clustered <- meps_new_clustered %>% arrange(id)
colnames(meps_new_clustered) <- c("id","period","cum_ER_expense")
meps_new_clustered$ER_expense <- unlist(meps_clean %>% 
  filter(id %in% meps_new_clustered$id) %>% 
  select(ER_expense))


# plot individuals expenses (raw and cumulative)
a = ggplot(filter(meps_new_clustered, id %in% index)) + geom_line(aes(x = as.numeric(period), y = cum_ER_expense, group = id))
a = a + labs(title="<Figure2: Cumulated ER expense over time>",x="Time (period)",y="Cumulated ER expense")
a = a + theme(axis.title.y = element_text(size = rel(2)), axis.title.x = element_text(size = rel(2)),plot.title = element_text(size=rel(2)))
a

a = ggplot(filter(meps_new_clustered, id %in% index)) + geom_line(aes(x = as.numeric(period), y = ER_expense, group = id))
a = a + labs(title="<Figure1: ER expense over time>",x="Time (period)",y="ER expense")
a = a + theme(axis.title.y = element_text(size = rel(2)), axis.title.x = element_text(size = rel(2)),plot.title = element_text(size=rel(2)))
a



########################## Clustering on shifted data: works really well! ################

set.seed(1357)
# change to wide-form
meps_wide <- dcast(meps_cum, id ~ period, value.var = "cum_ER_expense")
meps_wide <- as.data.frame(sapply(meps_wide, as.numeric))
# remove id variable
rownames(meps_wide) <- meps_wide$id
meps_wide <- meps_wide[,-1]
# subtract the mean:
meps_wide_shifted <- meps_wide - apply(meps_wide, 1, mean)



# perform clustering
# width <- c()
# for(i in 2:15) {
#   kmeans <- kmeans(meps_wide_shifted, centers = i)
#   width[i] <- summary(silhouette(kmeans$cluster, dist(meps_wide_shifted)))$avg.width
# }
# names(width) <- 2:15
# plot(width)
# 



kmeans <- kmeans(meps_wide_shifted, centers = 3)

# convert back to long-form
meps_new_clustered_shifted <- meps_wide_shifted
meps_new_clustered_shifted$id <- rownames(meps_new_clustered_shifted)
meps_new_clustered_shifted <- melt(meps_new_clustered_shifted, id = "id")
colnames(meps_new_clustered_shifted) <- c("id", "period", "ER_shifted_expense")
meps_new_clustered_shifted <- meps_new_clustered_shifted %>% arrange(id)
meps_new_clustered_shifted$cluster <- rep(kmeans$cluster, each = 5)
meps_new_clustered_shifted$ER_expense <- meps_new_clustered$ER_expense
meps_new_clustered_shifted$cum_ER_expense <- meps_new_clustered$cum_ER_expense


require(grid)
# plot clusters by looking at shifted cumulative expenses
index <- sample(unique(meps_new_clustered_shifted$id), 200)
ggplot(filter(meps_new_clustered_shifted, id %in% index)) + 
  geom_line(aes(x = as.numeric(period), y = ER_shifted_expense, group = id, col = factor(cluster))) +
  scale_y_continuous(name = "Shifted cumulative ER expenses") + 
  scale_x_continuous(name = "Time (period)") +
  ggtitle("<Figure3: Shifted cumulated ER expenses colored by cluster") +
  scale_colour_discrete(name="Cluster") +
  theme(axis.title.y = element_text(size = rel(2)), axis.title.x = element_text(size = rel(2)),plot.title = element_text(size=rel(2))) +
  theme(legend.key.size=unit(1.5,"cm"),legend.title=element_text(size=rel(1.5)),legend.text=element_text(size=rel(1.5)))



# plot clusters by looking at unshifted cumulative expenses
index <- sample(unique(meps_new_clustered_shifted$id), 400)
ggplot(filter(meps_new_clustered_shifted, id %in% index)) + 
  geom_line(aes(x = as.numeric(period), y = cum_ER_expense, group = id, col = factor(cluster))) +
  scale_y_continuous(name = "cumulative ER expenses") + 
  scale_x_continuous(name = "period") +
  ggtitle("Cumulative ER expenses colored by cluster")


# plot clusters by looking at unshifted expenses
index <- sample(unique(meps_new_clustered_shifted$id), 200)
ggplot(filter(meps_new_clustered_shifted, id %in% index)) + 
  geom_line(aes(x = as.numeric(period), y = ER_expense, group = id, col = factor(cluster))) + 
  scale_y_continuous(name = "ER expenses") + 
  scale_x_continuous(name = "period") +
  ggtitle("ER expenses colored by cluster")



  # average over cluster for cumulative expenses
index <- sample(unique(meps_new_clustered_shifted$id), 500)
meps_clustered_cumulative_mean <- meps_new_clustered_shifted %>% group_by(cluster, period) %>% summarize(mean_cum_ER_expense = mean(cum_ER_expense))
ggplot(meps_clustered_cumulative_mean) + 
  geom_line(aes(x = as.numeric(period), y = mean_cum_ER_expense, col = factor(cluster)), size = 4) +
  #geom_line(aes(x = as.numeric(period), y = cum_ER_expense, group = id, col = factor(cluster)), 
  #          data = filter(meps_new_clustered_shifted, id %in% index),
  #          alpha = 0.4) +
  scale_x_continuous(name = "Time (period)") +
  scale_y_continuous(name = "Average cumulated ER expense") + 
  ggtitle("<Figure5: Averaged cumulated ER expenses colored by cluster>") + 
  scale_colour_discrete(name="Cluster") +
  theme(axis.title.y = element_text(size = rel(2)), axis.title.x = element_text(size = rel(2)),plot.title = element_text(size=rel(2))) +
  theme(legend.key.size=unit(1.5,"cm"),legend.title=element_text(size=rel(1.5)),legend.text=element_text(size=rel(1.5)))

# so we are getting each of the peaks as well as one cluster that has very small expenses


# average over cluster for expenses
meps_clustered_cumulative_mean <- meps_new_clustered_shifted %>% group_by(cluster, period) %>% summarize(mean_ER_expense = mean(ER_expense))
ggplot(meps_clustered_cumulative_mean) + 
  geom_line(aes(x = as.numeric(period), y = mean_ER_expense, col = factor(cluster)), size = 4) +
  scale_x_continuous(name = "Time (period)") +
  scale_y_continuous(name = "Mean ER expense") +
  ggtitle("<Figure4: Averaged ER expenses colored by cluster>") + 
  scale_colour_discrete(name="Cluster") +
  theme(axis.title.y = element_text(size = rel(2)), axis.title.x = element_text(size = rel(2)),plot.title = element_text(size=rel(2))) +
  theme(legend.key.size=unit(1.5,"cm"),legend.title=element_text(size=rel(1.5)),legend.text=element_text(size=rel(1.5)))

# so we are getting each of the peaks as well as one cluster that has very small expenses




##################### Analyzing the data by cluster ##############

meps_clean$period <- factor(meps_clean$period)
meps_clustered_cumulative <- inner_join(meps_new_clustered_shifted, meps_clean, by = c("id","period"))
meps_clustered_cumulative <- meps_clustered_cumulative %>% select(id, period, cluster, cum_ER_expense, ER_expense.x, dwelling, age, region, sex, asian, black, white, hispanic, married, education_years, health_status, mental_health_status, pregnant, employed, income, insured, smoker2, ER_visits)
colnames(meps_clustered_cumulative) <- c("id", "period", "cluster", "cum_ER_expense", "ER_expense", "dwelling", "age", "region", "sex", "asian", "black", "white", "hispanic", "married", "education_years", "health_status", "mental_health_status", "pregnant", "employed", "income", "insured", "smoker2", "ER_visits")

# there are smokers and non-smokers in each cluster! Yay!
meps_clustered_cumulative %>% group_by(cluster, smoker2) %>% summarize(n = n())

# plot expenses averaged over cluster for both smokers and non-smokers
meps_clustered_cumulative_mean <- meps_clustered_cumulative %>% group_by(cluster, period, smoker2) %>% summarize(mean_ER_expense = mean(ER_expense))
ggplot(meps_clustered_cumulative_mean) + 
  geom_line(aes(x = as.numeric(period), y = mean_ER_expense, col = factor(smoker2)), size = 4) +
  scale_x_continuous(name = "period") +
  scale_y_continuous(name = "mean ER expense") + 
  facet_wrap(~cluster)



# plot expenses averaged over cluster for both smokers and non-smokers
meps_clustered_cumulative_mean <- meps_clustered_cumulative %>% group_by(cluster, period, smoker2) %>% summarize(mean_cum_ER_expense = mean(cum_ER_expense))
ggplot(meps_clustered_cumulative_mean) + 
  geom_line(aes(x = as.numeric(period), y = mean_cum_ER_expense, col = factor(smoker2)), size = 4) +
  scale_x_continuous(name = "Time (period)") +
  scale_y_continuous(name = "Mean cumulated ER expense") + 
  facet_wrap(~cluster) +
  ggtitle("<Figure6: Mean cumulated ER >") + 
  scale_colour_discrete(name="Cluster") +
  theme(axis.title.y = element_text(size = rel(2)), axis.title.x = element_text(size = rel(2)),plot.title = element_text(size=rel(2))) +
  theme(legend.key.size=unit(1.5,"cm"),legend.title=element_text(size=rel(1.5)),legend.text=element_text(size=rel(1.5)))




save(meps_clustered_cumulative,file="meps_clustered_cumulative.RData")


