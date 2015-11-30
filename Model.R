library(lme4)
library(nlme)

#### Mixed Effects Model with random intercept and random slope ###

load("meps_clustered_cumulative.RData")




########## fit to full dataset ############
meps_clustered_cumulative <- meps_clustered_cumulative %>% filter(!(is.na(education_years)), !(is.na(income)), !(is.na(employed)))

fit = lmer(ER_expense ~  (1 | id) + factor(smoker2) + factor(insured) +
              scale(age) + factor(white) + scale(education_years) + 
             factor(employed) + scale(income) + factor(sex),
            data = meps_clustered_cumulative)

summary(fit)$coeff

#residuals <- fitted(fit) - meps_clustered$ER_expense
# plot(fitted(fit),residuals)
# 
# full_df <- data.frame(summary(fit)$coeff)
# full_df$variable <- factor(c("intercept","smoker","insured","age","white","education years","employed","income","sex"), levels = c("intercept","smoker","insured","age","white","education years","employed","income","sex"))
# ggplot(full_df[-1,]) + 
#   geom_bar(aes(x = variable, y = t.value), position = "dodge", stat="identity") 
  



############## fit model to each cluster #################


list = list()

for(i in 1:length(unique(meps_clustered_cumulative$cluster))){
  # restrict data frame to cluster of interest
  cluster = meps_clustered_cumulative %>% filter(cluster == i)
  # fit a mixed effects linear model
  form = lmer(ER_expense ~  (1 | id) + factor(smoker2) + factor(insured) +
                 scale(age) + factor(white) + 
                 scale(education_years) + factor(employed) + scale(income) + factor(sex),
                      data = cluster)
  list[i]=form
}

cluster_df <- lapply(list, function(x) data.frame(summary(x)$coeff))
for(i in 1:length(cluster_df)) {
  cluster_df[[i]]$cluster <- i
  cluster_df[[i]]$variable <- factor(c("intercept","smoker","insured","age","white","education years","employed","income","sex"), levels = c("intercept","smoker","insured","age","white","education years","employed","income","sex"))
  cluster_df[[i]] <-  cluster_df[[i]][-1,]
}

cluster_df
