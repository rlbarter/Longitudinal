library(lme4)

#### Mixed Effects Model with random intercept and random slope ###

load("~/Desktop/Longtudinal/meps_clustered.RData")
View(meps_clustered)


list = list()

for(i in 1:6){
  cluster = meps_clustered[which(meps_clustered$cluster==i),]
  form = glmer(as.factor(ER_expense)~cbind(smoker2,period) + (1|id),
                      data = cluster, family = binomial)
  list[i]=form
}

list[6]
