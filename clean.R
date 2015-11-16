# The data were collected from 5 interview rounds over a 2 year period
# http://meps.ahrq.gov/mepsweb/data_stats/download_data_files.jsp


######################### READ IN DATA ######################

library(dplyr)

dat_raw <- foreign::read.xport("Long11_12.ssp")

#remove obs with missing date:
#to_rm <- c(which(is.na(dat_raw[,"BEGRFD1"])), which(is.na(dat_raw[,"BEGRFD2"])), which(is.na(dat_raw[,"BEGRFD3"])))
#dat_raw <- dat_raw[-to_rm,]

dat_raw[dat_raw[,"BEGRFY1"] < 0,"BEGRFY1"] <- NA
dat_raw[dat_raw[,"BEGRFY2"] < 0,"BEGRFY2"] <- NA
dat_raw[dat_raw[,"BEGRFY3"] < 0,"BEGRFY3"] <- NA
dat_raw[dat_raw[,"BEGRFY4"] < 0,"BEGRFY4"] <- NA
dat_raw[dat_raw[,"BEGRFY5"] < 0,"BEGRFY5"] <- NA
dat_raw[dat_raw[,"BEGRFM1"] < 0,"BEGRFM1"] <- NA
dat_raw[dat_raw[,"BEGRFM2"] < 0,"BEGRFM2"] <- NA
dat_raw[dat_raw[,"BEGRFM3"] < 0,"BEGRFM3"] <- NA
dat_raw[dat_raw[,"BEGRFM4"] < 0,"BEGRFM4"] <- NA
dat_raw[dat_raw[,"BEGRFM5"] < 0,"BEGRFM5"] <- NA
dat_raw[dat_raw[,"BEGRFD1"] < 0,"BEGRFD1"] <- NA
dat_raw[dat_raw[,"BEGRFD2"] < 0,"BEGRFD2"] <- NA
dat_raw[dat_raw[,"BEGRFD3"] < 0,"BEGRFD3"] <- NA
dat_raw[dat_raw[,"BEGRFD4"] < 0,"BEGRFD4"] <- NA
dat_raw[dat_raw[,"BEGRFD5"] < 0,"BEGRFD5"] <- NA

missing <- is.na(dat_raw$BEGRFY1) | is.na(dat_raw$BEGRFY2) | is.na(dat_raw$BEGRFY3) | is.na(dat_raw$BEGRFY4) | is.na(dat_raw$BEGRFY5)

dat_raw <- dat_raw[!missing,]

dat_raw12_ER <- foreign::read.xport("ER12.ssp")
dat_raw11_ER <- foreign::read.xport("ER11.ssp")

# identify samples in original dataset for which we also have ER data
#joined12 <- inner_join(dat_raw, dat_raw12_ER, by = "DUPERSID")
#joined11 <- inner_join(dat_raw, dat_raw11_ER, by = "DUPERSID")

#ids <- unique(c(joined12$DUPERSID, joined11$DUPERSID))

# restrict to samples for which we have both longitudinal data and ER data
#dat_raw <- dat_raw %>% filter(DUPERSID %in% ids)
#dat_raw12_ER <- dat_raw12_ER %>% filter(DUPERSID %in% ids)
#dat_raw11_ER <- dat_raw11_ER %>% filter(DUPERSID %in% ids)

################# RESTRICT VARIABLES FOR OVERALL DATA#######################


unknown_period <- unique(c(dat_raw$DUPERSID[(dat_raw$BEGRFY1 < 0)], 
                           dat_raw11_ER$DUPERSID[which(dat_raw11_ER$ERDATEMM < 0)],
                           dat_raw12_ER$DUPERSID[which(dat_raw12_ER$ERDATEMM < 0)]))

dat_raw <- dat_raw %>% filter(!(DUPERSID %in% unknown_period))
dat_raw12_ER <- dat_raw12_ER %>% filter(!(DUPERSID %in% unknown_period))
dat_raw11_ER <- dat_raw11_ER %>% filter(!(DUPERSID %in% unknown_period))





dat_raw$DATER1 <- as.Date(paste0(dat_raw$BEGRFY1, "-", dat_raw$BEGRFM1, "-", dat_raw$BEGRFD1))
dat_raw$DATER2 <- as.Date(paste0(dat_raw$BEGRFY2, "-", dat_raw$BEGRFM2, "-", dat_raw$BEGRFD2))
dat_raw$DATER3 <- as.Date(paste0(dat_raw$BEGRFY3, "-", dat_raw$BEGRFM3, "-", dat_raw$BEGRFD3))
dat_raw$DATER4 <- as.Date(paste0(dat_raw$BEGRFY4, "-", dat_raw$BEGRFM4, "-", dat_raw$BEGRFD4))
dat_raw$DATER5 <- as.Date(paste0(dat_raw$BEGRFY5, "-", dat_raw$BEGRFM5, "-", dat_raw$BEGRFD5))







vars <- c(rep("DUPERSID", 5),
          rep("DUID", 5),
          rep("AGEY1X", 5),
          "REGION1",
          "REGION2", 
          "REGION3",
          "REGION4",
          "REGION5",
          "DATER1",
          "DATER2",
          "DATER3",
          "DATER4",
          "DATER5",
          rep("SEX", 5),
          rep("RACEAX", 5),
          rep("RACEBX", 5),
          rep("RACEWX", 5),
          rep("HISPANX", 5),
          "MARRY1X",
          "MARRY2X",
          "MARRY3X",
          "MARRY4X",
          "MARRY5X",
          rep("EDUCYR", 5),
          "RTHLTH1",
          "RTHLTH2",
          "RTHLTH3",
          "RTHLTH4",
          "RTHLTH5",
          "MNHLTH1",
          "MNHLTH2",
          "MNHLTH3",
          "MNHLTH4",
          "MNHLTH5",
          "PREGNT1",
          "PREGNT2",
          "PREGNT3",
          "PREGNT4",
          "PREGNT5",
          rep("NEVILL2", 5),
          "EMPST1",
          "EMPST2",
          "EMPST3",
          "EMPST4",
          "EMPST5",
          "OFREMP1",
          "OFREMP2",
          "OFREMP3",
          "OFREMP4",
          "OFREMP5",
          rep("TTLPY1X", 5),
          "INS1X",
          "INS2X",
          "INS3X",
          "INS4X",
          "INS5X",
          rep("ADSMOK2", 5),
          rep("ADSMOK4", 5))

dat <- dat_raw[,vars]
k <- 1
a <- list()
for(i in 1:(length(vars)/5)) {
  a[[i]] <- k:(k + 4)
  k <- 5*i+1
}

dat_long <- data.frame(id = unlist(dat[,a[[1]]]))
for(i in 2:length(a)) {
  dat_long[,i] <- unlist(dat[,a[[i]]])
}

vars_names <- c("id", 
                "dwelling", 
                "age",
                "region",
                "begin_date",
                "sex", 
                "asian", 
                "black", 
                "white", 
                "hispanic", 
                "married",
                "education_years", 
                "health_status", 
                "mental_health_status",
                "pregnant",
                "never_ill",
                "employed",
                "empl_insurance",
                "income",
                "insured",
                "smoker2",
                "smoker4")


rownames(dat_long) <- NULL
colnames(dat_long) <- vars_names
dat_long$begin_date <- c(dat$DATER1, dat$DATER2, dat$DATER3, dat$DATER4, dat$DATER5)

dat_long <- data.frame(dat_long)
dat_long$period <- rep(1:5, each =  nrow(dat)) 

dat_long <- dat_long[c(1,ncol(dat_long),2:(ncol(dat_long) - 1))]
dat_long <- dat_long %>% arrange(id)


################### MAKE DATA LEGIBLE ###############
dat_long$employed <- as.numeric(as.vector(dat_long$employed))
dat_long[dat_long[,"employed"] == 1,"employed"] <- 1
dat_long[dat_long[,"employed"] == 2,"employed"] <- 1
dat_long[dat_long[,"employed"] == 3,"employed"] <- 1
dat_long[dat_long[,"employed"] == 4,"employed"] <- 0
dat_long[dat_long[,"employed"] < 0,"employed"] <- NA

dat_long[,"region"] <- as.numeric(as.vector(dat_long$region))
dat_long[dat_long[,"region"] == 1,"region"] <- "northwest"
dat_long[dat_long[,"region"] == 2,"region"] <- "midwest"
dat_long[dat_long[,"region"] == 3,"region"] <- "south"
dat_long[dat_long[,"region"] == 4,"region"] <- "west"
dat_long[dat_long[,"region"] < 0,"region"] <- NA

dat_long[,"sex"] <- as.numeric(as.vector(dat_long$sex))
dat_long[dat_long[,"sex"] == 1,"sex"] <- "male"
dat_long[dat_long[,"sex"] == 2,"sex"] <- "female"

dat_long[,"asian"] <- as.numeric(as.vector(dat_long$asian))
dat_long[dat_long[,"asian"] == 1,"asian"] <- 1
dat_long[dat_long[,"asian"] == 2,"asian"] <- 1
dat_long[dat_long[,"asian"] == 3,"asian"] <- 0

dat_long[,"black"] <- as.numeric(as.vector(dat_long$black))
dat_long[dat_long[,"black"] == 1,"black"] <- 1
dat_long[dat_long[,"black"] == 2,"black"] <- 1
dat_long[dat_long[,"black"] == 3,"black"] <- 0

dat_long[,"white"] <- as.numeric(as.vector(dat_long$white))
dat_long[dat_long[,"white"] == 1,"white"] <- 1
dat_long[dat_long[,"white"] == 2,"white"] <- 1
dat_long[dat_long[,"white"] == 3,"white"] <- 0

dat_long[,"hispanic"] <- as.numeric(as.vector(dat_long$hispanic))
dat_long[dat_long[,"hispanic"] == 1,"hispanic"] <- 1
dat_long[dat_long[,"hispanic"] == 2,"hispanic"] <- 0

dat_long[,"married"] <- as.numeric(as.vector(dat_long$married))
dat_long[dat_long[,"married"] == 1,"married"] <- 1
dat_long[dat_long[,"married"] > 1,"married"] <- 0
dat_long[dat_long[,"married"] < 0,"married"] <- NA


# note that health status is:
#     1 - excellent
#     2 - very good
#     3 - good
#     4 - fair
#     5 - poor
dat_long[,"health_status"] <- as.numeric(as.vector(dat_long$health_status))
dat_long[dat_long[,"health_status"] < 0,"health_status"] <- NA

# note that mental health status is:
#     1 - excellent
#     2 - very good
#     3 - good
#     4 - fair
#     5 - poor
dat_long[,"mental_health_status"] <- as.numeric(as.vector(dat_long$mental_health_status))
dat_long[dat_long[,"mental_health_status"] < 0,"mental_health_status"] <- NA

dat_long[,"education_years"] <- as.numeric(as.vector(dat_long$education_years))
dat_long[dat_long[,"education_years"] < 0,"education_years"] <- NA


dat_long[,"pregnant"] <- as.numeric(as.vector(dat_long$pregnant))
dat_long[dat_long[,"pregnant"] == 1,"pregnant"] <- 1
dat_long[dat_long[,"pregnant"] == 2,"pregnant"] <- 0
dat_long[dat_long[,"pregnant"] < 0,"pregnant"] <- NA


# scale for never been seriously ill is:
#     1 - definitely true
#     2 - mostly true
#     3 - don't know
#     4 - mostly false
#     5 - definitely false
dat_long[,"never_ill"] <- as.numeric(as.vector(dat_long$never_ill))
dat_long[dat_long[,"never_ill"] < 0,"never_ill"] <- NA


dat_long[,"empl_insurance"] <- as.numeric(as.vector(dat_long$empl_insurance))
dat_long[dat_long[,"empl_insurance"] == 1,"empl_insurance"] <- 1
dat_long[dat_long[,"empl_insurance"] == 2,"empl_insurance"] <- 0
dat_long[dat_long[,"empl_insurance"] < 0,"empl_insurance"] <- NA

dat_long[,"income"] <- as.numeric(as.vector(dat_long$income))
dat_long[dat_long[,"income"] < 0,"income"] <- NA

dat_long[,"insured"] <- as.numeric(as.vector(dat_long$insured))
dat_long[dat_long[,"insured"] == 1,"insured"] <- 1
dat_long[dat_long[,"insured"] == 2,"insured"] <- 0
dat_long[dat_long[,"insured"] < 0,"insured"] <- NA

dat_long[,"smoker2"] <- as.numeric(as.vector(dat_long$smoker2))
dat_long[dat_long[,"smoker2"] == 1,"smoker2"] <- 1
dat_long[dat_long[,"smoker2"] == 2,"smoker2"] <- 0
dat_long[dat_long[,"smoker2"] < 0,"smoker2"] <- NA

dat_long[,"smoker4"] <- as.numeric(as.vector(dat_long$smoker4))
dat_long[dat_long[,"smoker4"] == 1,"smoker4"] <- 1
dat_long[dat_long[,"smoker4"] == 2,"smoker4"] <- 0
dat_long[dat_long[,"smoker4"] < 0,"smoker4"] <- NA




###################################### RESTRICT VARIABLES FOR MEDICAL DATA ########################

# estimate that the event happened in the middle of the month
dat_raw11_ER$DATE <- as.Date(paste0(dat_raw11_ER$ERDATEYR, "-", dat_raw11_ER$ERDATEMM, "-", 15))
dat_raw12_ER$DATE <- as.Date(paste0(dat_raw12_ER$ERDATEYR, "-", dat_raw12_ER$ERDATEMM, "-", 15))


vars12_ER <- c("DUID",
             "DUPERSID",
             "DATE",
             "EVNTIDX",
             "ERXP12X")
vars11_ER <- c("DUID",
               "DUPERSID",
               "DATE",
               "EVNTIDX",
               "ERXP11X")
vars_ER_names <- c("dwelling", 
                   "id",
                   "date",
                   "eventid",
                   "expense")



## need to make it so that it matches up with the interview ranges 1-5
dat11_ER <- dat_raw11_ER[,vars11_ER]
dat12_ER <- dat_raw12_ER[,vars12_ER]
colnames(dat11_ER) <- colnames(dat12_ER) <- vars_ER_names


# unique(dat11_ER$id) %in% unique(dat_long$id)



# need to isolate the total number and cost of events in each "period" for each individual
ER_period <- data_frame(id = NA, period = NA, expense = NA)
for(i in unique(dat_long$id)) {
  if ((i %in% dat11_ER$id) | (i %in% dat12_ER$id)) {
    i <- as.numeric(as.vector(i))
    dat11_i <- filter(dat11_ER, id == i)
    dat12_i <- filter(dat12_ER, id == i)
    period_dates <- filter(dplyr::select(dat_long, id, period, begin_date), id == i)$begin_date
  
    r11 <- sapply(lapply(dat11_i$date, function(x) x < period_dates), function(x) which(x)[1] - 1)
    r12 <- sapply(lapply(dat12_i$date, function(x) x < period_dates), function(x) which(x)[1] - 1)
    r12[is.na(r12)] <- 5
    
    if (length(r11) > 0) df11 <- data.frame(id = i, period = unlist(r11), expense = dat11_i$expense)
      
    if (length(r12) > 0) df12 <- data.frame(id = i, period = unlist(r12), expense = dat12_i$expense)
    if (exists("df11") && exists("df12")) {
      df <- rbind(df11, df12)
      rm(df11, df12)
      } else if (exists("df11")) {
        df <- df11
        rm(df11)
      } else if (exists("df12")) {
        df <- df12
        rm(df12)
        }
    
    
    ER_period <- rbind(ER_period, df)
  }
}

ER_period <- ER_period[-1,]



ER_dat <- ER_period %>% group_by(id, period) %>% summarize(ER_visits = n(), ER_expense = sum(expense))


################# PUT ER DATA TOGETHER WITH COVARIATE DATA ####################


dat_long$id <- droplevels(dat_long$id)
ER_dat$id <- factor(ER_dat$id)
dat_final <- full_join(dat_long, ER_dat, by = c("id", "period"))
dat_final$ER_visits[is.na(dat_final$ER_visits)] <- 0
dat_final$ER_expense[is.na(dat_final$ER_expense)] <- 0

##################### FINALIZE DATA #################################

dim(dat_final)


# make all males "pregnant = 0"
dat_final[(!is.na(dat_final$sex) && (dat_final$sex == "male")),"pregnant"] <- 0



# check out sparsity of the data
apply(dat_final, 2, function(x) sum(is.na(x))/nrow(dat_final))


# remove all observations for whom we do not have smoking information
# this also removed a lot of the other missing values -- potential bias!?
missing_smoke2 <- which(is.na(dat_final[,"smoker2"]))
missing_smoke4 <- which(is.na(dat_final[,"smoker4"]))


dat_final <- dat_final %>% filter(!is.na(smoker2))
dat_final <- dat_final %>% filter(!is.na(smoker4))

dim(dat_final)


# check out sparsity of the data
apply(dat_final, 2, function(x) sum(is.na(x)))



### how many people's smoking status change?
change_smoking <- dat_final %>% filter(smoker2 != smoker4)
dim(change_smoking)


### let's remove all people who change smoking status
dat_final <- dat_final %>% filter(!(id %in% change_smoking$id))




meps_clean <- dat_final

save(meps_clean, file = "MEPS_clean.RData")




################### EXPLORATORY DATA ANALYSIS ###################


################## ANALYSIS IDEA 
# Within each of the matched groups:
# could use poisson regression to model number of visits; use GEE and random intercept mixed effects model
ggplot(meps_clean) + geom_histogram(aes(x = ER_visits, y = ..density..), binwidth = 1, col = "white") + facet_grid(.~smoker2)
# could use linear regression with random intercept and random slope (for coef of smoking) in mixed effects model
ggplot(filter(meps_clean, ER_expense != 0)) + geom_histogram(aes(x = ER_expense, y = ..density..), col = "white") + facet_grid(.~smoker2)





################## EXAMINE COVARIATES
### how many people smoke and how many people do not?
dat_final %>% group_by(smoker2) %>% summarize(n = n())
# approx 45,000 nonsmokers and 8,500 smokers


### Let's compare covariates on each of these two groups
dat_final$sex <- as.numeric(factor(dat_final$sex))-1
dat_final %>% group_by(smoker2) %>% summarize(m_age = mean(age), 
                                              sd_age = sd(age), 
                                              m_edu = mean(education_years),
                                              sd_edu = sd(education_years, na.rm = T),
                                              pct_male = mean(sex), 
                                              pct_asian = mean(asian),
                                              pct_black = mean(black),
                                              pct_hisp = mean(hispanic),
                                              pct_married = mean(married),
                                              pct_pregnant = mean(pregnant, na.rm = T),
                                              pct_never_ill = mean(never_ill, na.rm = T),
                                              pct_employed = mean(employed, na.rm = T))
dat_final %>% group_by(smoker2, region) %>% summarize(n = n())

# differences at baseline
dat_final %>% filter(period == 1) %>% 
  group_by(sex, age_range, smoker2) %>% 
  summarize(#m_age = mean(age), 
            #sd_age = sd(age),
            m_edu = mean(education_years,na.rm = T),
            sd_edu = sd(education_years, na.rm = T),
            pct_male = mean(sex), 
            pct_asian = mean(asian),
            pct_black = mean(black),
            pct_hisp = mean(hispanic),
            pct_married = mean(married),
            pct_pregnant = mean(pregnant, na.rm = T),
            pct_employed = mean(employed, na.rm = T)) # splitting by gender seems to even out things!
dat_final %>% group_by(sex, smoker2) %>% summarize(m_age = n())

dat_final$age_range <- cut(dat_final$age, breaks = c(17, 25, 40, 55, 70, 85))


# how did employment and imcome change over time?
change <- dat_final %>% 
  group_by(period, smoker2) %>% 
  summarize(m_income = mean(income, na.rm = T), 
            m_employed = mean(employed, na.rm = T),
            m_married = mean(married, na.rm = T),
            m_insured = mean(insured, na.rm = T),
            m_health = mean(health_status, na.rm = T),
            m_mental = mean(mental_health_status, na.rm = T))
ggplot(change) + geom_line(aes(x = period, y = m_employed, col = factor(smoker2)))
ggplot(change) + geom_line(aes(x = period, y = m_married, col = factor(smoker2)))
ggplot(change) + geom_line(aes(x = period, y = m_insured, col = factor(smoker2)))
ggplot(change) + geom_line(aes(x = period, y = m_health, col = factor(smoker2)))
ggplot(change) + geom_line(aes(x = period, y = m_mental, col = factor(smoker2)))






########### CALCULATE PROPENSITY SCORE ###########


library(Matching)
library(mi)

# perform multiple imputation to impute missing values (there are not too many)
dat_final <- dat_final[,!(colnames(dat_final) %in% c("never_ill","smoker4"))]
baseline <- dat_final %>% filter(period == 1)
miss_baseline <-  missing_data.frame(baseline[,-6])
show(miss_baseline)
baseline_imp <- mi(miss_baseline)
baseline_complete <- complete(baseline_imp, 1)
lapply(baseline_complete, summary)



baseline_complete$midwest <- as.numeric(baseline_complete$region == "midwest")
baseline_complete$northwest <- as.numeric(baseline_complete$region == "northwest")
baseline_complete$south <- as.numeric(baseline_complete$region == "south")
smoke <- as.numeric(as.vector(baseline_complete$smoker2))
init_vars <- c("age","dwelling","midwest","northwest","south","sex","asian","black","hispanic","white","married","education_years","pregnant","employed","income","insured")
baseline_complete$sex <- as.numeric(baseline_complete$sex) - 1
baseline_complete$asian <- as.numeric(baseline_complete$asian) - 1
baseline_complete$hispanic <- as.numeric(baseline_complete$hispanic) - 1
baseline_complete$white <- as.numeric(baseline_complete$white) - 1
baseline_complete$black <- as.numeric(baseline_complete$black) - 1
baseline_complete$married <- as.numeric(baseline_complete$married) - 1
baseline_complete$pregnant <- as.numeric(baseline_complete$pregnant) - 1
baseline_complete$employed <- as.numeric(baseline_complete$employed) - 1
baseline_complete$insured <- as.numeric(baseline_complete$insured) - 1
baseline_complete$health_status <- as.numeric(baseline_complete$health_status)
baseline_complete$mental_health_status <- as.numeric(baseline_complete$mental_health_status)

pscore.glm <- glm(smoke ~ as.matrix(baseline_complete[,init_vars]), family = "binomial")
pscore <- pscore.glm$fitted.values

lin_pscore <- log(pscore/(1 - pscore))
ctrl_lpscore <- mean(lin_pscore[smoke == 0])
tmt_lpscore <- mean(lin_pscore[smoke == 1])
ctrl_lpscore_sd <- sd(lin_pscore[smoke == 0])
tmt_lpscore_sd <- sd(lin_pscore[smoke == 1])

diff_lpscore <- (tmt_lpscore - ctrl_lpscore)/sqrt((ctrl_lpscore_sd^2 + tmt_lpscore_sd^2)/2)






######################## ESTIMATE BALANCE ################################


baseline_complete %>% 
  group_by(smoker2) %>% 
  summarize(m_age = mean(age), 
            m_sex = mean(sex), 
            m_asian = mean(asian),
            m_black = mean(black),
            m_white = mean(white),
            m_hispanic = mean(hispanic),
            m_married = mean(married),
            m_edu = mean(education_years),
            m_health = mean(health_status),
            m_menhealth = mean(mental_health_status),
            m_pregnant = mean(pregnant),
            m_employed = mean(employed),
            m_emplinsurance = mean(empl_insurance),
            mincome = mean(income))


