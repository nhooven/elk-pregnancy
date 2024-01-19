# Title: Drivers of mid-winter pregnancy and fetal/neonatal survival
# Subtitle: 03b - Calf success modeling
# Author: Nathan D. Hooven
# Email: nathan.hooven@wsu.edu
# Affiliation: School of the Environment, Washington State University
# Date began: 10 Dec 2022
# Date completed: 10 Dec 2022
# Date modified: 19 Jan 2024
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(mosaic)               # prop function
library(glmmTMB)              # mixed-effects models
library(AICcmodavg)           # AICc model selection
library(pROC)                 # ROC

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

# all with confirmed calf status
fns.1 <- read.csv("all_fns.csv")

# use correct "offspring viability" metrics
fns.1$viability <- fns.1$Calf.success

# change "viability" to 1s for each individual who had a living calf
fns.1$viability[fns.1$E.number %in% c("E20073",
                                      "E21004",
                                      "E21020",
                                      "E21009",
                                      "E22072",
                                      "E22031")] <- 1

# all rows with complete BCS and mass data
fns.body <- read.csv("fns_ageclass.csv")

# all rows with numeric ages
fns.num.age <- read.csv("fns_numeric.csv")

# use correct "offspring viability" metrics
fns.num.age$viability <- fns.num.age$Calf.success

# change "viability" to 1s for each individual who had a living calf
fns.num.age$viability[fns.num.age$E.number %in% c("E20073",
                                                  "E21004",
                                                  "E21020",
                                                  "E21009",
                                                  "E22072",
                                                  "E22031")] <- 1

# add quadratic  functional forms, and scale them
fns.num.age <- fns.num.age %>% mutate(qAge.lab = Age.lab^2,
                                        qFinal.mass = Final.mass^2,
                                        qBCS.rump = BCS.rump^2,
                                      
                                      # total body condition
                                      total.cond = BCS.ribs +
                                        BCS.withers +
                                        BCS.rump) %>%
  
                                 mutate(Age.lab.S = as.numeric(scale(Age.lab)),
                                        qAge.lab.S = as.numeric(scale(qAge.lab)),
                                        Final.mass.S = as.numeric(scale(Final.mass)),
                                        BCS.rump.S = as.numeric(scale(BCS.rump)),
                                        total.cond.S = as.numeric(scale(total.cond)))

# change year to "factor"
fns.body$Year <- as.factor(fns.body$Year)
fns.num.age$Year <- as.factor(fns.num.age$Year)

#_____________________________________________________________________________________________________________
# 3. Percent successful ----
#_____________________________________________________________________________________________________________

# remove non-pregnant and subadult individuals
fns.2 <- fns.1 %>% filter(Preg.lab == "Y" & Age.class == "Adult")

# overall
prop.test(nrow(fns.2[fns.2$viability == 1, ]),
          nrow(fns.2),
          conf.level = 0.95)

# test for differences between age classes
age.successes <- c(nrow(fns.2[fns.2$viability == 1 & fns.2$Age.class == "Adult", ]),
                   nrow(fns.2[fns.2$viability == 1 & fns.2$Age.class == "Yearling", ]))

age.trials <- c(nrow(fns.2[fns.2$Age.class == "Adult", ]),
                nrow(fns.2[fns.2$Age.class == "Yearling", ]))

prop.test(age.successes,
          age.trials)

# test for differences between years
year.successes <- c(nrow(fns.2[fns.2$viability == 1 & fns.2$Year == 2020, ]),
                    nrow(fns.2[fns.2$viability == 1 & fns.2$Year == 2021, ]),
                    nrow(fns.2[fns.2$viability == 1 & fns.2$Year == 2022, ]))

year.trials <- c(nrow(fns.2[fns.2$Year == 2020, ]),
                 nrow(fns.2[fns.2$Year == 2021, ]),
                 nrow(fns.2[fns.2$Year == 2022, ]))

prop.test(year.successes,
          year.trials)

# 2020
prop.test(nrow(fns.2[fns.2$viability == 1 & fns.2$Year == 2020, ]),
          nrow(fns.2[fns.2$Year == 2020, ]),
          conf.level = 0.95)

# 2021
prop.test(nrow(fns.2[fns.2$viability == 1 & fns.2$Year == 2021, ]),
          nrow(fns.2[fns.2$Year == 2021, ]),
          conf.level = 0.95)

# 2022
prop.test(nrow(fns.2[fns.2$viability == 1 & fns.2$Year == 2022, ]),
          nrow(fns.2[fns.2$Year == 2022, ]),
          conf.level = 0.95)

#_____________________________________________________________________________________________________________
# 3. Percent successful ----
#_____________________________________________________________________________________________________________

# here we'll use bootstraps to generate confidence intervals

# overall
set.seed(678)

prop.overall <- do(5000) * prop(~viability == "1", data = resample(fns.2))

quantile(prop.overall$prop_TRUE, probs = c(0.025, 0.975))


# 2020
set.seed(678)

prop.2020 <- do(5000) * prop(~viability == "1", data = resample(fns.2[fns.2$Year == "2020", ]))

quantile(prop.2020$prop_TRUE, probs = c(0.025, 0.975))

# 2021
set.seed(678)

prop.2021 <- do(5000) * prop(~viability == "1", data = resample(fns.2[fns.2$Year == "2021", ]))

quantile(prop.2021$prop_TRUE, probs = c(0.025, 0.975))

# 2022
set.seed(678)

prop.2022 <- do(5000) * prop(~viability == "1", data = resample(fns.2[fns.2$Year == "2022", ]))

quantile(prop.2022$prop_TRUE, probs = c(0.025, 0.975))

#_____________________________________________________________________________________________________________
# 4. Numeric age models ----
#_____________________________________________________________________________________________________________

# add weights for predicted status
fns.num.age$weight <- ifelse(fns.num.age$Conf.pred == "Conf",
                             1.00,
                             ifelse(fns.num.age$Calf.success == 1 & fns.num.age$Conf.pred == "Pred",
                                    0.919,
                                    0.850))

# remove subadults
fns.num.age.1 <- fns.num.age %>% filter(Age.class != "Yearling")

# remove non-pregnant individuals
fns.num.age.2 <- fns.num.age.1 %>% filter(Preg.lab == "Y")

#_____________________________________________________________________________________________________________
# 4a. Correlation ----
#_____________________________________________________________________________________________________________

cor(fns.num.age.2[ , c("BCS.rump.S", "Final.mass.S", "Age.lab.S", "qAge.lab.S", "total.cond.S")])

# year differences - Kruskal-Wallis tests
kruskal.test(BCS.rump.S ~ Year, data = fns.num.age.2)
kruskal.test(Final.mass.S ~ Year, data = fns.num.age.2)
kruskal.test(Age.lab.S ~ Year, data = fns.num.age.2)
kruskal.test(qAge.lab.S ~ Year, data = fns.num.age.2)

#_____________________________________________________________________________________________________________
# 5b. Model selection (mixed-effects models) ----
#_____________________________________________________________________________________________________________

# here we will fit several candidate models with different fixed effect structures, and the same
# random effect structure (Site nested within Year)

# model list
success.model.list <- list()

# model 1 - intercept-only, nested RE
success.model.list[[1]] <- glmmTMB(viability ~ 
                                     1 +
                                  (1 | Year/Site),
                                data = fns.num.age.2,
                                family = "binomial",
                                weights = weight)

# model 2 - Age, nested RE
success.model.list[[2]] <- glmmTMB(viability ~ 
                                     Age.lab.S +
                                  qAge.lab.S +
                                  (1 | Year/Site),
                                data = fns.num.age.2,
                                family = "binomial",
                                weights = weight)

# model 3 - Condition, nested RE
success.model.list[[3]] <- glmmTMB(viability ~ 
                                     total.cond.S +
                                  (1 | Year/Site),
                                data = fns.num.age.2,
                                family = "binomial",
                                weights = weight)

# model 4 - Mass, nested RE
success.model.list[[4]] <- glmmTMB(viability ~ 
                                     Final.mass.S +
                                  (1 | Year/Site),
                                data = fns.num.age.2,
                                family = "binomial",
                                weights = weight)

# model 5 - Age + Condition, nested RE
success.model.list[[5]] <- glmmTMB(viability ~ 
                                     Age.lab.S +
                                  qAge.lab.S +
                                    total.cond.S +
                                  (1 | Year/Site),
                                data = fns.num.age.2,
                                family = "binomial",
                                weights = weight)

# model 6 - Age + Mass, nested RE
success.model.list[[6]] <- glmmTMB(viability ~ 
                                     Age.lab.S +
                                         qAge.lab.S +
                                         Final.mass.S +
                                  (1 | Year/Site),
                                data = fns.num.age.2,
                                family = "binomial",
                                weights = weight)

# model 7 - Condition + Mass, nested RE
success.model.list[[7]] <- glmmTMB(viability ~ 
                                     total.cond.S +
                                  Final.mass.S +
                                  (1 | Year/Site),
                                data = fns.num.age.2,
                                family = "binomial",
                                weights = weight)

# model 8 - Age + Condition + Mass, nested RE
success.model.list[[8]] <- glmmTMB(viability ~ 
                                     Age.lab.S +
                                  qAge.lab.S +
                                  total.cond.S +
                                  Final.mass.S +
                                  (1 | Year/Site),
                                data = fns.num.age.2,
                                family = "binomial",
                                weights = weight)

# model selection
aictab(success.model.list)

summary(success.model.list[[3]])

#_____________________________________________________________________________________________________________
# 5c. GLM for prediction ----
#_____________________________________________________________________________________________________________

success.model.1 <- glmmTMB(viability ~ 
                             1 +
                                (1 | Year/Site),
                              data = fns.num.age.2,
                              family = "binomial",
                              weights = weight)

summary(success.model.1)

# inverse logit
exp(success.model.1$fit$par[1]) / (1 + exp(success.model.1$fit$par[1]))

#_____________________________________________________________________________________________________________
# 6. Copy parameter estimates and save image ----
#_____________________________________________________________________________________________________________

save.image("success_full_model.RData")
