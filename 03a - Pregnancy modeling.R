# Title: Drivers of mid-winter pregnancy and fetal/neonatal survival
# Subtitle: 03a - Pregnancy modeling
# Author: Nathan D. Hooven
# Email: nathan.hooven@wsu.edu
# Affiliation: School of the Environment, Washington State University
# Date began: 10 Dec 2022
# Date completed: 10 Dec 2022 
# Date modified: 17 Sep 2023
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(mosaic)               # prop function
library(glmmTMB)              # mixed-effects models
library(AICcmodavg)           # AICc model selection
library(pROC)                 # ROC
library(broom)                # extract parameter estimates

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

# all with confirmed pregnancy
preg.conf <- read.csv("all_confirmed_preg.csv")

# all rows with complete BCS and mass data
preg.body <- read.csv("preg_ageclass.csv")

# all rows with numeric ages
preg.num.age <- read.csv("preg_numeric.csv")

# add quadratic functional forms, and scale them
preg.num.age <- preg.num.age %>% mutate(qAge.lab = Age.lab^2,
                                        
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
preg.body$Year <- as.factor(preg.body$Year)
preg.num.age$Year <- as.factor(preg.num.age$Year)

#_____________________________________________________________________________________________________________
# 3. Percent pregnant ----
#_____________________________________________________________________________________________________________

# here we'll use bootstraps to generate confidence intervals

# overall
set.seed(678)

prop.overall <- do(5000) * prop(~Preg.lab == "1", data = resample(preg.conf))

quantile(prop.overall$prop_TRUE, probs = c(0.025, 0.975))

# yearlings only
set.seed(678)

prop.yearling <- do(5000) * prop(~Preg.lab == "1", data = resample(preg.conf[preg.conf$Age.class == "Yearling", ]))

quantile(prop.yearling$prop_TRUE, probs = c(0.025, 0.975))

# adults only
set.seed(678)

prop.adult <- do(5000) * prop(~Preg.lab == "1", data = resample(preg.conf[preg.conf$Age.class == "Adult", ]))

quantile(prop.adult$prop_TRUE, probs = c(0.025, 0.975))

# 2020
set.seed(678)

prop.2020 <- do(5000) * prop(~Preg.lab == "1", data = resample(preg.conf[preg.conf$Year == "2020", ]))

quantile(prop.2020$prop_TRUE, probs = c(0.025, 0.975))

# 2021
set.seed(678)

prop.2021 <- do(5000) * prop(~Preg.lab == "1", data = resample(preg.conf[preg.conf$Year == "2021", ]))

quantile(prop.2021$prop_TRUE, probs = c(0.025, 0.975))

# 2022
set.seed(678)

prop.2022 <- do(5000) * prop(~Preg.lab == "1", data = resample(preg.conf[preg.conf$Year == "2022", ]))

quantile(prop.2022$prop_TRUE, probs = c(0.025, 0.975))

#_____________________________________________________________________________________________________________
# 5. Numeric age models ----
#_____________________________________________________________________________________________________________
# 5a. Correlation ----
#_____________________________________________________________________________________________________________

cor(preg.num.age[ , c("Age.lab.S", "qAge.lab.S", "BCS.rump.S", "Final.mass.S", "total.cond.S")])

# year differences - Kruskal-Wallis tests
kruskal.test(BCS.rump.S ~ Year, data = preg.num.age)
kruskal.test(Final.mass.S ~ Year, data = preg.num.age)
kruskal.test(Age.lab.S ~ Year, data = preg.num.age)
kruskal.test(qAge.lab.S ~ Year, data = preg.num.age)

# remove subadults
preg.num.age.1 <- preg.num.age %>% filter(Age.class != "Yearling")

#_____________________________________________________________________________________________________________
# 5b. Model selection (mixed-effects models) ----
#_____________________________________________________________________________________________________________

# here we will fit several candidate models with different fixed effect structures, and the same
# random effect structure (Site nested within Year)

# model list
preg.model.list <- list()

# model 1 - intercept-only, nested RE
preg.model.list[[1]] <- glmmTMB(Preg.lab ~ 1 +
                                  (1 | Year/Site),
                                data = preg.num.age.1,
                                family = "binomial")

# model 2 - Age, nested RE
preg.model.list[[2]] <- glmmTMB(Preg.lab ~ Age.lab.S +
                                  qAge.lab.S +
                                  (1 | Year/Site),
                                data = preg.num.age.1,
                                family = "binomial")

# model 3 - Condition, nested RE
preg.model.list[[3]] <- glmmTMB(Preg.lab ~ total.cond.S +
                                  (1 | Year/Site),
                                data = preg.num.age.1,
                                family = "binomial")

# model 4 - Mass, nested RE
preg.model.list[[4]] <- glmmTMB(Preg.lab ~ Final.mass.S +
                                  (1 | Year/Site),
                                data = preg.num.age.1,
                                family = "binomial")

# model 5 - Age + Condition, nested RE
preg.model.list[[5]] <- glmmTMB(Preg.lab ~ Age.lab.S +
                                  qAge.lab.S +
                                  total.cond.S +
                                  (1 | Year/Site),
                                data = preg.num.age.1,
                                family = "binomial")

# model 6 - Age + Mass, nested RE DID NOT CONVERGE
#preg.model.list[[6]] <- glmmTMB(Preg.lab ~ Age.lab.S +
#                                  qAge.lab.S +
#                                  Final.mass.S +
#                                  (1 | Year/Site),
#                                data = preg.num.age.1,
#                                family = "binomial")

# model 6 - Condition + Mass, nested RE
preg.model.list[[6]] <- glmmTMB(Preg.lab ~ total.cond.S +
                                  Final.mass.S +
                                  (1 | Year/Site),
                                data = preg.num.age.1,
                                family = "binomial")

# model 7 - Age + Condition + Mass, nested RE
preg.model.list[[7]] <- glmmTMB(Preg.lab ~ Age.lab.S +
                                  qAge.lab.S +
                                  total.cond.S +
                                  Final.mass.S +
                                  (1 | Year/Site),
                                data = preg.num.age.1,
                                family = "binomial")

# model selection
aictab(preg.model.list)

#_____________________________________________________________________________________________________________
# 5c. GLMs for prediction ----
#_____________________________________________________________________________________________________________

preg.model.1 <- glmmTMB(Preg.lab ~ total.cond.S +
                          Final.mass.S +
                          (1 | Year/Site),
                        data = preg.num.age.1,
                        family = "binomial")

preg.model.2 <- glmmTMB(Preg.lab ~ Age.lab.S +
                          qAge.lab.S +
                          total.cond.S +
                          Final.mass.S +
                          (1 | Year/Site),
                        data = preg.num.age.1,
                        family = "binomial")


preg.model.3 <- glmmTMB(Preg.lab ~ Final.mass.S +
                          (1 | Year/Site),
                        data = preg.num.age.1,
                        family = "binomial")

summary(preg.model.1)
summary(preg.model.2)
summary(preg.model.3)

# ROC
roc(preg.num.age.1$Preg.lab, predict(preg.model.1))
roc(preg.num.age.1$Preg.lab, predict(preg.model.2))
roc(preg.num.age.1$Preg.lab, predict(preg.model.3))

#_____________________________________________________________________________________________________________
# 6. Save image ----
#_____________________________________________________________________________________________________________

save.image("preg_full_model.RData")
