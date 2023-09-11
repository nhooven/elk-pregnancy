# Title: Drivers of mid-winter pregnancy and fetal/neonatal survival
# Subtitle: 03b - Calf success modeling
# Author: Nathan D. Hooven
# Email: nathan.hooven@wsu.edu
# Affiliation: School of the Environment, Washington State University
# Date began: 10 Dec 2022
# Date completed: 10 Dec 2022
# Date modified: 10 Sep 2023
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(pROC)        # ROC
library(broom)

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

# all with confirmed calf status
fns.1 <- read.csv("all_fns.csv")

# all rows with complete BCS and mass data
fns.body <- read.csv("fns_ageclass.csv")

# all rows with numeric ages
fns.num.age <- read.csv("fns_numeric.csv")

# add quadratic  functional forms, and scale them
fns.body <- fns.body %>% mutate(Final.mass.S = as.numeric(scale(Final.mass)),
                                BCS.rump.S = as.numeric(scale(BCS.rump)))

fns.num.age <- fns.num.age %>% mutate(qAge.lab = Age.lab^2,
                                        qFinal.mass = Final.mass^2,
                                        qBCS.rump = BCS.rump^2) %>%
                                 mutate(Age.lab.S = as.numeric(scale(Age.lab)),
                                        qAge.lab.S = as.numeric(scale(qAge.lab)),
                                        Final.mass.S = as.numeric(scale(Final.mass)),
                                        BCS.rump.S = as.numeric(scale(BCS.rump)))

# change year to "factor"
fns.body$Year <- as.factor(fns.body$Year)
fns.num.age$Year <- as.factor(fns.num.age$Year)

#_____________________________________________________________________________________________________________
# 3. Percent pregnant ----
#_____________________________________________________________________________________________________________

# overall
prop.test(nrow(fns.1[fns.1$Calf.success == 1, ]),
          nrow(fns.1),
          conf.level = 0.95)

# yearlings only
prop.test(nrow(fns.1[fns.1$Calf.success == 1 & fns.1$Age.class == "Yearling", ]),
          nrow(fns.1[fns.1$Age.class == "Yearling", ]),
          conf.level = 0.95)

# adults only
prop.test(nrow(fns.1[fns.1$Calf.success == 1 & fns.1$Age.class == "Adult", ]),
          nrow(fns.1[fns.1$Age.class == "Adult", ]),
          conf.level = 0.95)

# test for differences between age classes
age.successes <- c(nrow(fns.1[fns.1$Calf.success == 1 & fns.1$Age.class == "Adult", ]),
                   nrow(fns.1[fns.1$Calf.success == 1 & fns.1$Age.class == "Yearling", ]))

age.trials <- c(nrow(fns.1[fns.1$Age.class == "Adult", ]),
                nrow(fns.1[fns.1$Age.class == "Yearling", ]))

prop.test(age.successes,
          age.trials)

# test for differences between years
year.successes <- c(nrow(fns.1[fns.1$Calf.success == 1 & fns.1$Year == 2020, ]),
                    nrow(fns.1[fns.1$Calf.success == 1 & fns.1$Year == 2021, ]),
                    nrow(fns.1[fns.1$Calf.success == 1 & fns.1$Year == 2022, ]))

year.trials <- c(nrow(fns.1[fns.1$Year == 2020, ]),
                 nrow(fns.1[fns.1$Year == 2021, ]),
                 nrow(fns.1[fns.1$Year == 2022, ]))

prop.test(year.successes,
          year.trials)

# 2020
prop.test(nrow(fns.1[fns.1$Calf.success == 1 & fns.1$Year == 2020, ]),
          nrow(fns.1[fns.1$Year == 2020, ]),
          conf.level = 0.95)

# 2021
prop.test(nrow(fns.1[fns.1$Calf.success == 1 & fns.1$Year == 2021, ]),
          nrow(fns.1[fns.1$Year == 2021, ]),
          conf.level = 0.95)

# 2022
prop.test(nrow(fns.1[fns.1$Calf.success == 1 & fns.1$Year == 2022, ]),
          nrow(fns.1[fns.1$Year == 2022, ]),
          conf.level = 0.95)

#_____________________________________________________________________________________________________________
# 3. Percent successful ----
#_____________________________________________________________________________________________________________

# here we'll use bootstraps to generate confidence intervals

# overall
set.seed(678)

prop.overall <- do(5000) * prop(~Calf.success == "1", data = resample(fns.1))

quantile(prop.overall$prop_TRUE, probs = c(0.025, 0.975))

# yearlings only
set.seed(678)

prop.yearling <- do(5000) * prop(~Calf.success == "1", data = resample(fns.1[fns.1$Age.class == "Yearling", ]))

quantile(prop.yearling$prop_TRUE, probs = c(0.025, 0.975))

# adults only
set.seed(678)

prop.adult <- do(5000) * prop(~Calf.success == "1", data = resample(fns.1[fns.1$Age.class == "Adult", ]))

quantile(prop.adult$prop_TRUE, probs = c(0.025, 0.975))

# 2020
set.seed(678)

prop.2020 <- do(5000) * prop(~Calf.success == "1", data = resample(fns.1[fns.1$Year == "2020", ]))

quantile(prop.2020$prop_TRUE, probs = c(0.025, 0.975))

# 2021
set.seed(678)

prop.2021 <- do(5000) * prop(~Calf.success == "1", data = resample(fns.1[fns.1$Year == "2021", ]))

quantile(prop.2021$prop_TRUE, probs = c(0.025, 0.975))

# 2022
set.seed(678)

prop.2022 <- do(5000) * prop(~Calf.success == "1", data = resample(fns.1[fns.1$Year == "2022", ]))

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

cor(fns.num.age.1[ , c("BCS.rump.S", "Final.mass.S", "Age.lab.S", "qAge.lab.S")])

# year differences - Kruskal-Wallis tests
kruskal.test(BCS.rump.S ~ Year, data = fns.num.age.1)
kruskal.test(Final.mass.S ~ Year, data = fns.num.age.1)
kruskal.test(Age.lab.S ~ Year, data = fns.num.age.1)
kruskal.test(qAge.lab.S ~ Year, data = fns.num.age.1)

#_____________________________________________________________________________________________________________
# 5b. GLM for prediction ----
#_____________________________________________________________________________________________________________

num.age.glm <- glm(Calf.success ~ Age.lab.S +
                              qAge.lab.S +
                              BCS.rump.S +
                              Final.mass.S,
                   weights = weight,
                   family = "binomial",
                   data = fns.num.age.2)

summary(num.age.glm)

plot(num.age.glm)

# ROC
roc(fns.num.age.2$Calf.success, predict(num.age.glm))

num.age.tidy <- tidy(num.age.glm) %>% 
                mutate(model = "success.num.age")

#_____________________________________________________________________________________________________________
# 6. Copy parameter estimates and save image ----
#_____________________________________________________________________________________________________________

write.table(num.age.tidy, "clipboard", sep = "\t")

save.image("success_full_model.RData")
