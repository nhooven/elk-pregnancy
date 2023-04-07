# Title: Drivers of mid-winter pregnancy and fetal/neonatal survival
# Subtitle: 3b - Calf success modeling
# Author: Nathan D. Hooven
# Email: nathan.hooven@wsu.edu
# Affiliation: School of the Environment, Washington State University
# Date began: 10 Dec 2022
# Date completed: 10 Dec 2022
# Date modified: 
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(glmnet)      # LASSO 
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
# 3. Categorical age models ----
#_____________________________________________________________________________________________________________

# add weights for predicted status
fns.body$weight <- ifelse(fns.body$Conf.pred == "Conf",
                          1.00,
                          ifelse(fns.body$Calf.success == 1 & fns.body$Conf.pred == "Pred",
                                 0.919,
                                 0.850))

#_____________________________________________________________________________________________________________
# 3a. Correlation ----
#_____________________________________________________________________________________________________________

cor(fns.body[ , c("BCS.rump.S", "Final.mass.S")])

# age and year differences - Kruskal-Wallis tests
# age
kruskal.test(BCS.rump.S ~ Age.class, data = fns.body)
kruskal.test(Final.mass.S ~ Age.class, data = fns.body)

# year
kruskal.test(BCS.rump.S ~ Year, data = fns.body)
kruskal.test(Final.mass.S ~ Year, data = fns.body)

#_____________________________________________________________________________________________________________
# 3b. LASSO for feature selection ----
#_____________________________________________________________________________________________________________

# extract input matrix
cat.age.matrix <- model.matrix(Calf.success ~ 
                                 Year + 
                                 Age.class + 
                                 BCS.rump.S + 
                                 Final.mass.S,
                               data = fns.body)

# use cross-validation to chose best model
cat.age.cv <- cv.glmnet(x = cat.age.matrix[ , -1],           # drop extra intercept term
                        y = fns.body[ , c("Calf.success")],  # response variable
                        family = "binomial",                 # logistic regression
                        alpha = 1,                           # LASSO regression
                        weights = fns.body$weight,           # weights for inferred status
                        standardize = FALSE)                 # predictors are already standardized

plot(cat.age.cv)

coef(cat.age.cv, s = cat.age.cv$lambda.min) %>% as.matrix()

cat.age.model <- glmnet(x = cat.age.matrix[ , -1],
                        y = fns.body[ , c("Calf.success")],
                        family = "binomial",
                        lambda = cat.age.cv$lambda.min,
                        alpha = 1,
                        weights = fns.body$weight,
                        standardize = FALSE)

coef(cat.age.model)

#_____________________________________________________________________________________________________________
# 4c. GLM for prediction ----
#_____________________________________________________________________________________________________________

cat.age.glm <- glm(Calf.success ~ Final.mass.S,
                   family = "binomial",
                   weights = weight,
                   data = fns.body)

summary(cat.age.glm)

plot(cat.age.glm)

# ROC
roc(fns.body$Calf.success, predict(cat.age.glm))

cat.age.tidy <- tidy(cat.age.glm) %>% 
                mutate(model = "success.cat.age")

#_____________________________________________________________________________________________________________
# 4. Numeric age models ----
#_____________________________________________________________________________________________________________

# add weights for predicted status
fns.num.age$weight <- ifelse(fns.num.age$Conf.pred == "Conf",
                             1.00,
                             ifelse(fns.num.age$Calf.success == 1 & fns.num.age$Conf.pred == "Pred",
                                    0.919,
                                    0.850))

#_____________________________________________________________________________________________________________
# 4a. Correlation ----
#_____________________________________________________________________________________________________________

cor(fns.num.age[ , c("BCS.rump.S", "Final.mass.S", "Age.lab.S", "qAge.lab.S")])

# year differences - Kruskal-Wallis tests
kruskal.test(BCS.rump.S ~ Year, data = fns.num.age)
kruskal.test(Final.mass.S ~ Year, data = fns.num.age)
kruskal.test(Age.lab.S ~ Year, data = fns.num.age)
kruskal.test(qAge.lab.S ~ Year, data = fns.num.age)

#_____________________________________________________________________________________________________________
# 4b. LASSO for feature selection ----
#_____________________________________________________________________________________________________________

# extract input matrix
num.age.matrix <- model.matrix(Calf.success ~ 
                                 Year + 
                                 Age.lab.S +
                                 qAge.lab.S +
                                 BCS.rump.S + 
                                 Final.mass.S,
                               data = fns.num.age)

# use cross-validation to chose best model
num.age.cv <- cv.glmnet(x = num.age.matrix[ , -1],              # drop extra intercept term
                        y = fns.num.age[ , c("Calf.success")],  # response variable
                        family = "binomial",                    # logistic regression
                        alpha = 1,                              # LASSO regression
                        weights = fns.num.age$weight,           # weights for inferred status
                        standardize = FALSE)                    # predictors are already standardized

plot(num.age.cv)

coef(num.age.cv, s = num.age.cv$lambda.min) %>% as.matrix()

num.age.model <- glmnet(x = num.age.matrix[ , -1],
                        y = fns.num.age[ , c("Calf.success")],
                        family = "binomial",
                        lambda = num.age.cv$lambda.min,
                        alpha = 1,
                        weights = fns.num.age$weight,
                        standardize = FALSE)

coef(num.age.model)

#_____________________________________________________________________________________________________________
# 5c. GLM for prediction ----
#_____________________________________________________________________________________________________________

num.age.glm <- glm(Calf.success ~ Age.lab.S +
                              qAge.lab.S +
                              Final.mass.S,
                   weights = weight,
                   family = "binomial",
                   data = fns.num.age)

summary(num.age.glm)

plot(num.age.glm)

# ROC
roc(fns.num.age$Calf.success, predict(num.age.glm))

num.age.tidy <- tidy(num.age.glm) %>% 
                mutate(model = "success.num.age")

#_____________________________________________________________________________________________________________
# 6. Copy parameter estimates and save image ----
#_____________________________________________________________________________________________________________

success.tidy <- rbind(cat.age.tidy, num.age.tidy)

write.table(success.tidy, "clipboard", sep = "\t")

save.image("success_full_model.RData")
