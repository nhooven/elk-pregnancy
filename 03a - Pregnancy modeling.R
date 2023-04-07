# Title: Drivers of mid-winter pregnancy and fetal/neonatal survival
# Subtitle: 3a - Pregnancy modeling
# Author: Nathan D. Hooven
# Email: nathan.hooven@wsu.edu
# Affiliation: School of the Environment, Washington State University
# Date began: 10 Dec 2022
# Date completed: 10 Dec 2022 
# Date modified: 07 Apr 2023
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(mosaic)      # prop function
library(pROC)        # ROC
library(broom)       # extract parameter estimates

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
preg.body <- preg.body %>% mutate(Final.mass.S = as.numeric(scale(Final.mass)),
                                  BCS.rump.S = as.numeric(scale(BCS.rump)))

preg.num.age <- preg.num.age %>% mutate(qAge.lab = Age.lab^2) %>%
                                 mutate(Age.lab.S = as.numeric(scale(Age.lab)),
                                        qAge.lab.S = as.numeric(scale(qAge.lab)),
                                        Final.mass.S = as.numeric(scale(Final.mass)),
                                        BCS.rump.S = as.numeric(scale(BCS.rump)))

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
# 4. Categorical age models ----
#_____________________________________________________________________________________________________________
# 4a. Correlation ----
#_____________________________________________________________________________________________________________

cor(preg.body[ , c("BCS.rump.S", "Final.mass.S")])

# age and year differences - Kruskal-Wallis tests
# age
kruskal.test(BCS.rump.S ~ Age.class, data = preg.body)
kruskal.test(Final.mass.S ~ Age.class, data = preg.body)

# year
kruskal.test(BCS.rump.S ~ Year, data = preg.body)
kruskal.test(Final.mass.S ~ Year, data = preg.body)

#_____________________________________________________________________________________________________________
# 4b. LASSO for feature selection ----
#_____________________________________________________________________________________________________________

# extract input matrix
cat.age.matrix <- model.matrix(Preg.lab ~ 
                                 Year + 
                                 Age.class + 
                                 BCS.rump.S + 
                                 Final.mass.S,
                               data = preg.body)

# use cross-validation to chose best model
cat.age.cv <- cv.glmnet(x = cat.age.matrix[ , -1],           # drop extra intercept term
                        y = preg.body[ , c("Preg.lab")],     # response variable
                        family = "binomial",                 # logistic regression
                        alpha = 1,                           # LASSO regression
                        standardize = FALSE)                 # predictors are already standardized

plot(cat.age.cv)

coef(cat.age.cv, s = cat.age.cv$lambda.min) %>% as.matrix()

cat.age.model <- glmnet(x = cat.age.matrix[ , -1],
                        y = preg.body[ , c("Preg.lab")],
                        family = "binomial",
                        lambda = cat.age.cv$lambda.min,
                        alpha = 1,
                        standardize = FALSE)

coef(cat.age.model)

#_____________________________________________________________________________________________________________
# 4c. GLM for prediction ----
#_____________________________________________________________________________________________________________

cat.age.glm <- glm(Preg.lab ~ BCS.rump.S + 
                              Final.mass.S,
                   family = "binomial",
                   data = preg.body)

summary(cat.age.glm)

plot(cat.age.glm)

# ROC
roc(preg.body$Preg.lab, predict(cat.age.glm))

cat.age.tidy <- tidy(cat.age.glm) %>% 
                mutate(model = "preg.cat.age")

#_____________________________________________________________________________________________________________
# 5. Categorical age models ----
#_____________________________________________________________________________________________________________
# 5a. Correlation ----
#_____________________________________________________________________________________________________________

cor(preg.num.age[ , c("Age.lab.S", "qAge.lab.S", "BCS.rump.S", "Final.mass.S")])

# year differences - Kruskal-Wallis tests
# year differences - Kruskal-Wallis tests
kruskal.test(BCS.rump.S ~ Year, data = preg.num.age)
kruskal.test(Final.mass.S ~ Year, data = preg.num.age)
kruskal.test(Age.lab.S ~ Year, data = preg.num.age)
kruskal.test(qAge.lab.S ~ Year, data = preg.num.age)

#_____________________________________________________________________________________________________________
# 5b. LASSO for feature selection ----
#_____________________________________________________________________________________________________________

# extract input matrix
num.age.matrix <- model.matrix(Preg.lab ~ 
                                 Year + 
                                 Age.lab.S +
                                 qAge.lab.S +
                                 BCS.rump.S + 
                                 Final.mass.S,
                               data = preg.num.age)

# use cross-validation to chose best model
num.age.cv <- cv.glmnet(x = num.age.matrix[ , -1],              # drop extra intercept term
                        y = preg.num.age[ , c("Preg.lab")],     # response variable
                        family = "binomial",                    # logistic regression
                        alpha = 1,                              # LASSO regression
                        standardize = FALSE)                    # predictors are already standardized

plot(num.age.cv)

coef(num.age.cv, s = num.age.cv$lambda.min) %>% as.matrix()

num.age.model <- glmnet(x = num.age.matrix[ , -1],
                        y = preg.num.age[ , c("Preg.lab")],
                        family = "binomial",
                        lambda = num.age.cv$lambda.min,
                        alpha = 1,
                        standardize = FALSE)

coef(num.age.model)

#_____________________________________________________________________________________________________________
# 5c. GLM for prediction ----
#_____________________________________________________________________________________________________________

num.age.glm <- glm(Preg.lab ~ Age.lab.S +
                              qAge.lab.S +
                              BCS.rump.S + 
                              Final.mass.S,
                   family = "binomial",
                   data = preg.num.age)

summary(num.age.glm)

plot(num.age.glm)

# ROC
roc(preg.num.age$Preg.lab, predict(num.age.glm))

num.age.tidy <- tidy(num.age.glm) %>% 
                mutate(model = "preg.num.age")

#_____________________________________________________________________________________________________________
# 6. Copy parameter estimates and save image ----
#_____________________________________________________________________________________________________________

preg.tidy <- rbind(cat.age.tidy, num.age.tidy)

write.table(preg.tidy, "clipboard", sep = "\t")

save.image("preg_full_model.RData")
