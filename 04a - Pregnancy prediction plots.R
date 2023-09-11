# Title: Drivers of mid-winter pregnancy and fetal/neonatal survival
# Subtitle: 04a - Pregnancy prediction plots
# Author: Nathan D. Hooven
# Email: nathan.hooven@wsu.edu
# Affiliation: School of the Environment, Washington State University
# Date began: 11 Dec 2022
# Date completed: 11 Dec 2022
# Date modified: 10 Sep 2023
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages and dataset ----
#_____________________________________________________________________________________________________________

library(tidyverse)

load("preg_full_model.RData")

#_____________________________________________________________________________________________________________
# 3. Numeric age model ----
#_____________________________________________________________________________________________________________
# 3a. Rump ----
#_____________________________________________________________________________________________________________

# newdata
num.rump.newdata <- data.frame(BCS.rump.S = seq(min(preg.num.age.1$BCS.rump.S), 
                                                max(preg.num.age.1$BCS.rump.S),
                                                length.out = 100),
                               Final.mass.S = 0,
                               Age.lab.S = 0,
                               qAge.lab.S = 0)

# predict
num.rump.predict <- predict(num.age.glm, 
                            newdata = num.rump.newdata,
                            se.fit = TRUE,
                            type = "response")

# as a df 
num.rump.predict.df <- data.frame(x = (num.rump.newdata$BCS.rump.S * sd(preg.num.age$BCS.rump)) + mean(preg.num.age$BCS.rump),
                                  fit = num.rump.predict$fit,
                                  low = num.rump.predict$fit - num.rump.predict$se.fit*1.96,
                                  upp = num.rump.predict$fit + num.rump.predict$se.fit*1.96,
                                  var = "Rump",
                                  age.var = "Numeric")

#_____________________________________________________________________________________________________________
# 3b. Mass ----
#_____________________________________________________________________________________________________________

# newdata
num.mass.newdata <- data.frame(BCS.rump.S = 0,
                               Final.mass.S = seq(min(preg.num.age.1$Final.mass.S), 
                                                  max(preg.num.age.1$Final.mass.S),
                                                  length.out = 100),
                               Age.lab.S = 0,
                               qAge.lab.S = 0)

# predict
num.mass.predict <- predict(num.age.glm, 
                            newdata = num.mass.newdata,
                            se.fit = TRUE,
                            type = "response")

# as a df 
num.mass.predict.df <- data.frame(x = (num.mass.newdata$Final.mass.S * sd(preg.num.age$Final.mass)) + mean(preg.num.age$Final.mass),
                                  fit = num.mass.predict$fit,
                                  low = num.mass.predict$fit - num.mass.predict$se.fit*1.96,
                                  upp = num.mass.predict$fit + num.mass.predict$se.fit*1.96,
                                  var = "Mass",
                                  age.var = "Numeric")

#_____________________________________________________________________________________________________________
# 3c. Age ----
#_____________________________________________________________________________________________________________

# newdata
num.age.newdata <- tibble(BCS.rump.S = 0,
                          Final.mass.S = 0,
                          Age.lab.S = seq(min(preg.num.age.1$Age.lab.S), 
                                          max(preg.num.age.1$Age.lab.S),
                                          length.out = 100)) %>% 
                   mutate(qAge.lab.S = (((Age.lab.S*sd(preg.num.age.1$Age.lab) 
                                          + mean(preg.num.age.1$Age.lab))^2) 
                                          - mean(preg.num.age.1$qAge.lab)) 
                                          / sd(preg.num.age.1$qAge.lab))

# predict
num.age.predict <- predict(num.age.glm, 
                           newdata = num.age.newdata,
                           se.fit = TRUE,
                           type = "response")

# as a df 
num.age.predict.df <- data.frame(x = (num.age.newdata$Age.lab.S* sd(preg.num.age$Age.lab)) + mean(preg.num.age$Age.lab),
                                 fit = num.age.predict$fit,
                                 low = num.age.predict$fit - num.age.predict$se.fit*1.96,
                                 upp = num.age.predict$fit + num.age.predict$se.fit*1.96,
                                 var = "Age",
                                 age.var = "Numeric")

#_____________________________________________________________________________________________________________
# 4. Bind dfs together ----
#_____________________________________________________________________________________________________________

all.predict.df <- rbind(num.rump.predict.df,
                        num.mass.predict.df,
                        num.age.predict.df)

#_____________________________________________________________________________________________________________
# 5. Facetted plot ----
#_____________________________________________________________________________________________________________

ggplot(data = all.predict.df,
       aes(x = x,
           y = fit)) +
  theme_bw() +
  facet_wrap( ~ var,
              ncol = 1,
             scales = "free_x") +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = low,
                  ymax = upp),
              alpha = 0.25,
              color = NA) +
  ylab("Probability of pregnancy") +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  coord_cartesian(ylim = c(0, 1))
