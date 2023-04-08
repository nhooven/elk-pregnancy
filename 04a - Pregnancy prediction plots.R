# Title: Drivers of mid-winter pregnancy and fetal/neonatal survival
# Subtitle: 04a - Pregnancy prediction plots
# Author: Nathan D. Hooven
# Email: nathan.hooven@wsu.edu
# Affiliation: School of the Environment, Washington State University
# Date began: 11 Dec 2022
# Date completed: 11 Dec 2022
# Date modified: 
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages and dataset ----
#_____________________________________________________________________________________________________________

library(tidyverse)

load("preg_full_model.RData")

#_____________________________________________________________________________________________________________
# 2. Categorical age model ----
#_____________________________________________________________________________________________________________
# 2a. Rump ----
#_____________________________________________________________________________________________________________

# newdata
cat.rump.newdata <- data.frame(BCS.rump.S = seq(min(preg.body$BCS.rump.S), 
                                                max(preg.body$BCS.rump.S),
                                                length.out = 100),
                               Final.mass.S = 0)

# predict
cat.rump.predict <- predict(cat.age.glm, 
                            newdata = cat.rump.newdata,
                            se.fit = TRUE,
                            type = "response")

# as a df 
cat.rump.predict.df <- data.frame(x = (cat.rump.newdata$BCS.rump.S * sd(preg.body$BCS.rump)) + mean(preg.body$BCS.rump),
                                  fit = cat.rump.predict$fit,
                                  low = cat.rump.predict$fit - cat.rump.predict$se.fit*1.96,
                                  upp = cat.rump.predict$fit + cat.rump.predict$se.fit*1.96,
                                  var = "Rump",
                                  age.var = "Categorical")

#_____________________________________________________________________________________________________________
# 2b. Mass ----
#_____________________________________________________________________________________________________________

# newdata
cat.mass.newdata <- data.frame(BCS.rump.S = 0,
                               Final.mass.S = seq(min(preg.body$Final.mass.S), 
                                                  max(preg.body$Final.mass.S),
                                                  length.out = 100))

# predict
cat.mass.predict <- predict(cat.age.glm, 
                            newdata = cat.mass.newdata,
                            se.fit = TRUE,
                            type = "response")

# as a df 
cat.mass.predict.df <- data.frame(x = (cat.mass.newdata$Final.mass.S * sd(preg.body$Final.mass)) + mean(preg.body$Final.mass),
                                  fit = cat.mass.predict$fit,
                                  low = cat.mass.predict$fit - cat.mass.predict$se.fit*1.96,
                                  upp = cat.mass.predict$fit + cat.mass.predict$se.fit*1.96,
                                  var = "Mass",
                                  age.var = "Categorical")

#_____________________________________________________________________________________________________________
# 3. Numeric age model ----
#_____________________________________________________________________________________________________________
# 3a. Rump ----
#_____________________________________________________________________________________________________________

# newdata
num.rump.newdata <- data.frame(BCS.rump.S = seq(min(preg.num.age$BCS.rump.S), 
                                                max(preg.num.age$BCS.rump.S),
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
                               Final.mass.S = seq(min(preg.num.age$Final.mass.S), 
                                                  max(preg.num.age$Final.mass.S),
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
                          Age.lab.S = seq(min(preg.num.age$Age.lab.S), 
                                          max(preg.num.age$Age.lab.S),
                                          length.out = 100)) %>% 
                   mutate(qAge.lab.S = (((Age.lab.S*sd(preg.num.age$Age.lab) 
                                          + mean(preg.num.age$Age.lab))^2) 
                                          - mean(preg.num.age$qAge.lab)) 
                                          / sd(preg.num.age$qAge.lab))

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

all.predict.df <- rbind(cat.rump.predict.df,
                        cat.mass.predict.df,
                        num.rump.predict.df,
                        num.mass.predict.df,
                        num.age.predict.df)

#_____________________________________________________________________________________________________________
# 5. Facetted plot ----
#_____________________________________________________________________________________________________________

ggplot(data = all.predict.df,
       aes(x = x,
           y = fit,
           color = age.var,
           fill = age.var)) +
  theme_bw() +
  facet_grid(age.var ~ var,
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
  scale_color_manual(values = c("black", "#FF3300")) +
  scale_fill_manual(values = c("black", "#FF3300")) +
  coord_cartesian(ylim = c(0, 1))

# 562 x 360
