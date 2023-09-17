# Title: Drivers of mid-winter pregnancy and fetal/neonatal survival
# Subtitle: 04a - Pregnancy prediction plots
# Author: Nathan D. Hooven
# Email: nathan.hooven@wsu.edu
# Affiliation: School of the Environment, Washington State University
# Date began: 11 Dec 2022
# Date completed: 11 Dec 2022
# Date modified: 17 Sep 2023
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages and dataset ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(cowplot)    # multiple plots

load("preg_full_model.RData")

#_____________________________________________________________________________________________________________
# 2. Age prediction curve ----

# newdata
age.newdata <- tibble(Year = NA,
                      Site = NA,
                      total.cond.S = 0,
                      Final.mass.S = 0,
                      Age.lab.S = seq(min(preg.num.age.1$Age.lab.S), 
                                      max(preg.num.age.1$Age.lab.S),
                                      length.out = 100)) %>% 
  mutate(qAge.lab.S = (((Age.lab.S*sd(preg.num.age.1$Age.lab) 
                         + mean(preg.num.age.1$Age.lab))^2) 
                       - mean(preg.num.age.1$qAge.lab)) 
         / sd(preg.num.age.1$qAge.lab))

#_____________________________________________________________________________________________________________
# 2a. Predictions ----
#_____________________________________________________________________________________________________________

# predict
age.predict.1 <- predict(preg.model.1, 
                         newdata = age.newdata,
                         se.fit = TRUE,
                         type = "response",
                         allow.new.levels = TRUE)

age.predict.2 <- predict(preg.model.2, 
                         newdata = age.newdata,
                         se.fit = TRUE,
                         type = "response",
                         allow.new.levels = TRUE)

age.predict.3 <- predict(preg.model.3, 
                         newdata = age.newdata,
                         se.fit = TRUE,
                         type = "response",
                         allow.new.levels = TRUE)

#_____________________________________________________________________________________________________________
# 2b. Model averaging ----
#_____________________________________________________________________________________________________________

age.modavg <- tibble(x = seq(min(preg.num.age.1$Age.lab), 
                             max(preg.num.age.1$Age.lab),
                             length.out = 100),
                     pred = age.predict.1$fit * 0.41 +
                       age.predict.2$fit * 0.37 +
                       age.predict.3$fit * 0.21,
                     pred.low = (age.predict.1$fit - 1.96*age.predict.1$se.fit) * 0.41 +
                       (age.predict.2$fit - 1.96*age.predict.2$se.fit) * 0.37 +
                       (age.predict.3$fit - 1.96*age.predict.3$se.fit) * 0.21,
                     pred.high = (age.predict.1$fit + 1.96*age.predict.1$se.fit) * 0.41 +
                       (age.predict.2$fit + 1.96*age.predict.2$se.fit) * 0.37 +
                       (age.predict.3$fit + 1.96*age.predict.3$se.fit) * 0.21)

#_____________________________________________________________________________________________________________
# 2d. Prediction plot ----
#_____________________________________________________________________________________________________________

ggplot(data = age.modavg,
       aes(x = x,
           y = pred)) +
  theme_bw() +
  geom_ribbon(aes(x = x,
                  y = pred,
                  ymin = pred.low,
                  ymax = pred.high),
              color = NA,
              alpha = 0.25) +
  geom_line(linewidth = 1.5) +
  theme(panel.grid = element_blank()) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  ylab("Probability of pregnancy") +
  xlab("Age (years)") -> 
  
  # assign to plot
  predict.preg.age

#_____________________________________________________________________________________________________________
# 3. Condition prediction curve ----

# newdata
cond.newdata <- tibble(Year = NA,
                       Site = NA,
                       total.cond.S = seq(min(preg.num.age.1$total.cond.S),
                                          max(preg.num.age.1$total.cond.S),
                                          length.out = 100),
                       Final.mass.S = 0,
                       Age.lab.S = 0,
                       qAge.lab.S = 0)


#_____________________________________________________________________________________________________________
# 3a. Predictions ----
#_____________________________________________________________________________________________________________

# predict
cond.predict.1 <- predict(preg.model.1, 
                          newdata = cond.newdata,
                          se.fit = TRUE,
                          type = "response",
                          allow.new.levels = TRUE)

cond.predict.2 <- predict(preg.model.2, 
                          newdata = cond.newdata,
                          se.fit = TRUE,
                          type = "response",
                          allow.new.levels = TRUE)

cond.predict.3 <- predict(preg.model.3, 
                          newdata = cond.newdata,
                          se.fit = TRUE,
                          type = "response",
                          allow.new.levels = TRUE)

#_____________________________________________________________________________________________________________
# 3b. Model averaging ----
#_____________________________________________________________________________________________________________

cond.modavg <- tibble(x = seq(min(preg.num.age.1$total.cond), 
                              max(preg.num.age.1$total.cond),
                              length.out = 100),
                      pred = cond.predict.1$fit * 0.41 +
                        cond.predict.2$fit * 0.37 +
                        cond.predict.3$fit * 0.21,
                      pred.low = (cond.predict.1$fit - 1.96*cond.predict.1$se.fit) * 0.41 +
                        (cond.predict.2$fit - 1.96*cond.predict.2$se.fit) * 0.37 +
                        (cond.predict.3$fit - 1.96*cond.predict.3$se.fit) * 0.21,
                      pred.high = (cond.predict.1$fit + 1.96*cond.predict.1$se.fit) * 0.41 +
                        (cond.predict.2$fit + 1.96*cond.predict.2$se.fit) * 0.37 +
                        (cond.predict.3$fit + 1.96*cond.predict.3$se.fit) * 0.21)

#_____________________________________________________________________________________________________________
# 3d. Prediction plot ----
#_____________________________________________________________________________________________________________

ggplot(data = cond.modavg,
       aes(x = x,
           y = pred)) +
  theme_bw() +
  geom_ribbon(aes(x = x,
                  y = pred,
                  ymin = pred.low,
                  ymax = pred.high),
              color = NA,
              alpha = 0.25) +
  geom_line(linewidth = 1.5) +
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_continuous(breaks = c(5, 7, 9, 11, 13, 15)) +
  ylab("") +
  xlab("Total body condition score") -> 
  
  # assign to plot
  predict.preg.cond

#_____________________________________________________________________________________________________________
# 4. Mass prediction curve ----

# newdata
mass.newdata <- tibble(Year = NA,
                       Site = NA,
                       total.cond.S = 0,
                       Final.mass.S = seq(min(preg.num.age.1$Final.mass.S),
                                          max(preg.num.age.1$Final.mass.S),
                                          length.out = 100),
                       Age.lab.S = 0,
                       qAge.lab.S = 0)


#_____________________________________________________________________________________________________________
# 4a. Predictions ----
#_____________________________________________________________________________________________________________

# predict
mass.predict.1 <- predict(preg.model.1, 
                          newdata = mass.newdata,
                          se.fit = TRUE,
                          type = "response",
                          allow.new.levels = TRUE)

mass.predict.2 <- predict(preg.model.2, 
                          newdata = mass.newdata,
                          se.fit = TRUE,
                          type = "response",
                          allow.new.levels = TRUE)

mass.predict.3 <- predict(preg.model.3, 
                          newdata = mass.newdata,
                          se.fit = TRUE,
                          type = "response",
                          allow.new.levels = TRUE)

#_____________________________________________________________________________________________________________
# 3b. Model averaging ----
#_____________________________________________________________________________________________________________

mass.modavg <- tibble(x = seq(min(preg.num.age.1$Final.mass), 
                              max(preg.num.age.1$Final.mass),
                              length.out = 100),
                      pred = mass.predict.1$fit * 0.41 +
                        mass.predict.2$fit * 0.37 +
                        mass.predict.3$fit * 0.21,
                      pred.low = (mass.predict.1$fit - 1.96*mass.predict.1$se.fit) * 0.41 +
                        (mass.predict.2$fit - 1.96*mass.predict.2$se.fit) * 0.37 +
                        (mass.predict.3$fit - 1.96*mass.predict.3$se.fit) * 0.21,
                      pred.high = (mass.predict.1$fit + 1.96*mass.predict.1$se.fit) * 0.41 +
                        (mass.predict.2$fit + 1.96*mass.predict.2$se.fit) * 0.37 +
                        (mass.predict.3$fit + 1.96*mass.predict.3$se.fit) * 0.21)

#_____________________________________________________________________________________________________________
# 3d. Prediction plot ----
#_____________________________________________________________________________________________________________

ggplot(data = mass.modavg,
       aes(x = x,
           y = pred)) +
  theme_bw() +
  geom_ribbon(aes(x = x,
                  y = pred,
                  ymin = pred.low,
                  ymax = pred.high),
              color = NA,
              alpha = 0.25) +
  geom_line(linewidth = 1.5) +
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_continuous(breaks = c(175, 200, 225, 250, 275, 300)) +
  ylab("") +
  xlab("Body mass (kg)") -> 
  
  # assign to plot
  predict.preg.mass

#_____________________________________________________________________________________________________________
# 4. Plot all ----
#_____________________________________________________________________________________________________________

plot_grid(predict.preg.age,
          predict.preg.cond,
          predict.preg.mass,
          nrow = 1,
          rel_widths = c(1.08, 1, 1))

# 734 x 248