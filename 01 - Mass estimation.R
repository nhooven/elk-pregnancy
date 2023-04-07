# Title: Drivers of mid-winter pregnancy and fetal/neonatal survival
# Subtitle: 01 - Mass estimation
# Author: Nathan D. Hooven
# Email: nathan.hooven@wsu.edu
# Affiliation: School of the Environment, Washington State University
# Date began: 16 Mar 2022
# Date completed: 16 Mar 2022
# Date modified: 18 Jul 2022
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)
library(mefa4)            # %notin%
library(AICcmodavg)       # model selection

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

preg <- read.csv("Pregnancy.csv")

#_____________________________________________________________________________________________________________
# 3. Convert lateral girth measurement to sternal ----
#_____________________________________________________________________________________________________________

# if there is no value for "Recumbency", replace chest girth measurement to NA
preg$Chest.girth <- ifelse(preg$Recumbency %notin% c("Sternal", "Lateral"),
                           NA,
                           preg$Chest.girth)

# y = 0.8807x + 15.386 (from Cook et al. 2003)
# y is the sternal girth measurement (cm) and x is the lateral girth measurement

preg$Chest.girth.stern <- ifelse(preg$Recumbency == "Sternal",
                                 preg$Chest.girth,
                                 0.8807*preg$Chest.girth + 15.386)

#_____________________________________________________________________________________________________________
# 4. Estimate mass (in kg) using sternal chest girth ----
#_____________________________________________________________________________________________________________

# BM = -193.4 + 2.777x (girth circumference [cm]) (from Cook et al. 2010)

preg$Est.mass <- -193.4 + 2.777*preg$Chest.girth.stern

#_____________________________________________________________________________________________________________
# 5. Assess estimated vs. measured masses (and apply correction factor) ----
#_____________________________________________________________________________________________________________

# convert mass to weight
preg$Heli.mass <- as.numeric(as.character(preg$Heli.weight))*0.453592

est.mass.model <- lm(Heli.mass ~ Est.mass, data = preg)

summary(est.mass.model)

ggplot(data = preg, aes(x = Est.mass, y = Heli.mass)) +
       theme_bw() +
       geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
       geom_point(shape = 21,
                  size = 2) +
       geom_smooth(method = "lm",
                   color = "black",
                   size = 1.25) +
       coord_cartesian(xlim = c(245, 340)) +
       scale_x_continuous(breaks = seq(250, 325, 25)) +
       xlab("Estimated mass (kg)") +
       ylab("Measured mass (kg)") +
       theme(panel.grid = element_blank())

# create corrected estimated mass column
preg$Est.mass.corrected <- predict(est.mass.model, preg)

#_____________________________________________________________________________________________________________
# 6. Create final mass variable and write to csv ----
#_____________________________________________________________________________________________________________

preg$Final.mass <- ifelse(is.na(preg$Heli.mass) == FALSE,
                          preg$Heli.mass,
                          preg$Est.mass.corrected)

write.csv(preg, "Pregnancy_final_mass.csv")
