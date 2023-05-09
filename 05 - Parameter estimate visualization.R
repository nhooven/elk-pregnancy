# Title: Drivers of mid-winter pregnancy and fetal/neonatal survival
# Subtitle: 05 - Parameter estimate visualization
# Author: Nathan D. Hooven
# Email: nathan.hooven@wsu.edu
# Affiliation: School of the Environment, Washington State University
# Date began: 10 Dec 2022
# Date completed: 10 Dec 2022
# Date modified: 09 May 2023
# R version: 3.6.2 / 4.2.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

params <- read.csv("parameter_est.csv")

#_____________________________________________________________________________________________________________
# 3. Calculate CIs and order factors----
#_____________________________________________________________________________________________________________

params <- params %>% dplyr::select(model, term, estimate, std.error) %>%
                     mutate(age.var = ifelse(model %in% c("preg.cat.age", "success.cat.age"),
                                                "Categorical",
                                                "Numeric"),
                            vital.rate = ifelse(model %in% c("preg.cat.age", "preg.num.age"),
                                                "Pregnancy",
                                                "Calf success"),
                            low.95 = estimate - std.error*1.96,
                            upp.95 = estimate + std.error*1.96,
                            low.50 = estimate - std.error*0.67,
                            upp.50 = estimate + std.error*0.67) %>%
                     mutate(term = factor(term, levels = rev(c("(Intercept)",
                                                               "BCS.rump.S",
                                                               "Final.mass.S",
                                                               "Age.lab.S",
                                                               "qAge.lab.S"))),
                            age.var = factor(age.var, levels = rev(c("Categorical",
                                                                     "Numeric"))),
                            vital.rate = factor(vital.rate, levels = c("Pregnancy",
                                                                       "Calf success")))

#_____________________________________________________________________________________________________________
# 4. Visualize ----
#_____________________________________________________________________________________________________________

ggplot(data = params,
       aes(x = estimate,
           y = term,
           color = term,
           fill = term,
           group = age.var,
           shape = age.var)) +
       theme_bw() +
       facet_wrap(~ vital.rate,
                  nrow = 2) + 
       geom_vline(xintercept = 0) +
       geom_errorbarh(aes(xmin = low.95,
                          xmax = upp.95),
                      position = position_dodge(width = 1),
                      height = 0,
                      linewidth = 2.5,
                      alpha = 0.5) +
       geom_errorbarh(aes(xmin = low.50,
                          xmax = upp.50),
                      position = position_dodge(width = 1),
                      height = 0,
                      linewidth = 3.5) +
       geom_point(aes(size = age.var),
                  position = position_dodge(width = 1),
                  color = "black",
                  stroke = 1.25) +
       scale_shape_manual(values = c(23, 21)) +
       scale_size_manual(values = c(2.5, 3)) +
       theme(legend.position = "none",
             panel.grid = element_blank(),
             strip.background = element_blank(),
             strip.text.x = element_blank(),
             axis.text.y = element_text(size = 10)) +
       xlab("Standardized coefficient") +
       ylab("") +
       scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
       scale_y_discrete(labels = c(expression(Age^2), 
                                   "Age", 
                                   "Mass", 
                                   "Rump", 
                                   "Intercept")) +
       scale_color_brewer(palette = "Paired") +
       scale_fill_brewer(palette = "Paired")

       