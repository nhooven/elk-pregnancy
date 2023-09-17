# Title: Drivers of mid-winter pregnancy and fetal/neonatal survival
# Subtitle: 05 - Parameter estimate visualization
# Author: Nathan D. Hooven
# Email: nathan.hooven@wsu.edu
# Affiliation: School of the Environment, Washington State University
# Date began: 10 Dec 2022
# Date completed: 10 Dec 2022
# Date modified: 17 Sep 2023
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

params <- params %>% mutate(low.95 = estimate - se*1.96,
                            upp.95 = estimate + se*1.96,
                            low.50 = estimate - se*0.67,
                            upp.50 = estimate + se*0.67) %>%
                     mutate(term = factor(term, levels = rev(c("Intercept",
                                                               "Age",
                                                               "Age2",
                                                               "Condition",
                                                               "Mass"))))

#_____________________________________________________________________________________________________________
# 4. Visualize ----
#_____________________________________________________________________________________________________________
# 4a. Pregnancy ----
#_____________________________________________________________________________________________________________

ggplot(data = params %>% filter(response == "preg"),
       aes(x = estimate,
           y = term,
           color = term,
           fill = term,
           group = as.factor(model),
           shape = as.factor(model))) +
        
       # light theme
       theme_bw() +
        
       # 95% confidence limits
       geom_errorbarh(aes(xmin = low.95,
                          xmax = upp.95),
                      position = position_dodge(width = 1),
                      height = 0,
                      linewidth = 2.5,
                      alpha = 0.5) +
        
       # 50% confidence limits
       geom_errorbarh(aes(xmin = low.50,
                          xmax = upp.50),
                      position = position_dodge(width = 1),
                      height = 0,
                      linewidth = 3.5) +
        
       # point estimates
       geom_point(aes(size = as.factor(model)),
                  position = position_dodge(width = 1),
                  color = "black",
                  stroke = 1.25) +
        scale_y_discrete(labels = c("Mass",
                                    "Condition",
                                    expression(Age^2), 
                                    "Age",
                                    "Intercept")) +
        
       # manual shapes and sizes
       scale_shape_manual(values = c(21, 23, 22)) +
       scale_size_manual(values = c(2.5, 2, 2)) +
        
       # theme modifications
       theme(legend.position = "none",
             panel.grid = element_blank(),
             strip.background = element_blank(),
             strip.text.x = element_blank(),
             axis.text.y = element_text(size = 10)) +
        
       # axis labels
       xlab("Standardized coefficient") +
       ylab("") +
       scale_x_continuous(breaks = c(-4, -3, -2, -1, 0, 1, 2, 3, 4)) +
        
       # colors
       scale_color_brewer(palette = "Paired") +
       scale_fill_brewer(palette = "Paired") +
        
       # add dashed lines between categories
       geom_hline(yintercept = 1.5, linetype = "dashed", alpha = 0.5) +
       geom_hline(yintercept = 2.5, linetype = "dashed", alpha = 0.5) +
       geom_hline(yintercept = 4.5, linetype = "dashed", alpha = 0.5) +
       
       # intercept line
       geom_vline(xintercept = 0)

# 585 x 346