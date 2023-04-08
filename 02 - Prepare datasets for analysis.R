# Title: Drivers of mid-winter pregnancy and fetal/neonatal survival
# Subtitle: 02 - Prepare datasets for analysis
# Author: Nathan D. Hooven
# Email: nathan.hooven@wsu.edu
# Affiliation: School of the Environment, Washington State University
# Date began: 19 Apr 2022
# Date completed: 28 Apr 2022
# Date modified: 18 Jul 2022
# R version: 3.6.2

#_____________________________________________________________________________________________________________
# 1. Load in required packages ----
#_____________________________________________________________________________________________________________

library(tidyverse)

#_____________________________________________________________________________________________________________
# 2. Read in data ----
#_____________________________________________________________________________________________________________

all.preg <- read.csv("Pregnancy_final_mass.csv")

#_____________________________________________________________________________________________________________
# 3. Pregnancy data ----
#_____________________________________________________________________________________________________________

# select columns we need for analysis
preg.only <- all.preg %>% dplyr::select(E.number, Year, Site, Age.class, Age.lab ,
                                        BCS.ribs, BCS.withers, BCS.rump, RF, Final.mass,
                                        Preg.lab)

# filter rows with lab-confirmed pregnancy and change to binary
preg.conf <- preg.only %>% filter(Preg.lab %in% c("Y", "N")) %>%
                           mutate(Preg.lab = ifelse(Preg.lab == "Y",
                                                    1,
                                                    0))

# we will use this dataset to calculate percent pregnant

# filter rows with complete BCS and mass data
preg.body <- preg.conf %>% filter(is.na(BCS.rump) == FALSE &
                                  is.na(Final.mass) == FALSE)

# filter rows with numeric ages
preg.num.age <- preg.body %>% filter(is.na(Age.lab) == FALSE)

# write to csvs
# all with confirmed pregnancy
write.csv(preg.conf, "all_confirmed_preg.csv")

# all rows with complete BCS and mass data
write.csv(preg.body, "preg_ageclass.csv")

# all rows with numeric ages
write.csv(preg.num.age, "preg_numeric.csv")

#_____________________________________________________________________________________________________________
# 4. Fetal/neonatal survival data ----
#_____________________________________________________________________________________________________________

# select columns we need for analysis
fns.only <- all.preg %>% dplyr::select(E.number, Year, Site, Age.class, Age.lab ,
                                        BCS.ribs, BCS.withers, BCS.rump, RF, Final.mass,
                                        Calf.success, Conf.pred)

# filter rows with Y/N success and change to binary
fns.1 <- fns.only %>% filter(Calf.success %in% c("Y", "N")) %>%
                      mutate(Calf.success = ifelse(Calf.success == "Y",
                                                   1,
                                                   0))

# filter rows with complete BCS and mass data
fns.body <- fns.1 %>% filter(is.na(BCS.rump) == FALSE &
                             is.na(Final.mass) == FALSE)

# filter rows with numeric ages
fns.num.age <- fns.body %>% filter(is.na(Age.lab) == FALSE)

# write to csvs
# all with confirmed pregnancy
write.csv(fns.1, "all_fns.csv")

# all rows with complete BCS and mass data
write.csv(fns.body, "fns_ageclass.csv")

# all rows with numeric ages
write.csv(fns.num.age, "fns_numeric.csv")

