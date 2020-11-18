# data translation project
# author: Andrew Nalundasan
# team: Arunima Roy, Brian Hsu
# for: OMSBA 5112, Seattle University
# date: 11/20/2020


# load libraries
library(tidyverse)
library(haven)
library(dplyr)

# read all dta files:
# sec8a1
sec8a1 <- read_dta("sec8a1.dta")
View(sec8a1)
summary(sec8a1)

# sec8a2
sec8a2 <- read_dta("sec8a2.dta")
View(sec8a2)
summary(sec8a2)

# sec8a3
sec8a3 <- read_dta("sec8a3.dta")
View(sec8a3)
summary(sec8a3)

# sec8b
sec8b <- read_dta("sec8b.dta")
View(sec8b)
summary(sec8b)

# sec8c1
sec8c1 <- read_dta("sec8c1.dta")
View(sec8c1)
summary(sec8c1)

# sec8c2
sec8c2 <- read_dta("sec8c2.dta")
View(sec8c2)
summary(sec8c2)

# sec8d
sec8d <- read_dta("02_raw_data/sec8d.dta")
View(sec8d)
summary(sec8d)

# sec8e
sec8e <- read_dta("02_raw_data/sec8e.dta")
View(sec8e)
summary(sec8e)

# sec8f
sec8f <- read_dta("02_raw_data/sec8f.dta")
View(sec8f)
summary(sec8f)

# sec8g
sec8g <- read_dta("02_raw_data/sec8g.dta")
View(sec8g)
summary(sec8g)

# sec8h
sec8h <- read_dta("02_raw_data/sec8h.dta")
View(sec8h)
summary(sec8h)

# sec8hid
sec8hid <- read_dta("02_raw_data/sec8hid.dta")
View(sec8hid)
summary(sec8hid)

# start joining files
com1 <- full_join(sec8a1, sec8a2, by = c("nh" = "nh", "clust" = "clust"))
com3 <- full_join(sec8c1, sec8c2, by = c("nh" = "nh", "clust" = "clust"))

com2 <- full_join(sec8a3, sec8b, by = c("nh" = "nh", "clust" = "clust"))

com4 <- full_join(com1, com2, by = c("nh" = "nh", "clust" = "clust"))

sec4b <- read_dta("sec4b.dta")
com2 <- full_join(sec8a3, sec8b, by = c("nh" = "nh", "clust" = "clust"))
com4 <- full_join(com1, com2, by = c("nh" = "nh", "clust" = "clust"))


# Arunima's Data Wrangling code

#-----------------------------------------------------------------------------------#
# Read in data files                                                                #
#-----------------------------------------------------------------------------------#
# Income Aggregates
agg1 <- read_dta('02_raw_data/glss4_new/agg1.dta')
agg2 <- read_dta('02_raw_data/glss4_new/agg2.dta')
agg4 <- read_dta('02_raw_data/glss4_new/agg4.dta')

# Expenditure Aggregates
agg7 <- read_dta('02_raw_data/glss4_new/agg7.dta')
agg8 <- read_dta('02_raw_data/glss4_new/agg8.dta')
agg9 <- read_dta('02_raw_data/glss4_new/agg9.dta')
agg10 <- read_dta('02_raw_data/glss4_new/agg10.dta')
agg11 <- read_dta('02_raw_data/glss4_new/agg11.dta')
agg12 <- read_dta('02_raw_data/glss4_new/agg12.dta')

# Household Roster
sec1 <- read_dta('02_raw_data/glss4_new/sec1.dta') 

# General Education
sec2A <- read_dta('02_raw_data/glss4_new/sec2A.dta')
#-----------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------#
# Select only necessary variables                                                   #
#-----------------------------------------------------------------------------------#
# Income variables
agg2 <- read_dta('02_raw_data/glss4_new/agg2.dta') %>% 
  select(clust:nh, agri1c, hhagdepn)

# Expenditure variables
agg7 <- read_dta('02_raw_data/glss4_new/agg7.dta') %>%
  select(clust, nh, expfoodc)

agg9 <- read_dta('02_raw_data/glss4_new/agg9.dta') %>%
  select(clust, nh, othexpc)

agg10 <- read_dta('02_raw_data/glss4_new/agg10.dta') %>%
  select(clust, nh, impfoodc)

# Survey Information
sec0A <- read_dta('02_raw_data/glss4_new/sec0a.dta') %>% 
  select(region, district, eanum, nh, clust, ez, loc2, loc5, loc3)

# Household Roster
sec1 <- read_dta('02_raw_data/glss4_new/sec1.dta') %>% 
  select(nh, agey, clust)

# General Education
sec2A <- read_dta('02_raw_data/glss4_new/sec2A.dta') %>% 
  select(nh, clust, s2aq2) %>% 
  rename(highest_educ = s2aq2)

#-----------------------------------------------------------------------------------#
# Clean data                                                                        #
#-----------------------------------------------------------------------------------#
# Aggregate avg age of years in household 
nh_age <- select(sec1, "nh", "agey", "clust") %>%
  na.omit() %>%
  group_by(nh) %>%
  summarise(av_yrs_age = round(mean(agey), 1))


#-----------------------------------------------------------------------------------#
# Combine data                                                                      #
#-----------------------------------------------------------------------------------#
# Combine agg1, agg2, agg4
inc_c1 <- left_join(agg1, agg2, agg4, by = c("nh" = "nh", "clust" = "clust")) 
# Combine expenditure
exp_c1 <- full_join(agg7, agg8, agg9, by = c("nh" = "nh", "clust" = "clust"))
exp_c2 <- full_join(agg10, agg11, agg12, by = c("nh" = "nh", "clust" = "clust"))
exp_full <- full_join(exp_c1, exp_c2, by = c("nh" = "nh", "clust" = "clust"))
# Combine Income and Expenditures
combined1 <- full_join(inc_c1, exp_full, by = c("nh" = "nh", "clust" = "clust"))
# Combine survey0A
combined2 <- left_join(combined1, sec0A, by = c("nh" = "nh", "clust" = "clust"))
# Combine age and sex
combined3 <- left_join(combined2, nh_age, by = c("nh" = "nh"))
# Combine education
combined4 <- left_join(combined3, sec2A, by = c("nh" = "nh","clust" = "clust"))









