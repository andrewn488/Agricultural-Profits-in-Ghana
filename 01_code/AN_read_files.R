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
# extract all column names from each dta file to find keys

sec8a1 <- read_dta("02_raw_data/sec8a1.dta")

sec8c1 <- read_dta("02_raw_data/sec8c1.dta")

sec8c2 <- read_dta("02_raw_data/sec8c2.dta")

sec8hid <- read_dta("02_raw_data/sec8hid.dta")

# size of plots - main agg file to use
# use dummy as '1' for owned / rendered
# what is size and how is it measured

# start selecting variables and rename
sec8a1 <- read_dta("02_raw_data/sec8a1.dta") %>% 
  select(nh, s8aq1, s8aq3, s8aq13, s8aq14, s8aq15, s8aq16, s8aq17, s8aq18, clust) %>% 
  rename(current_land_owner = s8aq1, unit_plot_areas = s8aq3, land_rented_out = s8aq13, qty_land_rented = s8aq14, amnt_from_rented_land = s8aq15,
         tot_land_s_crop = s8aq16, land_from_s_crop = s8aq17, amnt_from_s_crop = s8aq18)

sec8c1 <- read_dta("02_raw_data/sec8c1.dta") %>% 
  select(nh, s8cq13, clust) %>% 
  rename(tot_val_harvest = s8cq13)

sec8hid <- read_dta("02_raw_data/sec8hid.dta") %>% 
  select(nh, clust, hhid) %>% 
  rename(household_id = hhid)

# joins

com1 <- left_join(sec8a1, sec8c1, by = c("nh" = "nh", "clust" = "clust"))
com2 <- left_join(com1, sec8hid, by = c("nh" = "nh", "clust" = "clust"))


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

