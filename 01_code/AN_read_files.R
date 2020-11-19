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

sec8b <- read_dta("02_raw_data/sec8b.dta")

sec8c1 <- read_dta("02_raw_data/sec8c1.dta")

sec8c2 <- read_dta("02_raw_data/sec8c2.dta")

sec8hid <- read_dta("02_raw_data/sec8hid.dta")

# size of plots - main agg file to use
# use dummy as '1' for owned / rendered
# what is size and how is it measured

# start selecting variables and rename
sec8a1 <- read_dta("02_raw_data/sec8a1.dta") %>% 
  select(nh, s8aq1, s8aq3, s8aq4, s8aq13, s8aq14, s8aq15, s8aq16, s8aq17, s8aq18, clust) %>% 
  rename(current_land_owner = s8aq1, unit_plot_areas = s8aq3, land_own_by_HH = s8aq4, land_rented_out = s8aq13, qty_land_rented = s8aq14, amnt_from_rented_land = s8aq15,
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
agg1 <- read_dta('02_raw_data/agg1.dta')
agg2 <- read_dta('02_raw_data/agg2.dta')
agg4 <- read_dta('02_raw_data/agg4.dta')

# Expenditure Aggregates
agg7 <- read_dta('02_raw_data/agg7.dta')
agg8 <- read_dta('02_raw_data/agg8.dta')
agg9 <- read_dta('02_raw_data/agg9.dta')
agg10 <- read_dta('02_raw_data/agg10.dta')
agg11 <- read_dta('02_raw_data/agg11.dta')
agg12 <- read_dta('02_raw_data/agg12.dta')

# Household Roster
sec1 <- read_dta('02_raw_data/sec1.dta') 

# General Education
sec2A <- read_dta('02_raw_data/sec2A.dta')
#-----------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------#
# Select only necessary variables                                                   #
#-----------------------------------------------------------------------------------#
# Income variables
agg2 <- read_dta('02_raw_data/agg2.dta') %>% 
  select(clust:nh, agri1c, hhagdepn)

# Expenditure variables
agg7 <- read_dta('02_raw_data/agg7.dta') %>%
  select(clust, nh, expfoodc)

agg9 <- read_dta('02_raw_data/agg9.dta') %>%
  select(clust, nh, othexpc)

agg10 <- read_dta('02_raw_data/agg10.dta') %>%
  select(clust, nh, impfoodc)

# Survey Information
sec0A <- read_dta('02_raw_data/sec0a.dta') %>% 
  select(region, district, eanum, nh, clust, ez, loc2, loc5, loc3)

# Household Roster
sec1 <- read_dta('02_raw_data/sec1.dta') %>% 
  select(nh, agey, clust)

# General Education
sec2A <- read_dta('02_raw_data/sec2A.dta') %>% 
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

#-----------------------------------------------------------------------------------------------------------------------------
# brian's code:

#read seg5 file
sec5 <- read_dta("02_raw_data/sec5.dta") #this ds contain migration data for each person in the household. Not sure if this is useful.

#------------------------------------------------------------------------------------------------------------------------------

#read seg9 file
sec9a2 <- read_dta("02_raw_data/sec9a2.dta") #amount spent on non-food items from previous 7 visits, not important
sec9a11 <- read_dta("02_raw_data/sec9a11.dta") #Interviewer ID, not important
sec9a12 <- read_dta("02_raw_data/sec9a12.dta") #expenditure details on non-food items.
sec9b <- read_dta("02_raw_data/sec9b.dta") #amount spent on food items from previous 7 visits, not important

#------------------------------------------------------------------------------------------------------------------------------

#read community file
cs0 <- read_dta("02_raw_data/cs0.dta")
cs1 <- read_dta("02_raw_data/cs1.dta")
cs2 <- read_dta("02_raw_data/cs2.dta")
cs3 <- read_dta("02_raw_data/cs3.dta")
cs4a <- read_dta("02_raw_data/cs4a.dta")
cs4b <- read_dta("02_raw_data/cs4b.dta")
cs4c <- read_dta("02_raw_data/cs4c.dta")
cs5a <- read_dta("02_raw_data/cs5a.dta")
cs5b <- read_dta("02_raw_data/cs5b.dta")

#limiting the community questionnaire to the relevant variables
#cs2 - 
cs2_limit <- select(cs2, region:eanum, s2q4, s2q8, s2q19, s2q20, s2q23)
cs3_limit <- select(cs3, region:eanum, s3q1, s3q11)
#cs4a - Is there a (health provider) in this community
cs4a_limit <- select(cs4a, region:eanum, s4aq1)
#cs4b - Is there a (hospital, parmancy...etc) in this community?
cs4b_limit <- select(cs4b, region:eanum, s4bq5)
#cs4c has no important variables
cs5a_limit <- select(cs5a, region:eanum, s5aq1, s5aq2, s5aq4)
cs5b_limit <- select(cs5b, region:eanum, s5bq5, s5bq6, s5bq22a:s5bq22l, s5bq24:s5bq25b)

#clean cs2 data to just identify if the community has public transport or not
cs2_public_transport <-
  cs2_limit %>%
  select(region:eanum, s2q23) %>%
  pivot_longer(
    cols = s2q23,
    names_to = "column",
    values_to = "n",
    values_drop_na = TRUE
  ) %>%
  unique() %>%
  pivot_wider(
    names_from = n,
    values_from = column
  ) %>%
  rename(
    n = "2",
    y = "1"
  ) %>%
  mutate(public_transport = ifelse(is.na(y), "2", "1")) %>%
  select(1:3, 6)

#clean cs2 data to just identify if the community has periodic market or not
cs2_periodic_market <-
  cs2_limit %>%
  select(region:eanum, s2q20) %>%
  pivot_longer(
    cols = s2q20,
    names_to = "column",
    values_to = "n",
    values_drop_na = TRUE
  ) %>%
  unique() %>%
  pivot_wider(
    names_from = n,
    values_from = column
  ) %>%
  rename(
    n = "2",
    y = "1"
  ) %>%
  mutate(periodic_market = ifelse(is.na(y), "2", "1")) %>%
  select(1:3, 6)

#clean cs2 data to just identify if the community has permanent market or not
cs2_permanent_market <-
  cs2_limit %>%
  select(region:eanum, s2q19) %>%
  pivot_longer(
    cols = s2q19,
    names_to = "column",
    values_to = "n",
    values_drop_na = TRUE
  ) %>%
  unique() %>%
  pivot_wider(
    names_from = n,
    values_from = column
  ) %>%
  rename(
    n = "2",
    y = "1"
  ) %>%
  mutate(permanent_market = ifelse(is.na(y), "2", "1")) %>%
  select(1:3, 6)

#clean cs2 data to just identify if the community has road or not
cs2_road <-
  cs2_limit %>%
  select(region:eanum, s2q4) %>%
  pivot_longer(
    cols = s2q4,
    names_to = "column",
    values_to = "n",
    values_drop_na = TRUE
  ) %>%
  unique() %>%
  pivot_wider(
    names_from = n,
    values_from = column
  ) %>%
  rename(
    n = "2",
    y = "1"
  ) %>%
  mutate(road = ifelse(is.na(y), "2", "1")) %>%
  select(1:3, 6)

#clean cs3 data to just identify if the community has primary school and junior secondary school
cs3_school <-
  cs3_limit %>%
  pivot_longer(
    cols = s3q1,
    names_to = "column",
    values_to = "n",
    values_drop_na = TRUE
  ) %>%
  unique() %>%
  pivot_wider(
    names_from = n,
    values_from = column
  ) %>%
  rename(
    n_ps = "2",
    y_ps = "1"
  ) %>%
  mutate(primary_school = ifelse(is.na(y_ps), "2", "1")) %>%
  pivot_longer(
    cols = s3q11,
    names_to = "column",
    values_to = "n",
    values_drop_na = TRUE
  ) %>%
  unique() %>%
  pivot_wider(
    names_from = n,
    values_from = column
  ) %>%
  rename(
    n = "2",
    y = "1"
  ) %>%
  mutate(junior_secondary_school = ifelse(is.na(y), "2", "1")) %>%
  select(1:3, 6, 9)


#clean cs4a data to just identify if the community has health provider or not
cs4a_health_provider <-
  cs4a_limit %>%
  pivot_longer(
    cols = s4aq1,
    names_to = "column",
    values_to = "n",
    values_drop_na = TRUE
  ) %>%
  unique() %>%
  pivot_wider(
    names_from = n,
    values_from = column
  ) %>%
  rename(
    n = "2",
    y = "1"
  ) %>%
  mutate(health_provider = ifelse(is.na(y), "2", "1")) %>%
  select(1:3, 6)


#clean cs4b data to just identify if the community has hospital or not
cs4b_hospital <-
  cs4b_limit %>%
  pivot_longer(
    cols = s4bq5,
    names_to = "column",
    values_to = "n",
    values_drop_na = TRUE
  ) %>%
  unique() %>%
  pivot_wider(
    names_from = n,
    values_from = column
  ) %>%
  rename(
    n = "2",
    y = "1"
  ) %>%
  mutate(hospital = ifelse(is.na(y), "2", "1")) %>%
  select(1:3, 6)

#combine cs2, cs4, cs4a, cs4b, and cs5b together
cs_combine1 <- 
  left_join(cs2_road, cs2_public_transport, by = c("region"="region", "district"="district", "eanum"="eanum"))
cs_combine2 <- 
  left_join(cs_combine1, cs2_permanent_market, by = c("region"="region", "district"="district", "eanum"="eanum"))
cs_combine3 <- 
  left_join(cs_combine2, cs2_periodic_market, by = c("region"="region", "district"="district", "eanum"="eanum"))
cs_combine4 <- 
  left_join(cs_combine3, cs3_school, by = c("region"="region", "district"="district", "eanum"="eanum"))
cs_combine5 <- 
  left_join(cs_combine4, cs4a_health_provider, by = c("region"="region", "district"="district", "eanum"="eanum"))
cs_combine <-
  left_join(cs_combine5, cs4b_hospital, by = c("region"="region", "district"="district", "eanum"="eanum"))

#mutate clust for community combine file
cs_final <-
  cs_combine %>%
  mutate(clust = eanum+4000) %>%
  mutate(cluster_n = paste(clust, eanum, sep = "_"))

# combine Andrew and Arunima data
wrangled_data_final <- left_join(com2, combined4, by = c("nh", "clust"))

# combine all final datasets
wrangled_data_final_2 <- left_join(wrangled_data_final, cs_final, by = c("clust"))

#------------------------------------------------------------------------------------------------------------------------------
#edition from Brian to clean data and adding couple variables
#wrangled_data_final_c2 is the updated final dataframe

#clean com data by sum tot_val_harvest using group by
com_clean <-
  com2 %>%
  group_by(nh, current_land_owner, unit_plot_areas, land_own_by_HH, land_rented_out, qty_land_rented, amnt_from_rented_land, tot_land_s_crop, land_from_s_crop, amnt_from_s_crop, clust, household_id) %>%
  summarise(tot_val_harvest = sum(tot_val_harvest))

#load sec1 and limited to sex, rel, and agey
sec1_v1 <- read_dta('02_raw_data/sec1.dta') %>% 
  select(nh, sex, rel, agey, clust)

#add another dataframe to understand sex of HH
sec1_HH_sex <-
  sec1_v1 %>%
  filter(rel==1) %>%
  select(nh, clust, sex)

#clean sec2A data by calculated the educ as avg and find the top educ.
sec2A_clean <-
  sec2A %>%
  na.omit() %>%
  group_by(nh, clust) %>%
  summarise(avg_educ = round(mean(highest_educ),1), highest_educ = max(highest_educ) )

#new combined4 dataframe using previous edit data
combined4_clean <- 
  left_join(combined3, sec2A_clean, by = c("nh" = "nh","clust" = "clust")) %>%
  left_join(., sec1_HH_sex, by = c("nh" = "nh","clust" = "clust")) %>%
  rename(HH_sex = sex)

# combine Andrew and Arunima data with Brian's edit
wrangled_data_final_c1 <- left_join(com_clean, combined4_clean, by = c("nh", "clust"))

# combine all final datasets with Brian's edit
wrangled_data_final_c2 <- left_join(wrangled_data_final_c1, cs_final, by = c("clust"))

#adding a new variable for profit per unit
wrangled_data_final_c2 <-
  wrangled_data_final_c2 %>%
  mutate(profit_per_unit = agri1c/land_own_by_HH)

colnames(wrangled_data_final_c2)

#trying to see the correlation between profit and other variables
summary(lm(agri1c ~ unit_plot_areas + land_own_by_HH + tot_val_harvest + profit_per_unit +totemp
           + av_yrs_age + avg_educ + highest_educ + road + public_transport + permanent_market 
           + primary_school+ health_provider + hospital + HH_sex, data = wrangled_data_final_c2))


#clean cs4b data to just identify if the community has hospital or not
cs4b_hospital <-
  cs4b_limit %>%
  pivot_longer(
    cols = s4bq5,
    names_to = "column",
    values_to = "n",
    values_drop_na = TRUE
  ) %>%
  unique() %>%
  pivot_wider(
    names_from = n,
    values_from = column
  ) %>%
  rename(
    n = "2",
    y = "1"
  ) %>%
  mutate(hospital = ifelse(is.na(y), "2", "1")) %>%
  select(1:3, 6)

#combine cs2, cs4, cs4a, cs4b, and cs5b together
cs_combine1 <- 
  left_join(cs2_road, cs2_public_transport, by = c("region"="region", "district"="district", "eanum"="eanum"))
cs_combine2 <- 
  left_join(cs_combine1, cs2_permanent_market, by = c("region"="region", "district"="district", "eanum"="eanum"))
cs_combine3 <- 
  left_join(cs_combine2, cs2_periodic_market, by = c("region"="region", "district"="district", "eanum"="eanum"))
cs_combine4 <- 
  left_join(cs_combine3, cs3_school, by = c("region"="region", "district"="district", "eanum"="eanum"))
cs_combine5 <- 
  left_join(cs_combine4, cs4a_health_provider, by = c("region"="region", "district"="district", "eanum"="eanum"))
cs_combine <-
  left_join(cs_combine5, cs4b_hospital, by = c("region"="region", "district"="district", "eanum"="eanum"))

#mutate clust for community combine file
cs_final <-
  cs_combine %>%
  mutate(clust = eanum+4000) %>%
  mutate(cluster_n = paste(clust, eanum, sep = "_"))

# combine Andrew and Arunima data
wrangled_data_final <- left_join(com2, combined4, by = c("nh", "clust"))


# combine all final datasets
wrangled_data_final_2 <- left_join(wrangled_data_final, cs_final, by = c("clust"))
