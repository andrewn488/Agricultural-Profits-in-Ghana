#-------------------------------------------------------------------------------------------------------------------#
# data translation project                                                                                          #
# author: Andrew Nalundasan                                                                                         #
# team: Arunima Roy, Brian Hsu                                                                                      #
# for: OMSBA 5112, Seattle University                                                                               #
# date: 11/20/2020                                                                                                  #
#-------------------------------------------------------------------------------------------------------------------#

# load libraries
library(tidyverse)
library(haven)
library(dplyr)

#-------------------------------------------------------------------------------------------------------------------#
# Read in data files                                                                                                #
#-------------------------------------------------------------------------------------------------------------------#

#section 0 - Survey Information
sec0a <- read_dta('02_raw_data/sec0a.dta') 

#section 1 - Household Roster
sec1 <- read_dta('02_raw_data/sec1.dta') 

#section 2 - General Education
sec2a <- read_dta('02_raw_data/sec2a.dta')

#section 8 - Agriculture survey
sec8a1 <- read_dta("02_raw_data/sec8a1.dta")
sec8c1 <- read_dta("02_raw_data/sec8c1.dta")

# Income Aggregates
agg1 <- read_dta('02_raw_data/agg1.dta')
agg2 <- read_dta('02_raw_data/agg2.dta')
agg4 <- read_dta('02_raw_data/agg4.dta')

# Expenditure Aggregates
agg7 <- read_dta('02_raw_data/agg7.dta')
agg9 <- read_dta('02_raw_data/agg9.dta')
agg10 <- read_dta('02_raw_data/agg10.dta')
agg12 <- read_dta('02_raw_data/agg12.dta')

# Community Survey

cs2 <- read_dta("02_raw_data/cs2.dta")
cs3 <- read_dta("02_raw_data/cs3.dta")
cs4b <- read_dta("02_raw_data/cs4b.dta")

#-------------------------------------------------------------------------------------------------------------------#
# Select only necessary variables                                                                                   #
#-------------------------------------------------------------------------------------------------------------------#

# Survey Information
sec0a <- read_dta('02_raw_data/sec0a.dta') %>% 
  select(region, district, eanum, nh, clust, ez, loc2)

# Household Roster
sec1 <- read_dta('02_raw_data/sec1.dta') %>% 
  select(nh, sex, rel, agey, clust)

# General Education
sec2a <- read_dta('02_raw_data/sec2a.dta') %>% 
  select(nh, clust, s2aq2) %>% 
  rename(highest_educ = s2aq2)

# start selecting variables and rename
sec8a1 <- read_dta("02_raw_data/sec8a1.dta") %>% 
  select(nh, s8aq1, s8aq3, s8aq4, clust) %>% 
  rename(current_land_owner = s8aq1, unit_plot_areas = s8aq3, land_own_by_HH = s8aq4)

sec8c1 <- read_dta("02_raw_data/sec8c1.dta") %>% 
  select(nh, s8cq13, clust) %>% 
  rename(tot_val_harvest = s8cq13)

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

# Community variables
cs2 <- 
  select(cs2, region:eanum, s2q4, s2q8, s2q19, s2q20, s2q23)

cs3 <- 
  select(cs3, region:eanum, s3q1, s3q11)

cs4b <- 
  select(cs4b, region:eanum, s4bq5)


#-------------------------------------------------------------------------------------------------------------------#
# Clean Data                                                                                                        #
#-------------------------------------------------------------------------------------------------------------------#

#set the unaccountable education to 01
sec2a$highest_educ[sec2a$highest_educ == 17] <- 01
sec2a$highest_educ[sec2a$highest_educ == 96] <- 01

#get the sex and age of head of household
hh_sex <-
  sec1 %>%
  filter(rel==1) %>%
  select(nh, clust, sex, agey) %>%
  rename(hh_sex = sex, hh_age = agey)

#clean sec8c data by sum up tot_val_harvest to avoid duplication of household id
sec8c1_tot_harvest <-
  sec8c1 %>%
  group_by(nh, clust) %>%
  summarise(tot_val_harvest = sum(tot_val_harvest))

#clean sec2a data by find the top educ.
sec2a_high_educ <-
  sec2a %>%
  group_by(nh, clust) %>%
  summarise(highest_educ = max(highest_educ) )

#clean cs2 data to just identify if the community has permanent market or not
cs2_permanent_market <-
  cs2 %>%
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
  mutate(permanent_market = ifelse(is.na(y), 2, 1)) %>%
  select(1:3, 6)

#clean cs2 data to just identify if the community has road or not
cs2_road <-
  cs2 %>%
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
  mutate(road = ifelse(is.na(y), 2, 1)) %>%
  select(1:3, 6)

#clean cs3 data to just identify if the community has primary school and junior secondary school
cs3_school <-
  cs3 %>%
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
  mutate(primary_school = ifelse(is.na(y_ps), 2, 1)) %>%
  select(1:3, 7)

#clean cs4b data to just identify if the community has hospital or not
cs4b_hospital <-
  cs4b %>%
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
  mutate(hospital = ifelse(is.na(y), 2, 1)) %>%
  select(1:3, 6)

#-------------------------------------------------------------------------------------------------------------------#
# Combine Data                                                                                                      #
#-------------------------------------------------------------------------------------------------------------------#

#wrangle sec data
wrangle_data_sec <- 
  full_join(sec0a, hh_sex, by = c("nh" = "nh", "clust" = "clust")) %>% 
  full_join(., sec2a_high_educ, by = c("nh" = "nh", "clust" = "clust")) %>%
  full_join(., sec8a1, by = c("nh" = "nh", "clust" = "clust")) %>%
  full_join(., sec8c1_tot_harvest, by = c("nh" = "nh", "clust" = "clust"))

wrangle_data_sec[is.na(wrangle_data_sec)] <- 0 # Sets all NA values within data frame to 0
cor(wrangle_data_sec)  # Calculate correlation matrix

#wrangle aggregate data
wrangle_data_agg <-
  full_join(agg1, agg2, agg4, by = c("nh" = "nh", "clust" = "clust")) %>%
  full_join(., agg7, by = c("nh" = "nh", "clust" = "clust")) %>%
  full_join(., agg9, by = c("nh" = "nh", "clust" = "clust")) %>%
  full_join(., agg10, by = c("nh" = "nh", "clust" = "clust")) %>%
  full_join(., agg12, by = c("nh" = "nh", "clust" = "clust"))

cor(wrangle_data_agg) # Calculate correlation matrix


#wrangle community data
wrangle_data_cs <-
  full_join(cs2_road, cs2_permanent_market, by = c("region"="region", "district"="district", "eanum"="eanum")) %>%
  full_join(., cs3_school, by = c("region"="region", "district"="district", "eanum"="eanum")) %>%
  full_join(., cs4b_hospital, by = c("region"="region", "district"="district", "eanum"="eanum"))


cor(wrangle_data_cs) # Calculate correlation matrix

#combine all the data into a final wrangle dataframe
wrangle_data_final <-
  full_join(wrangle_data_sec, wrangle_data_agg, by = c("nh" = "nh", "clust" = "clust")) %>%
  full_join(., wrangle_data_cs, by = c("region"="region", "district"="district", "eanum"="eanum")) %>%
  select(-region, -district, -eanum)

wrangle_data_final[is.na(wrangle_data_final)] <- 0 # Sets all NA values within data frame to 0
cor(wrangle_data_final) # Calculate correlation matrix

#adding a new variable for profit per unit
profit_per_unit <- ifelse(wrangle_data_final$land_own_by_HH, wrangle_data_final$agri1c/wrangle_data_final$land_own_by_HH, 0)
wrangle_data_final$profit_per_unit <- profit_per_unit

wrangle_data_final[is.na(wrangle_data_final)] <- 0 # Sets all NA values within data frame to 0
cor_wrangle_data_final <- cor(wrangle_data_final) # Calculate correlation matrix
round(cor_wrangle_data_final, 2) # round correlation matrix to 2 decimal points

#-------------------------------------------------------------------------------------------------------------------#
# Analysis                                                                                                          #
#-------------------------------------------------------------------------------------------------------------------#

# Base model
regression_1 <-(lm(agri1c ~ nh + clust + ez + loc2 + hh_sex + hh_age + highest_educ + current_land_owner
                   + unit_plot_areas + land_own_by_HH + tot_val_harvest + totemp + hhagdepn + expfoodc 
                   + othexpc + impfoodc + expremit + road + permanent_market + primary_school + hospital
                   + profit_per_unit, data = wrangle_data_final))

summary(regression_1)

# Histogram for standard distribution of residuals
ggplot(regression_1, aes(x=rstandard(regression_1))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardize Residuals", 
       y = "Residual Count",
       title = "Assumption Review")

# Plot graph to check for constant variance
plot(fitted(regression_1), resid(regression_1),
     xlab = "Fitted", ylab = "Residuals",
     abline(h = 0, col = "blue"))

# Checking how HH highest education level affects agricultural profit
regression_2 <-(lm(agri1c ~ nh + ez + loc2 + unit_plot_areas + road + permanent_market + primary_school 
                   + hospital + highest_educ + (profit_per_unit*land_own_by_HH), data = wrangle_data_final))

summary(regression_2)

# Histogram for standard distribution of residuals
ggplot(regression_2, aes(x=rstandard(regression_2))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardize Residuals", 
       y = "Residual Count",
       title = "Assumption Review")

# Plot graph to check for constant variance
plot(fitted(regression_2), resid(regression_2),
     xlab = "Fitted", ylab = "Residuals",
     abline(h = 0, col = "blue"))


# Interaction testing between ecological zone and locality as well as profit per unit and land owned by household
regression_3 <-(lm(agri1c ~ nh + (ez*loc2) + unit_plot_areas + road + permanent_market + primary_school 
                   + hospital + highest_educ + (profit_per_unit*land_own_by_HH), data = wrangle_data_final))

summary(regression_3)

# Histogram for standard distribution of residuals
ggplot(regression_3, aes(x=rstandard(regression_3))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardize Residuals", 
       y = "Residual Count",
       title = "Assumption Review")

# Plot graph to check for constant variance
plot(fitted(regression_3), resid(regression_3),
     xlab = "Fitted", ylab = "Residuals",
     abline(h = 0, col = "blue"))

