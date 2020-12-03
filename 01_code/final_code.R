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
library(corrplot)
library(Hmisc)

#-------------------------------------------------------------------------------------------------------------------#
# Read in data files                                                                                                #
#-------------------------------------------------------------------------------------------------------------------#

#section 0 - Survey Information
sec0a <- read_dta('02_raw_data/sec0a.dta') 

#section 2 - General Education
sec2a <- read_dta('02_raw_data/sec2a.dta')

#section 8 - Agriculture survey
sec8a1 <- read_dta("02_raw_data/sec8a1.dta")

# Income Aggregates
agg1 <- read_dta('02_raw_data/agg1.dta')
agg2 <- read_dta('02_raw_data/agg2.dta')
agg4 <- read_dta('02_raw_data/agg4.dta')

# Expenditure Aggregates
agg7 <- read_dta('02_raw_data/agg7.dta')
agg9 <- read_dta('02_raw_data/agg9.dta')

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

# General Education
sec2a <- read_dta('02_raw_data/sec2a.dta') %>% 
  select(nh, clust, s2aq2) %>% 
  rename(highest_educ = s2aq2)

# start selecting variables and rename
sec8a1 <- read_dta("02_raw_data/sec8a1.dta") %>% 
  select(nh, s8aq1, s8aq3, s8aq4, clust) %>% 
  rename(current_land_owner = s8aq1, unit_plot_areas = s8aq3, land_own_by_HH = s8aq4)


# Income variables
agg2 <- read_dta('02_raw_data/agg2.dta') %>% 
  select(clust:nh, agri1c, hhagdepn)

# Expenditure variables
agg7 <- read_dta('02_raw_data/agg7.dta') %>%
  select(clust, nh, expfoodc)

agg9 <- read_dta('02_raw_data/agg9.dta') %>%
  select(clust, nh, othexpc)

# Community variables
cs2 <- 
  select(cs2, region:eanum, s2q4, s2q8, s2q20, s2q23)

cs3 <- 
  select(cs3, region:eanum, s3q1, s3q11)

cs4b <- 
  select(cs4b, region:eanum, s4bq5)


#-------------------------------------------------------------------------------------------------------------------#
# Clean Data                                                                                                        #
#-------------------------------------------------------------------------------------------------------------------#
# Set variable as factor
sec0a$ez <- as.factor(sec0a$ez)

#set the unaccountable education to 01
sec2a$highest_educ[sec2a$highest_educ == 17] <- 01
sec2a$highest_educ[sec2a$highest_educ == 96] <- 01

#clean sec2a data by find the top educ.
sec2a_high_educ <-
  sec2a %>%
  group_by(nh, clust) %>%
  summarise(highest_educ = max(highest_educ))


#clean sec8a and create a new variable for area unit as square feet
sec8a1_farm_land_size <- 
  sec8a1 %>%
  filter(unit_plot_areas != 4) %>%
  mutate(
    SqFt =
      case_when(
        unit_plot_areas == 1 ~ land_own_by_HH*43560,
        unit_plot_areas == 2 ~ land_own_by_HH*10890,
        unit_plot_areas == 3 ~ land_own_by_HH*12209
      )
  ) %>%
  group_by(clust, nh) %>%
  summarise(
    farm_land_size = sum(SqFt)
  )


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
  full_join(sec0a, sec2a_high_educ, by = c("nh" = "nh", "clust" = "clust")) %>%
  full_join(., sec8a1_farm_land_size, by = c("nh" = "nh", "clust" = "clust"))

wrangle_data_sec[is.na(wrangle_data_sec)] <- 0 # Sets all NA values within data frame to 0
cor(wrangle_data_sec)  # Calculate correlation matrix

#wrangle aggregate data
wrangle_data_agg <-
  full_join(agg1, agg2, agg4, by = c("nh" = "nh", "clust" = "clust")) %>%
  full_join(., agg7, by = c("nh" = "nh", "clust" = "clust")) %>%
  full_join(., agg9, by = c("nh" = "nh", "clust" = "clust"))

cor(wrangle_data_agg) # Calculate correlation matrix


#wrangle community data
wrangle_data_cs <-
  full_join(cs2_road, cs3_school, by = c("region"="region", "district"="district", "eanum"="eanum")) %>%
  full_join(., cs4b_hospital, by = c("region"="region", "district"="district", "eanum"="eanum"))


cor(wrangle_data_cs) # Calculate correlation matrix

#combine all the data into a final wrangle dataframe
wrangle_data_final <-
  full_join(wrangle_data_sec, wrangle_data_agg, by = c("nh" = "nh", "clust" = "clust")) %>%
  full_join(., wrangle_data_cs, by = c("region"="region", "district"="district", "eanum"="eanum")) %>%
  select(-region, -district, -eanum)

wrangle_data_final[is.na(wrangle_data_final)] <- 0 # Sets all NA values within data frame to 0

#adding a new variable for profit per unit
profit_per_unit <- ifelse(wrangle_data_final$farm_land_size, wrangle_data_final$agri1c/wrangle_data_final$farm_land_size, 0)
wrangle_data_final$profit_per_unit <- profit_per_unit

wrangle_data_final[is.na(wrangle_data_final)] <- 0 # Sets all NA values within data frame to 0

round_cor <- cor(wrangle_data_final) # Calculate correlation matrix
round_cor <- round(round_cor, 2)

#create a new dataframe to get log of profit for all positive profit HH record.
wrangle_data_final_has_profit <-
  wrangle_data_final %>%
  filter(profit_per_unit>0) %>%
  mutate(
    log_profit_per_unit = log(profit_per_unit)
  )

round_cor <- cor(wrangle_data_final) # Calculate correlation matrix
round_cor <- round(round_cor, 2)

#remove factor and drop all observation contains NA to create correlation matrix
wrangle_data_final_has_profit.cor <- 
  wrangle_data_final_has_profit %>%
  select(-ez) %>%
  drop_na() 
#change df to correlation table and create correlation plot
wrangle_data_final_has_profit.cor <- cor(wrangle_data_final_has_profit.cor)
corrplot(wrangle_data_final_has_profit.cor, cl.lim=c(min(wrangle_data_final_has_profit.cor),max(wrangle_data_final_has_profit.cor)))

#-------------------------------------------------------------------------------------------------------------------#
# Analysis                                                                                                          #
#-------------------------------------------------------------------------------------------------------------------#

# Base model using all variables 
regression_1 <-(lm(profit_per_unit ~ ez + loc2 + highest_educ
                   + totemp + hhagdepn + expfoodc + farm_land_size 
                   + othexpc + road + primary_school + hospital
                   , data = wrangle_data_final))

summary(regression_1)

# Histogram for standard distribution of residuals
std_dist_1 <- ggplot(regression_1, aes(x=rstandard(regression_1))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardize Residuals", 
       y = "Residual Count",
       title = "Assumption Review")

std_dist_1

# Plot graph to check for constant variance
plot(fitted(regression_1), resid(regression_1),
     xlab = "Fitted", ylab = "Residuals",
     abline(h = 0, col = "blue"))



# normalizes profit per unit
regression_2 <-(lm(log_profit_per_unit ~ highest_educ + road + primary_school 
                   + hospital + farm_land_size, data = wrangle_data_final_has_profit))

summary(regression_2)

# Histogram for standard distribution of residuals
std_dist_2 <- ggplot(regression_2, aes(x=rstandard(regression_2))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardize Residuals", 
       y = "Residual Count",
       title = "Assumption Review")

std_dist_2

# Plot graph to check for constant variance
const_var_2 <- plot(fitted(regression_2), resid(regression_2),
                    xlab = "Fitted", ylab = "Residuals",
                    abline(h = 0, col = "blue"))


const_var_2


# effect of locality and ecological zone on profit per unit
regression_3 <-(lm(log_profit_per_unit ~ loc2 + ez + road + primary_school 
                   + hospital + farm_land_size, data = wrangle_data_final_has_profit))

summary(regression_3)

# Histogram for standard distribution of residuals
std_dist_3 <- ggplot(regression_3, aes(x=rstandard(regression_3))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardize Residuals", 
       y = "Residual Count",
       title = "Assumption Review")

std_dist_3

# Plot graph to check for constant variance
const_var_3 <- plot(fitted(regression_3), resid(regression_3),
                    xlab = "Fitted", ylab = "Residuals",
                    abline(h = 0, col = "blue"))

const_var_3

# investigate effect of hhagdepn
regression_4 <-(lm(log_profit_per_unit ~ loc2 + ez + hhagdepn + road + primary_school 
                   + hospital + farm_land_size, data = wrangle_data_final_has_profit))

summary(regression_4)

# Histogram for standard distribution of residuals
std_dist_4 <- ggplot(regression_4, aes(x=rstandard(regression_4))) +
  geom_histogram(binwidth = .25) +
  labs(x = "Standardize Residuals", 
       y = "Residual Count",
       title = "Assumption Review")

std_dist_4

# Plot graph to check for constant variance
const_var_4 <- plot(fitted(regression_4), resid(regression_4),
                    xlab = "Fitted", ylab = "Residuals",
                    abline(h = 0, col = "blue"))

const_var_4

