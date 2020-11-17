# load libraries
library(tidyverse)
library(haven)

#read seg5 file
sec5 <- read_dta("02_raw_data/sec5.dta") #this ds contain migration data for each person in the household. Not sure if this is useful.

#------------------------------------------------------------------------------------------------------------------------------

#read seg9 file
sec9a2 <- read_dta("02_raw_data/sec9a2.dta") #Interviewer ID, not important
sec9a11 <- read_dta("02_raw_data/sec9a11.dta") #expenditure details on non-food items. 
sec9a12 <- read_dta("02_raw_data/sec9a12.dta") #amount spent on food items from previous 7 visits, not important
sec9b <- read_dta("02_raw_data/sec9b.dta") #amount spent on food items from previous 7 visits, not important

#------------------------------------------------------------------------------------------------------------------------------

#read community file
cs0 <- read_dta("02_raw_data/community/cs0.dta")
cs1 <- read_dta("02_raw_data/community/cs1.dta")
cs2 <- read_dta("02_raw_data/community/cs2.dta")
cs3 <- read_dta("02_raw_data/community/cs3.dta")
cs4a <- read_dta("02_raw_data/community/cs4a.dta")
cs4b <- read_dta("02_raw_data/community/cs4b.dta")
cs4c <- read_dta("02_raw_data/community/cs4c.dta")
cs5a <- read_dta("02_raw_data/community/cs5a.dta")
cs5b <- read_dta("02_raw_data/community/cs5b.dta")

#limiting the community questionnaire to the relevant variables
#cs2 - 
cs2_limit <- select(cs2, region:eanum, s2q1a:s2q1d, s2q4, s2q8, s2q19, s2q20, s2q23)
cs3_limit <- select(cs3, region:eanum, s3q1, s3q11)
#cs4a - Is there a (health provider) in this community
cs4a_limit <- select(cs4a, region:eanum, s4aq1)
#cs4b - Is there a (hospital, parmancy...etc) in this community?
cs4b_limit <- select(cs4b, region:eanum, s4bq5)
#cs4c has no important variables
cs5a_limit <- select(cs5a, region:eanum, s5aq1, s5aq2, s5aq4)
cs5b_limit <- select(cs5b, region:eanum, s5bq5, s5bq6, s5bq22a:s5bq22l, s5bq24:s5bq25b)

#clean cs4a data to just identify if the community has health provider or not
cs4a_limit <-
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
    n_hp = "2",
    y_hp = "1"
  ) %>%
  mutate(health_provider = ifelse(is.na(y_hp), "no", "yes")) %>%
  select(1:3, 6)

#clean cs4a data to just identify if the community has health provider or not
cs4b_limit <-
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
    n_hp = "2",
    y_hp = "1"
  ) %>%
  mutate(hospital = ifelse(is.na(y_hp), "no", "yes")) %>%
  select(1:3, 6)

#combine cs2, cs4, cs4a, cs4b, and cs5b together
cs_combine1 <- 
  left_join(cs2_limit, cs3_limit, by = c("region"="region", "district"="district", "eanum"="eanum"))
cs_combine2 <- 
  left_join(cs_combine1, cs4a_limit, by = c("region"="region", "district"="district", "eanum"="eanum"))
cs_combine3 <- 
  left_join(cs_combine2, cs4b_limit, by = c("region"="region", "district"="district", "eanum"="eanum"))
cs_combine <-
  left_join(cs_combine3, cs5b_limit, by = c("region"="region", "district"="district", "eanum"="eanum"))