# load libraries
library(tidyverse)
library(haven)

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
