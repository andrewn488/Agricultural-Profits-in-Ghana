# load libraries
library(tidyverse)
library(haven)
library(plyr)


agg1 <- read_dta("02_raw_data/aggregates/agg1.dta")
agg2 <- read_dta("02_raw_data/aggregates/agg2.dta")

sec1 <- read_dta("02_raw_data/sec1.dta")
sec0a <- read_dta("02_raw_data/sec0a.dta")
sec4b <- read_dta("02_raw_data/sec4b.dta")
sec5 <- read_dta("02_raw_data/sec5.dta")
sec8a1 <- read_dta("02_raw_data/sec8a1.dta")
sec9a2 <- read_dta("02_raw_data/sec9a2.dta")
sec9a11 <- read_dta("02_raw_data/sec9a11.dta")
sec9a12 <- read_dta("02_raw_data/sec9a12.dta")
sec9b <- read_dta("02_raw_data/sec9b.dta")
View(sec0a)
View(sec5)
View(sec9a2)
View(sec9a11)
View(sec9a12)
View(sec9b)
View(agg1)
View(agg2)
summary(agg1)
colnames(sec9a2)
colnames(sec9a11)
colnames(sec9a12)
colnames(sec9a2)
sec9_c1 <- full_join(sec9a2, sec9a11, by = c("nh" = "nh", "clust" = "clust"))
sec9_c2 <- full_join(sec9_c1, sec9a12, by = c("nh" = "nh", "clust" = "clust"))
sec9_combine <- full_join(sec9_c2, sec9b, by = c("nh" = "nh", "clust" = "clust"))
View(sec9_combine)

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
colnames(cs0)
colnames(cs1)
colnames(cs2)
colnames(cs3)
colnames(cs4a)
colnames(cs4b)
colnames(cs4c)
colnames(cs5a)
colnames(cs5b)

#limiting the community questionnaire to the relevant variables
#cs2 - 
cs2_limit <- select(cs2, region:eanum, s2q1a:s2q1d, s2q4, s2q8, s2q19, s2q20, s2q23)
cs3_limit <- select(cs3, region:eanum, s3q1, s3q11)
#cs4a - Is there a (health provider) in this community? How far is it in km?
cs4a_limit <- select(cs4a, region:eanum, s4aq1, s4aq2)
#cs4b - Is there a (hospital, parmancy...etc) in this community? How far is it in km? Does it have dispensary?
cs4b_limit <- select(cs4b, region:eanum, s4bq5, s4bq6, s4bq10)
#cs4c has no important variables
cs5a_limit <- select(cs5a, region:eanum, s5aq1, s5aq2, s5aq4)
cs4b_limit <- select(cs5b, region:eanum, s5bq5, s5bq6, )

?select

View(cs5b)
cs_c1 <- full_join(cs0, cs1)
cs_combine <- join_all(list(cs0, cs1, cs2, cs3, cs4a, cs4b, cs4c, cs5a, cs5b), by = c("region"="region", "district"="district", "eanum"="eanum"), type = "left")
           
cs_combine <-
  full_join(cs0, cs1, by = c("region"="region", "district"="district", "eanum"="eanum")) %>%
  full_join(., cs2, by = c("region"="region", "district"="district", "eanum"="eanum")) %>%
  full_join(., cs3, by = c("region"="region", "district"="district", "eanum"="eanum")) %>%
  full_join(., cs4a, by = c("region"="region", "district"="district", "eanum"="eanum")) %>%
  full_join(., cs4b, by = c("region"="region", "district"="district", "eanum"="eanum")) %>%
  full_join(., cs4c, by = c("region"="region", "district"="district", "eanum"="eanum")) %>%
  full_join(., cs5a, by = c("region"="region", "district"="district", "eanum"="eanum")) %>%
  full_join(., cs5b, by = c("region"="region", "district"="district", "eanum"="eanum"))
