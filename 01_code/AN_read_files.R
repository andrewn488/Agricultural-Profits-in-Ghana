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
colnames(sec8a1)
sec8a2 <- read_dta("02_raw_data/sec8a2.dta")
colnames(sec8a2)
sec8a3 <- read_dta("02_raw_data/sec8a3.dta")
colnames(sec8a3)
sec8b <- read_dta("02_raw_data/sec8b.dta")
colnames(sec8b)
sec8c1 <- read_dta("02_raw_data/sec8c1.dta")
colnames(sec8c1)
sec8c2 <- read_dta("02_raw_data/sec8c2.dta")
colnames(sec8c2)
sec8d <- read_dta("02_raw_data/sec8d.dta")
colnames(sec8d)
sec8e <- read_dta("02_raw_data/sec8e.dta")
colnames(sec8e)
sec8f <- read_dta("02_raw_data/sec8f.dta")
colnames(sec8f)
sec8g <- read_dta("02_raw_data/sec8g.dta")
colnames(sec8g)
sec8h <- read_dta("02_raw_data/sec8h.dta")
colnames(sec8h)
sec8hid <- read_dta("02_raw_data/sec8hid.dta")
colnames(sec8hid)

# start joining files
com1 <- left_join(sec8a1, sec8a2, by = c("nh" = "nh", "clust" = "clust"))
com2 <- left_join(com1, sec8a3, by = c("nh" = "nh", "clust" = "clust"))
com3 <- left_join(com2, sec8b, by = c("nh" = "nh", "clust" = "clust"))
com4 <- left_join(com3, sec8c1, by = c("nh" = "nh", "clust" = "clust"))
com5 <- left_join(com4, sec8c2, by = c("nh" = "nh", "clust" = "clust", "s8cq1" = "s8cq1"))
com6 <- left_join(com5, sec8d, by = c("nh" = "nh", "clust" = "clust"))
com7 <- left_join(com6, sec8e, by = c("nh" = "nh", "clust" = "clust"))
com8 <- left_join(com7, sec8f, by = c("nh" = "nh", "clust" = "clust"))
