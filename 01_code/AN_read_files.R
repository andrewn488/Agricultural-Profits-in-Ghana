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

<<<<<<< HEAD
sec4b <- read_dta("sec4b.dta")
=======
com2 <- full_join(sec8a3, sec8b, by = c("nh" = "nh", "clust" = "clust"))

com4 <- full_join(com1, com2, by = c("nh" = "nh", "clust" = "clust"))

>>>>>>> bcac3023dcceea3e283b195ed41c34818f173c8f
