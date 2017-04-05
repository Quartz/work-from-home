library(readr)
library(dplyr)
library(reshape2)
library(survey)

# Read IPUMS export
ipums.orig <- read_csv("usa_00028.csv", col_types="ciidi")

# Filter to only observations that include wages
ipums <- ipums.orig %>%
  filter(INCWAGE > 0 & INCWAGE < 999998)

# Convert years to a factor
ipums$year <- as.factor(ipums$YEAR)

# Convert TRANWORK
ipums$commute <- NA

ipums$commute[ipums$TRANWORK >= 10 & ipums$TRANWORK <= 15] <- "Car/Truck/Van"
ipums$commute[ipums$TRANWORK >= 30 & ipums$TRANWORK <= 32] <- "Bus/Streetcar"
ipums$commute[ipums$TRANWORK == 33] <- "Subway/Elevated"
ipums$commute[ipums$TRANWORK == 70] <- "Worked from home"

ipums$commute <- factor(ipums$commute, level=c("Car/Truck/Van", "Bus/Streetcar", "Subway/Elevated", "Worked from home"))

# Read occupation codes
occ <- read_csv("https://raw.githubusercontent.com/wireservice/lookup/master/occ/description.2010.csv", col_types="ic")

occ$description <- factor(occ$description)

# Read CPI rates
cpi <- read_csv("https://raw.githubusercontent.com/wireservice/lookup/master/year/cpi.csv", col_types="cd")

# Join CPI to ipums and adjust to 2015 dollars
# ipums.cpi <- ipums %>%
#   left_join(cpi, by = "year") %>%
#   mutate(incwage.adj = INCWAGE * 237.0 / cpi)

# Compute mean wages by year
# Perfect match for results computed with Hmisc (see below)
# ipums.means <- ipums %>%
#   group_by(year) %>%
#   summarise(
#     pop = sum(PERWT),
#     mean.wages = sum(INCWAGE * PERWT) / pop,
#     v = (sum(PERWT * (INCWAGE - mean.wages)^2)) / (pop - 1),
#     sd = sqrt(v),
#     se = sd / sqrt(pop)
#   )

# Test by comparing to results of Hmisc module
# library(Hmisc)
# 
# ipums.hmisc.means <- ipums %>%
#   group_by(year) %>%
#   summarise(
#     pop = sum(PERWT),
#     mean.wages = wtd.mean(INCWAGE, PERWT),
#     v = wtd.var(INCWAGE, PERWT),
#     sd = sqrt(v),
#     se = sd / sqrt(pop)
#   )
# 
# all.equal(ipums.means, ipums.hmisc.means)

# Compute mean wages by year and commute
# Perfect match for results from IPUMS online
ipums.means <- ipums %>%
  group_by(year, commute) %>%
  summarise(
    pop = sum(PERWT),
    mean.wages = sum(INCWAGE * PERWT) / pop,
    v = (sum(PERWT * (INCWAGE - mean.wages)^2)) / (pop - 1),
    sd = sqrt(v),
    se = sd / sqrt(pop)
  )

# CPI adjust results
ipums.means.cpi <- ipums.means %>%
  left_join(cpi, by = "year") %>%
  mutate(
    mean.wages = mean.wages * 237.0 / cpi,
    v = v * 237.0 / cpi,
    sd = sd * 237.0 / cpi,
    se = se * 237.0 / cpi
  ) %>%
  select(-cpi)

write_csv(ipums.means.cpi, "results/ipums.means.cpi.csv")

# Compute mean wages for home workers by year and occupation
ipums.occ.means <- ipums %>%
  filter(commute == "Worked from home") %>%
  group_by(year, OCC2010) %>%
  summarise(
    pop = sum(PERWT),
    mean.wages = sum(INCWAGE * PERWT) / pop,
    v = (sum(PERWT * (INCWAGE - mean.wages)^2)) / (pop - 1),
    sd = sqrt(v),
    se = sd / sqrt(pop)
  ) %>%
  left_join(occ, by = c("OCC2010" = "occ"))

# CPI adjust results
ipums.occ.means.cpi <- ipums.occ.means %>%
  left_join(cpi, by = "year") %>%
  mutate(
    mean.wages = mean.wages * 237.0 / cpi,
    v = v * 237.0 / cpi,
    sd = sd * 237.0 / cpi,
    se = se * 237.0 / cpi
  ) %>%
  select(-cpi)

ipums.occ.shares <- ipums.occ.means %>%
  group_by(year) %>%
  mutate(pct = pop / sum(pop)) %>%
  ungroup()

write_csv(ipums.occ.means.cpi, "results/ipums.occ.means.cpi.csv")
