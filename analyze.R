library(readr)
library(dplyr)
library(reshape2)
library(survey)

# Read IPUMS export
ipums.orig <- read_csv("usa_00028.csv", col_types="cicdi")

# Filter to only observations that include wages
ipums <- ipums.orig %>%
  filter(INCWAGE < 999998)

# Convert years to a factor
ipums$year <- as.factor(ipums$YEAR)

# Convert TRANWORK
ipums$commute <- NA

ipums$commute[ipums$TRANWORK >= 10 & ipums$TRANWORK < 20] <- "Car"
ipums$commute[ipums$TRANWORK >= 30 & ipums$TRANWORK <= 31] <- "Bus"
ipums$commute[ipums$TRANWORK == 33] <- "Subway"
ipums$commute[ipums$TRANWORK == 70] <- "Worked from home"

ipums$commute <- factor(ipums$commute, level=c("Car", "Bus", "Subway", "Worked from home"))

# Compute totals by year and commute
ipums.totals <- ipums %>%
  group_by(year, commute) %>%
  summarise(pop = sum(PERWT))

ipums.totals.pivot <- dcast(ipums.totals, commute ~ year, value.var="pop")

# Compute shares by year and commute
ipums.shares <- ipums.totals %>%
  group_by(year) %>%
  mutate(pct = pop / sum(pop)) %>%
  ungroup()

ipums.shares.pivot <- dcast(ipums.shares, commute ~ year, value.var="pct")

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

ipums.means.pivot <- dcast(ipums.means, commute ~ year, value.var="mean.wages")

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
  )

ipums.occ.shares <- ipums.occ.means %>%
  group_by(year) %>%
  mutate(pct = pop / sum(pop)) %>%
  ungroup()

ipums.occ.totals.pivot <- dcast(ipums.occ.means, OCC2010 ~ year, value.var="pop")
ipums.occ.shares.pivot <- dcast(ipums.occ.shares, OCC2010 ~ year, value.var="pop")
ipums.occ.means.pivot <- dcast(ipums.occ.means, OCC2010 ~ year, value.var="mean.wages")

write_csv(ipums.occ.totals.pivot, "ipums.occ.totals.pivot.csv")
write_csv(ipums.occ.shares.pivot, "ipums.occ.shares.pivot.csv")
write_csv(ipums.occ.means.pivot, "ipums.occ.means.pivot.csv")
