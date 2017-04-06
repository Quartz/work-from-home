# These calculations (originally in `ipums.R`) were used...
# ...to validate that the weighted calculations are correct)

# Compute mean wages by year
# Perfect match for results computed with Hmisc (see below)
ipums.means <- ipums %>%
  group_by(year) %>%
  summarise(
    pop = sum(PERWT),
    mean.wages = sum(INCWAGE * PERWT) / pop,
    v = (sum(PERWT * (INCWAGE - mean.wages)^2)) / (pop - 1),
    sd = sqrt(v),
    se = sd / sqrt(pop)
  )

# Test by comparing to results of Hmisc module
library(Hmisc)

ipums.hmisc.means <- ipums %>%
  group_by(year) %>%
  summarise(
    pop = sum(PERWT),
    mean.wages = wtd.mean(INCWAGE, PERWT),
    v = wtd.var(INCWAGE, PERWT),
    sd = sqrt(v),
    se = sd / sqrt(pop)
  )

all.equal(ipums.means, ipums.hmisc.means)