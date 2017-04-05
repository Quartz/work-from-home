library(readr)
library(dplyr)
library(reshape2)
library(survey)

# Read IPUMS export
atus <- read_csv("atus_00003.csv", col_types="cdidd")

# Convert years to a factor
atus$year <- as.factor(atus$YEAR)

# Convert to annual earnings
atus$earnyear <- atus$EARNWEEK * 52.14

atus.means <- atus %>%
  group_by(year, OCC2) %>%
  summarise(
    pop = sum(WT06),
    mean.wages = sum(earnyear * WT06) / pop,
    v = (sum(WT06 * (earnyear - mean.wages)^2)) / (pop - 1),
    sd = sqrt(v),
    se = sd / sqrt(pop)
  )

write_csv(atus.means, "results/atus.means.csv")
