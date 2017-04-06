library(readr)
library(dplyr)
library(reshape2)
library(survey)

# Export 4 from ATUS
# "Time spent working from home by broad occupation and class of worker (2003-2015) w/ CSV"

# NB: Earnings do not appear to be reliable in this data and should be ignored.

# ATUS estimation formulas, see page 37:
# https://www.bls.gov/tus/atususersguide.pdf

# TODO: Exclude self-employed workers

# Read ATUS export
atus <- read_csv("atus_00004.csv", col_types="cdiid")

# Convert years to a factor
atus$year <- as.factor(atus$YEAR)

# Flag for those that worked at home at all
atus$homeworkers <- (atus$workingfromhome > 0)

# Flag for those that worked at least 6 hours at home
atus$homeworkers.fullday <- (atus$workingfromhome >= 360)

# Flag for wage-workers
atus$wageworkers <- (atus$CLWKR <= 5)

# Load ATUS OCC2 code mapping
occ2 <- read_csv("atus-occ2.csv", col_types="ic")

# Join descriptions
atus <- atus %>%
  left_join(occ2, by = c("OCC2" = "occ2")) %>%
  rename(occupation = description)

# Count and means for all employees that ever worked at home
atus.totals <- atus %>%
  filter(homeworkers & wageworkers) %>%
  group_by(year) %>%
  summarise(
    n = sum(WT06) / 365,
    mean.mins = sum(workingfromhome * WT06) / sum(WT06)
  )

write_csv(atus.means, "results/atus.totals.csv")

# Count of fullday homeworkers
atus.fullday.totals <- atus %>%
  filter(homeworkers.fullday & wageworkers) %>%
  group_by(year) %>%
  summarise(
    n = sum(WT06) / 365
  )

write_csv(atus.fullday.totals, "results/atus.fullday.totals.csv")

# Counts and means by occupation
atus.occ.totals <- atus %>%
  filter(homeworkers & wageworkers) %>%
  group_by(year, occupation) %>%
  summarise(
    n = sum(WT06) / 365,
    mean.mins = sum(workingfromhome * WT06) / sum(WT06)
  )

write_csv(atus.occ.totals, "results/atus.occ.totals.csv")

# Count of fullday homeworkers by occupation
atus.fullday.occ.totals <- atus %>%
  filter(homeworkers.fullday & wageworkers) %>%
  group_by(year, occupation) %>%
  summarise(
    n = sum(WT06) / 365
  )

write_csv(atus.fullday.occ.totals, "results/atus.fullday.occ.totals.csv")

