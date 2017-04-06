library(readr)
library(dplyr)
library(reshape2)
library(survey)

# Export 4 from ATUS
# "Time spent working from home by broad occupation and class of worker (2003-2015) w/ CSV"

# ATUS estimation formulas, see page 37:
# https://www.bls.gov/tus/atususersguide.pdf

# Read ATUS export
atus <- read_csv("atus_00004.csv", col_types="cdiid")

# Convert years to a factor
atus$year <- as.factor(atus$YEAR)

# Flag for those that worked at home at all
atus$homeworkers <- (atus$workingfromhome > 0)

# Flag for those that worked at least 7 hours at home (~ 35 hours a week)
atus$homeworkers.fullday <- (atus$workingfromhome >= 420)

# Flag for wage-workers (not self-employed)
atus$wageworkers <- (atus$CLWKR <= 5)

# Load ATUS OCC2 code mapping
occ2 <- read_csv("atus-occ2.csv", col_types="ic")

# Join descriptions
atus <- atus %>%
  left_join(occ2, by = c("OCC2" = "occ2")) %>%
  rename(occupation = description)

# Counts and means for all employees by homeworker or not
atus.totals <- atus %>%
  filter(wageworkers) %>%
  group_by(year, homeworkers.fullday) %>%
  summarise(
    n = sum(WT06) / 365
  )

write_csv(atus.totals, "results/atus.totals.csv")

# Count and means for all employees that worked at home for any time
atus.anytime.totals <- atus %>%
  filter(homeworkers & wageworkers) %>%
  group_by(year) %>%
  summarise(
    n = sum(WT06) / 365,
    mean.mins = sum(workingfromhome * WT06) / sum(WT06)
  )

write_csv(atus.anytime.totals, "results/atus.anytime.totals.csv")

# Count of fullday homeworkers
atus.fullday.totals <- atus %>%
  filter(homeworkers.fullday & wageworkers) %>%
  group_by(year) %>%
  summarise(
    n = sum(WT06) / 365
  )

write_csv(atus.fullday.totals, "results/atus.fullday.totals.csv")

# Counts and means by occupation
atus.anytime.occ.totals <- atus %>%
  filter(homeworkers & wageworkers) %>%
  group_by(year, occupation) %>%
  summarise(
    n = sum(WT06) / 365,
    mean.mins = sum(workingfromhome * WT06) / sum(WT06)
  )

write_csv(atus.anytime.occ.totals, "results/atus.anytime.occ.totals.csv")

# Count of fullday homeworkers by occupation
atus.fullday.occ.totals <- atus %>%
  filter(homeworkers.fullday & wageworkers) %>%
  group_by(year, occupation) %>%
  summarise(
    n = sum(WT06) / 365
  )

write_csv(atus.fullday.occ.totals, "results/atus.fullday.occ.totals.csv")

