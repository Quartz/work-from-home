library(readr)
library(dplyr)
library(reshape2)
library(survey)

# Export 3 from ATUS
# "Time spent working from home by broad occupation (2003-2015) w/ CSV and Earnings"

# NB: Earnings do not appear to be reliable in this data and should be ignored.

# ATUS estimation formulas, see page 37:
# https://www.bls.gov/tus/atususersguide.pdf

# TODO: Exclude self-employed workers

# Read ATUS export
atus <- read_csv("atus_00003.csv", col_types="cdidd")

# Convert years to a factor
atus$year <- as.factor(atus$YEAR)

# Flag for those that worked at home at all
atus$homeworkers <- (atus$workingfromhome > 0)

# Flag for those that worked at least 6 hours at home
atus$homeworkers.fullday <- (atus$workingfromhome >= 360)

# Load ATUS OCC2 code mapping
occ2 <- read_csv("atus-occ2.csv", col_types="ic")

# Join descriptions
atus <- atus %>%
  left_join(occ2, by = c("OCC2" = "occ2")) %>%
  rename(occupation = description)

#atus <- merge(atus, occ2, by.x = "OCC2", by.y = "occ2")

# Count and means for all homeworkers
atus.means <- atus %>%
  filter(homeworkers.fullday) %>%
  group_by(year) %>%
  summarise(
    n = sum(WT06) / 365,
    mean.mins = sum(workingfromhome * WT06) / sum(WT06)
  )

write_csv(atus.means, "results/atus.means.csv")

# Counts and means by occupation
atus.occ.means <- atus %>%
  filter(homeworkers.fullday) %>%
  group_by(year, occupation) %>%
  summarise(
    n = sum(WT06) / 365,
    mean.mins = sum(workingfromhome * WT06) / sum(WT06)
  )

write_csv(atus.occ.means, "results/atus.occ.means.csv")
