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

# Compute share of employees that worked at home for any time by industry
atus.anytime.occ.shares <- atus %>%
  filter(wageworkers) %>%
  group_by(year, occupation, homeworkers) %>%
  summarise(
    n = sum(WT06) / 365
  ) %>%
  mutate(
    share = n / sum(n)
  )

write_csv(atus.anytime.occ.shares, "results/atus.anytime.occ.shares.csv")

# Count of fullday homeworkers by occupation
atus.fullday.occ.totals <- atus %>%
  filter(homeworkers.fullday & wageworkers) %>%
  group_by(year, occupation) %>%
  summarise(
    n = sum(WT06) / 365
  )

write_csv(atus.fullday.occ.totals, "results/atus.fullday.occ.totals.csv")

# Create 3-year pooled ATUS data
atus.pooled <- atus %>%
  filter(FALSE)

atus.pooled <- cbind(pool.year = character(), atus.pooled)

for (year in 2004:2014) {
  for (y in (year - 1):(year + 1)) {
    temp <- atus %>%
      filter(year == y)
    
    temp <- cbind(pool.year = as.character(year), temp)
    
    atus.pooled <- rbind(atus.pooled, temp)
  }
}

atus.pooled$pool.year <- as.factor(atus.pooled$pool.year)

# Pooled count and means for all employees that worked at home for any time
atus.pooled.anytime.totals <- atus.pooled %>%
  filter(homeworkers & wageworkers) %>%
  group_by(pool.year) %>%
  summarise(
    n = sum(WT06) / 1095,
    mean.mins = sum(workingfromhome * WT06) / sum(WT06)
  )

write_csv(atus.pooled.anytime.totals, "results/atus.pooled.anytime.totals.csv")

# Pooled counts and means by occupation
atus.pooled.anytime.occ.totals <- atus.pooled %>%
  filter(homeworkers & wageworkers) %>%
  group_by(pool.year, occupation) %>%
  summarise(
    n = sum(WT06) / 1095,
    mean.mins = sum(workingfromhome * WT06) / sum(WT06)
  )

write_csv(atus.pooled.anytime.occ.totals, "results/atus.pooled.anytime.occ.totals.csv")
