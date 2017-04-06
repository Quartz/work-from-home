library(readr)
library(dplyr)
library(reshape2)
library(survey)

# Export 29 from IPUMS
# "Method of travel to work, wages, occupation, class of worker, and usual hours worked (1980, 1990, 2000-2015) w/ CSV"

# Read IPUMS export
ipums.orig <- read_csv("usa_00029.csv", col_types="ciiiiidi")

# Filter to only full-time wageworkers (>= 35 hours per week, with wages, and not self-employed)
ipums <- ipums.orig %>%
  filter(INCWAGE > 0 & INCWAGE < 999998 & CLASSWKR == 2 & UHRSWORK >= 35)

# Convert years to a factor
ipums$year <- as.factor(ipums$YEAR)

# Convert TRANWORK
ipums$commute <- NA

ipums$commute[ipums$TRANWORK >= 10 & ipums$TRANWORK <= 20] <- "Private vehicle"
ipums$commute[ipums$TRANWORK >= 30 & ipums$TRANWORK <= 36] <- "Public transit"
ipums$commute[ipums$TRANWORK == 40] <- "Bicycle"
ipums$commute[ipums$TRANWORK == 50] <- "Walked"
ipums$commute[ipums$TRANWORK == 60] <- "Other"
ipums$commute[ipums$TRANWORK == 70] <- "Worked at home"

ipums$commute <- factor(ipums$commute, level=c("Private vehicle", "Public transit", "Bicycle", "Walked", "Other", "Worked at home"))

# Recode OCC2010 to match ATUS OCC2 coding
ipums$jobs <- NA

ipums$jobs[ipums$OCC2010 >= 10 & ipums$OCC2010 <= 430] <- "Management occupations"
ipums$jobs[ipums$OCC2010 >= 500 & ipums$OCC2010 <= 950] <- "Business and financial operations occupations"
ipums$jobs[ipums$OCC2010 >= 1000 & ipums$OCC2010 <= 1240] <- "Computer and mathematical science occupations"
ipums$jobs[ipums$OCC2010 >= 1300 & ipums$OCC2010 <= 1560] <- "Architecture and engineering occupations"
ipums$jobs[ipums$OCC2010 >= 1600 & ipums$OCC2010 <= 1980] <- "Life, physical, and social science occupations"
ipums$jobs[ipums$OCC2010 >= 2000 & ipums$OCC2010 <= 2060] <- "Community and social service occupations"
ipums$jobs[ipums$OCC2010 >= 2100 & ipums$OCC2010 <= 2150] <- "Legal occupations"
ipums$jobs[ipums$OCC2010 >= 2200 & ipums$OCC2010 <= 2550] <- "Education, training, and library occupations"
ipums$jobs[ipums$OCC2010 >= 2600 & ipums$OCC2010 <= 2920] <- "Arts, design, entertainment, sports, and media occupations"
ipums$jobs[ipums$OCC2010 >= 3000 & ipums$OCC2010 <= 3540] <- "Healthcare practitioner and technical occupations"
ipums$jobs[ipums$OCC2010 >= 3600 & ipums$OCC2010 <= 3650] <- "Healthcare support occupations"
ipums$jobs[ipums$OCC2010 >= 3700 & ipums$OCC2010 <= 3950] <- "Protective service occupations"
ipums$jobs[ipums$OCC2010 >= 4000 & ipums$OCC2010 <= 4150] <- "Food preparation and serving related occupations"
ipums$jobs[ipums$OCC2010 >= 4200 & ipums$OCC2010 <= 4250] <- "Building and grounds cleaning and maintenance occupations"
ipums$jobs[ipums$OCC2010 >= 4300 & ipums$OCC2010 <= 4650] <- "Personal care and service occupations"
ipums$jobs[ipums$OCC2010 >= 4700 & ipums$OCC2010 <= 4965] <- "Sales and related occupations"
ipums$jobs[ipums$OCC2010 >= 5000 & ipums$OCC2010 <= 5940] <- "Office and administrative support occupations"
ipums$jobs[ipums$OCC2010 >= 6005 & ipums$OCC2010 <= 6130] <- "Farming, fishing, and forestry occupations"
ipums$jobs[ipums$OCC2010 >= 6200 & ipums$OCC2010 <= 6940] <- "Construction and extraction occupations"
ipums$jobs[ipums$OCC2010 >= 7000 & ipums$OCC2010 <= 7630] <- "Installation, maintenance, and repair occupations"
ipums$jobs[ipums$OCC2010 >= 7700 & ipums$OCC2010 <= 8965] <- "Production occupations"
ipums$jobs[ipums$OCC2010 >= 9000 & ipums$OCC2010 <= 9750] <- "Transportation and material moving occupations"

ipums$jobs <- as.factor(ipums$jobs)

# Read occupation codes
occ <- read_csv("https://raw.githubusercontent.com/wireservice/lookup/master/occ/description.2010.csv", col_types="ic")

occ$description <- factor(occ$description)

# Read CPI rates
cpi <- read_csv("https://raw.githubusercontent.com/wireservice/lookup/master/year/cpi.csv", col_types="cd")

# Function to perform inflation adjustment
cpi.adjust <- function(input, year.column, columns) {
  temp <- left_join(input, cpi, by = year.column)
  
  for (column in columns) {
    temp[[column]] <- temp[[column]] * 237.0 / temp[["cpi"]]
  }
  
  temp$cpi <- NULL
  
  return(temp)
}

# Compute mean wages by year and commute
# Perfect match for results from IPUMS online
ipums.commute.means <- ipums %>%
  group_by(year, commute) %>%
  summarise(
    pop = sum(PERWT),
    mean.wages = sum(INCWAGE * PERWT) / pop,
    v = (sum(PERWT * (INCWAGE - mean.wages)^2)) / (pop - 1),
    sd = sqrt(v),
    se = sd / sqrt(pop)
  )

ipums.commute.means.cpi <- cpi.adjust(ipums.means, "year", c("mean.wages", "v", "sd", "se"))

write_csv(ipums.commute.means.cpi, "results/ipums.commute.means.cpi.csv")

# Compute mean wages for home workers by year and occupation
ipums.occ.means <- ipums %>%
  filter(commute == "Worked at home") %>%
  group_by(year, OCC2010) %>%
  summarise(
    pop = sum(PERWT),
    mean.wages = sum(INCWAGE * PERWT) / pop,
    v = (sum(PERWT * (INCWAGE - mean.wages)^2)) / (pop - 1),
    sd = sqrt(v),
    se = sd / sqrt(pop)
  ) %>%
  left_join(occ, by = c("OCC2010" = "occ"))

ipums.occ.means.cpi <- cpi.adjust(ipums.occ.means, "year", c("mean.wages", "v", "sd", "se"))

write_csv(ipums.occ.means.cpi, "results/ipums.occ.means.cpi.csv")

# Compute mean wages for home workers by year and job category
ipums.jobs.means <- ipums %>%
  filter(commute == "Worked at home") %>%
  group_by(year, jobs) %>%
  summarise(
    pop = sum(PERWT),
    mean.wages = sum(INCWAGE * PERWT) / pop,
    v = (sum(PERWT * (INCWAGE - mean.wages)^2)) / (pop - 1),
    sd = sqrt(v),
    se = sd / sqrt(pop)
  )

ipums.jobs.means.cpi <- cpi.adjust(ipums.jobs.means.cpi, "year", c("mean.wages", "v", "sd", "se"))

write_csv(ipums.jobs.means.cpi, "results/ipums.jobs.means.cpi.csv")

# Filter to just occupations that have ever had a large number of homeworkers
ipums.jobs.big.means.cpi <- ipums.jobs.means.cpi %>%
  group_by(jobs) %>%
  filter(any(pop >= 200000)) %>%
  ungroup()

write_csv(ipums.jobs.big.means.cpi, "results/ipums.jobs.big.means.cpi.csv")

# Compute mean wages by job category and commute type for 2015 only
ipums.jobs.2015.means <- ipums %>%
  filter(year == 2015) %>%
  group_by(jobs, commute) %>%
  summarise(
    pop = sum(PERWT),
    mean.wages = sum(INCWAGE * PERWT) / pop,
    v = (sum(PERWT * (INCWAGE - mean.wages)^2)) / (pop - 1),
    sd = sqrt(v),
    se = sd / sqrt(pop)
  )

write_csv(ipums.jobs.2015.means, "results/ipums.jobs.2015.means.csv")

# Compute fraction of occupation that homeworks by year
ipums.jobs.shares <- ipums %>%
  group_by(year, jobs, commute) %>%
  summarise(
    pop = sum(PERWT)
  ) %>%
  mutate(
    share = pop / sum(pop)
  )

write_csv(ipums.jobs.shares, "results/ipums.jobs.shares.csv")


