# DEPRECATED: THIS DOES NOT MAKE SURVEY FASTER

# library(readr)
# library(dplyr)
# library(reshape2)
library(MonetDBLite)
library(DBI)
library(survey)

# Connect to local database
db <- dbConnect(MonetDBLite::MonetDBLite(), dbname="monetdb")

# Load data into table
monetdb.read.csv(db, "usa_00028.csv", "ipums", col.names=c("svyyear", "perwt", "occ2010", "incwage", "tranwork"))

# Add column of ones for weighted countsdbGet
dbGetQuery(db, "ALTER TABLE ipums ADD COLUMN one INTEGER")
dbGetQuery(db, "UPDATE ipums SET one = 1")

# Create survey design
design <- svydesign(
  ids = ~ 1,
  weights = ~ perwt,
  data = "ipums",
  dbtype = "MonetDBLite",
  dbname = "monetdb"
)

ipums.totals <- svyby(~ one, ~ svyyear + tranwork, design, svytotal)

# # Read IPUMS export
# ipums <- read_csv("usa_00028.csv", col_types="cicii")
# 
# # Convert years to a factor
# ipums$year <- as.factor(ipums$YEAR)
# 
# # Convert TRANWORK
# ipums$commute <- NA
# 
# ipums$commute[ipums$TRANWORK >= 10 & ipums$TRANWORK < 20] <- "Car"
# ipums$commute[ipums$TRANWORK >= 30 & ipums$TRANWORK <= 31] <- "Bus"
# ipums$commute[ipums$TRANWORK == 33] <- "Subway"
# ipums$commute[ipums$TRANWORK == 70] <- "Worked from home"
# 
# ipums$commute <- factor(ipums$commute, level=c("Car", "Bus", "Subway", "Worked from home"))
# 
# # Add a column of ones for weighted counts
# ipums$one <- 1
# 
# # Compute totals by year and commute
# # ipums.totals <- ipums %>%
# #   group_by(year, commute) %>%
# #   summarise(pop = sum(PERWT))
# # 
# # ipums.totals.pivot <- dcast(ipums.totals, commute ~ year, value.var="pop")
# 
# # Compute shares by year and commute
# # ipums.shares <- ipums.totals %>%
# #   group_by(year) %>%
# #   mutate(pct = pop / sum(pop)) %>%
# #   ungroup()
# # 
# # ipums.shares.pivot <- dcast(ipums.shares, commute ~ year, value.var="pct")
# 
# # Construct survey design
# design <- svydesign(
#   ids = ~ 1,
#   data = ipums,
#   weights = ~ PERWT
# )
# 
# # Count observations by year and commute
# ipums.totals <- svyby(~ one, ~ year + commute, design, svytotal)
# 
# # Compute mean wages by year and commute
# ipums.means <- svyby(~ INCWAGE, ~ year + commute, design, svymean)
# 
# # Compute median wages by year and commute
# ipums.medians <- svyby(~ INCWAGE, ~ year + commute, design, svyquantile, quantiles=0.5, ci=TRUE)
