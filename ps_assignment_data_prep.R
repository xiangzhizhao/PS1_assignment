install.packages(c(
  "data.table",
  "collapse",
  "fixest",
  "here",
  "pacman",
  "ggplot2"
))

# ============================================================
# 01_clean_data.R
# Construct analytic CPS sample for 2007 and 2024
# ============================================================

library(data.table)

# Read raw IPUMS CPS data
dt <- fread("raw_data/cps_00001.csv")

# Basic checks
dim(dt)
names(dt)
table(dt$YEAR)

# ------------------------------------------------------------
# 2. Restrict sample to ages 16-65
# ------------------------------------------------------------

n_initial <- nrow(dt)

dt <- dt[AGE >= 16 & AGE <= 65]

n_after_age <- nrow(dt)

cat("Initial N:", n_initial, "\n")
cat("After age restriction:", n_after_age, "\n")
cat("Dropped:", n_initial - n_after_age, "\n")

table(dt$YEAR)


# ------------------------------------------------------------
# 3. Inspect class of worker
# ------------------------------------------------------------

table(dt$CLASSWKR, useNA = "ifany")

# Keep wage and salary workers
dt <- dt[CLASSWKR %in% c(22, 23, 25, 27, 28)]

cat("After class-of-worker restriction:", nrow(dt), "\n")


# ------------------------------------------------------------
# 4. Inspect usual weekly hours
# ------------------------------------------------------------

table(dt$UHRSWORK1, useNA = "ifany")
table(dt$UHRSWORKORG, useNA = "ifany")

# Check fallback hours for people whose usual hours vary
sum(dt$UHRSWORK1 == 997)

table(
  dt[UHRSWORK1 == 997, UHRSWORKORG],
  useNA = "ifany"
)

sum(
  dt$UHRSWORK1 == 997 &
    dt$UHRSWORKORG >= 1 &
    dt$UHRSWORKORG <= 99
)
# ------------------------------------------------------------
# 5. Construct usual weekly hours
# ------------------------------------------------------------

n_before_hours <- nrow(dt)

# Start with UHRSWORK1
dt[, weekly_hours := as.numeric(UHRSWORK1)]

# If UHRSWORK1 says "hours vary" (997),
# use UHRSWORKORG when it contains valid hours
dt[
  UHRSWORK1 == 997 &
    UHRSWORKORG >= 1 &
    UHRSWORKORG <= 99,
  weekly_hours := as.numeric(UHRSWORKORG)
]

# Valid weekly hours are 1-99
dt[
  weekly_hours < 1 | weekly_hours > 99,
  weekly_hours := NA_real_
]

# Drop observations without usable hours
dt <- dt[!is.na(weekly_hours)]

n_after_hours <- nrow(dt)

cat("Before hours cleaning:", n_before_hours, "\n")
cat("After hours cleaning:", n_after_hours, "\n")
cat("Dropped because of unusable hours:",
    n_before_hours - n_after_hours, "\n")

# Check final hours variable
summary(dt$weekly_hours)


# ------------------------------------------------------------
# 6. Inspect earnings allocation flags
# ------------------------------------------------------------

table(dt$PAIDHOUR, useNA = "ifany")

table(dt$QHOURWAG, useNA = "ifany")

table(dt$QEARNWEE, useNA = "ifany")

# ------------------------------------------------------------
# 7. Drop allocated earnings and construct hourly wage
# ------------------------------------------------------------

n_before_earnings <- nrow(dt)

# Keep non-allocated earnings relevant to each worker's wage measure
dt <- dt[
  (PAIDHOUR == 2 & QHOURWAG == 0) |
    (PAIDHOUR == 1 & QEARNWEE == 0)
]

n_after_allocation <- nrow(dt)

cat("Before allocation restriction:", n_before_earnings, "\n")
cat("After allocation restriction:", n_after_allocation, "\n")
cat("Dropped allocated earnings:",
    n_before_earnings - n_after_allocation, "\n")

# Construct nominal hourly wage
dt[, hourly_wage := NA_real_]

# Hourly-paid workers: reported hourly wage
dt[
  PAIDHOUR == 2,
  hourly_wage := HOURWAGE2
]

# Non-hourly workers: weekly earnings / usual weekly hours
dt[
  PAIDHOUR == 1,
  hourly_wage := EARNWEEK2 / weekly_hours
]

# Inspect before dropping invalid wages
summary(dt$hourly_wage)

# Check remaining sample by payment type
table(dt$PAIDHOUR)

# Inspect wage variables
summary(dt[PAIDHOUR == 2, HOURWAGE2])
summary(dt[PAIDHOUR == 1, EARNWEEK2])

# Count zero/missing-like constructed wages
sum(dt$hourly_wage == 0, na.rm = TRUE)
sum(is.na(dt$hourly_wage))

# ------------------------------------------------------------
# 8. Drop missing or zero wages
# ------------------------------------------------------------

n_before_wage <- nrow(dt)

dt <- dt[
  !is.na(hourly_wage) &
    hourly_wage > 0
]

cat("Dropped missing/zero wages:",
    n_before_wage - nrow(dt), "\n")

cat("After dropping missing/zero wages:",
    nrow(dt), "\n")

##
# ------------------------------------------------------------
# 9. Convert wages to 2024 dollars using CPI-U
# ------------------------------------------------------------

cpi_2007 <- 207.342
cpi_2024 <- 313.689

cpi_factor <- cpi_2024 / cpi_2007

cat("2007-to-2024 CPI adjustment factor:",
    cpi_factor, "\n")

# Start with nominal hourly wage
dt[, real_wage := hourly_wage]

# Inflate 2007 wages to 2024 dollars
dt[
  YEAR == 2007,
  real_wage := hourly_wage * cpi_factor
]

# 2024 wages remain unchanged

###3
# ------------------------------------------------------------
# 10. Restrict real hourly wages to $2-$300
# ------------------------------------------------------------

n_before_wage_cutoff <- nrow(dt)

dt <- dt[
  real_wage >= 2 &
    real_wage <= 300
]

cat("Dropped by $2-$300 wage restriction:",
    n_before_wage_cutoff - nrow(dt), "\n")

cat("Final N after wage cleaning:",
    nrow(dt), "\n")

summary(dt$real_wage)

table(dt$YEAR)

#######3
# ------------------------------------------------------------
# 11. Construct survey sample weight
# ------------------------------------------------------------

dt[, sample_weight := EARNWT * weekly_hours]

summary(dt$sample_weight)

sum(is.na(dt$sample_weight))
sum(dt$sample_weight <= 0)


# ------------------------------------------------------------
# 12. Inspect education
# ------------------------------------------------------------

table(dt$EDUC, useNA = "ifany")

sort(unique(dt$EDUC))

#####
# ------------------------------------------------------------
# 13. Construct education categories
# ------------------------------------------------------------

dt[, educ_group := NA_character_]

# Less than high school
dt[
  EDUC %in% c(2, 10, 20, 30, 40, 50, 60, 71),
  educ_group := "Less than high school"
]

# High school
dt[
  EDUC == 73,
  educ_group := "High school"
]

# Some college or associate degree
dt[
  EDUC %in% c(81, 91, 92),
  educ_group := "Some college / Associate"
]

# Bachelor's degree or more
dt[
  EDUC %in% c(111, 123, 124, 125),
  educ_group := "Bachelor's or more"
]

# Check
table(dt$educ_group, useNA = "ifany")


####
# ------------------------------------------------------------
# 14. Construct years of schooling and potential experience
# ------------------------------------------------------------

dt[, schooling_years := fcase(
  EDUC == 2,   0,
  EDUC == 10,  4,
  EDUC == 20,  6,
  EDUC == 30,  8,
  EDUC == 40,  9,
  EDUC == 50, 10,
  EDUC == 60, 11,
  EDUC == 71, 12,
  EDUC == 73, 12,
  EDUC == 81, 13,
  EDUC %in% c(91, 92), 14,
  EDUC == 111, 16,
  EDUC == 123, 18,
  EDUC == 124, 19,
  EDUC == 125, 20,
  default = NA_real_
)]

# Check schooling years
table(dt$schooling_years, useNA = "ifany")

# Potential experience
dt[, experience := AGE - schooling_years - 5]

summary(dt$experience)

# Check problematic values
sum(is.na(dt$experience))
sum(dt$experience < 0, na.rm = TRUE)

###
# ------------------------------------------------------------
# 15. Inspect demographic variables
# ------------------------------------------------------------

table(dt$SEX, useNA = "ifany")
table(dt$RACE, useNA = "ifany")
table(dt$HISPAN, useNA = "ifany")
table(dt$MARST, useNA = "ifany")
table(dt$UNION, useNA = "ifany")
table(dt$METRO, useNA = "ifany")
table(dt$REGION, useNA = "ifany")

###
# ------------------------------------------------------------
# 16. Construct demographic indicators
# ------------------------------------------------------------

# Female
dt[, female := as.integer(SEX == 2)]

# Black only
dt[, black := as.integer(RACE == 200)]

# ------------------------------------------------------------
# 21. Check variables for regression sample
# ------------------------------------------------------------

names(dt)

# Check key constructed variables
summary(dt[, .(
  hourly_wage,
  log_wage,
  AGE,
  schooling_years,
  experience,
  female,
  black,
  asian,
  hispanic,
  married,
  part_time,
  metro,
  unionized
)])

# Asian only
dt[, asian := as.integer(RACE == 651)]

# Hispanic
dt[, hispanic := as.integer(HISPAN != 0)]

# Married
dt[, married := as.integer(MARST %in% c(1, 2))]

# Part-time: usual weekly hours under 35
dt[, part_time := as.integer(weekly_hours < 35)]

# Metropolitan residence
# 1 = non-metro; 2/3/4 = metro; 0 = not identified
dt[, metro := fcase(
  METRO == 1, 0L,
  METRO %in% c(2, 3, 4), 1L,
  default = NA_integer_
)]

# ------------------------------------------------------------
# 17. Construct Census region
# ------------------------------------------------------------

dt[, census_region := fcase(
  REGION %in% c(11, 12), "Northeast",
  REGION %in% c(21, 22), "Midwest",
  REGION %in% c(31, 32, 33), "South",
  REGION %in% c(41, 42), "West",
  default = NA_character_
)]

dt[, census_region := factor(
  census_region,
  levels = c("Northeast", "Midwest", "South", "West")
)]

table(dt$census_region, useNA = "ifany")

# ------------------------------------------------------------
# 18. Construct union indicator
# ------------------------------------------------------------

dt[, unionized := as.integer(UNION %in% c(2, 3))]

# Check
table(dt$unionized)

# ------------------------------------------------------------
# 19. Inspect industry and occupation codes
# ------------------------------------------------------------

range(dt$IND)
range(dt$OCC)

length(unique(dt$IND))
length(unique(dt$OCC))

sort(unique(dt$IND))[1:20]
sort(unique(dt$OCC))[1:20]

####
# # ------------------------------------------------------------
# 20. Construct year-specific industry and occupation controls
# ------------------------------------------------------------

# Industry: distinguish the coding systems across years
dt[, industry_fe := paste0(YEAR, "_", IND)]

dt[, occupation_fe := paste0(YEAR, "_", OCC)]

dt[, industry_fe := factor(industry_fe)]
dt[, occupation_fe := factor(occupation_fe)]

# Check year-specific industry and occupation controls
head(dt[, .(YEAR, IND, industry_fe, OCC, occupation_fe)], 20)

length(unique(dt$industry_fe))
length(unique(dt$occupation_fe))

sum(is.na(dt$occupation_fe))
sum(is.na(dt$industry_fe))
sum(is.na(dt$occupation_fe))

# Check wage-related variable names
names(dt)[grepl("wage|earn", names(dt), ignore.case = TRUE)]
# ------------------------------------------------------------
# 21. Check variables for regression sample
# ------------------------------------------------------------

names(dt)

# Check key constructed variables
summary(dt[, .(
  hourly_wage,
  log_wage,
  AGE,
  schooling_years,
  experience,
  female,
  black,
  asian,
  hispanic,
  married,
  part_time,
  metro,
  unionized
  
  # Log real hourly wage
  dt[, log_wage := log(real_wage)]
)])

summary(dt$real_wage)
summary(dt$log_wage)

sum(is.na(dt$log_wage))
sum(is.infinite(dt$log_wage))

#######3
# ------------------------------------------------------------
# 21. Check variables for regression sample
# ------------------------------------------------------------

summary(dt[, .(
  log_wage,
  AGE,
  schooling_years,
  experience,
  female,
  black,
  asian,
  hispanic,
  married,
  part_time,
  metro,
  unionized
)])

# Check missing values
dt[, sapply(.SD, function(x) sum(is.na(x))),
   .SDcols = c(
     "log_wage",
     "AGE",
     "schooling_years",
     "experience",
     "female",
     "black",
     "asian",
     "hispanic",
     "married",
     "part_time",
     "metro",
     "unionized"
   )]

# Check fixed-effect variables
length(unique(dt$industry_fe))
length(unique(dt$occupation_fe))

table(dt$YEAR)
table(dt$census_region, useNA = "ifany")


reg_vars <- c(
  "log_wage",
  "AGE",
  "schooling_years",
  "experience",
  "female",
  "black",
  "asian",
  "hispanic",
  "married",
  "part_time",
  "metro",
  "unionized",
  "census_region",
  "industry_fe",
  "occupation_fe"
)

sum(complete.cases(dt[, ..reg_vars]))




########
# ------------------------------------------------------------
# 22. Construct final regression sample
# ------------------------------------------------------------

dt_reg <- dt[complete.cases(dt[, ..reg_vars])]

# Check final sample size
nrow(dt_reg)

# Check sample by year
table(dt_reg$YEAR)

# Check that regression variables have no missing values
dt_reg[, sapply(.SD, function(x) sum(is.na(x))),
       .SDcols = reg_vars]

#####3
# ------------------------------------------------------------
# 23. Descriptive statistics by year
# ------------------------------------------------------------

desc_vars <- c(
  "real_wage",
  "log_wage",
  "AGE",
  "schooling_years",
  "experience",
  "female",
  "black",
  "asian",
  "hispanic",
  "married",
  "part_time",
  "metro",
  "unionized"
)

# Mean by year
desc_mean <- dt_reg[
  , lapply(.SD, mean),
  by = YEAR,
  .SDcols = desc_vars
]

desc_mean


# Sample size by year
dt_reg[, .N, by = YEAR]

# ------------------------------------------------------------
# 24. Baseline wage regressions
# ------------------------------------------------------------

library(fixest)

# Baseline Mincer regression
m1 <- feols(
  log_wage ~ schooling_years + experience + I(experience^2),
  data = dt_reg
)

summary(m1)


#######3
# ------------------------------------------------------------
# 25. Add demographic and job controls
# ------------------------------------------------------------

m2 <- feols(
  log_wage ~ schooling_years +
    experience + I(experience^2) +
    female + black + asian + hispanic +
    married + part_time + metro + unionized,
  data = dt_reg
)

summary(m2)

######3
# ------------------------------------------------------------
# 25. Add demographic and job controls
# ------------------------------------------------------------

m2 <- feols(
  log_wage ~ schooling_years +
    experience + I(experience^2) +
    female + black + asian + hispanic +
    married + part_time + metro + unionized,
  data = dt_reg
)

summary(m2)

########3
# ------------------------------------------------------------
# 26. Add Census region controls
# ------------------------------------------------------------

m3 <- feols(
  log_wage ~ schooling_years +
    experience + I(experience^2) +
    female + black + asian + hispanic +
    married + part_time + metro + unionized +
    census_region,
  data = dt_reg
)

summary(m3)


#########
# ------------------------------------------------------------
# 27. Add industry and occupation fixed effects
# ------------------------------------------------------------

m4 <- feols(
  log_wage ~ schooling_years +
    experience + I(experience^2) +
    female + black + asian + hispanic +
    married + part_time + metro + unionized +
    census_region |
    industry_fe + occupation_fe,
  data = dt_reg
)

summary(m4)

# ------------------------------------------------------------
# 28. Compare returns to schooling: 2007 vs 2024
# ------------------------------------------------------------

m5 <- feols(
  log_wage ~ schooling_years * factor(YEAR) +
    experience + I(experience^2) +
    female + black + asian + hispanic +
    married + part_time + metro + unionized +
    census_region |
    industry_fe + occupation_fe,
  data = dt_reg
)

summary(m5)



######3
# ------------------------------------------------------------
# 29. Calculate schooling return in 2007 and 2024
# ------------------------------------------------------------

# 2007
coef(m5)["schooling_years"]

# Change from 2007 to 2024
coef(m5)["schooling_years:factor(YEAR)2024"]

# 2024 total schooling return
coef(m5)["schooling_years"] +
  coef(m5)["schooling_years:factor(YEAR)2024"]


##########
# Test whether schooling return changed between 2007 and 2024
wald(
  m5,
  "schooling_years:factor(YEAR)2024 = 0"
)


#######
# ------------------------------------------------------------
# 30. Test change in return to schooling
# ------------------------------------------------------------

coeftable(m5)["schooling_years:factor(YEAR)2024", ]


# ------------------------------------------------------------
# 31. Final regression table
# ------------------------------------------------------------

etable(
  m1, m2, m3, m4, m5,
  digits = 3,
  fitstat = ~ n + ar2
)

########3
# ------------------------------------------------------------
# 32. Export final regression table
# ------------------------------------------------------------

etable(
  m1, m2, m3, m4, m5,
  tex = TRUE,
  file = "Table2_regression_results.tex",
  digits = 3,
  fitstat = ~ n + ar2
)


#######3
# ------------------------------------------------------------
# 33. Table 1: Descriptive statistics
# ------------------------------------------------------------

desc_vars <- c(
  "real_wage",
  "schooling_years",
  "experience",
  "female",
  "black",
  "asian",
  "hispanic",
  "married",
  "part_time",
  "metro",
  "unionized"
)

table1 <- dt_reg[, c(
  lapply(.SD, mean),
  lapply(.SD, sd)
), by = YEAR, .SDcols = desc_vars]

table1


# ------------------------------------------------------------
# 34. Format Table 1
# ------------------------------------------------------------

table1_final <- rbindlist(lapply(c(2007, 2024), function(y) {
  
  d <- dt_reg[YEAR == y]
  
  data.table(
    variable = desc_vars,
    mean = sapply(d[, ..desc_vars], mean),
    sd   = sapply(d[, ..desc_vars], sd),
    year = y
  )
}))

table1_final <- dcast(
  table1_final,
  variable ~ year,
  value.var = c("mean", "sd")
)

# Reorder columns
setcolorder(
  table1_final,
  c("variable", "mean_2007", "sd_2007",
    "mean_2024", "sd_2024")
)

# Round
table1_final[, 2:5 := lapply(.SD, round, 3),
             .SDcols = 2:5]

table1_final

fwrite(
  table1_final,
  "Table1_descriptive_statistics.csv"
)


######3
# ------------------------------------------------------------
# 32. Wage distribution statistics: 2007 vs 2024
# ------------------------------------------------------------

wage_stats <- dt_reg[, .(
  N = .N,
  mean_wage = mean(real_wage),
  sd_wage = sd(real_wage),
  median_wage = median(real_wage),
  p10 = as.numeric(quantile(real_wage, 0.10)),
  p25 = as.numeric(quantile(real_wage, 0.25)),
  p50 = as.numeric(quantile(real_wage, 0.50)),
  p75 = as.numeric(quantile(real_wage, 0.75)),
  p90 = as.numeric(quantile(real_wage, 0.90)),
  mean_log_wage = mean(log_wage),
  sd_log_wage = sd(log_wage)
), by = YEAR]

wage_stats

#######
# ------------------------------------------------------------
# 33. Wage distribution figures
# ------------------------------------------------------------

library(ggplot2)

# Density of log real hourly wages
p_density <- ggplot(
  dt_reg,
  aes(x = log_wage, color = factor(YEAR))
) +
  geom_density(linewidth = 1) +
  labs(
    x = "Log Real Hourly Wage",
    y = "Density",
    color = "Year"
  ) +
  theme_minimal()

p_density

ggsave(
  "Figure1_log_wage_density.png",
  p_density,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "Figure1_log_real_wage_density.png",
  p_density,
  width = 7,
  height = 5,
  dpi = 300
)


# ------------------------------------------------------------
# 34. CDF of real hourly wages
# ------------------------------------------------------------

p_cdf <- ggplot(
  dt_reg,
  aes(x = real_wage, color = factor(YEAR))
) +
  stat_ecdf(linewidth = 1) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(
    x = "Real Hourly Wage",
    y = "Cumulative Probability",
    color = "Year"
  ) +
  theme_minimal()

p_cdf

ggsave(
  "Figure2_real_wage_cdf.png",
  p_cdf,
  width = 7,
  height = 5,
  dpi = 300
)
ggsave(
  "Figure2_real_wage_ECDF.png",
  width = 7,
  height = 5,
  dpi = 300
)


names(dt_reg)[grepl("weight|wt", names(dt_reg), ignore.case = TRUE)]

#########
# ------------------------------------------------------------
# 32. Propensity score reweighting
# ------------------------------------------------------------

# 1 = 2007, 0 = 2024
dt_reg[, year2007 := as.integer(YEAR == 2007)]

table(dt_reg$year2007, dt_reg$YEAR)

ps_model <- glm(
  year2007 ~ AGE +
    schooling_years +
    female +
    black +
    asian +
    hispanic +
    married +
    part_time +
    metro +
    unionized +
    census_region +
    factor(IND) +
    factor(OCC),
  data = dt_reg,
  family = binomial(link = "logit"),
  weights = sample_weight
)

summary(ps_model)


#####3
dt_reg[, pscore := predict(ps_model, type = "response")]

summary(dt_reg$pscore)

dt_reg[
  ,
  .(
    N = .N,
    mean_pscore = mean(pscore),
    min_pscore = min(pscore),
    p1 = quantile(pscore, 0.01),
    p50 = quantile(pscore, 0.50),
    p99 = quantile(pscore, 0.99),
    max_pscore = max(pscore)
  ),
  by = YEAR
]

dt_reg[, pscore := predict(ps_model, type = "response")]

dt_reg[, pscore := predict(ps_model, type = "response")]

# ------------------------------------------------------------
# 34. Check propensity score
# ------------------------------------------------------------

summary(dt_reg$pscore)

sum(is.na(dt_reg$pscore))

dt_reg[, .(
  N = .N,
  mean_ps = mean(pscore),
  sd_ps = sd(pscore),
  min_ps = min(pscore),
  p1 = quantile(pscore, 0.01),
  p5 = quantile(pscore, 0.05),
  median_ps = median(pscore),
  p95 = quantile(pscore, 0.95),
  p99 = quantile(pscore, 0.99),
  max_ps = max(pscore)
), by = YEAR]