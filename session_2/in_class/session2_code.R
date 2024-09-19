#' ---
#' title: "Lecture 2 code is available here"
#' author: "Dr Yueyang Zhong"
#' ---


# ---------------------------------------- LOAD PACKAGES -------------------------------------------------------
library(tidyverse)
library(mosaic)
library(janitor) # package to allow using function clean_names()
library(here)
library(lubridate)
library(ggrepel) # in order to use geom_text_repel() function inside ggplot2
library(infer)
# ---------------------------------------------------------------------------------------------------------------
# US CRIME EXAMPLE (2014)
# ---------------------------------------------------------------------------------------------------------------

us_crime <- read_csv(here::here("Data", "CrimeOneYearofData.csv")) # 51x8

us_crime <- us_crime %>% # 50x8 (we drop Washington DC)
  filter(State != "District of Columbia") %>% # Exclude row with Washington DC
  clean_names() %>% # resulting col/row names are unique and consist only of the _ character, numbers, and letters.
  mutate(state = str_to_lower(state)) # overwrite 'state' to all states with lower case letters

favstats(~ crime_total, data = us_crime)
#  min      Q1 median       Q3    max     mean       sd  n missing
#  622 5353.25  16042 26807.25 153709 23795.76 29960.49 50       0

# Display states with largest crimes
us_crime %>% 
  arrange(desc(crime_total)) %>% # re-ordering rows of data based on crime_total column in descending order
  View()
# note we only display the result but since we did not overwrite the us_crime variable with our result, we do not change us_crime dataframe itself.
View(us_crime) # note us_crime is unchanged (not ordered).

# --------------------------------------------------------------------------------------------------------------
# Histogram of Crime Total
ggplot(us_crime, aes(x = crime_total)) +
  geom_histogram() +
  geom_vline(xintercept = 16042, colour = 'blue', linewidth = 1) + # add a vertical line at the median value of 16042
  geom_vline(xintercept = 23796, colour = 'red', linewidth = 1) +  # add a vertical line at the mean value of 23796
  labs(title = "Crime in the USA (2014)", 
       x = "Total number of crimes", 
       caption = "Source: FBI Uniform Crime Reporting, https://www.fbi.gov/services/cjis/ucr/") +
  theme_bw() +
  NULL

# Density of Crime Total
ggplot(us_crime, aes(x = crime_total)) +
  geom_density() +
  geom_vline(xintercept = 16042, colour = 'blue', linewidth = 1) + # add a vertical line at the median value of 16042
  geom_vline(xintercept = 23796, colour ='red', linewidth = 1) +   # add a vertical line at the mean value of 23796
  labs(title = "Crime in the USA (2014)", 
       x = "Total number of crimes", 
       caption = "Source: FBI Uniform Crime Reporting, https://www.fbi.gov/services/cjis/ucr/") +
  theme_bw() +
  NULL

# --------------------------------------------------------------------------------------------------------------
# Plot crimes vs population scatterplot with line of best fit and text for states with crime > 30K
ggplot(us_crime, aes(x = population, y = crime_total)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) + # fit a linear model (lm) i.e. draw line of best fit
  geom_hline(yintercept = 23795, color = "red") + # mean crime (horizontal line)
  # add text directly to plot with geom_text_repel(); subset(data, column)
  geom_hline(yintercept = 30000, color = "magenta", linetype = 'dotted') + # line at 30K above which we display names
  geom_text_repel(data = subset(us_crime, crime_total > 30000), # show text for those states with crime > 30K
                  mapping = aes(label = state)) + # the labels are to be given by state column
  labs(title = "Crime in the USA (2014)", 
       x = "Population",
       y = "Total number of crimes", 
       caption = "Note: States with total crime incidents exceeding 30,000 are labeled by their names.") +
  theme_bw()

# --------------------------------------------------------------------------------------------------------------
# Calculate crime per-capita i.e. crime rate (per 100K population)
us_crime <- us_crime %>%
  mutate(crime_rate = 100000 * crime_total / population)#

us_crime <- us_crime %>%
  mutate(crime_level = case_when(crime_rate < 250 ~ 'low',
                                 crime_rate < 400 ~ 'med', 
                                 crime_rate >= 400 ~ 'high'))
us_crime$crime_level <- factor(us_crime$crime_level,
                       levels = c('low','med','high'),ordered = TRUE)

# Arrange in descending order
us_crime %>% 
  select(state, crime_rate, crime_level) %>% 
  arrange(desc(crime_rate)) %>% 
  View()

# Summary stats
favstats(~ crime_rate, data = us_crime)
#      min       Q1   median       Q3      max     mean       sd  n missing
#  99.2719 259.7249 325.1066 421.9927 635.7807 346.8088 128.8192 50       0

# Histogram of Violent Crime RATE
ggplot(us_crime, aes(x = crime_rate)) +
  geom_histogram() +
  geom_vline(xintercept = 325.1, colour = 'blue', size = 1) + # add a vertical line at the median value
  geom_vline(xintercept = 346.8, colour = 'red', size = 1) +  # add a vertical line at the mean value 
    labs(title = "Crime Rate Historgram, USA (2014)", 
       x ="Rate of violent crimes per 100K", 
       caption = "Source: FBI Uniform Crime Reporting, https://www.fbi.gov/services/cjis/ucr/") +
  theme_bw() +
  NULL

# Density of Violent Crime RATE
ggplot(us_crime, aes(x = crime_rate)) +
  geom_density() +
  geom_vline(xintercept = 325.1, colour = 'blue', size = 1) + #add a vertical line at the median value
  geom_vline(xintercept = 346.8, colour = 'red', size = 1) + #add a vertical line at the mean value 
  labs(title = "Violent Crime Rate Density, USA (2014)", 
       x = "Violent Crime Rate per 100K", 
       caption = "Source: FBI Uniform Crime Reporting, https://www.fbi.gov/services/cjis/ucr/")+
  theme_bw() +
  NULL

# Boxplot of Violent Crime RATE by crime_level
ggplot(us_crime, aes(x = crime_level, y = crime_rate)) +
  geom_boxplot() +
  theme_bw() +
  NULL

# --------------------------------------------------------------------------------------------------------------
# Plot crimes vs population scatterplot
ggplot(us_crime, aes(x = population, y = crime_rate)) +
  geom_point() +
  # geom_smooth(method = 'lm') +
  geom_smooth(method = 'lm', se = FALSE) + # fit a linear model (lm) i.e. draw line of best fit
  geom_hline(yintercept = 346.8, color = "red") +
  geom_text_repel(data = subset(us_crime, crime_rate > 400),
                  mapping = aes(label = state)) +
  geom_hline(yintercept = 400, color = "magenta", linetype = 'dotted') +
  labs(title = "Crime Rate, USA (2014)", 
       x = "Population",
       y = "Violent Crime Rate per 100K", 
       caption = "Note: States with crime rate exceeding 400 are labeled by their names.") +
  theme_bw()

# --------------------------------------------------------------------------------------------------------------
# Calculate Z scores for violent per-capita violent crime rate (per 100K population)
# let's take a look at what is the mean and std dev for crime_rate
us_crime %>%
  summarise(mu_crime = mean(crime_rate), sd_crime = sd(crime_rate)) # mu = 347; sd = 129

# obtain a new column with z scores for crime_rate (convert from X -> Z)
us_crime <- us_crime %>%
  mutate(crime_rate_z = (crime_rate - mean(crime_rate)) / sd(crime_rate)) 

us_crime %>% 
  select(state, population, crime_total, crime_rate, crime_rate_z) %>% 
  View()

# --------------------------------------------------------------------------------------
# HIRING POLICY EXAMPLE / NORMAL DISTRIBUTION / ZSCORES 
# --------------------------------------------------------------------------------------

# Test scores are normally distributed with a mean of 525 and standard deviation of 55. 
# Automatic accepts have exam score >= 600 and automatic rejects score <= 425
# Calculate the percentage of applicants who are automatic accepted and rejected
mosaic::xpnorm(600, mean = 525, sd = 55) # For those automatically accepted
mosaic::xpnorm(425, mean = 525, sd = 55) # For those automatically rejected

# --------------------------------------------------------------------------------------
# If we wanted to automatically accept 15% and automatically reject 10%, we are doing
# the inverse calculation, namely finding the Z that corresponds to the 85th and 10th 
# percentile, respectively
# Find the Z value that corresponds to the 85th percentile
qnorm(0.85, lower.tail = TRUE) # by default value is TRUE i.e. p(Z<z) = 0.85
qnorm(0.85) 
# if False is used then gives symmetrical value on the other side of the distribution 
qnorm(0.85, lower.tail = FALSE) # i.e. gives same value as the 15th percentile i.e. P(Z>z) = 0.85
qnorm(0.15) 
# If the mean = 525, and the sd = 55, the X values can be found as 
qnorm(0.85, mean = 525, sd = 55) 
qnorm(0.10, mean = 525, sd = 55) 

# --------------------------------------------------------------------------------------
# FIRESTONE CASE STUDY 
# --------------------------------------------------------------------------------------
# Different from the case discussed in the lecture, the data here only contains monthly snowfall records from January 1940 to December 2006. 
# So the results will exhibit a slight difference. 
toronto <- read_csv(here::here("Data", "toronto_snow.csv")) %>% 
  
  #add a new column with month_name rather than number
  #uses lubridate::month() function
  mutate(month_name=month(month, label = TRUE))

# Calculate the yearly totals by grouping by year
snow_totals <- toronto %>% 
  group_by(year) %>% 
  summarise(total = sum(snowfall))

# Plot a normal distribution (with same mean and std) curve on top of the density plot 
ggplot(snow_totals, aes(total)) +
  geom_density()+
  stat_function(
    fun = dnorm, 
    args = list(mean = mean_snow, sd = sd_snow), 
    lwd = 2, 
    col = 'red'
  )

# Calculate the thresholds below which the different levels of refund apply
refund100 <- 0.2 * mean_snow
refund75  <- 0.3 * mean_snow
refund50  <- 0.4 * mean_snow

xpnorm(refund100, mean = mean_snow, sd = sd_snow)
xpnorm(refund75, mean = mean_snow, sd = sd_snow)
xpnorm(refund50, mean = mean_snow, sd = sd_snow)

# Boostrap to construct confidence interval for snowfall in Jan
set.seed(1234)

boot_snowfall <- toronto %>%
  # Select January snowfall
  filter(month_name == "Jan") %>%
  # Specify the variable of interest
  specify(response = snowfall) %>%
  # Generate a bunch of bootstrap samples
  generate(reps = 1000, type = "bootstrap") %>%
  # Find the mean of each sample
  calculate(stat = "mean")

# 95% CI is the middle 95% of bootstrap distribution
percentile_ci <- boot_snowfall %>%
  get_confidence_interval(level = 0.95, type = "percentile")
percentile_ci  # [32.2, 42.3]


