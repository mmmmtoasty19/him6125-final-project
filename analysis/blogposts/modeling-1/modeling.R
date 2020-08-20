#' ---
#' author: Kyle Belanger
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#' 
#' ---

#+ include = FALSE
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 


# ---- script-description ------------------------------------------------------

#1. Delta btw years
#2. Detla btw mean of 2010 - 2015 and 2016, 2017
#3. Delta btw three slopes: a)2010 - 2015, b) 2010-2016, c)2010-2017

#4 Model a)2010-2014 b)2010-2015  c) 2010-2016, d)2010-2017


# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------

ds0 <- read_rds("./data-public/derived/diabetes-modeling-data.rds")




ds0 %>% ggplot(aes(x = year, y = diabetes_percentage)) +
  geom_line(aes(group = county_fips), alpha = 0.2)

ds0 %>% ggplot(aes(x = year, y = obesity_percentage)) +
  geom_line(aes(group = county_fips), alpha = 0.2)

ds0 %>% ggplot(aes(x = year, y = inactivity_percentage)) +
  geom_line(aes(group = county_fips), alpha = 0.2)


# ---- compute-measures --------------------------------------------------------

ds_delta <- ds0 %>% 
  select(year, state_abb, state_name, county_name, county_fips, 
         dbpct = diabetes_percentage
         ,obpct = obesity_percentage
         ,inactpct = inactivity_percentage) %>% 
  # filter(county_name %in% c("Albany County","Bladen County") ) %>% 
  arrange(state_abb, county_name) %>% 
  group_by(state_abb, county_name, county_fips) %>% 
  mutate(
    # lag = lag(dbpct, order_by = year),
    # delta_lag = dbpct - lag,
    # lag_positive = delta_lag > 0
    
    lag_db = dbpct - lag(dbpct, order_by = year)
    ,lag_ob = obpct -lag(obpct, order_by = year)
    ,lag_inact = inactpct -lag(inactpct, order_by = year)
  ) %>% 
  ungroup()



ds_delta %>% filter(year == 2016) %>% ggplot(aes(x = lag_ob, y = lag_db)) +
  geom_point(shape = 21) +
  geom_smooth(method = "loess", se = FALSE) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    , vjust = 3
  )
ds_delta %>% ggplot(aes(x = lag_ob, y = lag_db)) +
  geom_point(shape = 21) +
  geom_smooth(method = "loess", se = FALSE) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    , vjust = 3
  ) +
  facet_wrap(~year)

ds_delta %>% filter(year == 2016) %>% ggplot(aes(x = lag_inact, y = lag_db)) +
  geom_point(shape = 21) +
  geom_smooth(method = "loess", se = FALSE) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    , vjust = 3
  )

ds_delta %>% filter(year == 2016) %>% ggplot(aes(x = lag_ob, y = lag_in)) +
  geom_point(shape = 21) +
  geom_smooth(method = "loess", se = FALSE) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    , vjust = 3
  )

ds_delta %>% filter(year == 2016) %>% TabularManifest::histogram_continuous("delta_lag")
ds_delta %>% filter(year == 2016) %>% ggplot(aes(x = delta_lag)) +
  geom_histogram() + 
  facet_wrap(~ state_abb, scales = "free_y")
ds_delta  %>% ggplot(aes(x = lag_db)) +
  geom_histogram() + 
  facet_wrap(~ year)


ds1 <- ds_delta %>% 
  group_by(state_abb, year) %>% 
  select("state_abb","lag_db" ,"lag_ob", "lag_inact","dbpct","obpct","inactpct" ,"year") %>% 
  summarise(
    across(where(is.numeric), mean)
    ,.groups = "keep"
  ) %>% 
  ggplot(aes(x = lag_ob, y = lag_db)) +
  geom_point(shape = 21) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~year)
  


ds2 <-  ds0 %>% 
  select(year, state_abb, state_name, county_name, county_fips, 
         dbpct = diabetes_percentage
         ,obpct = obesity_percentage
         ,inactpct = inactivity_percentage
         ,pct_total_age_20_44                         
         ,pct_total_age_45_64                         
         ,pct_total_age_65_74                         
         ,pct_total_age_75_over ) %>% 
  # filter(county_name %in% c("Albany County","Bladen County") ) %>% 
  arrange(state_abb, county_name) %>% 
  group_by(state_abb, county_name, county_fips) %>% 
  mutate(
    # lag = lag(dbpct, order_by = year),
    # delta_lag = dbpct - lag,
    # lag_positive = delta_lag > 0
    
    lag_db = dbpct - lag(dbpct, order_by = year)
    ,lag_ob = obpct -lag(obpct, order_by = year)
    ,lag_inact = inactpct -lag(inactpct, order_by = year)
    ,lag_20_44    = pct_total_age_20_44 - lag(pct_total_age_20_44, order_by = year)
    ,lag_45_64    = pct_total_age_45_64 - lag(pct_total_age_45_64, order_by = year)
    ,lag_65_74    = pct_total_age_65_74 - lag(pct_total_age_65_74, order_by = year)
    ,lag_over_75  = pct_total_age_75_over - lag(pct_total_age_75_over, order_by = year)
  ) %>% 
  ungroup()

model_1 <- lm(lag_db ~ lag_ob + lag_inact + lag_20_44 + lag_45_64 + lag_65_74 +
                lag_over_75
              ,data = ds2 %>% filter(year == 2016))

model_2 <- lm(lag_db ~ lag_ob + lag_inact + lag_20_44 + lag_45_64 + lag_65_74 +
                lag_over_75
              ,data = ds2 %>% filter(year == 2012)) 

summary(model_2)





  