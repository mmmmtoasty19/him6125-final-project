#' ---
#' author: Kyle Belanger
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#' 
#' ---

#+ include = FALSE
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- knitr-opts --------------------------------------------------------------
#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
knitr::opts_knit$set(root.dir = "../../../")
options(knitr.table.format = "html")

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

ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)

# ---- load-data ---------------------------------------------------------------

ds_diabetes_raw <-  read_rds("./data-public/derived/us-diabetes-data.rds")

# ---- tweak -------------------------------------------------------------------

ds_diabetes <- ds_diabetes_raw %>% 
  left_join(read_csv("./data-public/metadata/county_fips.csv")) %>% 
  dplyr::relocate(
    c("area_name", "state_name", "state_abb")
    ,.after = "county_fips"
    ) %>% 
  rename(county_name = area_name) %>% 
  select(-diabetes_lower_limit, -diabetes_upper_limit)


# ---- delta by year -----------------------------------------------------------

ds_delta <- ds_diabetes %>% 
  rename(dbpct = diabetes_percentage) %>% 
  group_by(state_abb, county_name, county_fips) %>% 
  arrange(state_abb, county_name) %>%
  mutate(
    lag = lag(dbpct, order_by = year),
    delta_lag = dbpct - lag,
  )
 
# UNCOMMENT FOR REPORT PRINTING
 
# ds_delta %>% select(-dbpct, -lag) %>%  
#   pivot_wider(names_from = year ,values_from = delta_lag) %>% 
#   knitr::kable()


# Quick NC Graph

ds_delta %>% 
  filter(state_abb == "NC") %>% 
  ggplot(aes(x = year, y = delta_lag, color = county_fips)) +
  geom_line(aes(group = county_fips), na.rm = TRUE, show.legend = FALSE)


# ---- delta between 2010-2015 & 2016,17 ---------------------------------------

ds1 <- ds_diabetes %>% 
  rename(dbpct = diabetes_percentage) %>% 
  group_by(state_abb, county_name, county_fips) %>% 
  arrange(state_abb, county_name) %>%
  pivot_wider(names_from = year, values_from = dbpct) %>% 
  ungroup() %>% rowwise() %>% 
  mutate(
    `2010-2015_mean` = round(mean(c_across(`2010`:`2015`), na.rm = TRUE),2)
    ,delta_16        = `2016` - `2010-2015_mean`
    ,delta_17        = `2017`- `2016`
  ) %>% 
  select(-`2010`,-`2011`,-`2012`,-`2013`,-`2014`,-`2015`)
  
