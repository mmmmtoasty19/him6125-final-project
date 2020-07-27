#' ---
#' author: Kyle Belanger
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#' 
#' ---

#+ include = FALSE
# These first few lines run only when the file is run in RStudio, 
# !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------


# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: 
# http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------

# describe row in each data set

ds_rural_housing <- read_rds("./data-public/derived/percentage-rural.rds") 
ds_risk_factors  <- read_rds(
  "./data-public/derived/national-diabetes-risk-factors-2010-2020.rds")
ds_population    <- read_rds(
  "./data-public/derived/us-county-population-estimates-v2.rds")
ds_us_diabetes   <- read_rds("./data-public/derived/us-diabetes-data.rds")

# ---- filter-states -----------------------------------------------------------

ds_list0 <- list(
  rural_housing = ds_rural_housing
  ,risk_factors = ds_risk_factors
  ,population   = ds_population
  ,diabetes     = ds_us_diabetes
  )

# remove alaska and hawaii
ds_list1 <- ds_list0 %>% map(~filter(.,!str_detect(county_fips, "^02|^15")))

# ---- join-data ---------------------------------------------------------------

ds0 <- ds_list1[["population"]] %>% 
  mutate(across(year, ~as.character(.) %>% as.numeric(.))) %>% 
  left_join(ds_list1[["risk_factors"]]) %>% 
  left_join(ds_list1[["diabetes"]]) %>% 
  left_join(ds_list1[["rural_housing"]])

# ---- save-data ---------------------------------------------------------------

ds0 %>% write_rds(
  "./data-public/derived/diabetes-modeling.rds"
  ,compress = "gz")

ds0 %>% write_csv("./data-public/derived/diabetes-modeling.csv")


