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


# ---- knitr-opts --------------------------------------------------------------
#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
knitr::opts_knit$set(root.dir = "../")

# ---- load-sources ------------------------------------------------------------

#' # Load Packages
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: 
# http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

# ---- declare-globals ---------------------------------------------------------

#' # Load Data
# ---- load-data ---------------------------------------------------------------

ds_rural_housing_2000 <- read_rds(
  "./data-public/derived/percentage-rural-2000.rds"
  ) 
ds_rural_housing_2010 <- read_rds(
  "./data-public/derived/percentage-rural-2010.rds"
  ) 
ds_pop_2000    <- read_rds(
  "./data-public/derived/us-pop-estimate-2000-2009.rds"
  )
ds_pop_2010    <- read_rds(
  "./data-public/derived/us-pop-estimate-2010-2019.rds"
  )

ds_obesity     <- read_rds("./data-public/derived/us-obesity-data.rds")
ds_inactivity  <- read_rds("./data-public/derived/us-inactivity-data.rds")

ds_us_diabetes <- read_rds("./data-public/derived/us-diabetes-data.rds")


#' # Join Data
# ---- join-data ---------------------------------------------------------------

ds0 <- ds_pop_2000 %>% 
  bind_rows(ds_pop_2010) %>% 
  mutate(across(year, ~as.character(.) %>% as.numeric(.))) %>% 
  left_join(ds_obesity) %>% 
  left_join(ds_inactivity) %>% 
  left_join(ds_us_diabetes) %>% 
  filter(year > 2003, year < 2018) %>% 
  filter(!str_detect(county_fips, "^02|^15")) %>% 
  left_join(read_csv("./data-public/metadata/county_fips.csv")) %>% 
  relocate(c("area_name", "state_name", "state_abb"),.after = "county_fips") %>% 
  rename("county_name" = "area_name")

  

# ---- split-data --------------------------------------------------------------

ds2000 <- ds0 %>% 
  filter(year < 2010) %>% 
  left_join(ds_rural_housing_2000)


ds2010 <- ds0 %>% 
  filter(year > 2009) %>% 
  left_join(ds_rural_housing_2010)


ds1 <- ds2000 %>% 
  bind_rows(ds2010)

#' # Save to Disk
# ---- save-data ---------------------------------------------------------------

ds1 %>% write_rds(
  "./data-public/derived/diabetes-modeling-data.rds"
  ,compress = "gz")

ds1 %>% write_csv(
  gzfile("./data-public/derived/diabetes-modeling-data.csv.gz")
  )


