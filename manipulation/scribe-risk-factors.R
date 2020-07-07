#' ---
#' author: Kyle Belanger
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#' 
#' ---

#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse) # enables piping : %>% 

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------

national_risk_factors_raw <- read_rds("./data-public/derived/national-diabetes-risk-factors-2010-2020.rds")

nc_diabetes_data_raw <- read_rds("./data-public/derived/nc-diabetes-data.rds")


# ---- tweak-data -------------------------------------------------------------

# only using NC currently

risk_factor_ds <- national_risk_factors_raw %>% 
  filter(state_abbreviation == "NC") %>% 
  filter(name != "North Carolina") %>% 
  mutate(across(name, ~str_remove_all(.," County"))
         ,across(name, tolower))
  

nc_diabetes_ds <- nc_diabetes_data_raw %>% 
  filter(year > 2009) %>% 
  rename_with(tolower) %>% 
  rename(
    diabetes_percentage = percentage
  ) %>% 
  mutate(across(county,tolower))


# ---- merge-data -------------------------------------------------------------

ds_combine <- nc_diabetes_ds %>% 
  left_join(risk_factor_ds, by = c("county" = "name"
                                   ,"year" = "release_year")
            ) %>% 
  select(-countyfips,-state_abbreviation) %>% 
  relocate(diabetes_percentage, .after = county_fips)
  
  
# ---- save-data --------------------------------------------------------------

ds_combine %>% write_rds("./data-public/derived/nc_risk_factors.rds")
ds_combine %>% write_csv("./data-public/derived/nc_risk_factors.csv")

# ---- publish ----------------------------------------------------------------

rmarkdown::render(
  "./manipulation/scribe-risk-factors.R"
)
