#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse) # enables piping : %>% 

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------

ds_risk_factors <- read_rds("./data-public/derived/nc_risk_factors.rds")
ds_population   <- read_rds("./data-public/derived/combined-population-data.rds")
rural_counties  <- read_csv("./data-public/metadata/rural-counties.csv")

# ---- merge-data --------------------------------------------------------------

ds_risk_factors_population <- ds_population %>% 
  mutate(
    across(county, trimws)
    ,across(county, tolower) 
  ) %>% 
  relocate(year) %>% 
  left_join(ds_risk_factors) %>% 
  mutate(
    rural = county %in% tolower(rural_counties$rural_counties)
  ) %>% 
  relocate(
    c(county_fips,diabetes_percentage)
    ,.after = county
  )

# ---- save-data --------------------------------------------------------------

ds_risk_factors_population %>% write_rds("./data-public/derived/population-risk-factors.rds", compress = "gz")


ds_risk_factors_population %>% write_csv("./data-public/derived/population-risk-factors.csv")
