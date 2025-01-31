0-greeter-county-rankings-national.R
================
Kyle Belanger
07/27/2020

# Load Packages

``` r
# Attach these packages so their functions don't need to be qualified: 
# http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)  
```

# Declare Globals

``` r
folder_path <- "./data-unshared/raw/county-health-rankings-national"

import_data <- function(path_folder){
  files <- list.files(path_folder, full.names = TRUE)
  dto   <- list()
  
  for(item_i in seq_along(files)){
    item_path <- files[item_i]
    item_name <- item_path %>% basename() %>% 
      stringr::str_remove_all(".csv") %>% 
      tolower()
    item_year <- item_name %>% str_sub(start = 14, end = 17)
    
    col_names <- read_csv(item_path, n_max = 0) %>% colnames() %>% janitor::make_clean_names()
    
    
    
    dto[[item_name]] <- read_csv(item_path, skip = 2, col_names = col_names )
  }
  return(dto)
}
```

# Load Data

``` r
county_rankings <- import_data(folder_path)
```

# Filter Data

``` r
# filter data for diabetes risk factors, other additional factors

risk_factors <- list()


for(item in seq_along(county_rankings)){
  name <- names(county_rankings[item])
  d <- county_rankings[[item]] %>% 
    filter(state_abbreviation != "US") %>% 
    select(
      1:6  #in 2012 column names are different then other years
      ,contains(
        c(
          "smoking"
          ,"obesity"
          ,"inactivity"
          ,"food_environment_index"
        )
      )
    ) %>% 
    select(
      -contains(
        c(
          "numerator"
          ,"denominator"
          ,"ci_low"
          ,"ci_high"
          ,"pregnancy"
          ,"fast_food"
          ,"fl"
          ,"ny"
          ,"raw_value_1"
          # ,"environment"
          # ,"insecurity"
        )
      )
    )
  
  risk_factors[[name]] <- d
}


ds_risk_factors_raw <- bind_rows(risk_factors)
```

# Tweak Data

``` r
ds_risk_factors <- ds_risk_factors_raw %>% 
  rename(food_environment_index = food_environment_index_raw_value) %>% 
  mutate(across(contains("raw_value"), ~round(.x * 100,1))) %>% 
  rename_with(
    .cols = contains("raw_value")
    ,~str_replace_all(.,"raw_value", "percent")
    ) %>% 
  rename(
    county_fips = x5_digit_fips_code 
    ,year = release_year
  ) %>% 
  filter(county_fips_code != "000") %>% 
  select(
    -state_fips_code
    ,-county_fips_code
    ,-state_abbreviation
    ,-name
  )
```

# Save to Disk

``` r
ds_risk_factors %>% write_rds(
  "./data-public/derived/national-diabetes-risk-factors-2010-2020.rds"
  ,compress = 'gz'
  )

ds_risk_factors %>% write_csv(
  "./data-public/derived/national-diabetes-risk-factors-2010-2020.csv"
  )
```
