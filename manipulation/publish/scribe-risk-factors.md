scribe-risk-factors.R
================
Kyle Belanger
07/07/2020

``` r
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)
```

# Load Data

``` r
national_risk_factors_raw <- read_rds("./data-public/derived/national-diabetes-risk-factors-2010-2020.rds")

nc_diabetes_data_raw <- read_rds("./data-public/derived/nc-diabetes-data.rds")
```

``` r
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
```

``` r
ds_combine <- nc_diabetes_ds %>% 
  left_join(risk_factor_ds, by = c("county" = "name"
                                   ,"year" = "release_year")
            ) %>% 
  select(-countyfips,-state_abbreviation) %>% 
  relocate(diabetes_percentage, .after = county_fips)
```

``` r
ds_combine %>% write_rds("./data-public/derived/nc_risk_factors.rds")
ds_combine %>% write_csv("./data-public/derived/nc_risk_factors.csv")
```
