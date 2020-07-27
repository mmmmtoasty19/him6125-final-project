0-greeter-census-rural-housing.R
================
Kyle Belanger
07/27/2020

# Load Packages

``` r
# Attach these packages so their functions 
# don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)
```

# Load Data

``` r
ds_rural_housing_raw <- read_csv(
  "./data-unshared/raw/2010-census-rural-urban-counts.csv"
  ,skip = 1
  ) 
```

# Tweak Data

``` r
ds_rural_housing <- ds_rural_housing_raw %>% 
  janitor::clean_names() %>% 
  select(
    -total_urban_inside_urbanized_areas
    ,-total_urban_inside_urban_clusters
    ,-total_not_defined_for_this_file
    ) %>% 
  filter(!str_detect(geographic_area_name,"Puerto Rico")) %>% 
  separate(geographic_area_name
           ,into = c("county", "state")
           ,sep = ",") %>% 
   mutate(
     across(where(is.character), tolower)
     ,across(where(is.character), trimws)
     ,across(id, ~str_sub(.,-5))
     ,pct_rural = round((total_rural/total)*100,2)
     ,rural     = case_when(
       pct_rural == 100 ~ "Rural"
       ,pct_rural >= 50 ~ "Mostly Rural"
       ,TRUE ~ "Mostly Urban"
       )
     ) %>% 
  select(
    id
    ,pct_rural  
    ,rural 
    ) %>% rename(
    county_fips = id
  )
```

# Save to Disk

``` r
ds_rural_housing %>% write_rds("./data-public/derived/percentage-rural.rds"
                               ,compress = "gz")
ds_rural_housing %>% write_csv("./data-public/derived/percentage-rural.csv")
```
