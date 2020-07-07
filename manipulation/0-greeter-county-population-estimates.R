#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse) # enables piping : %>% 

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------

ds0 <- readxl::read_excel("./data-unshared/raw/nc_county_population_estimates_2010_2019.xlsx", skip = 3)

# ---- tweak-data -------------------------------------------------------------

ds1 <- ds0 %>% 
  select(-Census,-`Estimates Base`) %>% 
  rename(
    county = ...1
  ) %>% 
  mutate(
    across(
      county
      ,~str_remove_all(.,"County, North Carolina")
    )
  ) %>% 
  filter(county != "North Carolina") %>% 
  drop_na(`2010`) %>% 
  mutate(
      across(
        county
        ,~str_remove_all(.,"^.")
      )
    ) %>% 
  pivot_longer(
    cols = c(`2010`:`2019`)
    ,names_to = "year"
    ,values_to = "population"
  )

# ---- save-to-disk -----------------------------------------------------------

ds1 %>% write_rds("./data-public/derived/county-population-estimates.rds")
    
  
