#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse) # enables piping : %>% 

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------

ds_county_population <- read_rds(
  "./data-public/derived/county-population-estimates.rds"
  )

ds_cdc_wonder <- read_rds("./data-public/derived/cdc-wonder-agegroup.rds")



# ---- tweak-data -------------------------------------------------------------

ds_county_population <- ds_county_population %>% 
  mutate(
    across(year, as.integer)
  ) %>% 
  rename(
    total_population = population
  )

ds_cdc_wonder <- ds_cdc_wonder %>% 
  filter(!str_detect(age_group,"15-19"))


# group by race

ds0 <- ds_cdc_wonder %>% 
  group_by(county,race,year) %>% 
  summarise(
    population = sum(population)
  ) %>% 
  ungroup()

# group by ethnicity
 
ds1 <- ds_cdc_wonder %>% 
  group_by(county,ethnicity,year) %>% 
  summarise(
    population = sum(population)
  ) %>% 
  ungroup()

# group by age 

age_group_levels <- 
  c(
  "20-44"  = "20-24 " 
  ,"20-44" = "25-29 " 
  ,"20-44" = "30-34 " 
  ,"20-44" = "35-39 " 
  ,"20-44" = "40-44 " 
  ,"45-64" = "45-49 "
  ,"45-64" = "50-54 " 
  ,"45-64" = "55-59 " 
  ,"45-64" = "60-64 " 
  ,"65-74" = "65-69 " 
  ,"65-74" = "70-74 " 
  ,"75+"   = "75-79 "
  ,"75+"   = "80-84 " 
  ,"75+"   = "85+ "
  )

ds3 <- ds_cdc_wonder %>% 
  mutate(
    across(age_group,as_factor)
    ,across(age_group,~fct_recode(.,!!!age_group_levels))
  ) %>% 
  group_by(county,age_group,year) %>% 
  summarise(
    population = sum(population)
  ) %>% 
  ungroup()


# ---- merge-data -------------------------------------------------------------

ds_merge_race <- ds0 %>% 
  left_join(ds_county_population) %>% 
  drop_na() %>% 
  mutate(
    adult_pct_population = round((population/total_population)*100,2)
  )

ds4 <- ds_merge_race %>% 
  select(-population,-total_population) %>% 
  mutate(
    across(race, ~snakecase::to_snake_case(.))
  ) %>% 
  pivot_wider(names_from = race,values_from = adult_pct_population,names_prefix = "pct_")


ds_merge_ethnicity <- ds1 %>% 
  left_join(ds_county_population) %>% 
  drop_na() %>% 
  mutate(
    adult_pct_population = round((population/total_population)*100,2)
  )

ds5 <- ds_merge_ethnicity %>% 
  select(-population,-total_population) %>% 
  mutate(
    across(ethnicity, ~snakecase::to_snake_case(.))
  ) %>% 
  pivot_wider(names_from = ethnicity, values_from = adult_pct_population, names_prefix = "pct_")


ds_merge_age <- ds3 %>% 
  left_join(ds_county_population) %>% 
  drop_na() %>% 
  mutate(
    pct_population = round((population/total_population)*100,2)
  ) %>% 
  select(-population,-total_population) %>% 
  pivot_wider(names_from = age_group, values_from = pct_population, names_prefix = "pct_")


# ---- merge-all-data ---------------------------------------------------------

ds_combine_all <- ds4 %>% 
  left_join(ds5) %>% 
  left_join(ds_merge_age)


# ---- save-to-disk -----------------------------------------------------------

ds_combine_all %>% write_rds("./data-public/derived/combined-population-data.rds", compress = "gz")
