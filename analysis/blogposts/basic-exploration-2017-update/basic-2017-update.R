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
knitr::opts_knit$set(root.dir = "../../../")
options(knitr.table.format = "html")

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------

ds_diabetes_raw <-  read_rds("./data-public/derived/us-diabetes-data.rds")

ds_rural_housing_raw <- read_rds("./data-public/derived/percentage-rural.rds")

# --- merge-data ----------------------------------------------------------------


ds_diabetes <- ds_diabetes_raw %>% 
  left_join(ds_rural_housing_raw) %>% 
  left_join(read_csv("./data-public/metadata/county_fips.csv")) %>% 
  relocate(c("area_name", "state_name", "state_abb"), .after = "county_fips") %>% 
  rename(county_name = area_name)

# ---- filter ------------------------------------------------------------------

ds0 <- ds_diabetes %>% 
  filter(state_abb == "NC")

ds1 <- ds_diabetes %>% 
  filter(state_abb =="FL")

# ---- quick spagahitti --------------------------------------------------------

ds0 %>% ggplot(aes(x = year, y = diabetes_percentage)) +
  geom_line(aes(group = county_fips))
ds1 %>% ggplot(aes(x = year, y = diabetes_percentage)) +
  geom_line(aes(group = county_fips))

ds_diabetes %>% ggplot(aes(x = year, y = diabetes_percentage)) +
  geom_line(aes(group = county_fips)) + 
  facet_wrap(~state_abb)

# ---- calculate SD ------------------------------------------------------------

# Single State

ds0_nc_summary <- ds0 %>% 
  select(
  -diabetes_lower_limit
  ,-diabetes_upper_limit
  ) %>% 
  pivot_wider(names_from = year, values_from = diabetes_percentage) %>% 
  summarise(across(`2010`:`2017`, list(mean = mean, sd = sd)))


# All States (No Alaska or Hawaii)

ds_diabetes_summary <- ds_diabetes %>% 
  select(
    -diabetes_lower_limit
    ,-diabetes_upper_limit
  ) %>% 
  filter(!state_abb %in% c("AK", "HI")) %>% 
  pivot_wider(names_from = year, values_from = diabetes_percentage) %>% 
  group_by(state_abb) %>% 
  summarise(across(`2010`:`2017`, 
                   list(
                     mean = ~mean(.x,na.rm = TRUE)
                     ,sd  = ~sd(.x,na.rm = TRUE)))) %>% 
  mutate(
    sd_change_2015 = (`2015_sd` - `2014_sd`)
    ,sd_change_2016 = (`2016_sd`-`2015_sd`)
  )

# ds_diabetes_summary %>% knitr::kable() %>%
#   kableExtra::kable_styling() %>%
#   kableExtra::scroll_box(width = "100%", height = "500px")
