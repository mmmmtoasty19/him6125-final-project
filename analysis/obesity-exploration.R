#' ---
#' author: Kyle Belanger
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#' 
#' ---

#+ include = FALSE
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- knitr-opts --------------------------------------------------------------
#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
knitr::opts_knit$set(root.dir = "../../../")
options(knitr.table.format = "html")

# ---- script-description ------------------------------------------------------

#1. Delta btw years
#2. Detla btw mean of 2010 - 2015 and 2016, 2017
#3. Delta btw three slopes: a)2010 - 2015, b) 2010-2016, c)2010-2017

#4 Model a)2010-2014 b)2010-2015  c) 2010-2016, d)2010-2017

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

# ---- declare-globals ---------------------------------------------------------

ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)

# ---- load-data ---------------------------------------------------------------

ds_obesity_raw <-  read_rds("./data-public/derived/us-obesity-data.rds")

ds_diabetes_raw <-  read_rds("./data-public/derived/us-diabetes-data.rds")

# ---- tweak -------------------------------------------------------------------

ds_obese_diabetes <- ds_obesity_raw %>% 
  left_join(ds_diabetes_raw) %>% 
  left_join(read_csv("./data-public/metadata/county_fips.csv")) %>% 
  dplyr::relocate(
    c("area_name", "state_name", "state_abb")
    ,.after = "county_fips"
  ) %>% 
  rename(county_name = area_name) %>% 
  select(-obesity_lower_limit, -obesity_upper_limit,
         -diabetes_lower_limit, -diabetes_upper_limit)



ds_o_d_long <- ds_obese_diabetes %>% 
  pivot_longer(c("obesity_percentage","diabetes_percentage"))


# ---- spaghetti plots ---------------------------------------------------------

ds_obese_diabetes %>% filter(state_abb == "NC") %>% 
  ggplot(aes(x = year, y = obesity_percentage)) +
  geom_line(aes(group = county_fips)) +
  geom_smooth(method = "lm", se = FALSE, size = 2, color = "darkblue")

ds_obese_diabetes %>%
  ggplot(aes(x = year, y = obesity_percentage)) +
  geom_line(aes(group = county_fips)) +
  geom_smooth(method = "lm", se = FALSE, size = 2, color = "darkblue") +
  facet_wrap(~state_abb)


# ---- comparision -------------------------------------------------------------

#state mean
ds_obese_diabetes %>% filter(state_abb == "NC") %>% 
  group_by(state_abb, year) %>% 
  summarise(
    across(where(is.numeric), mean)
  ) %>% pivot_longer(c("obesity_percentage","diabetes_percentage")) %>% 
  ggplot(aes(x = year, y = value, color = name)) +
    geom_line(aes(group = name))





