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
library(tidymodels)

# ---- declare-globals ---------------------------------------------------------

ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)

# ---- load-data ---------------------------------------------------------------

ds_diabetes_raw <-  read_rds("./data-public/derived/us-diabetes-data.rds")

# ---- tweak -------------------------------------------------------------------

ds_diabetes <- ds_diabetes_raw %>% 
  left_join(read_csv("./data-public/metadata/county_fips.csv")) %>% 
  dplyr::relocate(
    c("area_name", "state_name", "state_abb")
    ,.after = "county_fips"
    ) %>% 
  rename(county_name = area_name) %>% 
  select(-diabetes_lower_limit, -diabetes_upper_limit)



# ---- NC LM -------------------------------------------------------------------

# Practicing using one year, will create loop 

# 2004-2014   


ds_nc_diabetes <- ds_diabetes %>% 
  filter(state_abb == "NC") %>% 
  select(-state_abb, -state_name)

# Bladen County Only - Testing

lm1 <- ds_nc_diabetes %>% 
  filter(year < 2015, county_fips == "37017") %>% 
  nest(data = c(-county_fips, -county_name)) %>% 
  mutate(
    fit = map(data, ~ lm(diabetes_percentage ~ 1 + year, data = .x))
    ,tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>% 
  select(-data, -fit)

print(lm1)


ds_nc_diabetes %>% filter(year < 2015, county_fips == "37017") %>% 
  ggplot(aes(x = year, y = diabetes_percentage)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    , vjust = 3
  )

ds_nc_diabetes %>% filter(county_fips == "37017") %>% 
  ggplot(aes(x = year, y = diabetes_percentage)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  ggpmisc::stat_poly_eq(
    formula = y ~ + x
    ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
    ,parse = TRUE
    , vjust = 3
  )

# Model Function - Code needs to be more concise 

many_models <- function(county_fips){
  id <- county_fips
  
  d <- ds_diabetes %>% filter(county_fips == id)
  
  lm1 <- d %>% filter(year < 2015) %>% 
    nest(data = c(-county_fips, -county_name)) %>% 
    mutate(
      fit = map(data, ~ lm(diabetes_percentage ~ 1 + year, data = .x))
      ,tidied = map(fit, tidy)
    ) %>% 
    unnest(tidied) %>% 
    select(-data, -fit)
  
  lm2 <- d %>% filter(year < 2016) %>% 
    nest(data = c(-county_fips, -county_name)) %>% 
    mutate(
      fit = map(data, ~ lm(diabetes_percentage ~ 1 + year, data = .x))
      ,tidied = map(fit, tidy)
    ) %>% 
    unnest(tidied) %>% 
    select(-data, -fit)
  
  lm3 <- d %>% filter(year < 2017) %>% 
    nest(data = c(-county_fips, -county_name)) %>% 
    mutate(
      fit = map(data, ~ lm(diabetes_percentage ~ 1 + year, data = .x))
      ,tidied = map(fit, tidy)
    ) %>% 
    unnest(tidied) %>% 
    select(-data, -fit)
  
  lm4 <- d %>% filter(year < 2018) %>% 
    nest(data = c(-county_fips, -county_name)) %>% 
    mutate(
      fit = map(data, ~ lm(diabetes_percentage ~ 1 + year, data = .x))
      ,tidied = map(fit, tidy)
    ) %>% 
    unnest(tidied) %>% 
    select(-data, -fit)
  
  
  d_out <- bind_rows(
    lm_2014  = lm1
    ,lm_2015 = lm2
    ,lm_2016 = lm3
    ,lm_2017 = lm4
    , .id = "id")
  
  
  
}




# How to use 

many_lm1 <- many_models("37017")






# ---- delta by year -----------------------------------------------------------

ds_delta <- ds_diabetes %>% 
  rename(dbpct = diabetes_percentage) %>% 
  group_by(state_abb, county_name, county_fips) %>% 
  arrange(state_abb, county_name) %>%
  mutate(
    lag = lag(dbpct, order_by = year),
    delta_lag = dbpct - lag,
  )
 
# UNCOMMENT FOR REPORT PRINTING
 
# ds_delta %>% select(-dbpct, -lag) %>%  
#   pivot_wider(names_from = year ,values_from = delta_lag) %>% 
#   knitr::kable()


# Quick NC Graph

ds_delta %>% 
  filter(state_abb == "NC") %>% 
  ggplot(aes(x = year, y = delta_lag, color = county_fips)) +
  geom_line(aes(group = county_fips), na.rm = TRUE, show.legend = FALSE)


# ---- delta between 2010-2015 & 2016,17 ---------------------------------------

ds1 <- ds_diabetes %>% 
  rename(dbpct = diabetes_percentage) %>% 
  group_by(state_abb, county_name, county_fips) %>% 
  arrange(state_abb, county_name) %>%
  pivot_wider(names_from = year, values_from = dbpct) %>% 
  ungroup() %>% rowwise() %>% 
  mutate(
    `2010-2015_mean` = round(mean(c_across(`2010`:`2015`), na.rm = TRUE),2)
    ,delta_16        = `2016` - `2010-2015_mean`
    ,delta_17        = `2017`- `2016`
  ) %>% 
  select(-`2010`,-`2011`,-`2012`,-`2013`,-`2014`,-`2015`)
  
