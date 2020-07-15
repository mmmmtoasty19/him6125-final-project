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
knitr::opts_knit$set(root.dir = "../")

# ---- load-sources ------------------------------------------------------------

#' # Load Packages
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

# ---- declare-globals ---------------------------------------------------------

year_key <- c(
  NULL    = "1"
  ,NULL   = "2"
  ,"2010" =  "3"  
  ,"2011" =  "4"  
  ,"2012" =  "5"  
  ,"2013" =  "6"  
  ,"2014" =  "7"  
  ,"2015" =  "8"  
  ,"2016" =  "9"  
  ,"2017" = "10"  
  ,"2018" = "11"  
  ,"2019" = "12"
)

age_group_key <- c(
  "Total"    = "0"
  ,"0-4"     = "1"
  ,"5-9"     = "2"
  ,"10-14"   = "3"
  ,"15-19"   = "4"
  ,"20-24"   = "5" 
  ,"25-29"   = "6" 
  ,"30-34"   = "7" 
  ,"35-39"   = "8" 
  ,"40-44"   = "9" 
  ,"45-49"   = "10"
  ,"50-54"   = "11"
  ,"55-59"   = "12"
  ,"60-64"   = "13"
  ,"65-69"   = "14"
  ,"70-74"   = "15"
  ,"75-79"   = "16"
  ,"80-84"   = "17"
  ,"85-over" = "18"
)


col_key <- c(
"state"                              = "STNAME"           
,"county_name"                       = "CTYNAME"           
,"year"                              = "YEAR"           
,"age_group"                         = "AGEGRP"           
,"total_population"                  = "TOT_POP"           
,"total_male_population"             = "TOT_MALE"           
,"total_female_population"           = "TOT_FEMALE"           
,"white_male_population"             = "WA_MALE"           
,"white_female_population"           = "WA_FEMALE"           
,"black_male_population"             = "BA_MALE"           
,"black_female_population"           = "BA_FEMALE"           
,"american_indian_male_population"   = "IA_MALE"           
,"american_indian_female_population" = "IA_FEMALE"     
,"asian_male_population"             = "AA_MALE"           
,"asian_female_population"           = "AA_FEMALE"           
,"native_hawaiian_male_population"   = "NA_MALE"
,"native_hawaiian_female_population" = "NA_FEMALE" 
,"not_hispanic_male_population"      = "NH_MALE"
,"not_hispanic_female_population"    = "NH_FEMALE"
,"hispanic_male_population"          = "H_MALE"
,"hispanic_female_population"        = "H_FEMALE"
)



# ---- load-data ---------------------------------------------------------------

ds_estimates_raw <- read_csv("./data-unshared/raw/us-population-estimate-2010-2019-age-race.csv")

# ---- tweak-data -------------------------------------------------------------

ds_estimates <- ds_estimates_raw %>%
  select(all_of(col_key)) %>% 
  mutate(across(c(year,age_group),as_factor)
         ,across(year, ~fct_recode(.,!!!year_key))
         ,across(age_group, ~fct_recode(.,!!!age_group_key))) %>% 
  drop_na(year)

ds_estimates %>% glimpse()
  
# ---- calculate-pct ----------------------------------------------------------

calculate_percent <- function(data, base_select_vars){
  #testing variables
  # data <- ds_test
   # base_select_vars <- c("state_name"
   #                      ,"county_name"
   #                      ,"year"
   #                      ,"age_group")
  #end testing variables
  
  d_total <- data %>% 
    filter(age_group == "Total") %>% 
    select(setdiff(all_of(base_select_vars),"age_group"),total_population) %>% 
    rename(year_total_population = total_population)
  
  d_gender <- data %>% 
    filter(age_group != "Total") %>% 
    select(all_of(base_select_vars),total_male_population, total_female_population) %>% 
    left_join(d_total) %>% 
    mutate(
      pct_male    = round((total_male_population/year_total_population)*100,2)
      ,pct_female = round((total_female_population/year_total_population)*100,2)
    )
  
  d_race <- data %>% 
    filter(age_group != "Total") %>%
    select(all_of(base_select_vars),
           white_male_population          
           ,white_female_population          
           ,black_male_population           
           ,black_female_population   
           ,american_indian_male_population 
           ,american_indian_female_population
           ,asian_male_population           
           ,asian_female_population   
           ,native_hawaiian_male_population 
           ,native_hawaiian_female_population
           ) %>% 
    left_join(d_total) %>% 
    mutate(
      pct_white_male_population              = round(
        (white_male_population/year_total_population)*100,2)          
      ,pct_white_female_population           = round(
        (white_female_population/year_total_population)*100,2)          
      ,pct_black_male_population             = round(
        (black_male_population/year_total_population)*100,2)
      ,pct_black_female_population           = round(
        (black_female_population/year_total_population)*100,2)
      ,pct_american_indian_male_population   = round(
        (american_indian_male_population/year_total_population)*100,2)
      ,pct_american_indian_female_population = round(
        (american_indian_female_population/year_total_population)*100,2)
      ,pct_asian_male_population             = round(
        (asian_male_population/year_total_population)*100,2)
      ,pct_asian_female_population           = round(
        (asian_female_population/year_total_population)*100,2)
      ,pct_native_hawaiian_male_population   = round(
        (native_hawaiian_male_population/year_total_population)*100,2)
      ,pct_native_hawaiian_female_population = round(
        (native_hawaiian_female_population/year_total_population)*100,2)
    )
  
  d_ethnicity <- data %>% 
    filter(age_group != "Total") %>%
    select(all_of(base_select_vars)
           ,not_hispanic_male_population     
           ,not_hispanic_female_population   
           ,hispanic_male_population         
           ,hispanic_female_population 
           ) %>% 
    left_join(d_total) %>% 
    mutate(
      pct_not_hispanic_male_population    = round(
        (not_hispanic_male_population/year_total_population)*100,2)
      ,pct_not_hispanic_female_population = round(
        (not_hispanic_female_population/year_total_population)*100,2)
      ,pct_hispanic_male_population       = round(
        (hispanic_male_population/year_total_population)*100,2)
      ,pct_hsipanic_female_population     = round(
        (hispanic_female_population/year_total_population)*100,2)
    )
    
  
  d_out <- d_gender %>% 
    left_join(d_race) %>% 
    left_join(d_ethnicity) %>% 
    relocate(contains("pct_"), .after = everything()) %>% 
    relocate(year_total_population, .after = age_group)

  
  }   


ds0 <- calculate_percent(ds_estimates, c("state"
                                         ,"county_name"
                                         ,"year"
                                         ,"age_group"))

ds0 %>% glimpse()

# ---- save-to-disk -----------------------------------------------------------

ds0 %>% write_rds("./data-public/derived/us-county-population-estimates-v2.rds"
                  ,compress = "gz"
                  )
ds0 %>% write_csv("./data-public/derived/us-county-population-estimates-v2.csv")
                  







# test

# testing summarizing data 

# test <- ds0 %>% 
#   filter(str_detect(county_name, "Bladen")) %>% 
#   select(-year_total_population, -age_group) %>% 
#   group_by(state_name, county_name, year) %>%
#   summarise(across(is.numeric,sum))
