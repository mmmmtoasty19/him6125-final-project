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

#' Declare Globals
# ---- declare-globals ---------------------------------------------------------

year_key <- c(
  NULL    = "1"
  ,NULL   = "12"
  ,NULL   = "13"
  ,"2000" =  "2"  
  ,"2001" =  "3"  
  ,"2002" =  "4"  
  ,"2003" =  "5"  
  ,"2004" =  "6"  
  ,"2005" =  "7"  
  ,"2006" =  "8"  
  ,"2007" = "9"  
  ,"2008" = "10"  
  ,"2009" = "11"
)

age_group_key <- c(
  "Total"    = "99"
  ,"0"       = "0"
  ,"1-4"     = "1"
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
  "state_fips"                         = "STATE"
  ,"county_fips"                       = "COUNTY"
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

data_files <- list.files("data-unshared/raw/pop-estimates-2000-2009"
                         ,full.names = TRUE)
dto <- list()

for(item_i in seq_along(data_files)){
  item_path <- data_files[item_i]
  item_name <- item_path %>% basename() %>% 
    stringr::str_replace(".csv","") %>% tolower()
  
  d_raw <- readr::read_csv(item_path) 
  
  d <- d_raw %>% 
    select(all_of(col_key)) %>%
    unite(col = "county_fips", state_fips:county_fips, sep = "") %>% 
    mutate(across(c(year,age_group),as_factor)
           ,across(year, ~fct_recode(.,!!!year_key))
           ,across(age_group, ~fct_recode(.,!!!age_group_key))) %>% 
    drop_na(year)
  
  dto[[item_name]] <- d
  
  
}
  

# ---- tweak data --------------------------------------------------------------

ds0 <- bind_rows(dto)


age_filter <- c(
  "20-24"   
  ,"25-29"   
  ,"30-34"   
  ,"35-39"   
  ,"40-44"   
  ,"45-49"   
  ,"50-54"   
  ,"55-59"   
  ,"60-64"   
  ,"65-69"   
  ,"70-74"   
  ,"75-79"   
  ,"80-84"   
  ,"85-over" 
  )


ds1 <- ds0 %>% filter(age_group %in% age_filter) %>% 
  group_by(year, county_fips) %>% 
  summarise(across(where(is.numeric), sum),.groups = "keep" ) %>% 
  summarise(across(where(is.numeric)
                   ,~(.x / total_population)*100
                   ,.names = "adult_pct_{col}"
                   )
            ,.groups = "keep"
            )


