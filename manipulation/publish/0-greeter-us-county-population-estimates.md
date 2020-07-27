0-greeter-us-county-population-estimates.R
================
Kyle Belanger
07/27/2020

# Load Packages

``` r
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)
```

Declare Globals

``` r
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
```

# Load Data

``` r
ds_estimates_raw <- read_csv(
  "./data-unshared/raw/us-population-estimate-2010-2019-age-race.csv"
  )
```

# Tweak Data

``` r
ds_estimates <- ds_estimates_raw %>%
  select(all_of(col_key)) %>%
  unite(col = "county_fips", state_fips:county_fips, sep = "") %>% 
  mutate(across(c(year,age_group),as_factor)
         ,across(year, ~fct_recode(.,!!!year_key))
         ,across(age_group, ~fct_recode(.,!!!age_group_key))) %>% 
  drop_na(year)

ds_estimates %>% glimpse()
```

    ## Rows: 596,980
    ## Columns: 20
    ## $ county_fips                       <chr> "01001", "01001"...
    ## $ year                              <fct> 2010, 2010, 2010...
    ## $ age_group                         <fct> Total, 0-4, 5-9,...
    ## $ total_population                  <dbl> 54773, 3575, 396...
    ## $ total_male_population             <dbl> 26672, 1863, 198...
    ## $ total_female_population           <dbl> 28101, 1712, 198...
    ## $ white_male_population             <dbl> 21359, 1415, 150...
    ## $ white_female_population           <dbl> 22061, 1314, 151...
    ## $ black_male_population             <dbl> 4596, 356, 398, ...
    ## $ black_female_population           <dbl> 5154, 319, 369, ...
    ## $ american_indian_male_population   <dbl> 117, 3, 15, 13, ...
    ## $ american_indian_female_population <dbl> 134, 2, 6, 13, 6...
    ## $ asian_male_population             <dbl> 201, 13, 15, 23,...
    ## $ asian_female_population           <dbl> 296, 15, 22, 19,...
    ## $ native_hawaiian_male_population   <dbl> 27, 0, 1, 4, 4, ...
    ## $ native_hawaiian_female_population <dbl> 19, 0, 4, 1, 2, ...
    ## $ not_hispanic_male_population      <dbl> 25980, 1778, 191...
    ## $ not_hispanic_female_population    <dbl> 27480, 1653, 190...
    ## $ hispanic_male_population          <dbl> 692, 85, 68, 65,...
    ## $ hispanic_female_population        <dbl> 621, 59, 72, 65,...

# Calculate Percentage

``` r
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


ds0 <- calculate_percent(ds_estimates, c("county_fips"
                                         ,"year"
                                         ,"age_group"))

ds0 %>% glimpse()
```

    ## Rows: 565,560
    ## Columns: 36
    ## $ county_fips                           <chr> "01001", "01...
    ## $ year                                  <fct> 2010, 2010, ...
    ## $ age_group                             <fct> 0-4, 5-9, 10...
    ## $ year_total_population                 <dbl> 54773, 54773...
    ## $ total_male_population                 <dbl> 1863, 1984, ...
    ## $ total_female_population               <dbl> 1712, 1980, ...
    ## $ white_male_population                 <dbl> 1415, 1506, ...
    ## $ white_female_population               <dbl> 1314, 1517, ...
    ## $ black_male_population                 <dbl> 356, 398, 42...
    ## $ black_female_population               <dbl> 319, 369, 40...
    ## $ american_indian_male_population       <dbl> 3, 15, 13, 1...
    ## $ american_indian_female_population     <dbl> 2, 6, 13, 6,...
    ## $ asian_male_population                 <dbl> 13, 15, 23, ...
    ## $ asian_female_population               <dbl> 15, 22, 19, ...
    ## $ native_hawaiian_male_population       <dbl> 0, 1, 4, 4, ...
    ## $ native_hawaiian_female_population     <dbl> 0, 4, 1, 2, ...
    ## $ not_hispanic_male_population          <dbl> 1778, 1916, ...
    ## $ not_hispanic_female_population        <dbl> 1653, 1908, ...
    ## $ hispanic_male_population              <dbl> 85, 68, 65, ...
    ## $ hispanic_female_population            <dbl> 59, 72, 65, ...
    ## $ pct_hsipanic_female_population        <dbl> 0.11, 0.13, ...
    ## $ pct_male                              <dbl> 3.40, 3.62, ...
    ## $ pct_female                            <dbl> 3.13, 3.61, ...
    ## $ pct_white_male_population             <dbl> 2.58, 2.75, ...
    ## $ pct_white_female_population           <dbl> 2.40, 2.77, ...
    ## $ pct_black_male_population             <dbl> 0.65, 0.73, ...
    ## $ pct_black_female_population           <dbl> 0.58, 0.67, ...
    ## $ pct_american_indian_male_population   <dbl> 0.01, 0.03, ...
    ## $ pct_american_indian_female_population <dbl> 0.00, 0.01, ...
    ## $ pct_asian_male_population             <dbl> 0.02, 0.03, ...
    ## $ pct_asian_female_population           <dbl> 0.03, 0.04, ...
    ## $ pct_native_hawaiian_male_population   <dbl> 0.00, 0.00, ...
    ## $ pct_native_hawaiian_female_population <dbl> 0.00, 0.01, ...
    ## $ pct_not_hispanic_male_population      <dbl> 3.25, 3.50, ...
    ## $ pct_not_hispanic_female_population    <dbl> 3.02, 3.48, ...
    ## $ pct_hispanic_male_population          <dbl> 0.16, 0.12, ...

# Save to Disk

``` r
ds0 %>% write_rds("./data-public/derived/us-county-population-estimates-v2.rds"
                  ,compress = "gz"
                  )
ds0 %>% write_csv("./data-public/derived/us-county-population-estimates-v2.csv")
```
