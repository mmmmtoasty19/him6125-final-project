#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# IN PROGRESS


# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

# ---- declare-globals ---------------------------------------------------------

folder_path <- "./data-unshared/raw/county_health_rankings_nc"



import_data <- function(path_folder){
  # browser()
  files <- list.files(path_folder, full.names = TRUE)
  dto   <- list()
  for(item_i in seq_along(files)){
    item_path <- files[item_i]
    item_name <- item_path %>% basename() %>% stringr::str_remove_all(".xls|.xlsx") %>% 
      tolower()
    item_year <- item_name %>% str_sub(end = 4)
    
    sheet <- 4
    if(item_year == 2013){
      sheet <- 5
    }
    
    col_names <- readxl::read_excel(item_path
                                    ,sheet = sheet
                                    ,n_max = 2
                                    ,col_names = FALSE) %>% 
      t() %>% 
      as_tibble() %>% 
      fill(V1) %>% 
      mutate(name = paste(V1,V2, sep = "_")) %>% 
      mutate(across(name, ~ stringr::str_remove_all(.,"NA_"))) %>% 
      pull(name) %>% 
      janitor::make_clean_names()
      
    
    if(item_year %in% c(2013, 2014)){
      col_names <- replace(col_names,1:3,c("FIPS","State", "County"))
    } 
   
    
    dto[[item_name]] <- readxl::read_excel(
      item_path
      ,sheet     = sheet
      ,skip      = 2
      ,col_names = col_names) %>% 
      mutate(
        year = item_year
      ) %>% 
      drop_na(3)  #drops NA's from County column (name not consistent)
      
    
  }
  return(dto)
}

# ---- load-data ---------------------------------------------------------------

county_rankings <- import_data(folder_path)

# ---- filter-data -----

# filtering for diabetes risk factors


risk_factors <- list()

for (item in seq_along(county_rankings)){
  name <- names(county_rankings[item])
  d <- county_rankings[[item]] %>% 
    select(1:3
           ,year
           ,contains(c(
             "smoking"
             ,"obesity"
             ,"inactivity"))) %>% 
    select(         # remove unneeded columns
      -contains(c(
          "ci_low"
          ,"ci_high"
          ,"z_score"
          ,"sample_size"
          )
        )
      ) %>% 
    rename_with(.fn = ~str_extract(.,"adult_smoking|adult_obesity|physical_inactivity") 
                ,.cols = contains(c(
                  "smoking"
                  ,"obesity"
                  ,"inactivity"
                  ))) %>% 
    rename_with(
      .cols = contains(c(
        "smoking"
        ,"obesity"
        ,"inactivity"))
      ,.fn = ~paste0(.,"_percent")) %>% 
    rename_with(tolower)
                      
  risk_factors[[name]] <- d 
}



risk_factors_ds <- bind_rows(risk_factors)

# save to disk

risk_factors_ds %>% write_rds("./data-public/derived/nc-diabetes-risk-factors-2010-2020.rds"
                            ,compress = "gz")
risk_factors_ds %>% write_csv("./data-public/derived/nc-diabetes-risk-factors-2010-2020.csv")


 # ---- testing single file ----
# 
# list.files(file_path)
# 
# 
# ds0 <- readxl::read_excel(paste0(folder_path,"/","2013 County Health Ranking North Carolina Data - v1_0.xls"),sheet = 5, n_max = 2, col_names = FALSE)
# 
# 
# 
# ds1 <- data.frame(t(apply(ds0,1, zoo::na.locf,na.rm = FALSE))) #version 1, ds2 in use
# 
# 
# ds2 <- ds0 %>% t() %>% as_tibble() %>% fill(V1) %>% mutate(name = paste(V1,V2, sep = "_")) %>% 
#   mutate(across(name, ~ stringr::str_remove_all(.,"NA_"))) %>% pull(name)
# 
# names <- ds1 %>% t() %>% as_tibble() %>% mutate(name = paste(V1,V2, sep = "_")) %>% 
#   mutate(across(name, ~ stringr::str_remove_all(.,"NA_"))) %>% pull(name)
# ds3 <- readxl::read_excel(paste0(file_path,"/","2010 County Health Ranking North Carolina Data - v2.xls"),sheet = 4, skip = 2, col_names = names)
# 
# 




