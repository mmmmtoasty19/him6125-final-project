#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages --------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(dplyr)    # data wrangling
library(ggplot2)  # graphs
library(tidyr)    # data tidying

# ---- load-sources ---------------------------------------------------

# ---- load-data  ------------------------------------------------
import_data <- function(path_folder){
  files <- list.files(path_folder,full.names = TRUE)
  dto <- list()
  for(item_i in seq_along(files)){
    item_path <- files[item_i]
    item_name <- item_path %>% basename() %>% stringr::str_replace(".csv","") %>% tolower()
    item_year <- stringr::str_sub(item_name, -2)
    dto[[item_name]] <- readr::read_csv(item_path,col_names = TRUE, skip = 2) %>% 
      drop_na() %>% mutate(
        year = as.integer(item_year)+2000
        ,CountyFIPS = as.character(CountyFIPS)) %>% 
      select(County,Percentage,CountyFIPS,year) %>% 
      mutate_at("County", ~stringr::str_replace_all(.,"County",""))
  }
  return(dto)
}

diabetes_list <- import_data("./data-public/raw/nc_diabetes_data") 

diabetes_list <-  do.call(rbind.data.frame,diabetes_list) 
rownames(diabetes_list) <- c()




readr::write_rds(diabetes_list,"./data-public/derived/nc-diabetes-data.rds")
readr::write_csv(diabetes_list,"./data-public/derived/nc-diabetes-data.csv")


# ---- save-data ------------------------------------------------------
