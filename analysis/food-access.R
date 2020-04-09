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

# ---- declare-globals ------------------------------------------------

# ---- load-data ------------------------------------------------------
# readxl::excel_sheets("./data-public/raw/usda_food_atlas.xls")

food_access <-  readxl::read_xls("./data-public/raw/usda_food_atlas.xls"
  ,sheet = "ACCESS"
) 

# ---- inspect-data ---------------------------------------------------

# ---- tweak-data -----------------------------------------------------

food_access <- food_access %>% 
  filter(State == "NC") %>% 
  select("County", "PCT_LACCESS_POP15") %>% 
  mutate_at("County", ~tolower(.))
  
colnames(food_access) <- purrr::map(colnames(food_access), tolower)




# ---- graph -------------------------------------------------------------------

g_food <- food_access %>% 
  ggplot(aes(x = county, y = pct_laccess_pop15)) +
  geom_col()

g_food