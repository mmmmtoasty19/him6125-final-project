#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages --------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(dplyr)    # data wrangling
library(ggplot2)  # graphs
library(tidyr)    # data tidying
library(maps)
library(mapdata)



# ---- load-sources ---------------------------------------------------



# ---- declare-globals ----------------------------------------------------

#set ggplot theme
ggplot2::theme_set(theme_bw())

# ---- load-data ------------------------------------------------------

# load the data, and have all column names in lowercase

nc_diabetes_data_raw <- readr::read_rds("./data-public/derived/nc-diabetes-data.rds") %>% 
  rename_all(tolower)

us_diabetes_data_raw <- readr::read_csv("data-public/raw/us_diabetes_totals.csv", 
                                        skip = 2)

rural_counties <- readr::read_csv("./data-public/metadata/rural-counties.csv")

county_centers_raw <- readxl::read_xlsx("./data-public/raw/nc_county_centers.xlsx", col_names = c("county", "lat","long"))
  
# load map data for NC

nc_county_map <- map_data("county",region = "north carolina")



# ---- tweak-data --------------------------------------------------------------

county_centers <- county_centers_raw %>% 
  mutate_all(~stringr::str_replace_all(.,
                                       c("\\°"  = ""
                                         ,"\\+" = ""
                                         ,"\\–" = "-"
                                        )
                                      )
                                    ) %>% 
  mutate_at(c("lat","long"),as.numeric) %>% 
  mutate_at("county", tolower)


us_diabetes_data <- us_diabetes_data_raw %>% 
  filter(Year >= 2006) %>% 
  select( "Year","Total - Percentage") %>% 
  rename(year = Year , us_pct = `Total - Percentage`)

#join us totals

nc_diabetes_data <- nc_diabetes_data_raw %>% 
  mutate(
    rural = county %in% rural_counties$rural_counties
  ) %>% 
  mutate_at("county",tolower) %>% 
  left_join(us_diabetes_data)
 
# join all data, filter by year before making maps
nc_diabetes_map <- nc_county_map %>% 
  left_join(nc_diabetes_data, by = c('subregion' = 'county'))



# ---- g1 ----------------------------------------------------------------------

nc_diabetes_data %>% 
  group_by(year) %>% 
  summarise(
    pct = mean(percentage)
    ,us_pct = mean(us_pct)
  ) %>% 
  pivot_longer(
    cols       = c("pct", "us_pct")
    ,names_to  = "metric"
    ,values_to = "values"
  ) %>% 
  mutate(
    metric = factor(metric
                    ,levels = c("pct","us_pct")
                    ,labels = c("NC", "National"))
  ) %>% 
  ggplot(aes(x = year, y = values, color = metric)) +
  geom_line() +
  geom_point(shape = 21, size = 3) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x      = NULL
    ,y     = NULL
    ,color = NULL
    ,title = "Percent of Adults (20+) with Diagnosed Diabetes"
  )
    

