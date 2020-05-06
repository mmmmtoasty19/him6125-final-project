#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages --------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(dplyr)    # data wrangling
library(ggplot2)  # graphs
library(tidyr)    # data tidying
library(flextable)

# ---- load-sources ---------------------------------------------------

# ---- declare-globals ------------------------------------------------

# ---- load-data ------------------------------------------------------


rural_counties <- readr::read_csv("./data-public/metadata/rural-counties.csv")

us_diabetes_totals <- readr::read_csv("data-public/raw/us_diabetes_totals.csv", 
                               skip = 2) %>% select(-X5)
nc_diabetes_data <- readr::read_rds("./data-public/derived/nc-diabetes-data.rds") %>% 
  mutate_at("County", trimws)

nc_obestiy_raw <- readr::read_csv("data-public/raw/nc_obesity_16.csv", 
                                  skip = 2) %>% drop_na()


# ---- tweak-data -----------------------------------------------------
nc_obestiy_16 <- nc_obestiy_raw %>% 
  mutate_at("County", ~stringr::str_replace_all(.,"County","")) %>% 
  select(County,Percentage) %>% 
  mutate_at("County", trimws)
colnames(nc_obestiy_16) <- c("county", "pct_obese")


nc_diabetes_data <- nc_diabetes_data %>% 
  mutate(
    rural = County %in% rural_counties$rural_counties
  ) %>% select(-CountyFIPS)

us_diabetes_totals <- us_diabetes_totals %>% 
  filter(Year >= 2006)

ds0 <- nc_diabetes_data %>% 
  left_join(us_diabetes_totals, by = c("year" = "Year")) 


# ---- pct-diff ----------------------------------------------------------------

pct_diff <- ds0 %>% 
  filter(year == 2016) %>% 
  mutate(
    percentage = Percentage
    ,us_total = `Total - Percentage`
  ) %>% 
  select(County,rural,percentage,us_total) %>% 
  group_by(rural) %>% 
  summarise(
    n = n()
    ,pct_diabetes = round(mean(percentage),2)
    ,us_total = max(us_total)
    ,pct_diff = round((pct_diabetes - us_total)/us_total*100,2)
  )
colnames(pct_diff) <- c("Rural", "N","NC Diabetes (%)","US Diabetes (%)","% Diff")

pct_diff_table <- flextable(pct_diff)
pct_diff_table

# ---- line-graph ----------------------------------------------------

g1 <- ds0 %>% 
  group_by(year,rural) %>% 
  summarise(
    percentage = mean(Percentage)
    ,us_total  = mean(`Total - Percentage`)
  ) %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = percentage, group = rural, color = rural)) +
  geom_point(aes(y = percentage, color = rural)) +
  geom_line(aes(y = us_total),color = "#666666") +
  geom_point(aes(y = us_total),color = "#666666") +
  annotate(geom = "text", x = 2011, y = 8, label = "National Average") +
  scale_y_continuous(breaks = seq(6,13,0.5), labels = function(x) paste0(x, "%")) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2", labels = c( "TRUE" = "Rural"
                                                    ,"FALSE" = "Non-Rural")) +
  labs(
    title = "Diagnosed Diabetes in Adults \nRural and Non-Rural Counties in North Carolina"
    ,x = NULL
    ,y = NULL
    ,color = NULL
  )
g1



# ---- obesity graph ---------------------------------------------------------

#merge data

nc_diabetes_data_16 <- nc_diabetes_data %>% 
  filter(year == 2016) %>% 
  left_join(nc_obestiy_16, by = c("County" = "county"))

#graph

g_obesity <- nc_diabetes_data_16 %>% 
  ggplot(aes(x = Percentage, y = pct_obese, color = rural)) +
  geom_point(na.rm = TRUE) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2", labels = c( "TRUE" = "Rural"
                                                    ,"FALSE" = "Non-Rural")) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title  = "Diagnosed Diabetes in Adults \nagainst County Adult Obesity Rates, 2016"
    ,x     = "Diagnosed Diabetes"
    ,y     = "Adult Obesity"
    ,color = NULL
  )

  
g_obesity


# ---- NC-Population ------------------------------------------------------

nc_pop <- 10488084
pct_diabetes <- 0.13
programs <- 77

pop_per_program <-  (nc_pop*pct_diabetes)/programs



