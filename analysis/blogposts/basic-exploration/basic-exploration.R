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
library(sf)
library(gganimate)



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
    

# ---- g2 -----------------------------------------------------------------

d <- nc_diabetes_data %>% 
  select(-us_pct) %>% 
  mutate(
    pct_rural  = if_else(rural == TRUE, percentage, NULL)
    ,pct_urban = if_else(rural == FALSE, percentage, NULL)
  ) %>% 
  select(-countyfips,-percentage) %>% 
  group_by(year) %>% 
  summarise(
    pct_rural = mean(pct_rural,na.rm = TRUE)
    ,pct_urban = mean(pct_urban,na.rm = TRUE)
  ) %>% left_join(us_diabetes_data) %>% 
  pivot_longer(
    cols       = c("us_pct", "pct_rural","pct_urban")
    ,names_to  = "metric"
    ,values_to = "value"
    ,values_drop_na = TRUE
  ) %>% 
  mutate(
    metric = factor(metric,
                    levels  = c("pct_rural","pct_urban","us_pct")
                    ,labels = c("Rural","Urban","US")
                    )
  )



d %>% ggplot(aes(x = year, y = value, color = metric)) +
  geom_line() +
  geom_point(shape = 21, size = 3) +
  # geom_smooth(method = "lm",se = FALSE) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x      = NULL
    ,y     = NULL
    ,color = NULL
    ,title = "Percent of Adults (20+) with Diagnosed Diabetes \nDisplaying Rural vs Urban"
  ) 
  # ggpmisc::stat_poly_eq(formula = y ~ + x 
  #                     ,aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))
  #                       ,parse = TRUE
  #                       )


# ---- g3 -----------------------------------------------------------------

# 2006 Map Graph

counties <- st_as_sf(map("county",region = "north carolina", plot = FALSE,fill = TRUE)) %>% 
  mutate_at("ID", ~stringr::str_remove(.,"north carolina,")) %>% 
  left_join(nc_diabetes_data, by = c("ID" = "county")) 


county_centers <- st_as_sf(county_centers, coords = c("long","lat")
                           ,remove = FALSE, agr = "constant", crs = 4326)  


county_centers <- county_centers  %>% 
  left_join(nc_diabetes_data) %>% 
  mutate(
    rural = if_else(rural,"R","U")
  )

county_centers_2006 <- county_centers %>% filter(year == 2006) 
county_centers_2016 <- county_centers %>% filter(year == 2016)


counties %>% 
  filter(year == 2006) %>% 
  ggplot() +
  geom_sf(aes(fill = rural)) +
  geom_sf(data = county_centers_2006
          ,aes(size = percentage)
          ,shape = 21
          ,fill = "#0571b0"
          ,color = "black"
          ,alpha = 0.8) +
  scale_size(range = c(1,10)) +
  scale_fill_viridis_d(alpha = 0.5, direction = -1) +
  guides(
    fill = guide_legend(title = "Rural")
    ,size = guide_legend(title = "Percentage")
  ) +
  labs(
    title = "Diagnosied Diabetes by County 2006"
  )





counties %>% 
  filter(year == 2006) %>% 
  ggplot() +
  geom_sf(aes(fill = percentage)) +
  scale_fill_viridis_c(alpha = 0.6, direction = -1) +
  geom_sf_text(aes(label = rural), data = county_centers_2006, color = "#666666") +
  labs(
    title = "Diagnosied Diabetes by County 2006"
    ,x    = NULL
    ,y    = NULL
    ,fill = "Percentage"
  )


# ---- g4 -----------------------------------------------------------------

#2016 Map

counties %>% 
  filter(year == 2016) %>% 
  ggplot() +
  geom_sf(aes(fill = rural)) +
  geom_sf(data = county_centers_2016
          ,aes(size = percentage)
          ,shape = 21
          ,fill = "#0571b0"
          ,color = "black"
          ,alpha = 0.8) +
  scale_size(range = c(1,10)) +
  scale_fill_viridis_d(alpha = 0.5, direction = -1) +
  guides(
    fill = guide_legend(title = "Rural")
    ,size = guide_legend(title = "Percentage")
  ) +
  labs(
    title = "Diagnosied Diabetes by County 2016"
  )


counties %>% 
  filter(year == 2016) %>% 
  mutate(
   percentage = if_else(percentage <25,percentage, NULL)
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = percentage)) +
  scale_fill_viridis_c(alpha = 0.6
                       ,direction = -1
  ) +
  geom_sf_text(aes(label = rural), data = county_centers_2016, color = "#666666") +
  labs(
    title = "Diagnosied Diabetes by County 2016"
    ,x    = NULL
    ,y    = NULL
    ,fill = "Percentage"
    ,caption = "Note : Jones County = 27.1%"
  )

# ---- animate ---------

g <- counties %>% 
  ggplot() +
  geom_sf(aes(fill = percentage)) +
  scale_fill_viridis_c(alpha = 0.6
                       ,direction = -1
  ) +
  transition_manual(year)

g <-  animate(g,end_pause = 10)

anim_save("./analysis/blogposts/basic-exploration/figure_rmd/animate_1.gif")


         
