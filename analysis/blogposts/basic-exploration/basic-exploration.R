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
library(readr)

# ---- load-sources ---------------------------------------------------



# ---- declare-globals ----------------------------------------------------

#set ggplot theme
ggplot2::theme_set(theme_bw())

# ---- load-data ------------------------------------------------------

# load the data, and have all column names in lowercase

nc_diabetes_data_raw <- read_rds("./data-public/derived/nc-diabetes-data.rds") %>% 
  rename_all(tolower)

us_diabetes_data_raw <- read_csv("data-public/raw/us_diabetes_totals.csv", 
                                        skip = 2)

rural_counties <- read_csv("./data-public/metadata/rural-counties.csv")

county_centers_raw <- readxl::read_xlsx("./data-public/raw/nc_county_centers.xlsx", col_names = c("county", "lat","long"))
  
diabetes_atlas_data_raw <- read_csv("data-public/raw/DiabetesAtlasData.csv", 
                              col_types = cols(LowerLimit = col_skip(), 
                                               UpperLimit = col_skip(),
                                               Percentage = col_double()), skip = 2)




# ---- load-map-data ----------------------------------------------------------

# load in both US State Map and NC County Map

nc_counties_map <- st_as_sf(map("county",region = "north carolina", plot = FALSE,fill = TRUE)) %>% 
  mutate_at("ID", ~stringr::str_remove(.,"north carolina,"))

state_map_raw <- st_as_sf(map("state",plot = FALSE,fill = TRUE ))


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
  filter(Year >= 2000) %>% 
  select( "Year","Total - Percentage") %>% 
  rename(year = Year , us_pct = `Total - Percentage`)

diabetes_atlas_data <- diabetes_atlas_data_raw %>% 
  mutate_at("State", tolower) %>% 
  filter(Year >= 2000)

state_map_abb <- state_map_raw %>% 
  left_join(read_csv("./data-public/metadata/state-abb.csv") %>% 
              mutate_at("state", tolower)
            ,by = c("ID" = "state") )
  


# ---- merge-data ---------------------------------------------------------

#join US totals to NC data 

nc_diabetes_data <- nc_diabetes_data_raw %>% 
  mutate(
    rural = county %in% rural_counties$rural_counties
  ) %>% 
  mutate_at("county",tolower) %>% 
  left_join(us_diabetes_data)


nc_counties_map <- nc_counties_map %>% 
  left_join(nc_diabetes_data, by = c("ID" = "county")) %>% 
  left_join(county_centers, by = c("ID" = "county")) %>% 
  rename(
    center_long = long
    ,center_lat = lat)

state_map <- state_map_abb %>% 
  left_join(diabetes_atlas_data, by = c("ID" = "State")) %>% 
  rename_all(tolower)
  


# ---- o-g1 ------------------------------------------------------------------

o_g1 <- us_diabetes_data %>% 
  ggplot(aes(x = year, y = us_pct)) +
  geom_line(color= "#D95F02") +
  geom_point(shape = 21, size = 3,color= "#D95F02") +
  labs(
    title    = "Percentage of Diagnosed Diabetes in Adults (18+), National Level"
    ,x       = NULL
    ,y       = NULL
    ,caption = "Note: Data from the CDC's National Health Interview Survey (NHIS)"
  )

o_g1



# ---- s-g1 -----------------------------------------------------------------


s_g1 <- state_map %>% 
  st_drop_geometry() %>% 
  ggplot(aes(x = year, y = percentage, color = region)) +
  geom_line(aes(group = id ),na.rm = TRUE) +
  geom_vline(xintercept = 2011, linetype = "dashed", color = "gray") +
  scale_color_brewer(palette    = "Dark2"
                     ,direction = -1
                     ,labels    = snakecase::to_title_case
                     ) +
  labs(
    title    = "Percentage of Diagnosed Diabetes in Adults (18+) \nby State and Region"
    ,x       = NULL
    ,y       = NULL
    ,color   = "Region"
    ,caption = "Regions from US Census Bureau"
  ) 

s_g1




# ---- s-g2 ---------------------------------------------------------------

s_g2 <- state_map %>% 
  st_drop_geometry() %>% 
  filter(region == "south") %>% 
  mutate_at("id", ~snakecase::to_title_case(.)) %>% 
  ggplot(aes(x = year, y = percentage)) +
  geom_line(aes(group = id ),na.rm = TRUE, color= "#D95F02") +
  gghighlight::gghighlight(id == "North Carolina", label_params = list(vjust = 3)) +
  labs(
    title    = "Percentage of Diagnosed Diabetes in Adults (18+) \nSouth Region"
    ,x       = NULL
    ,y       = NULL
    ,caption = "Regions from US Census Bureau"
    
  )

s_g2



# ---- nc-g1 ----------------------------------------------------------------------

d1 <- nc_diabetes_data %>% 
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
  )

nc_g1 <- d1 %>% 
  ggplot(aes(x = year, y = values, color = metric)) +
  geom_line() +
  geom_point(shape = 21, size = 3) +
  geom_vline(xintercept = 2011, linetype = "dashed", color = "gray") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x      = NULL
    ,y     = NULL
    ,color = NULL
    ,title = "Percent of Adults (20+) with Diagnosed Diabetes"
  )
   
nc_g1 

# ---- nc-g2 -----------------------------------------------------------------

d2 <- nc_diabetes_data %>% 
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

nc_g2 <- d2 %>% ggplot(aes(x = year, y = value, color = metric)) +
  geom_line() +
  geom_point(shape = 21, size = 3) +
  geom_vline(xintercept = 2011, linetype = "dashed", color = "gray") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x      = NULL
    ,y     = NULL
    ,color = NULL
    ,title = "Percent of Adults (20+) with Diagnosed Diabetes \nDisplaying Rural vs Urban"
  )

nc_g2


# ---- c-g1 --------------------------------------------------------------


nc_counties_map_binned <- nc_counties_map %>% 
  filter(year < 2016) %>% 
  mutate(
    bin = dlookr::binning(.$percentage, nbins = 6 ,type = "equal")
    ,bin = forcats::fct_recode(bin
      ,"6.5 - 7.97"  =  "[6.5,7.97]"
      ,"7.97 - 9.43" =  "(7.97,9.43]" 
      ,"9.43 - 10.9" =  "(9.43,10.9]" 
      ,"10.9 - 12.4" =  "(10.9,12.4]"
      ,"12.4 - 13.8" =  "(12.4,13.8]"  
      ,"13.8 - 15.3" =  "(13.8,15.3]"
    )
  )

c_g1 <- nc_counties_map_binned %>% 
  filter(year == 2006) %>% 
  ggplot() +
  geom_sf(aes(fill = bin, color = rural)) +
  scale_size(range = c(1,5)) +
  scale_fill_viridis_d(alpha = 0.6, direction = -1) +
  scale_color_manual(
    values = c(
      "TRUE" = "black"
      ,"FALSE" = "lightgrey"
      
    ),guide = 'none') +
  labs(
    title = "Diagnosied Diabetes (Adults 20+) by County 2006"
    ,fill = "Rural"
  )

c_g1





  

# ---- c-g2  --------------------------------------------------------------

c_g2 <- nc_counties_map_binned %>% 
  filter(year == 2011) %>% 
  ggplot() +
  geom_sf(aes(fill = bin, color = rural)) +
  scale_size(range = c(1,5)) +
  scale_fill_viridis_d(alpha = 0.6, direction = -1) +
  scale_color_manual(
    values = c(
      "TRUE" = "black"
      ,"FALSE" = "lightgrey"
    ),guide = 'none') +
  labs(
    title = "Diagnosied Diabetes (Adults 20+) by County 2011"
    ,fill = "Rural"
  )

c_g2

# ---- c-g3 -------------------------------------------------------------

c_g3 <- nc_counties_map_binned %>% 
  filter(year == 2012) %>% 
  ggplot() +
  geom_sf(aes(fill = bin, color = rural)) +
  scale_size(range = c(1,5)) +
  scale_fill_viridis_d(alpha = 0.6, direction = -1) +
  scale_color_manual(
    values = c(
      "TRUE" = "black"
      ,"FALSE" = "lightgrey"
    ),guide = 'none') +
  labs(
    title = "Diagnosied Diabetes (Adults 20+) by County 2012"
    ,fill = "Rural"
  )

c_g3


# ----c-g4 ----------------------------------------------------------------

c_g4 <- nc_counties_map_binned %>% 
  filter(year == 2015) %>% 
  ggplot() +
  geom_sf(aes(fill = bin, color = rural)) +
  scale_size(range = c(1,5)) +
  scale_fill_viridis_d(alpha = 0.6, direction = -1) +
  scale_color_manual(
    values = c(
      "TRUE" = "black"
      ,"FALSE" = "lightgrey"
    ),guide = 'none') +
  labs(
    title = "Diagnosied Diabetes (Adults 20+) by County 2015"
    ,fill = "Rural"
  )

c_g4





# ---- testing -----------------------------------------------------------------

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


# label with text instead of bubbles, size depends on percentage

counties %>% 
  filter(year == 2006) %>% 
  ggplot() +
  geom_sf(aes(fill = rural)) +
  # geom_sf(data = county_centers_2006
  #         ,aes(size = percentage)
  #         ,shape = 21
  #         ,fill = "#0571b0"
  #         ,color = "black"
  #         ,alpha = 0.8) +
  geom_text(data = county_centers_2006
            ,aes(x = long, y = lat, label = percentage, size = percentage)
            ,color = "black") +
  scale_size(range = c(1,5)) +
  scale_fill_viridis_d(alpha = 0.5, direction = -1) +
  guides(
    fill = guide_legend(title = "Rural")
    ,size = guide_legend(title = "Percentage")
  ) +
  labs(
    title = "Diagnosied Diabetes by County 2006"
  )


# text labels, over color gradient, with outlines

counties %>% 
  filter(year == 2006) %>% 
  ggplot() +
  geom_sf(aes(fill = percentage, color = rural)) +
  # geom_sf(data = county_centers_2006
  #         ,aes(size = percentage)
  #         ,shape = 21
  #         ,fill = "#0571b0"
  #         ,color = "black"
  #         ,alpha = 0.8) +
  # geom_text(data = county_centers_2006
  #           ,aes(x = long, y = lat, label = percentage
  #                # ,size = percentage
  #                )
            # ,color = "#353839") +
  scale_size(range = c(1,5)) +
  scale_fill_viridis_c(alpha = 0.6, direction = -1) +
  scale_color_manual(
    values = c(
      "TRUE" = "black"
      ,"FALSE" = "lightgrey"
    )) +
  guides(
    fill = guide_legend(title = "Rural")
    ,size = guide_legend(title = "Percentage")
  ) +
  labs(
    title = "Diagnosied Diabetes by County 2006"
  )



#seperate scales for rural and urban

counties %>% 
  filter(year == 2006) %>% 
  mutate(
    percentage = if_else(rural, percentage, percentage * -1 ) ) %>% 
  ggplot() +
    geom_sf(aes(fill = percentage)) +
  scale_fill_gradient2(
    low = "#fc8d59"
    ,mid = "white"
    ,high = "#91bfdb"
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


counties %>% 
  filter(year == 2006) %>% 
  ggplot() +
  geom_sf(aes(fill = percentage, color = rural)) +
  scale_fill_viridis_c(alpha = 0.6, direction = -1) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "white")) +
  # geom_sf_text(aes(label = rural), data = county_centers_2006, color = "#666666") +
  labs(
    title = "Diagnosied Diabetes by County 2006"
    ,x    = NULL
    ,y    = NULL
    ,fill = "Percentage"
  )

#2016 Map

counties %>% 
  filter(year == 2016) %>% 
  mutate(
    percentage = if_else(percentage <25,percentage, NULL)
  ) %>% 
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

# animate

# g <- counties %>% 
#   mutate(
#     percentage = if_else(percentage <25,percentage, NULL)
#   ) %>% 
#   ggplot() +
#   geom_sf(aes(fill = percentage)) +
#   scale_fill_viridis_c(alpha = 0.6
#                        ,direction = -1
#   ) +
#   geom_sf_text(aes(label = rural), data = county_centers, color = "#666666") +
#   labs(
#     title = "Year: {current_frame}"
#   ) +
#   transition_manual(year)
# 
# g <-  animate(g,end_pause = 10, duration = 15)
# 
# anim_save("./analysis/blogposts/basic-exploration/figure_rmd/animate_1.gif")


#testing different graphs

g10 <- nc_diabetes_data %>% 
  filter(year == 2006) %>% 
  ggplot(aes(x = reorder(county, percentage), y = percentage, fill = rural)) +
  geom_col() +
  coord_flip()

g10

g11 <- nc_diabetes_data %>% 
  filter(year == 2006) %>% 
  ggplot(aes(x = percentage, fill = rural)) +
  # geom_histogram() +
  geom_freqpoly(aes(color = rural))
g11


g12 <- nc_diabetes_data %>% 
  mutate(
    percentage = if_else(percentage <25,percentage, NULL)
  ) %>% 
  ggplot(aes(x = county, y = year, fill = percentage)) +
  geom_tile() +
  scale_y_continuous(breaks = seq(2006,2017,1)) +
  scale_fill_viridis_c()
g12


g15 <- nc_diabetes_data %>% 
  ggplot(aes(x = year, y = percentage, group = county)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2006,2017,1)) 
g15


g13 <- nc_diabetes_data %>% 
  mutate(
    percentage = if_else(percentage <25,percentage, NULL)
  ) %>% 
  ggplot(aes(x = year, y = percentage, group = year)) +
  geom_boxplot()

g13


g14 <- diabetes_atlas_data %>% 
  ggplot(aes(x = Year, y = Percentage, group = State)) +
  geom_line(na.rm = TRUE, color = "#2b83ba", size = 1) +
  gghighlight::gghighlight(State == "North Carolina", use_direct_label = FALSE)

g14


