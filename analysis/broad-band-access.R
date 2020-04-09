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

# ---- declare-globals ------------------------------------------------

# ---- load-data ------------------------------------------------------

acs_raw <- readr::read_csv("./data-public/raw/ACS/ACSDP5Y2018.DP02_data_with_overlays_2020-04-09T140126.csv"
                           ) 
# ---- load-map-data -------------------------------------------------

nc_county_map <- map_data("county",region = "north carolina")



# ---- tweak-data -----------------------------------------------------

acs0 <- acs_raw %>% 
  select(NAME,DP02_0152PE) %>% 
  filter(!NAME == "Geographic Area Name") %>% 
  mutate_at(
    "NAME", ~stringr::str_remove_all(.," County, North Carolina")
  ) %>% 
  rename(county = NAME,pct_internet_access = DP02_0152PE) %>% 
  mutate_at(
    "county", tolower) %>% 
  mutate(
    pct_internet_access  = as.numeric(pct_internet_access)
    ,pct_without_internet = 100 - pct_internet_access
  )


# ---- map -------------------------------------------------------------------

broadband_map <- acs0 %>% 
  left_join(nc_county_map, by = c("county" = "subregion")) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = pct_without_internet)) +
  geom_polygon(color = "black") +
  coord_map() +
  theme_void() +
  theme(
    legend.direction = "horizontal"
    ,legend.position = c(0.3,0.1)
    ,plot.title = element_text(hjust = 0.5)
    ,plot.caption = element_text(hjust = 0.1)
  ) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(
    title = "Precent of Homes without Internet Connection 2018"
    ,fill = NULL
  )

broadband_map
