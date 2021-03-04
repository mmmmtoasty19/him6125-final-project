library(ggplot2)
library(magrittr)
library(dplyr)

prints_folder <- paste0("./analysis/ideas/prints/")
if(!file.exists(prints_folder)){
  dir.create(file.path(prints_folder))
}

ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey95", color = NA)
    )
)
quick_save <- function(g,name,...){
  ggplot2::ggsave(
    plot     = g,
    filename = paste0(name,".jpg"),
    device   = "jpg",
    path     = prints_folder,
    # width    = width,
    # height   = height,
    # units = "cm",
    dpi      = 'retina',
    limitsize = FALSE,
    ...
  )
}










ds <- `diabetes-modeling-data`

a_sample <- ds %>% dplyr::distinct(county_fips) %>% pull(county_fips) %>% 
  sample(100, replace = FALSE)


glimpse(ds)


`diabetes-modeling-data` %>% 
  filter(county_fips %in% a_sample) %>% 
  filter(year %in% c(2012:2018)) %>% 
  ggplot(aes(x = year, y = diabetes_percentage, group = county_fips)) +
           geom_line(alpha = 0.3)



ds1 <- ds %>% 
  filter(year %in% c(2014:2017)) %>% 
  # filter(county_fips %in% c(19069, 20109)) %>% 
  group_by(county_fips) %>% 
  mutate(
    lag_dif1 = diabetes_percentage - lag(diabetes_percentage, 1)
    ,lag_dif2 = diabetes_percentage - lag(diabetes_percentage, 2)
  )

ds1 %>% select(county_fips, year, diabetes_percentage, lag_dif1, lag_dif2) %>% print()


d15_16 <- ds1 %>% 
  filter(year == 2016) %>% 
  mutate(
    lag15_16 = lag_dif1
  ) %>% 
  select(county_fips, lag15_16, state_abb)




d16_17 <- ds1 %>% 
  filter(year == 2017) %>% 
  mutate(
    lag16_17 = lag_dif1
  ) %>% select(county_fips, lag16_17, state_abb)




d15_17 <- ds1 %>% 
  filter(year == 2017) %>% 
  mutate(
    lag15_17 = lag_dif2
  ) %>% 
  select(county_fips, lag15_17, state_abb)



ds_diff <- d15_16 %>% 
  left_join(d16_17) %>% 
  left_join(d15_17)

ds_diff


(ds_diff %>% ggplot(aes(x = lag15_16, y = lag15_17)) +
  geom_point(shape = 21, alpha = 0.5) +
  geom_hline(yintercept = 0, alpha = 0.4, linetype = "dashed") +
  geom_vline(xintercept = 0, alpha = 0.4, linetype = "dashed") +
  facet_wrap(~state_abb
             # , scales = "free"
             ) +
  geom_smooth(method = "loess", se = FALSE)) %>% 
  quick_save("01_scatter_diff", width = 10, height = 10)



(ds_diff %>% ggplot(aes(x = lag15_16, y = lag16_17)) +
  geom_point(shape = 21, alpha = 0.5) +
  geom_hline(yintercept = 0, alpha = 0.4, linetype = "dashed") +
  geom_vline(xintercept = 0, alpha = 0.4, linetype = "dashed") +
  facet_wrap(~state_abb
             # , scales = "free"
  ) +
  geom_smooth(method = "loess", se = FALSE)) %>% 
  quick_save("02_scatter_diff", width = 10, height = 10)

