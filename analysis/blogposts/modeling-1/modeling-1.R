#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)
library(rsample)
library(rpart)
library(rpart.plot)
library(caret)
library(ranger)

# ---- declare-globals ---------------------------------------------------------

knitr::opts_knit$set(root.dir='../../../')
set.seed(1234)

# ---- load-data ---------------------------------------------------------------

ds0 <- read_rds("./data-public/derived/population-risk-factors.rds")

# ---- tweak-&-sample-data ----------------------------------------------------

ds_modeling <- ds0 %>% 
   drop_na() %>%  
  select(-year,-county, -county_fips, -physical_inactivity_percent) %>% 
  rename(
    pct_75_over = `pct_75+`
  ) %>% 
  rename_with(~str_replace_all(.,"-","_")) %>% 
  drop_na() %>% 
  mutate(across(rural, as_factor))

ds_split <- initial_split(ds_modeling, prop = 0.7, strata = "diabetes_percentage")
ds_train <- training(ds_split)
ds_test  <- testing(ds_split)

# ---- glm ------------------------------------------------------------------

diabetes_glm <- glm(diabetes_percentage ~ ., data = ds_train) 

broom::tidy(diabetes_glm)

diabetes_glm_cv <- train(
  form = diabetes_percentage ~ ., 
  data = ds_train, 
  method = "glm",
  trControl = trainControl(method = "cv", number = 10) 
)


broom::tidy(diabetes_glm_cv$finalModel)

ds2 <- broom::augment(diabetes_glm_cv$finalModel, data = ds_train)

ds2 %>% ggplot(aes(x = .fitted, y = .resid)) +
  geom_point()

vip::vip(diabetes_glm_cv)


# ---- decision-tree ----------------------------------------------------------


diabetes_dt1 <- rpart(
  formula = diabetes_percentage ~ .,
  data    = ds_train,
  method  = "anova" 
)

diabetes_dt1

rpart.plot(diabetes_dt1)
summary(diabetes_dt1)

vip::vip(diabetes_dt1)

# ---- random-forest ----------------------------------------------------------


# number of features
n_features <- length(setdiff(names(ds_train), "diabetes_percentage"))

# train a default random forest model
diabetes_rf1 <- ranger(
  diabetes_percentage ~ ., 
  data = ds_train,
  mtry = floor(n_features / 3),
  seed = 1234,
  importance = "impurity"
  ,respect.unordered.factors = TRUE
)


(default_rmse <- sqrt(diabetes_rf1$prediction.error))


vip::vip(diabetes_rf1, geom = "point")





