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

# ---- declare-globals ---------------------------------------------------------
knitr::opts_knit$set(root.dir='../../../')
# ---- load-data ---------------------------------------------------------------


ds0 <- read_csv("./data-public/derived/population-risk-factors.csv")



# ---- decision-tree ----------------------------------------------------------
ds1 <- ds0 %>% 
  filter(year == 2010) %>% 
  select(-year,-county, -countyfips, -physical_inactivity_percent) %>% 
  rename(
    pct_75_over = `pct_75+`
  ) %>% 
  rename_with(~str_replace_all(.,"-","_")) %>% 
  drop_na() %>% 
  mutate(across(rural, as_factor))

set.seed(1234)

split_1  <- initial_split(ds1, prop = 0.7)
train_3  <- training(split_1)
test_3   <- testing(split_1)



diabetes_dt1 <- rpart(
  formula = diabetes_percentage ~ .,
  data    = train_3,
  method  = "anova" 
)


diabetes_dt1

rpart.plot(diabetes_dt1)
summary(diabetes_dt1)

# ---- random-forest ----------------------------------------------------------


library(ranger)

# number of features
n_features <- length(setdiff(names(train_3), "diabetes_percentage"))

# train a default random forest model
diabetes_rf1 <- ranger(
  diabetes_percentage ~ ., 
  data = train_3,
  mtry = floor(n_features / 3),
  seed = 1234,
  importance = "impurity"
  ,respect.unordered.factors = TRUE
)


(default_rmse <- sqrt(diabetes_rf1$prediction.error))


g1 <- vip::vip(diabetes_rf1, geom = "point")
g1

pred <- predict(diabetes_rf1, data = test_3)



diabetes_predict <- test_3 %>% 
  bind_cols("predictions" = pred[["predictions"]]) %>% 
  mutate(
    residual = predictions -diabetes_percentage
  ) 

diabetes_predict %>% 
  ggplot(aes(x = predictions, y = residual)) +
  geom_point()


# ---- glm ------------------------------------------------------------------

diabetes_glm <- glm(diabetes_percentage ~ ., data = train_3) 
diabetes_glm

broom::tidy(diabetes_glm)

diabetes_glm_cv <- train(
  form = diabetes_percentage ~ ., 
  data = train_3, 
  method = "glm",
  trControl = trainControl(method = "cv", number = 10) 
)
diabetes_glm_cv

ds2 <- broom::augment(diabetes_glm_cv$finalModel, data = train_3)

ds2 %>% ggplot(aes(x = .fitted, y = .resid)) +
  geom_point()

vip::vip(diabetes_glm_cv)


# ---- publish ----------------------------------------------------------------

# rmarkdown::render("./analysis/blogposts/modeling-1/modeling-1.R")
