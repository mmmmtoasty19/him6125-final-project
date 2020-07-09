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

# box plots to compare variables by county type
# do this first

# post stcture
# diabetes by county (reuse from last post)
# sequence of graphs, showing (all) variables by county type
# test variables for significant of rural counties (create list outcome)
# using above create facet graph with box plots, including signif test codes
# extract signif from glm model 

# put all predictors in order, step forward model



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

broom::tidy(diabetes_glm) %>% arrange(p.value)
predictors <- broom::tidy(diabetes_glm) %>% arrange(p.value) %>% pull(term)

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


vip::vip(diabetes_rf1, geom = "col")


# ---- function ------

scatter_by_groups <- function(
  d,xvar, yvar, groupvar, jitterwidth=0, jitterheight=0,  xlabel = xvar, ylabel=yvar, grouplabel=groupvar
){
  # d <- dsm2
  # xvar = "knowledge_oud_tx"
  # xvar = "n_tx_helpful"
  # yvar = "hr_support"
  # groupvar = "sex"
  
  g1 <- d %>%
    ggplot(aes_string(x = xvar, y = yvar, color = groupvar))+
    
    geom_point(shape = 21, size = 3, alpha = .6,
               position = position_jitter(width=jitterwidth,height = jitterheight, seed = 42))+
    scale_color_viridis_d(begin = 0, end = .6, option = "plasma")+
    geom_smooth(method="lm", se = F)+
    ggpmisc::stat_poly_eq(formula = y ~ + x ,
                          aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                          parse = TRUE)+
    labs(
      title = paste0("Relationship between (", yvar,") and (", xvar,") for different levels of (",groupvar,")"),
      caption = paste0("N = ", nrow(d)),
      x = xlabel,
      y = ylabel,
      color = grouplabel
    )
  return(g1)
  
}


ds_modeling %>% scatter_by_groups(
  "diabetes_percentage"
  ,"adult_obesity_percent"
  ,"rural"
)

ds_train %>% scatter_by_groups(
  yvar ="diabetes_percentage"
  ,xvar = "pct_white"
  ,groupvar = "rural"
)
ds_train %>% scatter_by_groups(
  yvar ="diabetes_percentage"
  ,xvar = "pct_black_or_african_american "
  ,groupvar = "rural"
)





# rerun GLM in order of pvalue smallest to biggest.  Don't
#use dot
anova(diabetes_glm, test = "Chisq")

#  use to pass predictors to glm models

outcome <- "diabetes_percentage ~ "
predictors_00 <- c(
  "adult_obesity_percent"                
  ,"adult_smoking_percent"
  ,"rural"                           
  ,"pct_75_over"                          
  ,"pct_45_64"                           
  ,"pct_20_44"                            
  ,"pct_65_74"                           
  # ,"pct_not_hispanic_or_latino"           
  # ,"pct_hispanic_or_latino"              
  # ,"pct_asian_or_pacific_islander"        
  ,"pct_white"
  ,"pct_black_or_african_american"        
  # ,"pct_american_indian_or_alaska_native"           
)

eq_formula <- as.formula(paste0(outcome, paste(predictors_00, collapse = " + ") ) )

model_00 <- stats::glm(
  formula = eq_formula
  ,data = ds_train 
)

anova(model_00, test = "Chisq")


# ---- model_01 ------
# test wether rural status of county contributes to model performance after controling for all other predictors 

outcome <- "diabetes_percentage ~ "
predictors_01 <- c(
  "adult_obesity_percent"
  ,"adult_smoking_percent"
  ,"pct_75_over"
  ,"pct_45_64"
  ,"pct_20_44"
  ,"pct_65_74"
  ,"pct_not_hispanic_or_latino"
  ,"pct_hispanic_or_latino"
  ,"pct_asian_or_pacific_islander"
  ,"pct_white"
  ,"pct_black_or_african_american"
  ,"pct_american_indian_or_alaska_native"
  ,"rural"
)

eq_formula <- as.formula(paste0(outcome, paste(predictors_01, collapse = " + ") ) )

model_01 <- stats::glm(
  formula = eq_formula
  ,data = ds_train 
)

anova(model_01, test = "Chisq")


glm(pct_white ~ rural, data = ds_train) %>% summary()
glm(adult_obesity_percent ~ rural, data = ds_train) %>% summary()
