#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

# ---- declare-globals ---------------------------------------------------------

publish_report <- function(file, format){
  rmarkdown::render(
    input = file
    ,output_format = format
    ,output_dir = "./manipulation/publish"
  )
}

# ---- publish ---------------------------------------------------------------

base_dir <- "./manipulation/"

scripts <- c(
  "0-greeter-census-rural-housing.R"
  ,"0-greeter-county-rankings-national.R"
  ,"0-greeter-us-county-population-estimates.R"
  ,"0-greeter-us-diabetes.R"
  ,"1-scribe-diabetes-data-set.R"
  ,"0-greeter-pop-estimates-2000-2009.R"
)



publish_report(paste0(base_dir, scripts[1]),"github_document")
publish_report(paste0(base_dir, scripts[2]),"github_document")
publish_report(paste0(base_dir, scripts[3]),"github_document")
publish_report(paste0(base_dir, scripts[4]),"github_document")
publish_report(paste0(base_dir, scripts[5]),"github_document")
publish_report(paste0(base_dir, scripts[6]),"github_document")


