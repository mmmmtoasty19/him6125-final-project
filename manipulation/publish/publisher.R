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


# publish_report("./manipulation/scribe-population-data.R", "html_document")
publish_report("./manipulation/scribe-population-data.R", "github_document")
