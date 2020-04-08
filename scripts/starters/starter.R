#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages --------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(dplyr)    # data wrangling
library(ggplot2)  # graphs
library(tidyr)    # data tidying

# ---- load-sources ---------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graphing/graph-presets.R") # fonts, colors, themes

# ---- declare-globals ------------------------------------------------

# ---- load-data ------------------------------------------------------

# ---- inspect-data ---------------------------------------------------

# ---- tweak-data -----------------------------------------------------

# ---- basic-table ----------------------------------------------------

# ---- basic-graph ----------------------------------------------------


