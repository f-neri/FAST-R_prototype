# FAST-Data-Analysis

# To run the data analysis app for the FAST workflow, run the the code below

# Note that you'll need to remove the "#" in front of all of the install.packages lines (e.g. "install.packages("shiny")") if
# this is the first time you're using this script (and have never installed the required packages before).

# install.packages("shiny")
# install.packages("devtools")
# install.packages("remotes")
# remotes::install_github('wleepang/shiny-directory-input')
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("plater")
# install.packages("beepr")
# install.packages("openxlsx")
# install.packages("scales")
# remotes::install_github("dmurdoch/rgl")
# install.packages("htmlwidgets")


library(shiny)
runGitHub("FAST-Data-Analysis", "f-neri")

