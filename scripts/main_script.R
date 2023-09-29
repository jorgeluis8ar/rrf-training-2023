# 0. Base instructions ------------------------------------------------------

# Project name: RRF 2023 repository
# Description: This scripts is the masters script for the project
# Data: Secondary data analysis
# Date: 09/25/2023
# Author: Jorge Luis Ochoa Rincon

# 1. Cleaning the environment and console -----------------------------------

cat("\f")
rm(list = ls())

# renv::init()
# renv::restore()
# renv::snapshot()
#
# install.packages("pacman")

packages <- c("tidyr","ggplot2","readr","dplyr","data.table","fixest",
              "collapse","stargazer","stringi","labelled","dlookr")

pacman::p_load(packages,character.only = TRUE,install = FALSE)


# 2. Running scripts --------------------------------------------------------

source("scripts/cleaning.R")
source("scripts/analysis.R")



