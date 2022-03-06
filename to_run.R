
##%######################################################%##
#                                                          #
####                 Required packages                  ####
#                                                          #
##%######################################################%##

library(magrittr)
library(xml2)
library(tibble)
library(dplyr)

##%######################################################%##
#                                                          #
####                    Parameters                      ####
#                                                          #
##%######################################################%##

source("create diagram.R")
source("helper_functions.R")
source("styles_and_parameters.R")
source("create_ids.R")

steps_style <- c(cell_style = "circle")
datamodels_style <- c(cell_style = "yellow")
arrows_style <- c(arrow_style = "circle arrow")

path = "Program.csv"
direction = "TB"
pages = 1

test_xml <- create_diagram(path, pages, arrows_style, steps_style, datamodels_style, direction)
xml2::write_xml(test_xml, "roel diagram.xml")

