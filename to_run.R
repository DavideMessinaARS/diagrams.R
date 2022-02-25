
##%######################################################%##
#                                                          #
####                 Required packages                  ####
#                                                          #
##%######################################################%##

library(magrittr)
library(xml2)

##%######################################################%##
#                                                          #
####                    Parameters                      ####
#                                                          #
##%######################################################%##

source("create diagram.R")
source("helper_functions.R")
source("styles_and_parameters.R")

steps_style <- c(cell_style = "circle")
datamodels_style <- c(cell_style = "yellow")
arrows_style <- c(arrow_style = "circle arrow")

test_xml <- create_diagram("Program.csv", pages = 1, arrows_style, steps_style, datamodels_style, direction = "TB")
xml2::write_xml(test_xml, "roel diagram.xml")

