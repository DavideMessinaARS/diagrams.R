library(readr)
library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)

conception_to_roel <- function(file) {
  
  test1 <- file
  test2 <- test1 %>% select(dataset_name, step_producing_this_dataset, folder_where_the_datset_is_stored) %>%
    mutate(TYPE = "OUTPUT") %>%
    unique()
  
  test3 <- test1 %>% select(step_producing_this_dataset, folder_where_the_datset_is_stored,
                            dataset_name = input_datasets_for_the_step) %>%
    drop_na(dataset_name) %>%
    mutate(TYPE = "INPUT") %>%
    unique() %>%
    separate_rows(dataset_name, sep = " ", convert = FALSE)
  
  test23 <- rbind(test2, test3)
  test23 <- test23 %>%
    mutate(folder_where_the_datset_is_stored = "a") %>%
    rename(PROGRAM = "step_producing_this_dataset") %>%
    rename(FOLDER_VAR = "folder_where_the_datset_is_stored") %>%
    rename(FILE = "dataset_name") %>%
    select(PROGRAM, FOLDER_VAR, FILE, TYPE)
  
  return(test23)
}





data.table::fwrite(test23, "index.csv")

source("create diagram.R")
source("helper_functions.R")
source("styles_and_parameters.R")

steps_style <- c(cell_style = "circle")
datamodels_style <- c(cell_style = "yellow")
arrows_style <- c(arrow_style = "circle arrow")

test_xml <- create_diagram("index.csv", pages = 1, arrows_style, steps_style, datamodels_style, direction = "TB")
xml2::write_xml(test_xml, "test CovE.xml")
