library(readr)
library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)

test <- readr::read_csv2("Program.csv",
                         locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
                         show_col_types = FALSE) %>%
  select(PROGRAM, FOLDER_VAR, FILE, TYPE)


test1 <- readxl::read_excel("index.xlsx")
test2 <- test1 %>% select(dataset_name, step_producing_this_dataset, folder_where_the_datset_is_stored) %>%
  mutate(TYPE = "OUTPUT") %>%
  unique()

test3 <- test1 %>% select(step_producing_this_dataset, folder_where_the_datset_is_stored,
                          dataset_name = input_datasets_for_the_step) %>%
  drop_na(dataset_name) %>%
  mutate(TYPE = "INPUT") %>%
  unique()

test23 <- rbind(test2, test3)
