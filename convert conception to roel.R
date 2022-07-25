library(readr)
library(readxl)

test <- readr::read_csv2("Program.csv",
                         locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
                         show_col_types = FALSE) %>%
  select(PROGRAM, FOLDER_VAR, FILE, TYPE)


test1 <- readxl::read_excel("index.xlsx")
test1 <- test1 %>% select(dataset_name, step_producing_this_dataset, folder_where_the_datset_is_stored,
                          input_datasets_for_the_step) %>%
  

test1 <- test1(TYPE)
