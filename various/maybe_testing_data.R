nodes_init <- tibble::tribble(
  ~id,                   ~level, ~url_test,
  "SURVEY_ID",                1, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "SURVEY_OBSERVATIONS",      2, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "VACCINES",                 3, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "OBSERVATION_PERIODS",      4, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "PERSONS",                  5, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "CDM_SOURCE",               6, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "METADATA",                 7, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "INSTANCE",                 8, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts'
)

nodes_init <- nodes_init %>% dplyr::mutate(shape = "box", label = id, group = "0", x = 1, fixed = T)
nodes_invis <- nodes_init %>% dplyr::mutate(id = paste0(id, "_invisible"), group = "0_invisible",
                                            url_test = NULL, x = 2, color = 'rgba(0,0,0,0)', font.color = 'rgba(0,0,0,0)')
edges_invis <- nodes_init %>% dplyr::transmute(from = id) %>%
  dplyr::mutate(to = nodes_invis$id, length = 300, color = "rgba(0,0,0,0)")

edges_init <- tibble::tribble(
  ~from,                 ~to,       
  "SURVEY_ID",           "SURVEY_OBSERVATIONS",
  "SURVEY_OBSERVATIONS", "VACCINES",
  "VACCINES",            "OBSERVATION_PERIODS",
  "OBSERVATION_PERIODS", "PERSONS",
  "PERSONS",             "CDM_SOURCE",  
  "CDM_SOURCE",          "METADATA",  
  "METADATA",            "INSTANCE"
)
edges_init <- edges_init %>% dplyr::mutate(length = 150, color = "rgba(0,0,0,0)")

nodes_init <- dplyr::bind_rows(nodes_init, nodes_invis)
edges_init <- dplyr::bind_rows(edges_init, edges_invis)

table1 <- tibble::tribble(
  ~Name, ~Description, ~Format_Vocabulary, ~Comments,
  "var1", "first variable test",  "Number, integer",  "",
  "var2", "second variable test",  "ARS<br>BIFAP<br>CPRD<br>PHARMO",  ""
)

table2 <- tibble::tribble(
  ~Name, ~Description, ~Format_Vocabulary, ~Comments,
  "id", "id test",  "Number, integer",  "",
  "var2", "second variable test",  "ARS<br>BIFAP<br>CPRD<br>PHARMO",  ""
)

table3 <- tibble::tribble(
  ~Name, ~Description, ~Format_Vocabulary, ~Comments,
  "sex", "first variable test",  "Number, integer",  "",
  "var2", "second variable test",  "ARS<br>BIFAP<br>CPRD<br>PHARMO",  ""
)

table4 <- tibble::tribble(
  ~Name, ~Description, ~Format_Vocabulary, ~Comments,
  "birth cohort", "first variable test",  "yymmdd",  "",
  "var2", "second variable test",  "ARS<br>BIFAP<br>CPRD<br>PHARMO",  ""
)

table5 <- tibble::tribble(
  ~Name, ~Description, ~Format_Vocabulary, ~Comments,
  "dose", "first variable test",  "0=not<br>vaccinated<br>1=1°dose<br>2=2°dose",  "",
  "var2", "second variable test",  "ARS<br>BIFAP<br>CPRD<br>PHARMO",  ""
)