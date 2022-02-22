step_01_1 <- createCell("step_01_1", cell_style = "white", label = "step 01_1", link = "https://github.com/ARS-toscana/ECVM/wiki/Actions-in-each-step#step-01_1-create-conceptset-datasets-t21", level = 1,
                        input = c("VACCINES"),
                        output = c("CONCEPT"))
step_01_2 <- createCell("step_01_2", cell_style = "white", label = "step 01_2", link = "https://github.com/ARS-toscana/ECVM/wiki/Actions-in-each-step#step-01_2-create-spells-t21", level = 1,
                        input = c("OBSERVATION_PERIODS"),
                        output = c("D3_output_spells_category"))
step_02_1 <- createCell("step_02_1", cell_style = "white", label = "step 02_1", link = "https://github.com/ARS-toscana/ECVM/wiki/Actions-in-each-step#step-02_1-create-quality-check-criteria-t2", level = 3,
                      input = c("CONCEPT"),
                      output = c("D3_concepts_QC_criteria"))
step_02_2 <- createCell("step_02_2", cell_style = "white", label = "step 02_2", link = "https://github.com/ARS-toscana/ECVM/wiki/Actions-in-each-step#step-02_2-apply-quality-check-criteria-t3", level = 3,
                      input = c("D3_concepts_QC_criteria"),
                      output = c("Flowchart_QC_criteria", "selected_doses"))
step_03_1 <- createCell("step_03_1", cell_style = "white", label = "step 03_1", link = "https://github.com/ARS-toscana/ECVM/wiki/Actions-in-each-step#step-03_1-create-exclusion-criteria-t2", level = 5,
                        input = c("PERSONS", "OBSERVATION_PERIODS", "D3_output_spells_category"),
                        output = c("D3_selection_criteria_doses"))
step_03_2 <- createCell("step_03_2", cell_style = "white", label = "step 03_2", link = "https://github.com/ARS-toscana/ECVM/wiki/Actions-in-each-step#step-02_2-apply-quality-check-criteria-t3", level = 5,
                        input = c("D3_selection_criteria_doses", "D3_concepts_QC_criteria", "output_spells_category"),
                        output = c("persons_doses"))
step_04_1 <- createCell("step_04_1", cell_style = "white", label = "step 04_1", link = "https://github.com/ARS-toscana/ECVM/wiki/Actions-in-each-step#step-04_1-apply-exclusion-criteria-t3", level = 7,
                      input = c("D3_selection_criteria_doses"),
                      output = c("D4_study_population", "Flowchart_exclusion_criteria", "D4_study_source_population"))
step_04_2 <- createCell("step_04_2", cell_style = "white", label = "step 04_2", link = "https://github.com/ARS-toscana/ECVM/wiki/Actions-in-each-step#step-04_2-apply-quality-checks-exclusion-criteria-t3", level = 7,
                      input = c("persons_doses"),
                      output = c("Flowchart_doses", "bar_chart"))
step_05 <- createCell("step_05", cell_style = "white", label = "step 05", link = "https://github.com/ARS-toscana/ECVM/wiki/Actions-in-each-step#step-05-create-d3-datasetst2", level = 9,
                        input = c("D4_study_population", "selected_doses"),
                        output = c("D3_study_population", "D3_Vaccin_cohort", "D3_studyweeks", "D3_vaxweeks"))
step_06_1 <- createCell("step_06_1", cell_style = "white", label = "step 06_1", link = "https://github.com/ARS-toscana/ECVM/wiki/Actions-in-each-step#step-06_1-create-d4_doses_weeks-t3", level = 11,
                        input = c("D3_studyweeks"),
                        output = c("D4_doses_weeks"))
step_06_2 <- createCell("step_06_2", cell_style = "white", label = "step 06_2", link = "https://github.com/ARS-toscana/ECVM/wiki/Actions-in-each-step#step-06_2-create-dashboard-tables-t3", level = 11,
                        input = c("D3_vaxweeks", "D3_Vaccin_cohort", "D4_study_source_population"),
                        output = c("DOSES_BIRTHCOHORTS", "COVERAGE_BIRTHCOHORTS"))
step_06_3 <- createCell("step_06_3", cell_style = "white", label = "step 06_3", link = "https://github.com/ARS-toscana/ECVM/wiki/Actions-in-each-step#step-06_3-create-descriptive-tables-t3", level = 11,
                        input = c("D3_study_population", "D3_Vaccin_cohort"),
                        output = c("D4_descriptive_dataset_age_studystart", "D4_descriptive_dataset_ageband_studystart", "D4_descriptive_dataset_sex_studystart", "D4_followup_fromstudystart",
                                   "D4_descriptive_dataset_age_vax1", "D4_descriptive_dataset_ageband_vax", "D4_descriptive_dataset_sex_vaccination", "D4_followup_from_vax", "D4_distance_doses"))


steps_list <- list(step_01_1, step_01_2, step_02_1, step_02_2, step_03_1, step_03_2, step_04_1, step_04_2, step_05,
                   step_06_1, step_06_2, step_06_3)

VACCINES <- createCell("VACCINES", cell_style = "green", label = "VACCINES", link = "", level = 0)
PERSONS <- createCell("PERSONS", cell_style = "light blue", label = "PERSONS", link = "", level = 0)
OBSERVATION_PERIODS <- createCell("OBSERVATION_PERIODS", cell_style = "light blue", label = "OBSERVATION_PERIODS", link = "", level = 0)

initial_dfs_list <- list(VACCINES, PERSONS, OBSERVATION_PERIODS)

D3_output_spells_category <- createCell("D3_output_spells_category", cell_style = "yellow",
                                        label = "D3_output_spells_category", link = "", level = 2)
CONCEPT <- createCell("CONCEPT", cell_style = "white", label = "CONCEPT", link = "", level = 2)

D3_concepts_QC_criteria <- createCell("D3_concepts_QC_criteria", cell_style = "yellow",
                                       label = "D3_concepts_QC_criteria", link = "", level = 4)
Flowchart_QC_criteria <- createCell("Flowchart_QC_criteria", cell_style = "white",
                                       label = "Flowchart_QC_criteria", link = "", level = 4)
selected_doses <- createCell("selected_doses", cell_style = "white",
                                       label = "selected_doses", link = "", level = 4)
D3_selection_criteria_doses <- createCell("D3_selection_criteria_doses", cell_style = "yellow",
                                       label = "D3_selection_criteria_doses", link = "", level = 6)
persons_doses <- createCell("persons_doses", cell_style = "white",
                            label = "persons_doses", link = "", level = 6)
Flowchart_exclusion_criteria <- createCell("Flowchart_exclusion_criteria", cell_style = "white",
                            label = "Flowchart_exclusion_criteria", link = "", level = 8)
D4_study_population <- createCell("D4_study_population", cell_style = "orange",
                            label = "D4_study_population", link = "", level = 8)
D4_study_source_population <- createCell("D4_study_source_population", cell_style = "orange",
                                         label = "D4_study_source_population", link = "", level = 8)
Flowchart_doses <- createCell("Flowchart_doses", cell_style = "orange",
                            label = "Flowchart_doses", link = "", level = 8)
bar_chart <- createCell("bar_chart", cell_style = "orange",
                            label = "bar_chart", link = "", level = 8)
D3_study_population <- createCell("D3_study_population", cell_style = "yellow",
                            label = "D3_study_population", link = "", level = 10)
D3_Vaccin_cohort <- createCell("D3_Vaccin_cohort", cell_style = "yellow",
                            label = "D3_Vaccin_cohort", link = "", level = 10)
D3_studyweeks <- createCell("D3_studyweeks", cell_style = "yellow",
                            label = "D3_studyweeks", link = "", level = 10)
D3_vaxweeks <- createCell("D3_vaxweeks", cell_style = "yellow",
                            label = "D3_vaxweeks", link = "", level = 10)
D4_doses_weeks <- createCell("D4_doses_weeks", cell_style = "orange",
                          label = "D4_doses_weeks", link = "", level = 12)
DOSES_BIRTHCOHORTS <- createCell("DOSES_BIRTHCOHORTS", cell_style = "orange",
                          label = "DOSES_BIRTHCOHORTS", link = "", level = 10)
COVERAGE_BIRTHCOHORTS <- createCell("COVERAGE_BIRTHCOHORTS", cell_style = "orange",
                          label = "COVERAGE_BIRTHCOHORTS", link = "", level = 10)
D4_descriptive_dataset_age_studystart <- createCell("D4_descriptive_dataset_age_studystart", cell_style = "orange",
                          label = "D4_descriptive_dataset_age_studystart", link = "", level = 10)
D4_descriptive_dataset_ageband_studystart <- createCell("D4_descriptive_dataset_ageband_studystart", cell_style = "orange",
                          label = "D4_descriptive_dataset_ageband_studystart", link = "", level = 10)
D4_descriptive_dataset_sex_studystart <- createCell("D4_descriptive_dataset_sex_studystart", cell_style = "orange",
                          label = "D4_descriptive_dataset_sex_studystart", link = "", level = 10)
D4_followup_fromstudystart <- createCell("D4_followup_fromstudystart", cell_style = "orange",
                          label = "D4_followup_fromstudystart", link = "", level = 10)
D4_descriptive_dataset_age_vax1 <- createCell("D4_descriptive_dataset_age_vax1", cell_style = "orange",
                          label = "D4_descriptive_dataset_age_vax1", link = "", level = 10)
D4_descriptive_dataset_ageband_vax <- createCell("D4_descriptive_dataset_ageband_vax", cell_style = "orange",
                          label = "D4_descriptive_dataset_ageband_vax", link = "", level = 10)
D4_descriptive_dataset_sex_vaccination <- createCell("D4_descriptive_dataset_sex_vaccination", cell_style = "orange",
                          label = "D4_descriptive_dataset_sex_vaccination", link = "", level = 10)
D4_followup_from_vax <- createCell("D4_followup_from_vax", cell_style = "orange",
                                   label = "D4_followup_from_vax", link = "", level = 10)
D4_distance_doses <- createCell("D4_distance_doses", cell_style = "orange",
                                   label = "D4_distance_doses", link = "", level = 10)
dfs_list <- list(D3_output_spells_category, CONCEPT, D3_concepts_QC_criteria, Flowchart_QC_criteria, selected_doses,
                 D3_selection_criteria_doses, persons_doses, Flowchart_exclusion_criteria, D4_study_population,
                 D4_study_source_population, Flowchart_doses, bar_chart, D3_study_population, D3_Vaccin_cohort,
                 D3_studyweeks, D3_vaxweeks, D4_doses_weeks, DOSES_BIRTHCOHORTS, COVERAGE_BIRTHCOHORTS,
                 D4_descriptive_dataset_age_studystart, D4_descriptive_dataset_ageband_studystart,
                 D4_descriptive_dataset_sex_studystart, D4_followup_fromstudystart, D4_descriptive_dataset_age_vax1,
                 D4_descriptive_dataset_ageband_vax, D4_descriptive_dataset_sex_vaccination, D4_followup_from_vax,
                 D4_distance_doses)

cell_list <- append(steps_list, initial_dfs_list)
cell_list <- append(cell_list, dfs_list)
pages <- 1
arrows_style <- "circle arrow"

test_xml <- create_diagram(cell_list, pages, arrows_style, direction = "TB")

write_xml(test_xml, "ECVM.xml")