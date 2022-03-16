step_01_1 <- createCell("step_01_1", cell_style = "circle", label = "step 01_1", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-01_1-create-conceptset-datasets-t21", level = 1,
                        input = c("EVENTS", "MEDICINES", "SURVEY_OBSERVATIONS", "MEDICAL_OBSERVATIONS"),
                        output = c("CONCEPT"))
step_01_2 <- createCell("step_01_2", cell_style = "circle", label = "step 01_2", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-01_2-create-spells-t21", level = 1,
                        input = c("OBSERVATION_PERIODS"),
                        output = c("D3_output_spells_category"))
step_01_3 <- createCell("step_01_3", cell_style = "circle", label = "step 01_3", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-01_3-create-dates-from-persons-t21", level = 1,
                        input = c("PERSONS", "OBSERVATION_PERIODS"),
                        output = c("D3_PERSONS", "D3_events_DEATH"))
step_01_4 <- createCell("step_01_4", cell_style = "circle", label = "step 01_4", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-01_4-create-prompt-and-itemset-datasets-t21", level = 1,
                        input = c("SURVEY_OBSERVATIONS", "SURVEY_ID"),
                        output = c("prompt_datasets", "itemset_datasets"))
step_02 <- createCell("step_02", cell_style = "circle", label = "step 02", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-02-count-codes-qc", level = 3,
                        input = c("CONCEPT"),
                        output = c("QC_code_counts_in_CDMinstanceCONCEPTYEAR"))
step_04 <- createCell("step_04", cell_style = "circle", label = "step_04", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-04-create-exclusion-criteria-t2", level = 5,
                        input = c("PERSONS", "D3_output_spells_category"),
                        output = c("D3_selection_criteria"))
step_05 <- createCell("step_05", cell_style = "circle", label = "step_05", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-05-apply-exclusion-criteria-t3", level = 7,
                        input = c("D3_selection_criteria"),
                        output = c("flowchart", "D4_study_population"))
step_06_1 <- createCell("step_06_1", cell_style = "circle", label = "step 06_1", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-06_1-components-t22", level = 9,
                        input = c("CONCEPT", "D4_study_population"),
                        output = c("D3_events_OUTCOME_TYPE", "D3_componentsOUTCOME"))
step_06_2 <- createCell("step_06_2", cell_style = "circle", label = "step 06_2", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-06_2--create-secondary-components-t22", level = 9,
                        input = c("D4_study_population", "CONCEPT"),
                        output = c("D3_eventsSecondary_SECCOMP"))
#TODO check CONCEPT if ok
step_06_3 <- createCell("step_06_3", cell_style = "circle", label = "step 06_3", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-06_3--create-events-all-outcomes-t2", level = 9,
                        input = c("D4_study_population", "CONCEPT"),
                        output = c("D3_events_ALL_OUTCOMES"))
step_06_4 <- createCell("step_06_4", cell_style = "circle", label = "step 06_4", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-06_4--code-counts-of-first-occurrence-of-outcomes-in-the-study-population-qc", level = 9,
                        input = c("D4_study_population", "D3_events_ALL_OUTCOMES"),
                        output = c("QC_code_counts_in_study_population_OUTCOME_YEAR"))
step_06_5 <- createCell("step_06_5", cell_style = "circle", label = "step 06_5", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-06_5-apply-component-strategy-qc", level = 9,
                        input = c("D4_study_population", "D3_events_OUTCOME_TYPE"),
                        output = c("QC_all_components_OUTCOME"))
step_06_6 <- createCell("step_06_6", cell_style = "circle", label = "step 06_6", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-06_6-covariates-at-baseline-t22", level = 9,
                        input = c("D4_study_population", "CONCEPTS"),
                        output = c("D3_study_population_covariates"))
step_06_7 <- createCell("step_06_7", cell_style = "circle", label = "step 06_7", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-06_7-drug-proxy-at-baseline-t22", level = 9,
                        input = c("D4_study_population", "CONCEPTS"),
                        output = c("D3_study_population_DP"))
step_06_8 <- createCell("step_06_8", cell_style = "circle", label = "step 06_8", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-06_8-baseline-characteristics-t23", level = 9,
                        input = c("D4_study_population", "D3_study_population_covariates"),
                        output = c("D4_study_population_cov"))
step_06_9 <- createCell("step_06_9", cell_style = "circle", label = "step 06_9", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-06_9-all-covariates-at-baseline-t23", level = 9,
                        input = c("D4_study_population", "D4_study_population_cov", "D3_study_population_DP"),
                        output = c("D3_study_population_cov_ALL"))
step_07_1 <- createCell("step_07_1", cell_style = "circle", label = "step 07_1", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-07_1-aggregate-baseline-characteristics-t3", level = 11,
                        input = c("D4_study_population_cov"),
                        output = c("D4_descriptive_dataset_covariates", "D4_descriptive_dataset_age", "D4_descriptive_dataset_ageband"))
step_07_2 <- createCell("step_07_2", cell_style = "circle", label = "step 07_2", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-07_2-aggregate-baseline-covariates-and-dp-t3", level = 11,
                        input = c("D3_study_population_cov_ALL"),
                        output = c("D4_descriptive_dataset_covariate_ALL"))
step_07_3 <- createCell("step_07_3", cell_style = "circle", label = "step 07_3", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-07_3-create-person-time-t3", level = 11,
                        input = c("D4_study_population", "D3_events_ALL_OUTCOMES"),
                        output = c("D4_persontime_ALL_OUTCOMES_year"))
step_08 <- createCell("step_08", cell_style = "circle", label = "step_08", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-08-create-exclusion-criteria-coprimary-c-and-d-t2", level = 13,
                        input = c("PERSONS", "OBSERVATION_PERIODS", "D3_output_spells_category"),
                        output = c("D3_selection_criteria_coprimary_c_d"))
step_09 <- createCell("step_09", cell_style = "circle", label = "step_09", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-09-apply-exclusion-criteria-coprimary-c-and-d-t3", level = 15,
                        input = c("D3_selection_criteria_coprimary_c_d"),
                        output = c("flowchart_objective_c", "D4_study_population_coprimary_c", "flowchart_objective_d", "D4_study_population_coprimary_d"))
step_10_1 <- createCell("step_10_1", cell_style = "circle", label = "step_10_1", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-10_1-covariates-at-baseline-coprimary-c-and-d-t21", level = 17,
                      input = c("D4_study_population_coprimary_c"),
                      output = c("D3_study_population_covariates_at_baseline_coprimary_c_d"))
step_10_2 <- createCell("step_10_2", cell_style = "circle", label = "step_10_2", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-10_2-diagnosis-at-baseline-coprimary-c-and-d-t22", level = 17,
                      input = c("D4_study_population_coprimary_c", "CONCEPT"),
                      output = c("D3_study_population_covariates_DX_coprimary_c_d"))
step_10_3 <- createCell("step_10_3", cell_style = "circle", label = "step_10_3", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-10_3-drug-proxy-at-baseline-coprimary-c-and-d-t22", level = 17,
                      input = c("D4_study_population_coprimary_c", "CONCEPT"),
                      output = c("D3_study_population_covariates_DP_coprimary_c_d"))
step_10_4 <- createCell("step_10_4", cell_style = "circle", label = "step_10_4", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-10_4-all-at-baseline-t23", level = 17,
                      input = c("D3_study_population_covariates_at_baseline_coprimary_c_d", "D3_study_population_covariates_DX_coprimary_c_d", "D3_study_population_covariates_DP_coprimary_c_d"),
                      output = c("D3_study_population_covariates_ALL_coprimary_c_d"))
step_10_5 <- createCell("step_10_5", cell_style = "circle", label = "step_10_5", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-10_5-components-for-covid-severity-t22", level = 17,
                      input = c("D4_study_population_coprimary_c", "D3_events_OUTCOME_TYPE", "D3_events_DEATH"),
                      output = c("D3_components_covid_severity"))
step_10_6 <- createCell("step_10_6", cell_style = "circle", label = "step_10_6", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-10_6-algorithms-for-covid-severity-t23", level = 17,
                        input = c("D3_components_covid_severity", "D4_study_population_coprimary_c"),
                        output = c("D3_algorithm_covid"))
step_11_1 <- createCell("step_11_1", cell_style = "circle", label = "step_11_1", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-11_1--aggregate-baseline-all-t3", level = 19,
                      input = c("D3_study_population_covariates_ALL_coprimary_c_d"),
                      output = c("D4_descriptive_dataset_covariate_ALL_coprimary_c", "D4_descriptive_dataset_age_coprimary_c",
                                 "D4_descriptive_dataset_ageband_coprimary_c", "D4_descriptive_dataset_covariate_ALL_coprimary_d",
                                 "D4_descriptive_dataset_age_coprimary_d", "D4_descriptive_dataset_ageband_coprimary_d"))
step_11_2 <- createCell("step_11_2", cell_style = "circle", label = "step_11_2", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-11_2-create-person-time-coprimary-c-t3", level = 19,
                      input = c("D4_study_population_coprimary_c", "D3_algorithm_covid", "list_outcomes_observed_COVID"),
                      output = c("D4_persontime_coprimary_c_month"))
step_11_3 <- createCell("step_11_3", cell_style = "circle", label = "step_11_3", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-11_3-create-person-time-coprimary-d-t3", level = 19,
                      input = c("D4_study_population_coprimary_d", "D3_events_OUTCOME_TYPE"),
                      output = c("D4_persontime_coprimary_d_month"))
step_12_1 <- createCell("step_12_1", cell_style = "circle", label = "step_12_1", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-12_1-create-covariate-dx-first-date--t22", level = 21,
                        input = c("D4_study_population", "CONCEPT"),
                        output = c("D3_COVARIATE_DX_first_date"))
step_12_2 <- createCell("step_12_2", cell_style = "circle", label = "step_12_2", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-12_2-create-covariate-dp-second-date--t22", level = 21,
                        input = c("D4_study_population", "CONCEPT"),
                        output = c("D3_COVARIATE_DP_second_date"))
step_12_3 <- createCell("step_12_3", cell_style = "circle", label = "step_12_3", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-12_3-create-covariate-either-dx-or-dp-start-date-t23", level = 21,
                        input = c("D4_study_population", "D3_COVARIATE_DX_first_date", "D3_COVARIATE_DP_second_date"),
                        output = c("D3_risk_factors"))
step_12_4 <- createCell("step_12_4", cell_style = "circle", label = "step_12_4", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-12_4-create-exclusion-criteria-secondary-e--t2", level = 21,
                        input = c("PERSONS", "D3_output_spells_category", "D3_risk_factors", "OBSERVATION_PERIODS"),
                        output = c("D3_selection_criteria_secondary_e"))
step_13 <- createCell("step_13", cell_style = "circle", label = "step_13", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-13-apply-exclusion-criteria-secondary-e-t3", level = 23,
                        input = c("D3_selection_criteria_secondary_e"),
                        output = c("D4_study_population_secondary_e"))
step_14_1 <- createCell("step_14_1", cell_style = "circle", label = "step_14_1", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-14_1-covariates-at-baseline-t21", level = 25,
                        input = c("D4_study_population_secondary_e"),
                        output = c("D3_study_population_covariates_at_baseline_secondary_e"))
step_14_2 <- createCell("step_14_2", cell_style = "circle", label = "step_14_2", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-14_2-dx-at-baseline-e-t22", level = 25,
                        input = c("D4_study_population_secondary_e", "CONCEPT"),
                        output = c("D3_study_population_covariates_DX_secondary_e"))
step_14_3 <- createCell("step_14_3", cell_style = "circle", label = "step_14_3", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-14_3--dp-at-baseline-e-t22", level = 25,
                        input = c("D4_study_population_secondary_e", "CONCEPT"),
                        output = c("D3_study_population_covariates_DP_secondary_e"))
step_14_4 <- createCell("step_14_4", cell_style = "circle", label = "step_14_4", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-14_4--all-at-baseline-e-t23", level = 25,
                        input = c("D3_study_population_covariates_at_baseline_secondary_e", "D3_study_population_covariates_DX_secondary_e",
                                  "D3_study_population_covariates_DP_secondary_e"),
                        output = c("D3_study_population_covariates_ALL_secondary_e"))
step_14_5 <- createCell("step_14_5", cell_style = "circle", label = "step_14_5", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-14_5-presence-at-risk-conditions-year-t23", level = 25,
                        input = c("D4_study_population_secondary_e", "D3_study_population_covariates_ALL_secondary_e", "D3_risk_factors"),
                        output = c("D3_presence_high_risk_medical_conditions"))
step_15_1 <- createCell("step_15_1", cell_style = "circle", label = "step_15_1", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-15_1-aggregate-baseline-all-e--t3", level = 27,
                        input = c("D3_study_population_covariates_ALL_secondary_e"),
                        output = c("D4_descriptive_dataset_covariate_ALL_secondary_e", "D4_descriptive_dataset_age_secondary_e", "D4_descriptive_dataset_ageband_secondary_e"))
step_15_2 <- createCell("step_15_2", cell_style = "circle", label = "step_15_2", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-15_2-create-prevalence-risk-factors--t3", level = 27,
                        input = c("D3_presence_high_risk_medical_conditions"),
                        output = c("D4_prevalence_high_risk_medical_conditions"))
step_15_3 <- createCell("step_15_3", cell_style = "circle", label = "step_15_3", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-15_3-create-person-time-secondary-e-t3", level = 27,
                        input = c("D4_study_population_secondary_e", "D3_events_ALL_OUTCOMES"),
                        output = c("D4_persontime_secondary_e_year"))
step_16_1 <- createCell("step_16_1", cell_style = "circle", label = "step_16_1", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-16_1-create-person-time-month-t3", level = 29,
                        input = c("D4_study_population", "D3_events_ALL_OUTCOMES"),
                        output = c("D4_persontime_ALL_OUTCOMES_month"))
step_16_2 <- createCell("step_16_2", cell_style = "circle", label = "step_16_2", link = "https://github.com/ARS-toscana/Complex-script/wiki/Actions-in-each-step#step-16_2-create-person-time-coprimary-c-week-t3-only-if-study-includes-2020", level = 29,
                        input = c("D4_study_population_coprimary_c", "D3_algorithm_covid"),
                        output = c("D4_persontime_coprimary_c_week"))

steps_list <- list(step_01_1, step_01_2, step_01_3, step_01_4, step_02, step_04, step_05, step_06_1, step_06_2,
                   step_06_3, step_06_4, step_06_5, step_06_6, step_06_7, step_06_8, step_06_9, step_07_1, step_07_2,
                   step_07_3, step_08, step_09, step_10_1, step_10_2, step_10_3, step_10_4, step_10_5, step_10_6,
                   step_11_1, step_11_2, step_11_3, step_12_1, step_12_2, step_12_3, step_12_4, step_13, step_14_1,
                   step_14_2, step_14_3, step_14_4, step_14_5, step_15_1, step_15_2, step_15_3, step_16_1, step_16_2)

EVENTS <- createCell("EVENTS", cell_style = "green", label = "EVENTS", link = "", level = 0)
MEDICINES <- createCell("MEDICINES", cell_style = "green", label = "MEDICINES", link = "", level = 0)
MEDICAL_OBSERVATIONS <- createCell("MEDICAL_OBSERVATIONS", cell_style = "green", label = "MEDICAL_OBSERVATIONS", link = "", level = 0)
SURVEY_ID <- createCell("SURVEY_ID", cell_style = "dark blue", label = "SURVEY_ID", link = "", level = 0)
SURVEY_OBSERVATIONS <- createCell("SURVEY_OBSERVATIONS", cell_style = "dark blue", label = "SURVEY_OBSERVATIONS", link = "", level = 0)
PERSONS <- createCell("PERSONS", cell_style = "light blue", label = "PERSONS", link = "", level = 0)
OBSERVATION_PERIODS <- createCell("OBSERVATION_PERIODS", cell_style = "light blue", label = "OBSERVATION_PERIODS", link = "", level = 0)

initial_dfs_list <- list(EVENTS, MEDICINES, MEDICAL_OBSERVATIONS, SURVEY_ID, SURVEY_OBSERVATIONS, PERSONS,
                      OBSERVATION_PERIODS)

D3_output_spells_category <- createCell("D3_output_spells_category", cell_style = "white",
                                        label = "D3_output_spells_category", link = "", level = 2)
CONCEPT <- createCell("CONCEPT", cell_style = "white", label = "CONCEPT", link = "", level = 2)
D3_PERSONS <- createCell("D3_PERSONS", cell_style = "white", label = "D3_PERSONS", link = "", level = 2)
D3_events_DEATH <- createCell("D3_events_DEATH", cell_style = "white", label = "D3_events_DEATH", link = "", level = 2)
prompt_datasets <- createCell("prompt_datasets", cell_style = "white", label = "prompt_datasets", link = "", level = 2)
itemset_datasets <- createCell("itemset_datasets", cell_style = "white", label = "itemset_datasets", link = "", level = 2)
QC_code_counts_in_CDMinstanceCONCEPTYEAR <- createCell("QC_code_counts_in_CDMinstanceCONCEPTYEAR", cell_style = "white",
                                                       label = "QC_code_counts_in_CDMinstanceCONCEPTYEAR", link = "", level = 4)
D3_selection_criteria <- createCell("D3_selection_criteria", cell_style = "orange",
                                    label = "D3_selection_criteria", link = "", level = 6)
flowchart <- createCell("flowchart", cell_style = "yellow", label = "flowchart", link = "", level = 8)
D4_study_population <- createCell("D4_study_population", cell_style = "orange",
                                  label = "D4_study_population", link = "", level = 8)
D3_events_OUTCOME_TYPE <- createCell("D3_events_OUTCOME_TYPE", cell_style = "yellow",
                                     label = "D3_events_OUTCOME_TYPE", link = "", level = 10)
D3_componentsOUTCOME <- createCell("D3_componentsOUTCOME", cell_style = "yellow",
                                   label = "D3_componentsOUTCOME", link = "", level = 10)
D3_eventsSecondary_SECCOMP <- createCell("D3_eventsSecondary_SECCOMP", cell_style = "yellow",
                                         label = "D3_eventsSecondary_SECCOMP", link = "", level = 10)
D3_events_ALL_OUTCOMES <- createCell("D3_events_ALL_OUTCOMES", cell_style = "yellow",
                                     label = "D3_events_ALL_OUTCOMES", link = "", level = 10)
QC_code_counts_in_study_population_OUTCOME_YEAR <- createCell("QC_code_counts_in_study_population_OUTCOME_YEAR", cell_style = "white",
                                                              label = "QC_code_counts_in_study_population_OUTCOME_YEAR", link = "", level = 10)
QC_all_components_OUTCOME <- createCell("QC_all_components_OUTCOME", cell_style = "white",
                                        label = "QC_all_components_OUTCOME", link = "", level = 10)
D3_study_population_covariates <- createCell("D3_study_population_covariates", cell_style = "yellow",
                                             label = "D3_study_population_covariates", link = "", level = 10)
D3_study_population_DP <- createCell("D3_study_population_DP", cell_style = "yellow",
                                     label = "D3_study_population_DP", link = "", level = 10)
D4_study_population_cov <- createCell("D4_study_population_cov", cell_style = "orange",
                                      label = "D4_study_population_cov", link = "", level = 10)
D3_study_population_cov_ALL <- createCell("D3_study_population_cov_ALL", cell_style = "yellow",
                                          label = "D3_study_population_cov_ALL", link = "", level = 10)
D4_descriptive_dataset_covariates <- createCell("D4_descriptive_dataset_covariates", cell_style = "orange",
                                                label = "D4_descriptive_dataset_covariates", link = "", level = 12)
D4_descriptive_dataset_age <- createCell("D4_descriptive_dataset_age", cell_style = "orange",
                                         label = "D4_descriptive_dataset_age", link = "", level = 12)
D4_descriptive_dataset_ageband <- createCell("D4_descriptive_dataset_ageband", cell_style = "orange",
                                             label = "D4_descriptive_dataset_ageband", link = "", level = 12)
D4_descriptive_dataset_covariate_ALL <- createCell("D4_descriptive_dataset_covariate_ALL", cell_style = "orange",
                                                   label = "D4_descriptive_dataset_covariate_ALL", link = "", level = 12)
D4_persontime_ALL_OUTCOMES_year <- createCell("D4_persontime_ALL_OUTCOMES_year", cell_style = "orange",
                                              label = "D4_persontime_ALL_OUTCOMES_year", link = "", level = 12)
D3_selection_criteria_coprimary_c_d <- createCell("D3_selection_criteria_coprimary_c_d", cell_style = "yellow",
                                                  label = "D3_selection_criteria_coprimary_c_d", link = "", level = 14)
flowchart_objective_c <- createCell("flowchart_objective_c", cell_style = "yellow",
                                    label = "flowchart_objective_c", link = "", level = 16)
D4_study_population_coprimary_c <- createCell("D4_study_population_coprimary_c", cell_style = "orange",
                                              label = "D4_study_population_coprimary_c", link = "", level = 16)
flowchart_objective_d <- createCell("flowchart_objective_d", cell_style = "yellow",
                                    label = "flowchart_objective_d", link = "", level = 16)
D4_study_population_coprimary_d <- createCell("D4_study_population_coprimary_d", cell_style = "orange",
                                              label = "D4_study_population_coprimary_d", link = "", level = 16)
D3_study_population_covariates_at_baseline_coprimary_c_d <- createCell("D3_study_population_covariates_at_baseline_coprimary_c_d", cell_style = "yellow",
                                                                       label = "D3_study_population_covariates_at_baseline_coprimary_c_d", link = "", level = 18)
D3_study_population_covariates_DX_coprimary_c_d <- createCell("D3_study_population_covariates_DX_coprimary_c_d", cell_style = "yellow",
                                                              label = "D3_study_population_covariates_DX_coprimary_c_d", link = "", level = 18)
D3_study_population_covariates_DP_coprimary_c_d <- createCell("D3_study_population_covariates_DP_coprimary_c_d", cell_style = "yellow",
                                                              label = "D3_study_population_covariates_DP_coprimary_c_d", link = "", level = 18)
D3_study_population_covariates_ALL_coprimary_c_d <- createCell("D3_study_population_covariates_ALL_coprimary_c_d", cell_style = "yellow",
                                                               label = "D3_study_population_covariates_ALL_coprimary_c_d", link = "", level = 18)
D3_components_covid_severity <- createCell("D3_components_covid_severity", cell_style = "yellow",
                                           label = "D3_components_covid_severity", link = "", level = 18)
D3_algorithm_covid <- createCell("D3_algorithm_covid", cell_style = "yellow",
                                 label = "D3_algorithm_covid", link = "", level = 18)
D4_descriptive_dataset_covariate_ALL_coprimary_c <- createCell("D4_descriptive_dataset_covariate_ALL_coprimary_c", cell_style = "orange",
                                                               label = "D4_descriptive_dataset_covariate_ALL_coprimary_c", link = "", level = 20)
D4_descriptive_dataset_age_coprimary_c <- createCell("D4_descriptive_dataset_age_coprimary_c", cell_style = "orange",
                                                     label = "D4_descriptive_dataset_age_coprimary_c", link = "", level = 20)
D4_descriptive_dataset_ageband_coprimary_c <- createCell("D4_descriptive_dataset_ageband_coprimary_c", cell_style = "orange",
                                                         label = "D4_descriptive_dataset_ageband_coprimary_c", link = "", level = 20)
D4_descriptive_dataset_covariate_ALL_coprimary_d <- createCell("D4_descriptive_dataset_covariate_ALL_coprimary_d", cell_style = "orange",
                                                               label = "D4_descriptive_dataset_covariate_ALL_coprimary_d", link = "", level = 20)
D4_descriptive_dataset_age_coprimary_d <- createCell("D4_descriptive_dataset_age_coprimary_d", cell_style = "orange",
                                                     label = "D4_descriptive_dataset_age_coprimary_d", link = "", level = 20)
D4_descriptive_dataset_ageband_coprimary_d <- createCell("D4_descriptive_dataset_ageband_coprimary_d", cell_style = "orange",
                                                         label = "D4_descriptive_dataset_ageband_coprimary_d", link = "", level = 20)
D4_persontime_coprimary_c_month <- createCell("D4_persontime_coprimary_c_month", cell_style = "orange",
                                              label = "D4_persontime_coprimary_c_month", link = "", level = 20)
D4_persontime_coprimary_d_month <- createCell("D4_persontime_coprimary_d_month", cell_style = "orange",
                                              label = "D4_persontime_coprimary_d_month", link = "", level = 20)
D3_COVARIATE_DX_first_date <- createCell("D3_COVARIATE_DX_first_date", cell_style = "yellow",
                                         label = "D3_COVARIATE_DX_first_date", link = "", level = 22)
D3_COVARIATE_DP_second_date <- createCell("D3_COVARIATE_DP_second_date", cell_style = "yellow",
                                          label = "D3_COVARIATE_DP_second_date", link = "", level = 22)
D3_risk_factors <- createCell("D3_risk_factors", cell_style = "yellow",
                              label = "D3_risk_factors", link = "", level = 22)
D3_selection_criteria_secondary_e <- createCell("D3_selection_criteria_secondary_e", cell_style = "yellow",
                                                label = "D3_selection_criteria_secondary_e", link = "", level = 22)
D4_study_population_secondary_e <- createCell("D4_study_population_secondary_e", cell_style = "orange",
                                              label = "D4_study_population_secondary_e", link = "", level = 24)
D3_study_population_covariates_at_baseline_secondary_e <- createCell("D3_study_population_covariates_at_baseline_secondary_e", cell_style = "yellow",
                                                                     label = "D3_study_population_covariates_at_baseline_secondary_e", link = "", level = 26)
D3_study_population_covariates_DX_secondary_e <- createCell("D3_study_population_covariates_DX_secondary_e", cell_style = "yellow",
                                                            label = "D3_study_population_covariates_DX_secondary_e", link = "", level = 26)
D3_study_population_covariates_DP_secondary_e <- createCell("D3_study_population_covariates_DP_secondary_e", cell_style = "yellow",
                                                            label = "D3_study_population_covariates_DP_secondary_e", link = "", level = 26)
D3_study_population_covariates_ALL_secondary_e <- createCell("D3_study_population_covariates_ALL_secondary_e", cell_style = "yellow",
                                                             label = "D3_study_population_covariates_ALL_secondary_e", link = "", level = 26)
D3_presence_high_risk_medical_conditions <- createCell("D3_presence_high_risk_medical_conditions", cell_style = "yellow",
                                                       label = "D3_presence_high_risk_medical_conditions", link = "", level = 26)
D4_descriptive_dataset_covariate_ALL_secondary_e <- createCell("D4_descriptive_dataset_covariate_ALL_secondary_e", cell_style = "orange",
                                                               label = "D4_descriptive_dataset_covariate_ALL_secondary_e", link = "", level = 28)
D4_descriptive_dataset_age_secondary_e <- createCell("D4_descriptive_dataset_age_secondary_e", cell_style = "orange",
                                                     label = "D4_descriptive_dataset_age_secondary_e", link = "", level = 28)
D4_descriptive_dataset_ageband_secondary_e <- createCell("D4_descriptive_dataset_ageband_secondary_e", cell_style = "orange",
                                                         label = "D4_descriptive_dataset_ageband_secondary_e", link = "", level = 28)
D4_prevalence_high_risk_medical_conditions <- createCell("D4_prevalence_high_risk_medical_conditions", cell_style = "orange",
                                                         label = "D4_prevalence_high_risk_medical_conditions", link = "", level = 28)
D4_persontime_secondary_e_year <- createCell("D4_persontime_secondary_e_year", cell_style = "orange",
                                             label = "D4_persontime_secondary_e_year", link = "", level = 28)
D4_persontime_ALL_OUTCOMES_month <- createCell("D4_persontime_ALL_OUTCOMES_month", cell_style = "orange",
                                               label = "D4_persontime_ALL_OUTCOMES_month", link = "", level = 30)
D4_persontime_coprimary_c_week <- createCell("D4_persontime_coprimary_c_week", cell_style = "orange",
                                             label = "D4_persontime_coprimary_c_week", link = "", level = 30)

dfs_list <- list(D3_output_spells_category, CONCEPT, D3_PERSONS, D3_events_DEATH, prompt_datasets, itemset_datasets,
                 QC_code_counts_in_CDMinstanceCONCEPTYEAR, D3_selection_criteria, flowchart, D4_study_population,
                 D3_events_OUTCOME_TYPE, D3_componentsOUTCOME, D3_eventsSecondary_SECCOMP, D3_events_ALL_OUTCOMES,
                 QC_code_counts_in_study_population_OUTCOME_YEAR, QC_all_components_OUTCOME,
                 D3_study_population_covariates, D3_study_population_DP, D4_study_population_cov,
                 D3_study_population_cov_ALL, D4_descriptive_dataset_covariates, D4_descriptive_dataset_age,
                 D4_descriptive_dataset_ageband, D4_descriptive_dataset_covariate_ALL, D4_persontime_ALL_OUTCOMES_year,
                 D3_selection_criteria_coprimary_c_d, flowchart_objective_c, D4_study_population_coprimary_c,
                 flowchart_objective_d, D4_study_population_coprimary_d, D3_study_population_covariates_at_baseline_coprimary_c_d,
                 D3_study_population_covariates_DX_coprimary_c_d, D3_study_population_covariates_DP_coprimary_c_d,
                 D3_study_population_covariates_ALL_coprimary_c_d, D3_components_covid_severity, D3_algorithm_covid,
                 D4_descriptive_dataset_covariate_ALL_coprimary_c, D4_descriptive_dataset_age_coprimary_c,
                 D4_descriptive_dataset_ageband_coprimary_c, D4_descriptive_dataset_covariate_ALL_coprimary_d,
                 D4_descriptive_dataset_age_coprimary_d, D4_descriptive_dataset_ageband_coprimary_d,
                 D4_persontime_coprimary_c_month, D4_persontime_coprimary_d_month, D3_COVARIATE_DX_first_date,
                 D3_COVARIATE_DP_second_date, D3_risk_factors, D3_selection_criteria_secondary_e,
                 D4_study_population_secondary_e, D3_study_population_covariates_at_baseline_secondary_e,
                 D3_study_population_covariates_DX_secondary_e, D3_study_population_covariates_DP_secondary_e,
                 D3_study_population_covariates_ALL_secondary_e, D3_presence_high_risk_medical_conditions,
                 D4_descriptive_dataset_covariate_ALL_secondary_e, D4_descriptive_dataset_age_secondary_e,
                 D4_descriptive_dataset_ageband_secondary_e, D4_prevalence_high_risk_medical_conditions,
                 D4_persontime_secondary_e_year, D4_persontime_ALL_OUTCOMES_month, D4_persontime_coprimary_c_week)

cell_list <- append(steps_list, initial_dfs_list)
cell_list <- append(cell_list, dfs_list)
pages <- 1
arrows_style <- "circle arrow"

test_xml <- create_diagram(cell_list, pages, arrows_style, direction = "TB")

write_xml(test_xml, "test r.xml")


##%######################################################%##
#                                                          #
####                   script to run                    ####
#                                                          #
##%######################################################%##

# TODO add a default standard in case not specified style or a style_name

# cella1 <- createCell("cella1", cell_style = "orange", label = "cella_arancione1", tags = "cell1 test", link = "", level = 0)
# cella2 <- createCell("cella2", cell_style = "orange", label = "cella_arancione2", tags = "cell2", link = "", level = 0)
# cella2a <- createCell("cella2a", cell_style = "orange", label = "cella_arancione2a", tags = "cell2", link = "", level = 1)
# cella3 <- createCell("cella3", cell_style = "yellow", label = "cella_gialla", tags = "cell3", link = "",
#                      level = 2, input = c("cella1", "cella2", "cella2a"), output = c("cella4", "cella5"))
# cella4 <- createCell("cella4", cell_style = "orange", label = "cella_arancione3", tags = "cell4", link = "", level = 3)
# cella5 <- createCell("cella5", cell_style = "orange", label = "cella_arancione4", tags = "cell5 aaa", link = "", level = 3)
# 
# cell_list <- list(cella1, cella2, cella2a, cella3, cella4, cella5)
# pages <- 1
# arrows_style <- "circle arrow"
# 
# test_xml <- create_diagram(cell_list, pages, arrows_style, direction = "TB")
# 
# write_xml(test_xml, "test r.xml")
