library(dplyr)
library(tidyr)

working_directory

## group variables 
### if empty vector use character()
analysis_vars_df <- selected_vars_df[selected_vars_df$select == "retain" & !is.na(selected_vars_df$select),]

consent_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group == "consent" 
                                              & !is.na(analysis_vars_df$select_group)]
strata_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group == "strata" 
                                              & !is.na(analysis_vars_df$select_group)]
outcome_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "outcome" 
                                              & !is.na(analysis_vars_df$select_group)]
socio_demo_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "socio_demo" 
                                                 & !is.na(analysis_vars_df$select_group)]
diabetes_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "diabetes" 
                                               & !is.na(analysis_vars_df$select_group)]
medical_conditions_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "medical_conditions" 
                                                         & !is.na(analysis_vars_df$select_group)]
lifestyle_history_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "lifestyle_history" 
                                                        & !is.na(analysis_vars_df$select_group)]
health_eating_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "health_eating_baseline_pca" 
                                                    | analysis_vars_df$select_group  == "health_eating_followup_pca"
                                                    | analysis_vars_df$select_group  == "health_eating"
                                                    & !is.na(analysis_vars_df$select_group)]
exercise_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "exercise_baseline_pca"
                                               | analysis_vars_df$select_group  == "exercise_followup_pca"
                                               | analysis_vars_df$select_group  == "exercise"
                                               & !is.na(analysis_vars_df$select_group)]

clinical_anthropometric_vars <- analysis_vars_df$new_variable[analysis_vars_df$select_group  == "clinical_anthropometric" 
                                                              & !is.na(analysis_vars_df$select_group)]

## make dataset with variables for descriptive and inferential statistics
df_analysis <- df_merged_final %>%
  dplyr::filter(wdf_q2_1 == "Yes", wdf_q1_9!= "One year follow-up") %>% #filter diabetes and baseline/6months followup
  dplyr::select(any_of(c(id_vars, consent_vars, strata_vars, outcome_vars, socio_demo_vars, diabetes_vars, 
                         medical_conditions_vars, lifestyle_history_vars, health_eating_vars, exercise_vars,
                         clinical_anthropometric_vars))
                ) %>%
  dplyr::mutate(across(any_of(strata_vars),  ~fct_drop(.x )) #drop unused factor levels
                ) %>%
  dplyr::distinct(study_id, wdf_q1_9, .keep_all = TRUE #study_id 43 has 2 entries for 6 month followup.
                  )  %>%
  dplyr::group_by(study_id) %>%
  dplyr::filter(!((n()==1) & (wdf_q1_9 == "Six month follow-up"))) %>% #remove those who entered in 6month follow up (73, 353)
  dplyr::ungroup()

filtered_report <- paste0(nrow(df_merged_final)-nrow(df_analysis), " Dropped observations with non-diabetes and One year follow-up", 
                          ", ", nrow(df_analysis), " Final observations with diabetes and Baseline/6 month follow-up"
                          )

print(filtered_report)


analysis_report <- paste0(
  paste0(analysis_vars_df$new_variable,collapse=", ")," ", length(analysis_vars_df$new_variable)
  , " variables used for analysis" 
  )

print(analysis_report)

none_analysis_report <- paste0(
  paste0(selected_vars_df$new_variable[selected_vars_df$select == "drop"],
         collapse=", ")," ", 
  length(selected_vars_df$new_variable[selected_vars_df$select == "drop"])
  , " variables not used for analysis"
  )

print(none_analysis_report)

