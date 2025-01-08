library(dplyr)
library(tidyr)
library(forcats)
library(gtsummary)

working_directory

my_gtsummary_theme

gtsummary_compact_theme

pre_post_overall_paired_stats <- sapply(outcome_vars, function(x){
  nn <- x
  
  ### combine raw and imputed data then transform to long format
  df <- dplyr::bind_rows(df_drop_vars %>% dplyr::mutate(data_type = "Raw Data"),
                         df_imputation_final %>% dplyr::mutate(data_type = "Imputed Data")
                         ) %>%
    labelled::set_variable_labels(!!!final_labels_baseline[names(final_labels_baseline) %in% names(.)]
                                  ) %>%
    labelled::set_variable_labels(!!!final_labels_followup[names(final_labels_followup) %in% names(.)]
                                  ) %>%
    dplyr::select(any_of(c(id_vars, strata_vars ,outcome_vars)),data_type) %>%
    dplyr::mutate(data_type = forcats::as_factor(data_type)
                  , wdf_q1_9 = forcats::fct_rev(wdf_q1_9)) %>%
    tidyr::pivot_longer(
      cols = any_of(outcome_vars),
      names_to = "clinical_parameters",
      values_to = "value"
      ) %>%
    dplyr::filter(clinical_parameters == nn) %>%
    tidyr::drop_na(value) %>%
    dplyr::group_by(study_id, data_type) %>%
    dplyr::filter(n()==2) %>%
    dplyr::ungroup()
  
   ### rename value column to outcome names
   colnames(df)[colnames(df) == "value"] = nn
  
   df_new <- df
   
   ### tbl_summary paired test formula stratified by data type
   table_out <- tbl_strata(df_new,
                           strata = data_type,
                           .tbl_fun = ~ .x %>%
                             tbl_summary(
                               by = wdf_q1_9, include = -c(study_id,clinical_parameters),
                               type = list(
                                 any_of(outcome_vars) ~ "continuous2",
                                 all_continuous() ~ "continuous2")
                               , statistic = all_continuous() ~ c(
                                 "{mean} ({sd})",
                                 "{median} ({p25}, {p75})")
                               , digits = all_continuous() ~ 2
                               , missing = "ifany" # don't list missing data separately #ifany #no
                               ,missing_text = "Missing"
                               ) %>% 
                             modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**"
                                           ) %>% # update the column header
                             bold_labels() %>%
                             italicize_levels()%>%
                             add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
                                    # add column with total number of non-missing observations
                                    ) %>% 
                             add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
                                   test = list(any_of(outcome_vars) ~ "paired.wilcox.test"), group = study_id ) %>%
                             bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
                             add_difference(test = list(any_of(outcome_vars) ~ "paired_cohens_d"),
                                            group = study_id) %>% #requires gtsummary version 2.0.1
                             modify_footnote(
                               all_stat_cols() ~ "Mean (SD); Median (IQR)"
                               ),
                           .header = "**{strata}**"
                           )
  
  }, simplify = FALSE
)

pre_post_overall_paired_stats_merge <- tbl_stack(pre_post_overall_paired_stats,
                                                 group_header = attribute$label[attribute$variable %in% outcome_vars]
                                                 ) %>%
  gtsummary::as_flex_table()


pre_post_gender_paired_stats <- sapply(unique(as.character(df_drop_vars$wdf_q1_5)), function(y){
  gg <- y
  
  out <- sapply(outcome_vars, function(x){
    nn <- x
    
    ### combine raw and imputed data then transform to long format
    df <- dplyr::bind_rows(df_drop_vars %>% dplyr::mutate(data_type = "Raw Data"),
                           df_imputation_final %>% dplyr::mutate(data_type = "Imputed Data")
                           ) %>%
      labelled::set_variable_labels(!!!final_labels_baseline[names(final_labels_baseline) %in% names(.)]
                                    ) %>%
      labelled::set_variable_labels(!!!final_labels_followup[names(final_labels_followup) %in% names(.)]
                                    ) %>%
      dplyr::select(any_of(c(id_vars, strata_vars ,outcome_vars)),data_type, wdf_q1_5) %>%
      dplyr::mutate(data_type = forcats::as_factor(data_type)
                    , wdf_q1_9 = forcats::fct_rev(wdf_q1_9)) %>%
      dplyr::filter(wdf_q1_5 == gg) %>%
      dplyr::select(-wdf_q1_5) %>%
      tidyr::pivot_longer(
        cols = any_of(outcome_vars),
        names_to = "clinical_parameters",
        values_to = "value"
        ) %>%
      dplyr::filter(clinical_parameters == nn) %>%
      tidyr::drop_na(value) %>%
      dplyr::group_by(study_id, data_type) %>%
      dplyr::filter(n()==2) %>%
      dplyr::ungroup()
    
    ### rename value column to outcome names
    colnames(df)[colnames(df) == "value"] = nn
    
    df_new <- df
    
    ### tbl_summary paired test formula stratified by data type
    table_out <- tbl_strata(df_new,
                            strata = data_type,
                            .tbl_fun = ~ .x %>%
                              tbl_summary(
                                by = wdf_q1_9, include = -c(study_id,clinical_parameters),
                                type = list(
                                  any_of(outcome_vars) ~ "continuous2",
                                  all_continuous() ~ "continuous2")
                                , statistic = all_continuous() ~ c(
                                  "{mean} ({sd})",
                                  "{median} ({p25}, {p75})")
                                , digits = all_continuous() ~ 2
                                , missing = "ifany" # don't list missing data separately #ifany #no
                                ,missing_text = "Missing"
                                ) %>% 
                              modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**"
                                            ) %>% # update the column header
                              bold_labels() %>%
                              italicize_levels()%>%
                              add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
                                     # add column with total number of non-missing observations
                                     ) %>% 
                              add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
                                    test = list(any_of(outcome_vars) ~ "paired.wilcox.test"), group = study_id ) %>%
                              bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
                              add_difference(test = list(any_of(outcome_vars) ~ "paired_cohens_d"),
                                             group = study_id) %>% #requires gtsummary version 2.0.1
                              modify_footnote(
                                all_stat_cols() ~ "Mean (SD); Median (IQR)"
                                ),
                            .header = "**{strata}**"
                            )
    
  }, simplify = FALSE
  )
 
  out_merge <- tbl_stack(out,
                         group_header = attribute$label[attribute$variable %in% outcome_vars]
                         ) %>%
    gtsummary::as_flex_table() 
  
}, simplify = FALSE
)
