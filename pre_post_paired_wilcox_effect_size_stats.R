library(dplyr)
library(tidyr)
library(forcats)
library(rstatix)
library(coin)

working_directory

pre_post_overall_paired_wilcox_stats <- sapply(outcome_vars, function(x){
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
                  , wdf_q1_9 = forcats::fct_rev(wdf_q1_9)
                  ) %>%
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
  
   # Paired-samples wilcoxon effect size
   effect_size <- df %>% 
     dplyr::group_by(data_type) %>%
     rstatix::wilcox_effsize(value ~ wdf_q1_9, paired = TRUE, ci = FALSE)
  
  }, simplify = FALSE
)

pre_post_overall_paired_wilcox_stats_merge <- dplyr::bind_rows(pre_post_overall_paired_wilcox_stats) %>%
  dplyr::mutate(variable = rep(names(pre_post_overall_paired_wilcox_stats), each = 2)) %>%
  dplyr::left_join(selected_vars_df %>% dplyr::select(new_variable, new_label),
                   by = c("variable" = "new_variable")
                   ) %>%
  tibble::as_tibble()

pre_post_gender_paired_wilcox_stats <- sapply(unique(as.character(df_drop_vars$wdf_q1_5)), function(y){
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
    
    # Paired-samples wilcoxon effect size
    effect_size <- df %>% 
      dplyr::group_by(data_type) %>%
      rstatix::wilcox_effsize(value ~ wdf_q1_9, paired = TRUE, ci = FALSE)
    
  }, simplify = FALSE
  )
  
  out_merge <- dplyr::bind_rows(out) %>%
    dplyr::mutate( gender = gg
                   , variable = rep(names(out), each = 2)) %>%
    dplyr::left_join(selected_vars_df %>% dplyr::select(new_variable, new_label),
                     by = c("variable" = "new_variable")
                     ) %>%
    tibble::as_tibble()
  
}, simplify = FALSE
)

pre_post_gender_paired_wilcox_stats_merge <- dplyr::bind_rows(pre_post_gender_paired_wilcox_stats)
