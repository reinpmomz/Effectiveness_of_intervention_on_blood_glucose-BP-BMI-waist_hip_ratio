library(dplyr)
library(tidyr)
library(forcats)
library(caret)
library(lme4)
library(lmerTest)
library(performance)

working_directory

linear_mixed_model_performance_raw <- sapply(outcome_vars, function(x){
  nn <- x
  
  ### transform data to long format
  df <- df_drop_vars %>%
    tidyr::pivot_longer(
      cols = any_of(outcome_vars),
      names_to = "clinical_parameters",
      values_to = "value"
      ) %>%
    dplyr::filter(clinical_parameters == nn) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(study_id) %>%
    dplyr::filter(n()==2) %>%
    dplyr::ungroup()
   
   ### dropping factor variables with one level
   values_count <- sapply(lapply(df, unique), length)
   
   df_final <- df[ , values_count > 1]
   
   ### Normalize outcome
   #process <- caret::preProcess(df_final %>% dplyr::select(value), method = c("center")) #method: "center", "scale", "range"
   #norm_scale <- predict(process, df_final %>% dplyr::select(value))
   
   df_new <- df_final #%>%
     #dplyr::mutate(value = norm_scale[["value"]])
   
   ### random intercept model
   ### REML = TRUE (REstricted Maximum Likelihood criterion to be used for optimization of parameter estimates.)
   ### REML = FALSE (loglikelihood criterion to be used for optimization of parameter estimates.)
   
   #model <- lmer4::lmer(value ~ .-study_id + (1 | study_id), data = df_new, REML = TRUE) #doesnt generate pvalues. use lmerTest
   
   model <- lmerTest::lmer(value ~ .-study_id + (1 | study_id), data = df_new, REML = TRUE)
   
   model_performance <- performance::model_performance(model)
   
  }, simplify = FALSE
)

linear_mixed_model_performance_raw_merge <- dplyr::bind_rows(linear_mixed_model_performance_raw) %>%
  dplyr::mutate(variable = names(linear_mixed_model_performance_raw)) %>%
  dplyr::left_join(selected_vars_df %>% dplyr::select(new_variable, new_label),
                   by = c("variable" = "new_variable")
                   ) %>%
  tibble::as_tibble()


linear_mixed_model_performance_imputed <- sapply(outcome_vars, function(x){
  nn <- x
  
  ### transform data to long format
  df <- df_imputation_final %>%
    tidyr::pivot_longer(
      cols = any_of(outcome_vars),
      names_to = "clinical_parameters",
      values_to = "value"
    ) %>%
    dplyr::filter(clinical_parameters == nn) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(study_id) %>%
    dplyr::filter(n()==2) %>%
    dplyr::ungroup()
  
  ### dropping factor variables with one level
  values_count <- sapply(lapply(df, unique), length)
  
  df_final <- df[ , values_count > 1]
  
  ### Normalize outcome
  #process <- caret::preProcess(df_final %>% dplyr::select(value), method = c("center")) #method: "center", "scale", "range"
  #norm_scale <- predict(process, df_final %>% dplyr::select(value))
  
  df_new <- df_final #%>%
    #dplyr::mutate(value = norm_scale[["value"]])
  
  ### random intercept model
  ### REML = TRUE (REstricted Maximum Likelihood criterion to be used for optimization of parameter estimates.)
  ### REML = FALSE (loglikelihood criterion to be used for optimization of parameter estimates.)
  
  #model <- lmer4::lmer(value ~ .-study_id + (1 | study_id), data = df_new, REML = TRUE) #doesnt generate pvalues. use lmerTest
  
  model <- lmerTest::lmer(value ~ .-study_id + (1 | study_id), data = df_new, REML = TRUE)
  
  model_performance <- performance::model_performance(model)
  
}, simplify = FALSE
)

linear_mixed_model_performance_imputed_merge <- dplyr::bind_rows(linear_mixed_model_performance_imputed) %>%
  dplyr::mutate(variable = names(linear_mixed_model_performance_imputed)) %>%
  dplyr::left_join(selected_vars_df %>% dplyr::select(new_variable, new_label),
                   by = c("variable" = "new_variable")
  ) %>%
  tibble::as_tibble()
