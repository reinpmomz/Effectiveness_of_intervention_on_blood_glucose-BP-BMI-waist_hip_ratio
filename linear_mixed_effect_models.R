library(dplyr)
library(tidyr)
library(forcats)
library(caret)
library(lme4)
library(lmerTest)
library(gtsummary)

working_directory

my_gtsummary_theme

gtsummary_compact_theme

linear_mixed_model_raw <- sapply(outcome_vars, function(x){
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
   
   ### loglikelihood estimates are unbiased for the fixed effects but biased for the random effects 
   ### REML estimates are biased for the fixed effects and unbiased for the random effects.
   
   #model <- lmer4::lmer(value ~ .-study_id + (1 | study_id), data = df_new, REML = TRUE) #doesnt generate pvalues. use lmerTest
   
   model <- lmerTest::lmer(value ~ .-study_id + (1 | study_id), data = df_new, REML = TRUE)
   
   table_out <- model %>%
     tbl_regression(exponentiate = FALSE,
                    intercept = TRUE,
                    estimate_fun = ~style_sigfig(.x, digits = 3),
                    pvalue_fun = ~style_pvalue(.x, digits = 3)
                    ) %>%
     #add_global_p(keep = TRUE) %>% # add global p-value for categorical variables
     add_glance_table() %>%
     bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
     bold_labels() %>%
     italicize_levels() %>% 
     modify_header(label = "**Linear Mixed Effects - Random intercept**")%>% # update the column header
     add_significance_stars(
       pattern = "{estimate} [{conf.low} to {conf.high}]{stars}",
       hide_ci = TRUE, hide_se = TRUE , hide_p = FALSE) %>%
     modify_header(estimate ~ "**Beta (95% CI)**") %>%
     modify_footnote(estimate ~ "Beta = Estimate, CI = Confidence Interval", abbreviation = TRUE)
   
  }, simplify = FALSE
)

linear_mixed_model_raw_merge <- tbl_merge(linear_mixed_model_raw,
                                          tab_spanner = attribute$label[attribute$variable %in% outcome_vars]
                                          ) %>%
  gtsummary::as_flex_table()


linear_mixed_model_imputed <- sapply(outcome_vars, function(x){
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
  
  ### loglikelihood estimates are unbiased for the fixed effects but biased for the random effects 
  ### REML estimates are biased for the fixed effects and unbiased for the random effects.
  
  #model <- lmer4::lmer(value ~ .-study_id + (1 | study_id), data = df_new, REML = TRUE) #doesnt generate pvalues. use lmerTest
  
  model <- lmerTest::lmer(value ~ .-study_id + (1 | study_id), data = df_new, REML = TRUE)
  
  table_out <- model %>%
    tbl_regression(exponentiate = FALSE,
                   intercept = TRUE,
                   estimate_fun = ~style_sigfig(.x, digits = 3),
                   pvalue_fun = ~style_pvalue(.x, digits = 3)
                   ) %>%
    #add_global_p(keep = TRUE) %>% # add global p-value for categorical variables
    add_glance_table() %>%
    bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
    bold_labels() %>%
    italicize_levels() %>% 
    modify_header(label = "**Linear Mixed Effects - Random intercept**")%>% # update the column header
    add_significance_stars(
      pattern = "{estimate} [{conf.low} to {conf.high}]{stars}",
      hide_ci = TRUE, hide_se = TRUE , hide_p = FALSE) %>%
    modify_header(estimate ~ "**Beta (95% CI)**") %>%
    modify_footnote(estimate ~ "Beta = Estimate, CI = Confidence Interval", abbreviation = TRUE)
  
}, simplify = FALSE
)

linear_mixed_model_imputed_merge <- tbl_merge(linear_mixed_model_imputed,
                                              tab_spanner = attribute$label[attribute$variable %in% outcome_vars]
                                              ) %>%
  gtsummary::as_flex_table()
