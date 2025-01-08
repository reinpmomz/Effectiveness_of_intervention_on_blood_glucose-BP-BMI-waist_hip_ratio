library(dplyr)
library(stringr)
library(tidyr)
library(forcats)
library(caret)
library(lme4)
library(lmerTest)
library(nlme)
library(performance)
library(see)
library(qqplotr)
library(gridExtra)
library(ggpubr)

working_directory

linear_mixed_model_random_effects_raw <- sapply(outcome_vars, function(x){
  nn <- x
  
  labels <- attribute$label[attribute$variable == nn]
  
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
   
   model_random_effects <- performance::check_normality(model, effects = "random") #Check model for normality of Random Effects
   plot_random_effects <- plot(model_random_effects)[[1]] +
     labs(x = NULL, y = NULL, subtitle = NULL, title = stringr::str_wrap(labels, width = 50)) + 
     theme(
       plot.title = element_text(hjust = 0.5, size = 9 #, face = "bold"
       )
     )
   
  }, simplify = FALSE
)

linear_mixed_model_random_effects_raw_merge <- 
  ggpubr::ggarrange(plotlist = linear_mixed_model_random_effects_raw[!names(linear_mixed_model_random_effects_raw) %in% c("wdf_q5_8a", "wdf_q5_8b", "wdf_q5_6_7")],
                    ncol = NULL,
                    nrow = length(linear_mixed_model_random_effects_raw[!names(linear_mixed_model_random_effects_raw) %in% c("wdf_q5_8a", "wdf_q5_8b", "wdf_q5_6_7")]),
                    labels = "Sample Data",
                    hjust = -0.5,
                    vjust = -0.5,
                    font.label = list(size = 12, color = "black", face = "bold", family = NULL)
                    )

linear_mixed_model_random_effects_imputed <- sapply(outcome_vars, function(x){
  nn <- x
  
  labels <- attribute$label[attribute$variable == nn]
  
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
  
  model_random_effects <- performance::check_normality(model, effects = "random") #Check model for normality of Random Effects
  
  plot_random_effects <- plot(model_random_effects)[[1]] +
      labs(x = NULL, y = NULL, subtitle = NULL, title = stringr::str_wrap(labels, width = 50)) + 
      theme(
        plot.title = element_text(hjust = 0.5, size = 9 #, face = "bold"
                                  )
      )
  
}, simplify = FALSE
)


linear_mixed_model_random_effects_imputed_merge <- 
  ggpubr::ggarrange(plotlist = linear_mixed_model_random_effects_imputed[!names(linear_mixed_model_random_effects_imputed) %in% c("wdf_q5_8a", "wdf_q5_8b", "wdf_q5_6_7")],
                    ncol = NULL,
                    nrow = length(linear_mixed_model_random_effects_imputed[!names(linear_mixed_model_random_effects_imputed) %in% c("wdf_q5_8a", "wdf_q5_8b", "wdf_q5_6_7")]),
                    labels = "Imputed Data",
                    hjust = -0.5,
                    vjust = -0.5,
                    font.label = list(size = 12, color = "black", face = "bold", family = NULL)
                    )


linear_mixed_model_random_effects_final_merge <- ggpubr::annotate_figure(
  gridExtra::grid.arrange(linear_mixed_model_random_effects_raw_merge, 
                          linear_mixed_model_random_effects_imputed_merge,
                          ncol = 2
                          ),
  top = "",
  right = NULL,
  left = text_grob("RE Quantiles", color = "black", face = "bold", size = 12, rot = 90),
  bottom = text_grob("Theoretical Quantiles", color = "black", face = "bold", size = 12)
)
