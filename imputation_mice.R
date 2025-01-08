library(dplyr)
library(forcats)
library(labelled)
library(sjlabelled)
library(ggplot2)
library(ggstats)
library(naniar)
library(mice)

working_directory

## Imputation

### Create raw imputation data
df_imputation_raw <- dplyr:: bind_rows( df_drop_vars ,
                                        df_drop_vars %>%
                                          dplyr::group_by(study_id) %>%
                                          dplyr::filter(n()==1) %>%
                                          dplyr::ungroup() %>%
                                          dplyr::filter(wdf_q1_9 == "Baseline") %>%
                                          dplyr::mutate( across(any_of(outcome_vars), ~if_else(!is.na(.x), NA, .x))
                                                         , across(c("healthy_eating_index", "regular_physical_activity_index"), ~if_else(!is.na(.x), NA, .x))
                                                         , wdf_q1_4_age = wdf_q1_4_age + 0.50
                                                         , wdf_q2_3a_b_years = wdf_q2_3a_b_years + 0.500
                                                         , wdf_q1_9 = "Six month follow-up"
                                                         , wdf_q1_9 = factor(wdf_q1_9, levels = c("Baseline", "Six month follow-up"))
                                                         )
                                        ) %>%
  labelled::set_variable_labels(!!!final_labels_baseline[names(final_labels_baseline) %in% names(.)]
                                ) %>%
  labelled::set_variable_labels(!!!final_labels_followup[names(final_labels_followup) %in% names(.)]
                                ) #labeling variables from data dictionary
  

### Confirming if values in raw imputation data are MCAR
imputation_raw_mcar_metrics <- data.frame(naniar::mcar_test(df_imputation_raw)
                                ) %>%
  dplyr::mutate(mcar_result = if_else(p.value < 0.05, "missing values are not missing completely at random",
                                      "missing values are missing completely at random")
                , non_missing_rows_percent = nrow(na.omit(df_imputation_raw)) / nrow(df_imputation_raw) * 100
                , missing_rows_percent = 100-non_missing_rows_percent)
  

### Explore missing values patterns in raw imputation data

#### Save as PNG using png() and dev.off()
grDevices::png(filename = base::file.path(output_Dir, "imputation_raw_missing_pattern_plot.png"),
               height = 9.5,
               width = 12.5,
               units = "in",
               res = 300,
               bg = "white"
               )

df_imputation_raw %>%
  sjlabelled:::label_to_colnames() %>% #variable labels to column names.
  mice::md.pattern(rotate.names = TRUE) %>%
  invisible()

#### Close the PNG device
grDevices::dev.off()


### Impute Missing Values - Mice (Multivariate Imputation by Chained Equations)

#### Imputation methods: pmm (Predictive mean matching), cart (Classification and regression trees)
#### laso.norm (Lasso linear regression), rf (Random Forest), norm (Bayesian linear regression)
#### logreg (Logistic regression), lasso.logreg (Lasso logistic regression)

impute_vars <- c(outcome_vars, "healthy_eating_index", "regular_physical_activity_index")

imputation_mice_plots <- sapply(impute_vars, function(x){
  nn <- x
  nn_ <- impute_vars[!impute_vars %in% nn]
  df_train <- df_imputation_raw
  df_new <- df_train %>%
    dplyr::select(-c(study_id, wdf_q1_4_age, any_of(nn_))
                  ) #dataframe only has only one target variable 
  
  train <- df_train[[nn]]
  
  label <- if (is.null(labelled::var_label(train))) {nn
    } else {
    labelled::var_label(train)
  }
  
  impute_pmm <- complete(mice(df_new, method = "pmm", printFlag = FALSE, seed = seed_imputation))[[nn]] #numeric and categorical
  
  impute_cart <- complete(mice(df_new, method = "cart", printFlag = FALSE, seed = seed_imputation))[[nn]] #numeric and categorical
  
  impute_norm <- if (nn == "healthy_eating_index" | nn == "regular_physical_activity_index") { NA
    } else {
      complete(mice(df_new, method = "norm", printFlag = FALSE, seed = seed_imputation))[[nn]] #numeric only
      }
  
  impute_lasso_norm <- if (nn == "healthy_eating_index" | nn == "regular_physical_activity_index") { NA
    } else {
      complete(mice(df_new, method = "lasso.norm", printFlag = FALSE, seed = seed_imputation))[[nn]] #numeric only
      }
  
  impute_rf <- complete(mice(df_new, method = "rf", printFlag = FALSE, seed = seed_imputation))[[nn]] #numeric and categorical
  
  impute_logreg <- if (nn == "healthy_eating_index" | nn == "regular_physical_activity_index") { 
    complete(mice(df_new, method = "logreg", printFlag = FALSE, seed = seed_imputation))[[nn]] #categorical only
  } else {
      NA
  }
  
  impute_lasso_logreg <- if (nn == "healthy_eating_index" | nn == "regular_physical_activity_index") { 
    complete(mice(df_new, method = "lasso.logreg", printFlag = FALSE, seed = seed_imputation))[[nn]] #categorical only
    } else {
      NA
      }
  
  df_impute_final <- data.frame(
    round = df_train$wdf_q1_9,
    original = train,
    imputed_pmm = impute_pmm,
    imputed_cart = impute_cart,
    imputed_norm = impute_norm,
    imputed_lasso_norm = impute_lasso_norm,
    imputed_rf = impute_rf,
    imputed_logreg = impute_logreg,
    imputed_lasso_logreg = impute_lasso_logreg
    )
  
  df_impute_plot <- df_impute_final %>%
      tidyr::pivot_longer(
        cols = !round,
        names_to = "name",
        values_to = "value",
        values_drop_na = TRUE
      ) %>%
      dplyr::mutate(name = forcats::as_factor(name))
  
  plot <- if (nn == "healthy_eating_index" | nn == "regular_physical_activity_index") { 
    df_impute_plot %>%
      dplyr::filter(round == "Six month follow-up") %>%
      dplyr::select(-round) %>%
      ggplot(aes(x=name, fill = forcats::fct_infreq(value), by = name)
             ) +
      geom_bar(width=0.9, position="fill", stat="count", show.legend = TRUE) +
      geom_text(stat = "prop", position = position_fill(.5), color="black", angle = 0,
                size=3, fontface = "bold") +
      guides(fill=guide_legend(nrow = 1, ncol = NULL)) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
      scale_y_continuous(labels = scales::percent, n.breaks = 10, expand = expansion(mult = c(0.01,0.05) )) +
      labs(x="", y="", fill = "", title = stringr::str_wrap(label, width = 50)) +
      theme_minimal() + 
      theme(
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.line.y = element_line(colour = "grey",inherit.blank = FALSE),
        axis.line.x = element_line(colour = "grey",inherit.blank = FALSE),
        axis.ticks.y = element_line(linewidth = 0.5, color="black"),
        axis.ticks.x = element_line(linewidth = 0.5, color="black"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    
  } else {
    df_impute_plot %>%
      dplyr::filter(round == "Six month follow-up") %>%
      dplyr::select(-round) %>%
      ggplot(aes(x = as.numeric(value))
      ) +
      geom_histogram(aes(y = after_stat(density), fill = name), bins = 50, 
                     color = "#000000", position = "identity", show.legend = FALSE
      ) +
      geom_density(linewidth = 0.5, colour = "royalblue",
                   fill = "royalblue", alpha = 0.25) +  
      geom_vline(aes(xintercept = mean(value, na.rm=TRUE)), 
                 linetype = "dashed", linewidth = 0.6) +
      scale_x_continuous(n.breaks = 10) +
      scale_y_continuous(n.breaks = 10, expand = expansion(mult = c(0.01,0.05))
      ) +
      facet_wrap(~name, ncol = 3, scales = "free_x") +
      labs(x="value", y="density", title = stringr::str_wrap(label, width = 50)) +
      theme_minimal() + 
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.line.y = element_line(colour = "grey",inherit.blank = FALSE),
        axis.line.x = element_line(colour = "grey",inherit.blank = FALSE),
        axis.ticks.y = element_line(linewidth = 0.5, color="black"),
        axis.ticks.x = element_line(linewidth = 0.5, color="black"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  }
  
  plot
  
}, simplify = FALSE
)

#### Saving imputation plots using loops
for (i in seq(length(imputation_mice_plots))) {
  ggsave(plot=imputation_mice_plots[[i]], height = 7, width = 12,
         filename = paste0("imputation_plots_",names(imputation_mice_plots)[[i]],".png"),
         path = output_Dir, bg='white')  
}

#### The imputed distributions overall look much closer to the original one. 
#### rf imputation closer for healthy eating, wdf_q5_1_2_3a, wdf_q5_1_2_3b, wdf_q5_6_7
#### logreg imputation closer for physical activity
#### lasso.norm imputation closer for wdf_q5_5_6_7_bmi, wdf_q5_8a_b_waist_hip_ratio
#### cart imputation closer for wdf_q5_8a, wdf_q5_8b
#### pmm imputation closer for healthy eating, wdf_q5_10

imputation_mice_final <- sapply(impute_vars, function(x){
  nn <- x
  nn_ <- impute_vars[!impute_vars %in% nn]
  df_train <- df_imputation_raw
  df_new <- df_train %>%
    dplyr::select(-c(study_id, wdf_q1_4_age, any_of(nn_)))
  
  train <- df_train[[nn]]

  impute_final <- if (nn == "wdf_q5_1_2_3a" | nn == "wdf_q5_1_2_3b" | nn == "wdf_q5_6_7") { 
    complete(mice(df_new, method = "rf", printFlag = FALSE, seed = seed_imputation))[[nn]] 
  } else if (nn == "regular_physical_activity_index") {
    complete(mice(df_new, method = "logreg", printFlag = FALSE, seed = seed_imputation))[[nn]] 
  } else if (nn == "wdf_q5_5_6_7_bmi"| nn == "wdf_q5_8a_b_waist_hip_ratio") {
    complete(mice(df_new, method = "lasso.norm", printFlag = FALSE, seed = seed_imputation))[[nn]] 
  } else if (nn == "wdf_q5_8a" | nn == "wdf_q5_8b" ) {
    complete(mice(df_new, method = "cart", printFlag = FALSE, seed = seed_imputation))[[nn]] 
  } else if (nn == "wdf_q5_10" | nn == "healthy_eating_index" ) {
    complete(mice(df_new, method = "pmm", printFlag = FALSE, seed = seed_imputation))[[nn]] 
  } 
  
}, simplify = FALSE
)

### Create final imputation data
df_imputation_final <- df_imputation_raw %>%
  dplyr::mutate(wdf_q5_8a = round(imputation_mice_final[["wdf_q5_8a"]],1)
                , wdf_q5_8b = round(imputation_mice_final[["wdf_q5_8b"]],1)
                , wdf_q5_10 = round(imputation_mice_final[["wdf_q5_10"]],1)
                , wdf_q5_1_2_3a = round(imputation_mice_final[["wdf_q5_1_2_3a"]],0)
                , wdf_q5_1_2_3b = round(imputation_mice_final[["wdf_q5_1_2_3b"]],0)
                , wdf_q5_6_7 = round(imputation_mice_final[["wdf_q5_6_7"]],1)
                , wdf_q5_5_6_7_bmi = round(imputation_mice_final[["wdf_q5_5_6_7_bmi"]],2)
                , wdf_q5_8a_b_waist_hip_ratio = round(imputation_mice_final[["wdf_q5_8a_b_waist_hip_ratio"]],2)
                , healthy_eating_index = imputation_mice_final[["healthy_eating_index"]]
                , regular_physical_activity_index = imputation_mice_final[["regular_physical_activity_index"]]
                )

### Confirming if values in final imputation data are MCAR
imputation_final_mcar_metrics <- data.frame(naniar::mcar_test(df_imputation_final)
                                            ) %>%
  dplyr::mutate(mcar_result = if_else(p.value < 0.05, "missing values are not missing completely at random",
                                      "missing values are missing completely at random")
                , non_missing_rows_percent = nrow(na.omit(df_imputation_final)) / nrow(df_imputation_final) * 100
                , missing_rows_percent = 100-non_missing_rows_percent)

### Explore missing values patterns in finalimputation data

#### Save as PNG using png() and dev.off()
grDevices::png(filename = base::file.path(output_Dir, "imputation_final_missing_pattern_plot.png"),
               height = 9.5,
               width = 12.5,
               units = "in",
               res = 300,
               bg = "white"
               )

df_imputation_final %>%
  sjlabelled:::label_to_colnames() %>% #variable labels to column names.
  mice::md.pattern(rotate.names = TRUE) %>%
  invisible()

#### Close the PNG device
grDevices::dev.off()


### Saving mcar metrics
writexl::write_xlsx(list(raw_mcar_metrics = imputation_raw_mcar_metrics,
                         final_mcar_metrics = imputation_final_mcar_metrics
                         ),
                    path = base::file.path(output_Dir, paste0("imputation_raw_final_mcar_metrics.xlsx") )
                    )


 
