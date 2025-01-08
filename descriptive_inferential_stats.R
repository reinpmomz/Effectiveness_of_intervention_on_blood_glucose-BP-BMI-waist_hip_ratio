library(dplyr)

working_directory

my_gtsummary_theme

gtsummary_compact_theme

## Descriptive statistics
descriptive_stats <- inferential_table(df = df_analysis,
                                       by_vars = strata_vars,
                                       foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                       caption = "Descriptive Statistics",
                                       include = names(df_analysis)[!(names(df_analysis) %in% id_vars)],
                                       continous_digits = 2,
                                       percent = "column", #default
                                       p_value = FALSE,
                                       mean_vars = c("wdf_q3_18", "wdf_q3_20", "wdf_q3_33", "wdf_q3_36", "wdf_q3_39",
                                                     "wdf_q3_42", "wdf_q3_45", "wdf_q3_34a_b_hours", "wdf_q3_37a_b_hours",
                                                     "wdf_q3_40a_b_hours", "wdf_q3_43a_b_hours", "wdf_q3_46a_b_hours"
                                                     ),
                                       flex_table = TRUE
                                       )

print(descriptive_stats)

## Inferential statistics 

inferential_vars <- selected_vars_df$new_variable[selected_vars_df$inferential == "yes" &
                                                    !is.na(selected_vars_df$inferential)]


inferential_stats <- inferential_strata_table(df = df_analysis,
                                              foot_note = "n (%); Mean (SD); Median (IQR); Range",
                                              caption = "Inferential Statistics",
                                              include = names(df_analysis)[!(names(df_analysis) %in% id_vars)],
                                              strata_vars = strata_vars,
                                              by_vars = inferential_vars,
                                              continous_digits = 2,
                                              percent = "column", #default
                                              p_value = TRUE, #default
                                              mean_vars = c("wdf_q3_18", "wdf_q3_20", "wdf_q3_33", "wdf_q3_36", "wdf_q3_39",
                                                            "wdf_q3_42", "wdf_q3_45", "wdf_q3_34a_b_hours", "wdf_q3_37a_b_hours",
                                                            "wdf_q3_40a_b_hours", "wdf_q3_43a_b_hours", "wdf_q3_46a_b_hours"
                                                            ),
                                              flex_table = TRUE
                                              )

print(inferential_stats)

