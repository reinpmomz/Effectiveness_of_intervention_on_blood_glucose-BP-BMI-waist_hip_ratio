library(dplyr)
library(janitor)
library(labelled)
library(writexl)

working_directory

## Merging to get one dataset

### Comparison of dataframes to indicate whether they will successfully bind together by rows

df_comparison_rows_all <- janitor::compare_df_cols(df_final_baseline, df_final_followup,
                                                   return = "all"
                                                   )

df_comparison_rows_match <- janitor::compare_df_cols(df_final_baseline, df_final_followup,
                                                     return = "match"
                                                     )

df_comparison_rows_mismatch <- janitor::compare_df_cols(df_final_baseline, df_final_followup,
                                                        return = "mismatch"
                                                        )

writexl::write_xlsx(list(comparison_all = df_comparison_rows_all,
                         comparison_match = df_comparison_rows_match,
                         comparison_mismatch = df_comparison_rows_mismatch
                         ),
                    path = base::file.path(output_Dir, paste0("bindrows_data_report.xlsx") )
                    )

### Check if output is true
janitor::compare_df_cols_same(df_final_baseline, df_final_followup
                                   )

df_merged_final <- dplyr::bind_rows( df_final_baseline, df_final_followup) %>%
  labelled::set_variable_labels(!!!final_labels_baseline[names(final_labels_baseline) %in% names(.)]
                                ) %>%
  labelled::set_variable_labels(!!!final_labels_followup[names(final_labels_followup) %in% names(.)]
                                ) #labeling variables from data dictionary

### creating data dictionary
attribute <- as.data.frame(labelled::generate_dictionary(df_merged_final, labels = TRUE, values = TRUE)
                           )

### Saving data dictionary
writexl::write_xlsx(attribute,
                    path = base::file.path(output_Dir, paste0("data_dictionary_merged_final.xlsx") )
                    )

## saving merged dataset
# haven::write_dta(data= df_merged_final, 
#                  path = base::file.path(output_Dir, "wdf_baseline_followup_row_merge.dta")
#                  )

# haven::write_sav(data= df_merged_final, 
#                  path = base::file.path(output_Dir, "wdf_baseline_followup_row_merge.sav")
#                  )

