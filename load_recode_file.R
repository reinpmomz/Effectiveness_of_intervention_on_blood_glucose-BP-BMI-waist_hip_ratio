library(dplyr)
library(readxl)
library(tibble)

working_directory

## Reading the recode file sheet

recode_file <- read_excel_allsheets("wdf_baseline_followup_recode_file.xlsx")

study_vars_df <- recode_file[["study_title"]] #df for study details

location_vars_df <- recode_file[["location"]] #df for factor reordering location details

rename_vars_baseline_df <- recode_file[["rename_vars_baseline"]] #df for renaming variable labels

rename_vars_followup_df <- recode_file[["rename_vars_followup"]] #df for renaming variable labels

selected_vars_df <- recode_file[["selected_vars"]] #df for choosing variables for analysis and plots

drop_selected_vars_df <- recode_file[["drop_selected_vars"]] #df for dropping analysis variables not needed for modelling

model_params_df <- recode_file[["model_params"]] #df for model pre-processing


## Creating a named vector to quickly assign the new variable labels
rename_vars_baseline_df <- (rename_vars_baseline_df %>%
                              dplyr::mutate(new_label = stringr::str_to_sentence(new_label))
                            )

rename_vars_followup_df <- (rename_vars_followup_df %>%
                              dplyr::mutate(new_label = stringr::str_to_sentence(new_label))
                            )

new_labels_baseline <- rename_vars_baseline_df %>%
  dplyr::select(new_variable, new_label)%>%
  tibble::deframe()

new_labels_followup <- rename_vars_followup_df %>%
  dplyr::select(new_variable, new_label)%>%
  tibble::deframe()

## Creating a named vector to quickly assign the new factor levels
new_levels_location <- location_vars_df %>%
  dplyr::select(location_new, location_old)%>%
  tibble::deframe()


