library(dplyr)
library(haven)
library(labelled)

working_directory

## Reading data from local folder

data_files <- list.files(path = data_Dir, full.names = F)

df_list <- sapply(data_files, function(x){
  nn <- x
  
  df_raw <- haven::read_dta(base::file.path(data_Dir, nn))
  
  df <- df_raw %>%
    dplyr::mutate(dplyr::across(dplyr::where(haven::is.labelled), ~ haven::as_factor(.x))
                  ) #converts only labelled columns to factors
  
}, simplify=FALSE)

df_raw_baseline <- df_list[["wdfbaseline_cleaned.dta"]]

df_raw_followup <- df_list[["wdffollowup_cleaned.dta"]]


## creating data dictionary
attribute_raw_baseline <- base::as.data.frame(labelled::generate_dictionary(df_raw_baseline, labels = TRUE, values = TRUE))

attribute_raw_followup <- base::as.data.frame(labelled::generate_dictionary(df_raw_followup, labels = TRUE, values = TRUE))

### Saving data dictionary
writexl::write_xlsx(list(wdf_baseline = attribute_raw_baseline,
                         wdf_followup = attribute_raw_followup
                         ),
                    path = base::file.path(output_Dir, paste0("data_dictionary_raw.xlsx") )
                    )
