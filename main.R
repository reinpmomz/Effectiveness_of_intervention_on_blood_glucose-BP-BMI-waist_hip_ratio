######################################################################
### Restart R
#.rs.restartR()

### Setting work directory
working_directory <- base::setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#working_directory <- base::setwd(".")

### Load Rdata
Rdata_files <- list.files(path = working_directory, pattern = "*.RData", full.names = T)

if ( length(Rdata_files) >0) {
  invisible(lapply(Rdata_files,load,.GlobalEnv))
} else {
  paste(c(".RData files", "do not exist"), collapse = " ")
}

### Install required packages
source("requirements.R")

### helper/customized functions
source("helperfuns_1.R")
source("helperfuns_2.R")
source("helperfuns_plots_3.R")
source("helperfuns_plots_4.R")
source("helperfuns_5.R")
source("helperfuns_6.R")

### Load data 
#source("load_data_drive.R")
source("load_data_local.R")

### Load recode file 
source("load_recode_file.R")

######################################################################

### Data cleaning - Baseline
source("cleaning_baseline.R")

### Create SES, healthy eating and physical activity based on PCA - Baseline
source("ses_pca_baseline.R")
source("healthy_eating_pca_baseline.R")
source("physical_activity_pca_baseline.R")

######################################################################

### Data cleaning - Followup
source("cleaning_followup.R")

### Create SES, healthy eating and physical activity based on PCA - Followup
source("ses_pca_followup.R")
source("healthy_eating_pca_followup.R")
source("physical_activity_pca_followup.R")

######################################################################

### Merge the baseline and followup data
source("merge_data.R")

### Select variables for descriptive and inferential analysis
source("analysis_data.R")

### Descriptive and Inferential stats
source("descriptive_inferential_stats.R")

### correlation and reliability stats
source("reliability_correlation_tools_stats.R")

### Save stats output
source("save_descriptive_inferential_output.R")
source("save_reliability_correlation_tools_output.R")

### Descriptive plots
source("descriptive_plots.R")

######################################################################

### Check normality and distribution of continous outcome variable
source("check_normality_distribution.R")

### Select variables required for modelling
source("modelling_data.R")

######################################################################
### Imputation
source("imputation_mice.R")

### Paired tests
source("pre_post_paired_test_stats.R")
source("pre_post_paired_test_change_stats.R")
source("pre_post_paired_wilcox_effect_size_stats.R")

source("save_pre_post_paired_output.R")

### Mixed effects models
source("linear_mixed_effect_models.R")
source("linear_mixed_effect_model_performance.R")
source("linear_mixed_effect_model_residuals.R")
source("linear_mixed_effect_model_random_effects.R")

source("save_linear_mixed_effect_models_output.R")

######################################################################

## Save workspace at the end without working directory path
save(list = ls(all.names = TRUE)[!ls(all.names = TRUE) %in% c("working_directory", "mainDir", "subDir_output", "output_Dir",
                                                              "local_download", "file_id", "list_folders", "list_files",
                                                              "Rdata_files")],
     file = "wdf_baseline_followup.RData",
     envir = .GlobalEnv #parent.frame()
     )

######################################################################

## Run all files in Rstudio
source("main.R")

######################################################################

