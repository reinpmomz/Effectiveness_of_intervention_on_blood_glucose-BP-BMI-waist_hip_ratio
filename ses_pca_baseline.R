library(dplyr)
library(factoextra)
library(labelled)
library(tidyr)
library(tibble)

working_directory

## group ses variables 
### if empty vector use character()

ses_pca_vars <- selected_vars_df$new_variable[selected_vars_df$select_group == "ses pca" & !is.na(selected_vars_df$select_group)]
id_vars <- selected_vars_df$new_variable[selected_vars_df$select_group == "study_id"& !is.na(selected_vars_df$select_group)]

if (length(ses_pca_vars)>1) {
  
ses_baseline_df <- df_clean_baseline %>%
  dplyr::select(any_of(c(id_vars, ses_pca_vars))
                ) %>%
  dplyr::mutate(across(any_of(ses_pca_vars), ~as.numeric(.x))
         ) %>%
  tidyr::drop_na()

## Perform PCA

### prcomp() uses the singular value decomposition (SVD) which examines the covariances / correlations between individuals
ses_baseline_pca <- stats::prcomp(ses_baseline_df %>% dplyr::select(-any_of(id_vars)), center = FALSE, scale. = TRUE)
ses_baseline_pca_summ <- summary(ses_baseline_pca)
print(ses_baseline_pca_summ)

### The first principal component explains the largest proportion of the total variance and it is used as the wealth index to represent 
### the household's wealth
ses_baseline_scores <- ses_baseline_pca$x[, 1, drop=TRUE]

ses_baseline_loadings <- ses_baseline_pca_summ$rotation[, 1, drop=TRUE]

if(min(sign(ses_baseline_loadings)) != max(sign(ses_baseline_loadings))){
	stop("PC1 is not a positively signed index")
}

ses_baseline_report <- paste0(paste0(ses_pca_vars[ses_pca_vars %in% names(ses_baseline_df)], collapse=", "), " used to create SES index")
print(ses_baseline_report)

## Variance explained plot

### Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
ses_baseline_explained_plot <- factoextra::fviz_screeplot(ses_baseline_pca, addlabels = TRUE)
print(ses_baseline_explained_plot)

ggsave(plot=ses_baseline_explained_plot, height = 7, width = 10,
       filename = "ses_baseline_scree_plot.png", path = output_Dir, bg='white')

## Principal Component plots

### Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides 
###of the graph.

ses_baseline_pc_plot <- fviz_pca_var(ses_baseline_pca,
                            col.var = "contrib", # Color by contributions to the PC
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            repel = TRUE     # Avoid text overlapping
                            )

print(ses_baseline_pc_plot)

ggsave(plot=ses_baseline_pc_plot, height = 7, width = 10,
       filename = "ses_baseline_pc_plot.png", path = output_Dir, bg='white')

## adding ses columns to data set
df_final_baseline <- df_clean_baseline %>%
  dplyr::left_join(ses_baseline_df %>%
              dplyr::mutate(ses_scores = ses_baseline_scores) %>%
              dplyr::select(any_of(id_vars), ses_scores), by = c(id_vars)) %>%
  dplyr::mutate(ses_index = cut(ses_scores, breaks = 5, 
                         labels = c("Quintile1", "Quintile2", "Quintile3", "Quintile4", "Quintile5"))) %>%
  labelled::set_variable_labels(#labeling created variables
    ses_scores = "socio-economic status scores",
    ses_index = "socio-economic status"
  )

## creating data dictionary
attribute_final_baseline <- 
  base::as.data.frame(labelled::generate_dictionary(df_final_baseline, labels = TRUE, values = TRUE))

## Creating a named vector to quickly assign the variable labels
final_labels_baseline <- attribute_final_baseline %>%
  dplyr::select(variable, label)%>%
  tibble::deframe()
} else {
  
  df_final_baseline <- df_clean_baseline
  
  ## creating data dictionary
  attribute_final_baseline <- 
    base::as.data.frame(labelled::generate_dictionary(df_final_baseline, labels = TRUE, values = TRUE))
  
  ## Creating a named vector to quickly assign the variable labels
  final_labels_baseline <- attribute_final_baseline %>%
    dplyr::select(variable, label)%>%
    tibble::deframe()
  
}

