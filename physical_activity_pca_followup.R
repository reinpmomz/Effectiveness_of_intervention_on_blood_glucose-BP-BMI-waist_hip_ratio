library(dplyr)
library(factoextra)
library(labelled)
library(tidyr)
library(tibble)

working_directory

## group exercise variables 
### if empty vector use character()

physical_activity_followup_pca_vars <- selected_vars_df$new_variable[selected_vars_df$select_group == "exercise_followup_pca"
                                                                     & !is.na(selected_vars_df$select_group)]
id_vars <- selected_vars_df$new_variable[selected_vars_df$select_group == "study_id" & !is.na(selected_vars_df$select_group)]

if (length(physical_activity_followup_pca_vars)>1) {
  
physical_activity_followup_df <- df_final_followup %>%
  dplyr::select(any_of(c(id_vars, physical_activity_followup_pca_vars)), wdf_q1_2
                ) %>%
  dplyr::mutate(across(any_of(physical_activity_followup_pca_vars), ~as.numeric(.x))
         ) %>%
  tidyr::drop_na()

## Perform PCA

### prcomp() uses the singular value decomposition (SVD) which examines the covariances / correlations between individuals
physical_activity_followup_pca <- stats::prcomp(physical_activity_followup_df %>% dplyr::select(-c(any_of(id_vars), wdf_q1_2)),
                                                center = FALSE, scale. = TRUE)
physical_activity_followup_pca_summ <- summary(physical_activity_followup_pca)
print(physical_activity_followup_pca_summ)

### The first principal component explains the largest proportion of the total variance and it is used as the wealth index to represent 
### the household's wealth
physical_activity_followup_scores <- physical_activity_followup_pca$x[, 1, drop=TRUE]

physical_activity_followup_loadings <- physical_activity_followup_pca_summ$rotation[, 1, drop=TRUE]

if(min(sign(physical_activity_followup_loadings)) != max(sign(physical_activity_followup_loadings))){
stop("PC1 is not a positively signed index")
}

physical_activity_followup_report <- paste0(paste0(physical_activity_followup_pca_vars, collapse=", "), " used to create Physical activity index")
print(physical_activity_followup_report)

## Variance explained plot

### Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
physical_activity_followup_explained_plot <- factoextra::fviz_screeplot(physical_activity_followup_pca, addlabels = TRUE)
print(physical_activity_followup_explained_plot)

ggsave(plot=physical_activity_followup_explained_plot, height = 7, width = 10,
       filename = "physical_activity_followup_scree_plot.png", path = output_Dir, bg='white')

## Principal Component plots

### Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides 
###of the graph.

physical_activity_followup_pc_plot <- fviz_pca_var(physical_activity_followup_pca,
                            col.var = "contrib", # Color by contributions to the PC
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            repel = TRUE     # Avoid text overlapping
                            )

print(physical_activity_followup_pc_plot)

ggsave(plot=physical_activity_followup_pc_plot, height = 7, width = 10,
       filename = "physical_activity_followup_pc_plot.png", path = output_Dir, bg='white')

## adding physical activity columns to data set
df_final_followup <- (df_final_followup %>%
  dplyr::left_join(physical_activity_followup_df %>%
                     dplyr::mutate(regular_physical_activity_scores = physical_activity_followup_scores) %>%
                     dplyr::select(any_of(id_vars), regular_physical_activity_scores, wdf_q1_2),
                   by = c(id_vars, "wdf_q1_2")
                   ) %>%
  coalesce_multi(noisy = TRUE) %>%
  dplyr::mutate(regular_physical_activity_index = cut(regular_physical_activity_scores, breaks = 2, 
                         labels = c("No", "Yes"))) %>%
  labelled::set_variable_labels(#labeling created variables
    regular_physical_activity_scores = "Regular physical activity scores",
    regular_physical_activity_index = "Regular physical activity"
    )
  )

## creating data dictionary
attribute_final_followup <- 
  base::as.data.frame(labelled::generate_dictionary(df_final_followup, labels = TRUE, values = TRUE))

## Creating a named vector to quickly assign the variable labels
final_labels_followup <- attribute_final_followup %>%
  dplyr::select(variable, label)%>%
  tibble::deframe()
} else {
  
  df_final_followup <- (df_final_followup)
  
  ## creating data dictionary
  attribute_final_followup <- 
    base::as.data.frame(labelled::generate_dictionary(df_final_followup, labels = TRUE, values = TRUE))
  
  ## Creating a named vector to quickly assign the variable labels
  final_labels_followup <- attribute_final_followup %>%
    dplyr::select(variable, label)%>%
    tibble::deframe()
  
}

