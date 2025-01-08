library(dplyr)
library(factoextra)
library(labelled)
library(tidyr)
library(tibble)

working_directory

## group healthy eating variables 
### if empty vector use character()

healthy_eating_baseline_pca_vars <- selected_vars_df$new_variable[selected_vars_df$select_group == "health_eating_baseline_pca"
                                                                  & !is.na(selected_vars_df$select_group)]
id_vars <- selected_vars_df$new_variable[selected_vars_df$select_group == "study_id" & !is.na(selected_vars_df$select_group)]

if (length(healthy_eating_baseline_pca_vars)>1) {
  
healthy_eating_baseline_df <- df_final_baseline %>%
  dplyr::select(any_of(c(id_vars, healthy_eating_baseline_pca_vars))
                ) %>%
  dplyr::mutate(across(any_of(healthy_eating_baseline_pca_vars), ~as.numeric(.x))
         ) %>%
  tidyr::drop_na()

## Perform PCA

### prcomp() uses the singular value decomposition (SVD) which examines the covariances / correlations between individuals
healthy_eating_baseline_pca <- stats::prcomp(healthy_eating_baseline_df %>% dplyr::select(-any_of(id_vars)),
                                             center = FALSE, scale. = TRUE)
healthy_eating_baseline_pca_summ <- summary(healthy_eating_baseline_pca)
print(healthy_eating_baseline_pca_summ)

### The first principal component explains the largest proportion of the total variance and it is used as the wealth index to represent 
### the household's wealth
healthy_eating_baseline_scores <- healthy_eating_baseline_pca_summ$x[, 1, drop=TRUE]

healthy_eating_baseline_loadings <- healthy_eating_baseline_pca_summ$rotation[, 1, drop=TRUE]

if(min(sign(healthy_eating_baseline_loadings)) != max(sign(healthy_eating_baseline_loadings))){
stop("PC1 is not a positively signed index")
}

healthy_eating_baseline_report <- paste0(paste0(healthy_eating_baseline_pca_vars, collapse=", "), " used to create Healthy eating index")
print(healthy_eating_baseline_report)

## Variance explained plot

### Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
healthy_eating_baseline_explained_plot <- factoextra::fviz_screeplot(healthy_eating_baseline_pca, addlabels = TRUE)
print(healthy_eating_baseline_explained_plot)

ggsave(plot=healthy_eating_baseline_explained_plot, height = 7, width = 10,
       filename = "healthy_eating_baseline_scree_plot.png", path = output_Dir, bg='white')

## Principal Component plots

### Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides 
###of the graph.

healthy_eating_baseline_pc_plot <- fviz_pca_var(healthy_eating_baseline_pca,
                            col.var = "contrib", # Color by contributions to the PC
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                            repel = TRUE     # Avoid text overlapping
                            )

print(healthy_eating_baseline_pc_plot)

ggsave(plot=healthy_eating_baseline_pc_plot, height = 7, width = 10,
       filename = "healthy_eating_baseline_pc_plot.png", path = output_Dir, bg='white')

## adding healthy eating columns to data set
df_final_baseline <- (df_final_baseline %>%
  dplyr::left_join(healthy_eating_baseline_df %>%
              dplyr::mutate(healthy_eating_scores = healthy_eating_baseline_scores) %>%
              dplyr::select(any_of(id_vars), healthy_eating_scores), by = c(id_vars)) %>%
  coalesce_multi(noisy = TRUE) %>%
  dplyr::mutate(healthy_eating_index = cut(healthy_eating_scores, breaks = 2, 
                         labels = c("Bad", "Good"))) %>%
  labelled::set_variable_labels(#labeling created variables
    healthy_eating_scores = "Healthy eating habit scores",
    healthy_eating_index = "Healthy eating habit"
  )
  )

## creating data dictionary
attribute_final_baseline <- 
  base::as.data.frame(labelled::generate_dictionary(df_final_baseline, labels = TRUE, values = TRUE))

## Creating a named vector to quickly assign the variable labels
final_labels_baseline <- attribute_final_baseline %>%
  dplyr::select(variable, label)%>%
  tibble::deframe()
} else {
  
  df_final_baseline <- (df_final_baseline)
  
  ## creating data dictionary
  attribute_final_baseline <- 
    base::as.data.frame(labelled::generate_dictionary(df_final_baseline, labels = TRUE, values = TRUE))
  
  ## Creating a named vector to quickly assign the variable labels
  final_labels_baseline <- attribute_final_baseline %>%
    dplyr::select(variable, label)%>%
    tibble::deframe()
  
}

