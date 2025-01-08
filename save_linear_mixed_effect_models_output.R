library(dplyr)
library(writexl)
library(flextable)
library(officer)

working_directory

## Saving linear mixed models output

flextable::save_as_docx(linear_mixed_model_raw_merge, linear_mixed_model_imputed_merge,
                        path = base::file.path(output_Dir, "linear_mixed_model_raw_imputed_stats.docx"),
                        align = "center", #left, center (default) or right.
                        pr_section = officer::prop_section(
                          page_size = officer::page_size(orient = "landscape"), #Use NULL (default value) for no content.
                          page_margins = officer::page_mar(), #Use NULL (default value) for no content.
                          type = "nextPage", # "continuous", "evenPage", "oddPage", "nextColumn", "nextPage"
                          section_columns = NULL, #Use NULL (default value) for no content.
                          header_default = NULL, #Use NULL (default value) for no content.
                          header_even = NULL, #Use NULL (default value) for no content.
                          header_first = NULL, #Use NULL (default value) for no content.
                          footer_default = NULL, #Use NULL (default value) for no content.
                          footer_even = NULL, #Use NULL (default value) for no content.
                          footer_first = NULL #Use NULL (default value) for no content.
                          )
                        )

## Saving model performance output
writexl::write_xlsx(list(raw_performance_metrics = linear_mixed_model_performance_raw_merge,
                         imputed_performance_metrics = linear_mixed_model_performance_imputed_merge
                         ),
                    path = base::file.path(output_Dir, paste0("linear_mixed_model_raw_imputed_performance_metrics.xlsx") )
                    )

## Saving the model diagnostics residual plots
ggsave(plot=linear_mixed_model_residuals_final_merge, height = 7, width = 12,
       filename = paste0("linear_mixed_model_normality_residuals_raw_imputed",".png"),
       path = output_Dir, 
       bg='white'
       )

## Saving the model diagnostics random effects plots
ggsave(plot=linear_mixed_model_random_effects_final_merge, height = 7, width = 12,
       filename = paste0("linear_mixed_model_normality_random_effects_raw_imputed",".png"),
       path = output_Dir, 
       bg='white'
)

