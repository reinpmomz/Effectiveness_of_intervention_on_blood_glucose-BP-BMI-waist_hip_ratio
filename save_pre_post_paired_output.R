library(dplyr)
library(flextable)
library(officer)

working_directory

## Saving pre_post_overall output

flextable::save_as_docx(pre_post_overall_paired_stats_merge, pre_post_overall_paired_change_stats_merge, 
                        path = base::file.path(output_Dir, "pre_post_overall_paired_stats.docx"),
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

## Saving pre_post_gender output

flextable::save_as_docx(values = c(pre_post_gender_paired_stats, pre_post_gender_paired_change_stats), 
                        path = base::file.path(output_Dir, "pre_post_gender_paired_stats.docx"),
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


## Saving wilcox effect size output
writexl::write_xlsx(list(overall = pre_post_overall_paired_wilcox_stats_merge,
                         gender = pre_post_gender_paired_wilcox_stats_merge
                         ),
                    path = base::file.path(output_Dir, paste0("pre_post_paired_wilcox_effectsize_stats.xlsx") )
                    )

