library(dplyr)
library(tibble)
library(writexl)

working_directory

## Saving reliability and correlation stats Output

if (length(unique(selected_vars_df$tool_correlation[selected_vars_df$tool_correlation != "none" &
                                                    !is.na(selected_vars_df$tool_correlation)]))>0) {
writexl::write_xlsx(correlation_tools_stats,
                    path = base::file.path(output_Dir, "correlation_tools.xlsx" )
                    )
 } else {
    print(paste0("No correlation analysis done for tools"))
  }


if (length(unique(selected_vars_df$tool_reliability[selected_vars_df$tool_reliability != "none" &
                                                    !is.na(selected_vars_df$tool_reliability)]))>0) {
  sapply(names(reliability_tools_stats_all), function(x){
    nn <- x
    writexl::write_xlsx(list(reliability = reliability_tools_stats_all[[nn]][["total"]],
                             alpha_ci = data.frame(lower = unname(reliability_tools_stats_all[[nn]][["feldt"]][["lower.ci"]]),
                                                   alpha = unname(reliability_tools_stats_all[[nn]][["feldt"]][["alpha"]]),
                                                   upper = unname(reliability_tools_stats_all[[nn]][["feldt"]][["upper.ci"]])
                                                   ),
                             reliability_alphadrop = tibble::rownames_to_column(reliability_tools_stats_all[[nn]][["alpha.drop"]]),
                             reliability_itemstats = tibble::rownames_to_column(reliability_tools_stats_all[[nn]][["item.stats"]]),
                             reliability_itemfreq = tibble::rownames_to_column(as.data.frame(reliability_tools_stats_all[[nn]][["response.freq"]]))
                             ),
                        path = base::file.path(output_Dir, paste0("reliability_",nn,".xlsx") )
                        )
                         
                       }, simplify = FALSE
    )

} else {
  print(paste0("No reliability analysis done"))
}


if (length(unique(selected_vars_df$tool_reliability_group[selected_vars_df$tool_reliability_group != "none" &
                                                    !is.na(selected_vars_df$tool_reliability_group)]))>0) {
  sapply(names(reliability_tools_stats_group), function(x){
    nn <- x
    writexl::write_xlsx(list(reliability = reliability_tools_stats_group[[nn]][["total"]],
                             alpha_ci = data.frame(lower = unname(reliability_tools_stats_group[[nn]][["feldt"]][["lower.ci"]]),
                                                   alpha = unname(reliability_tools_stats_group[[nn]][["feldt"]][["alpha"]]),
                                                   upper = unname(reliability_tools_stats_group[[nn]][["feldt"]][["upper.ci"]])
                             ),
                             reliability_alphadrop = tibble::rownames_to_column(reliability_tools_stats_group[[nn]][["alpha.drop"]]),
                             reliability_itemstats = tibble::rownames_to_column(reliability_tools_stats_group[[nn]][["item.stats"]]),
                             reliability_itemfreq = tibble::rownames_to_column(as.data.frame(reliability_tools_stats_group[[nn]][["response.freq"]]))
                             ),
                        path = base::file.path(output_Dir, paste0("reliability_group_",nn,".xlsx") )
                        )
    
  }, simplify = FALSE
  )
  
} else {
  print(paste0("No reliability group analysis done"))
}

