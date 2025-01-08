library(psych)
library(dplyr)
library(tidyr)
library(rstatix)

working_directory

## Reliability Analysis 

#reliability_report <- paste0("raw_alpha: Cronbach’s $\alpha$ (values ≥ .7 or .8 indicate good reliability; Kline (1999))")

reliability_tools_stats_all <- if (length(unique(selected_vars_df$tool_reliability[selected_vars_df$tool_reliability != "none" &
                                                                             !is.na(selected_vars_df$tool_reliability)]))>0) {
  sapply(unique(selected_vars_df$tool_reliability[selected_vars_df$tool_reliability != "none" &
                                                                           !is.na(selected_vars_df$tool_reliability)]),
                                function(x){
                                  nn <- x
                                  index <- selected_vars_df$new_variable[selected_vars_df$tool_reliability == nn 
                                                                             & !is.na(selected_vars_df$tool_reliability)]
                                  n_index <- length(index)
                                  
                                  reliability_rescale <- selected_vars_df$tool_reliability[selected_vars_df$tool_reliability_scale == "rescale" &
                                                                                             !is.na(selected_vars_df$tool_reliability_scale)]
                                  
                                  reliability <- if (nn %in% reliability_rescale) {
                                    psych::alpha(df_analysis %>%
                                                   dplyr::select(any_of(index)) %>%
                                                   dplyr::mutate(across(c(any_of(index)), ~as.numeric(.x)-1)
                                                                 )
                                                 , check.keys = FALSE
                                                 )
                                  } else {
                                    psych::alpha(df_analysis %>%
                                                   dplyr::select(any_of(index)) %>%
                                                   dplyr::mutate(across(c(any_of(index)),  ~as.numeric(.x))
                                                                 )
                                                 , check.keys = FALSE
                                                 )
                                      
                                    }
                                  
                                }, simplify = FALSE
         )
  
  } else {
  print(paste0("No reliability analysis done"))
  }

reliability_tools_stats_group <- if (length(unique(selected_vars_df$tool_reliability_group[selected_vars_df$tool_reliability_group != "none" &
                                                                                     !is.na(selected_vars_df$tool_reliability_group)]))>0) {
  sapply(unique(selected_vars_df$tool_reliability_group[selected_vars_df$tool_reliability_group != "none" &
                                                                           !is.na(selected_vars_df$tool_reliability_group)]),
                                function(x){
                                  nn <- x
                                  index <- selected_vars_df$new_variable[selected_vars_df$tool_reliability_group == nn 
                                                                         & !is.na(selected_vars_df$tool_reliability_group)]
                                  n_index <- length(index)
                                  
                                  reliability_rescale <- unique(selected_vars_df$tool_reliability_group[selected_vars_df$tool_reliability_group_scale == "rescale" &
                                                                                                    !is.na(selected_vars_df$tool_reliability_group_scale)])
                                  
                                  reliability <- if (nn %in% reliability_rescale) {
                                    psych::alpha(df_analysis %>%
                                                   dplyr::select(any_of(index)) %>%
                                                   dplyr::mutate(across(c(any_of(index)), ~as.numeric(.x)-1)
                                                   )
                                                 , check.keys = FALSE
                                    )
                                  } else {
                                    psych::alpha(df_analysis %>%
                                                   dplyr::select(any_of(index)) %>%
                                                   dplyr::mutate(across(c(any_of(index)),  ~as.numeric(.x))
                                                   )
                                                 , check.keys = FALSE
                                    )
                                    
                                  }
                                  
                                }, simplify = FALSE
         )
  
  } else {
    print(paste0("No reliability group analysis done"))
    }
  
## Correlation Analysis 

correlation_tools_stats <- if (length(unique(selected_vars_df$tool_correlation[selected_vars_df$tool_correlation != "none" &
                                                                                     !is.na(selected_vars_df$tool_correlation)]))>0) {
  sapply(unique(selected_vars_df$tool_correlation[selected_vars_df$tool_correlation != "none" &
                                                          !is.na(selected_vars_df$tool_correlation)]),
         function(x){
           nn <- x
           index <- selected_vars_df$new_variable[selected_vars_df$tool_correlation == nn 
                                                  & !is.na(selected_vars_df$tool_correlation)]
           
           correlation <- df_analysis %>%
             dplyr::select(any_of(index)) %>%
             rstatix::cor_test(
               vars = NULL,
               vars2 = NULL
               ) %>%
             left_join(final_attribute %>%
                         dplyr::select(variable, label),
                       by = c("var2" = "variable")
                       )
        
         }, simplify = FALSE
         
         )
  
} else {
  print(paste0("No tool correlation analysis done"))
}


