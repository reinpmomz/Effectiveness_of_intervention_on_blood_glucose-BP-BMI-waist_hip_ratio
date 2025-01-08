library(dplyr)
library(gtsummary)
library(flextable)
library(labelled)
library(sjlabelled)

working_directory

### continous summary tables
continous_table <- 
  function(df, foot_note = "", caption = "", continous_vars, sum=FALSE, display_mean = FALSE, p_value = FALSE,
           continous_digits = 1, include, flex_table = TRUE) {
  df <- (df %>%
           mutate(across(where(is.character), sjlabelled::as_factor))
         )
  
  summ1 <- if (sum == FALSE) {
    summ <- if (display_mean == TRUE) {
    gtsummary::tbl_continuous(df
                              ,variable = any_of(continous_vars)
                              ,include = any_of(include)
                              ,statistic = everything() ~ c(
                                "{mean} ({sd})")
                              ,digits = list(everything() ~ continous_digits
                                             )
                              ) 
    } else {
      gtsummary::tbl_continuous(df
                                ,variable = any_of(continous_vars)
                                ,include = any_of(include)
                                ,statistic = everything() ~ c(
                                  "{mean} ({sd}) / {median} ({p25}, {p75})")
                                ,digits = list(everything() ~ continous_digits
                                               )
                                )
    }
    summ
  } else {
    summ <- if (display_mean == TRUE) {
      gtsummary::tbl_continuous(df
                                , variable = any_of(continous_vars)
                                , include = any_of(include)
                                , statistic = everything() ~ c(
                                  "{sum} / {mean} ({sd})")
                                , digits = list(everything() ~ continous_digits
                                                )
                                )
    } else {
      gtsummary::tbl_continuous(df
                                , variable = any_of(continous_vars)
                                , include = any_of(include)
                                , statistic = everything() ~ c(
                                  "{sum}")
                                , digits = list(everything() ~ continous_digits
                                                )
                                )
    }
    summ
    }
  
  summ2 <- if (p_value == TRUE) {
    summ1 %>%
      add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
      bold_p(t= 0.05) # bold p-values under a given threshold (default 0.05) 
    
  } else { summ1 }
  
  summ3 <- summ2 %>% 
    modify_header(label = "**Variables**"# update the column header
    ) %>% 
    bold_labels() %>%
    italicize_levels() %>% 
    modify_footnote(all_stat_cols() ~ foot_note) %>%
    modify_caption(caption)
  
  summ4 <- if (flex_table == TRUE) { 
    summ3 %>%
      gtsummary::as_flex_table() 
    # as_kable_extra() covert gtsummary object to knitrkable object. 
    #as_flex_table() maintains identation, footnotes, spanning headers
  } else {
    summ3
  }
  
  summ4
  
}

### continous by summary tables
continous_by_table <- 
  function(df, foot_note = "", caption = "", continous_vars, by_vars, sum=FALSE, display_mean = FALSE,
           p_value = FALSE, continous_digits = 1, include, flex_table = TRUE) {
    df <- (df %>%
             mutate(across(where(is.character), sjlabelled::as_factor))
           )
    
    out <- lapply(by_vars, function(x){
      summ1 <- if (sum == FALSE) {
        summ <- if (display_mean == TRUE) {
          gtsummary::tbl_continuous(df
                                    ,variable = any_of(continous_vars)
                                    , by = any_of(x)
                                    ,include = any_of(include)
                                    ,statistic = everything() ~ c(
                                      "{mean} ({sd})")
                                    ,digits = list(everything() ~ continous_digits
                                                   )
                                    ) 
          } else {
          gtsummary::tbl_continuous(df
                                    ,variable = any_of(continous_vars)
                                    , by = any_of(x)
                                    ,include = any_of(include)
                                    ,statistic = everything() ~ c(
                                      "{mean} ({sd}) / {median} ({p25}, {p75})")
                                    ,digits = list(everything() ~ continous_digits
                                                   )
                                    )
            }
        summ
        } else {
        summ <- if (display_mean == TRUE) {
          gtsummary::tbl_continuous(df
                                    , variable = any_of(continous_vars)
                                    , by = any_of(x)
                                    , include = any_of(include)
                                    , statistic = everything() ~ c(
                                      "{sum} / {mean} ({sd})")
                                    , digits = list(everything() ~ continous_digits
                                                    )
                                    )  
          } else {
          gtsummary::tbl_continuous(df
                                    , variable = any_of(continous_vars)
                                    , by = any_of(x)
                                    , include = any_of(include)
                                    , statistic = everything() ~ c(
                                      "{sum}")
                                    , digits = list(everything() ~ continous_digits
                                                    )
                                    ) 
            }
        summ
        }
    
      summ2 <- if (p_value == TRUE) {
        summ1 %>%
        add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
        bold_p(t= 0.05) # bold p-values under a given threshold (default 0.05) 
        
        } else { summ1 }
    
      summ3 <- summ2 %>% 
      modify_header(label = "**Variables**" # update the column header
      ) %>% 
      bold_labels() %>%
      italicize_levels() %>% 
      modify_footnote(all_stat_cols() ~ foot_note) %>%
      modify_caption(caption)
      
      summ4 <- if (flex_table == TRUE) { 
        summ3 %>%
          gtsummary::as_flex_table() 
        # as_kable_extra() covert gtsummary object to knitrkable object. 
        #as_flex_table() maintains identation, footnotes, spanning headers
      } else {
        summ3
      }
    
      summ4
      
    })
    out
    
  }

### continous strata summary tables
continous_strata_table <- 
  function(df, foot_note = "", caption = "", strata_vars, continous_vars, by_vars, display_mean = FALSE,
           sum=FALSE, p_value = FALSE, continous_digits = 1, include, flex_table = TRUE) {
    df <- (df %>%
             mutate(across(where(is.character), sjlabelled::as_factor))
           )
    
    out <- lapply(by_vars, function(y){
      summ2 <- if (p_value == FALSE) {
        summ1 <- if (sum == FALSE) {
          summ <- if (display_mean == TRUE) {
          tbl_strata(df,
                   strata = any_of(strata_vars),
                   .tbl_fun = ~ .x %>%
                     gtsummary::tbl_continuous(
                                   variable = any_of(continous_vars)
                                  , by = any_of(y)
                                  , include = any_of(include)
                                  , statistic = everything() ~ c(
                                    "{mean} ({sd})")
                                  , digits = list(everything() ~ continous_digits
                                                  )
                                  ) %>% 
                     modify_header(label = "**Variables**" # update the column header
                     ) %>% 
                     bold_labels() %>%
                     italicize_levels() %>% 
                     modify_footnote(all_stat_cols() ~ foot_note) %>%
                     modify_caption(caption)
                   , .header = paste(paste0("**", strata_vars, "**"), "**{strata}**, N = {n}")
                   ) 
          } else {
            tbl_strata(df,
                       strata = any_of(strata_vars),
                       .tbl_fun = ~ .x %>%
                         gtsummary::tbl_continuous(
                           variable = any_of(continous_vars)
                           , by = any_of(y)
                           , include = any_of(include)
                           , statistic = everything() ~ c(
                             "{mean} ({sd}) / {median} ({p25}, {p75})")
                           , digits = list(everything() ~ continous_digits
                           )
                         ) %>% 
                         modify_header(label = "**Variables**" # update the column header
                         ) %>% 
                         bold_labels() %>%
                         italicize_levels() %>% 
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption)
                       , .header = paste(paste0("**", strata_vars, "**"), "**{strata}**, N = {n}")
            )
          }
          summ
          } else { 
            summ <- if (display_mean == TRUE) {
              tbl_strata(df,
                         strata = any_of(strata_vars),
                         .tbl_fun = ~ .x %>%
                           gtsummary::tbl_continuous(
                             variable = any_of(continous_vars)
                             , by = any_of(y)
                             , include = any_of(include)
                             , statistic = everything() ~ c(
                               "{sum} / {mean} ({sd})")
                             , digits = list(everything() ~ continous_digits
                             )
                           ) %>% 
                           modify_header(label = "**Variables**"# update the column header
                           ) %>% 
                           bold_labels() %>%
                           italicize_levels() %>% 
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption) 
                         , .header = paste(paste0("**", strata_vars, "**"), "**{strata}**, N = {n}")
                         ) 
            } else {
              tbl_strata(df,
                         strata = any_of(strata_vars),
                         .tbl_fun = ~ .x %>%
                           gtsummary::tbl_continuous(
                             variable = any_of(continous_vars)
                             , by = any_of(y)
                             , include = any_of(include)
                             , statistic = everything() ~ c(
                               "{sum}")
                             , digits = list(everything() ~ continous_digits
                             )
                           ) %>% 
                           modify_header(label = "**Variables**"# update the column header
                           ) %>% 
                           bold_labels() %>%
                           italicize_levels() %>% 
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption) 
                         , .header = paste(paste0("**", strata_vars, "**"), "**{strata}**, N = {n}")
                         )
            } 
            } 
        summ1
        
        } else {
        summ1 <- if (sum == FALSE) {
          summ <- if (display_mean == TRUE) {
            tbl_strata(df,
                       strata = any_of(strata_vars),
                       .tbl_fun = ~ .x %>%
                         gtsummary::tbl_continuous(
                           variable = any_of(continous_vars)
                           , by = any_of(y)
                           , include = any_of(include)
                           , statistic = everything() ~ c(
                             "{mean} ({sd})")
                           , digits = list(everything() ~ continous_digits
                           )
                         ) %>%  
                         modify_header(label = "**Variables**" # update the column header
                         ) %>% 
                         bold_labels() %>%
                         italicize_levels() %>% 
                         add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
                         bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05) 
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption)
                       , .header = paste(paste0("**", strata_vars, "**"), "**{strata}**, N = {n}")
                       )
          } else {
            tbl_strata(df,
                       strata = any_of(strata_vars),
                       .tbl_fun = ~ .x %>%
                         gtsummary::tbl_continuous(
                           variable = any_of(continous_vars)
                           , by = any_of(y)
                           , include = any_of(include)
                           , statistic = everything() ~ c(
                             "{mean} ({sd}) / {median} ({p25}, {p75})")
                           , digits = list(everything() ~ continous_digits
                           )
                         ) %>%  
                         modify_header(label = "**Variables**" # update the column header
                         ) %>% 
                         bold_labels() %>%
                         italicize_levels() %>% 
                         add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
                         bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05) 
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption)
                       , .header = paste(paste0("**", strata_vars, "**"), "**{strata}**, N = {n}")
                       )
          }
          
          summ
        } else {
          summ <- if (display_mean == TRUE) {
            tbl_strata(df,
                       strata = any_of(strata_vars),
                       .tbl_fun = ~ .x %>%
                         gtsummary::tbl_continuous(
                           variable = any_of(continous_vars)
                           , by = any_of(y)
                           , include = any_of(include)
                           , statistic = everything() ~ c(
                             "{sum} / {mean} ({sd})")
                           , digits = list(everything() ~ continous_digits
                           )
                         ) %>% 
                         modify_header(label = "**Variables**" # update the column header
                         ) %>% 
                         bold_labels() %>%
                         italicize_levels() %>%
                         add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
                         bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05) 
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption)
                       , .header = paste(paste0("**", strata_vars, "**"), "**{strata}**, N = {n}")
                       ) 
            } else {
              tbl_strata(df,
                         strata = any_of(strata_vars),
                         .tbl_fun = ~ .x %>%
                           gtsummary::tbl_continuous(
                             variable = any_of(continous_vars)
                             , by = any_of(y)
                             , include = any_of(include)
                             , statistic = everything() ~ c(
                               "{sum}")
                             , digits = list(everything() ~ continous_digits
                             )
                           ) %>% 
                           modify_header(label = "**Variables**" # update the column header
                           ) %>% 
                           bold_labels() %>%
                           italicize_levels() %>%
                           add_p(pvalue_fun = ~style_pvalue(.x, digits = 3)) %>%
                           bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05) 
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption)
                         , .header = paste(paste0("**", strata_vars, "**"), "**{strata}**, N = {n}")
                         ) 
            }
          summ
        }
        summ1
        
      }
      
      summ3 <- if (flex_table == TRUE) { 
        summ2 %>%
          gtsummary::as_flex_table() 
        # as_kable_extra() covert gtsummary object to knitrkable object. 
        #as_flex_table() maintains identation, footnotes, spanning headers
      } else {
        summ2
      }
      
      summ3
      
    })
    out
    
  }

