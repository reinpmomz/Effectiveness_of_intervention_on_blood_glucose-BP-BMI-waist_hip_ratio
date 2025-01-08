library(dplyr)
library(rstatix)
library(sjlabelled)
library(labelled)
library(data.table)
library(tidyr)
library(tibble)
library(checkmate)

working_directory

## effect size/correlation summary tables
effectsize_corr_table <- function(df, by_vars) {
  
  stopifnot(
    "`df` must be a data.frame" = checkmate::test_data_frame(df),
    "by_vars must be of same class" = checkmate::test_set_equal(length(unique(sapply(df[,by_vars], class))),1
                                                                )
  )
  
  ### dropping variables with one level
  levels_df <- df[ , sapply(lapply(df, unique), length) > 1]
  
  ### dividing factor/character/logical and numeric/integer variables
  factor_character_vars <- names(levels_df[sapply(levels_df,
                                                  function(v) is.factor(v) | is.character(v) | is.logical(v))])
  
  numeric_integer_vars <- names(levels_df[sapply(levels_df, function(v) is.numeric(v) | is.integer(v))])
  
  df_new <- df %>%
    dplyr::select(all_of(c(factor_character_vars, numeric_integer_vars))
                  ) %>%
    dplyr::mutate(across(all_of(factor_character_vars), sjlabelled::as_factor))
  
  attribute <- as.data.frame(labelled::generate_dictionary(df_new, labels = TRUE, values = TRUE)) %>%
    dplyr::select(variable, label)
  
  if (length(factor_character_vars) > 1 && length(numeric_integer_vars) > 1) {
  out <- if (all(by_vars %in% factor_character_vars == "TRUE")) {
    sapply(by_vars[by_vars %in% factor_character_vars], function(x) {
      nn <- x
      
      level_by_vars <- length(unique(df_new[[nn]]))
      
      effect_size1 <- sapply(factor_character_vars[!factor_character_vars %in% nn], function(y) {
        rstatix::cramer_v(df_new[[y]], df_new[[nn]])
        }, simplify = TRUE
        )
      
      effect_size1_df <- data.frame(outcome = nn, effsize = effect_size1, type = "cramer_v"
                                    ,variables = factor_character_vars[!factor_character_vars %in% nn]
                                    )
      
      if (level_by_vars == 2) {
        
        effect_size2 <- sapply(numeric_integer_vars, function(y) {
        df_new %>%
          rstatix::wilcox_effsize(as.formula(paste(y, "~", nn)))
      }, simplify = FALSE
      )
      
      effect_size2_df <- data.table::rbindlist(effect_size2)
      #effect_size2_output <- data.frame(t(effect_size2)) ## When simplify is TRUE
      #effect_size2_df <- effect_size2_output %>% tidyr::unnest() ## When simplify is TRUE
      
      effect_size2_df$outcome <- nn
      effect_size2_df$type <- "wilcoxon"
      effect_size2_df$variables <- numeric_integer_vars
      } else if (level_by_vars > 2) {
        effect_size2 <- sapply(numeric_integer_vars, function(y) {
          df_new %>%
            rstatix::kruskal_effsize(as.formula(paste(y, "~", nn)))
        }, simplify = FALSE
        )
        
        effect_size2_df <- data.table::rbindlist(effect_size2)
        #effect_size2_output <- data.frame(t(effect_size2)) ## When simplify is TRUE
        #effect_size2_df <- effect_size2_output %>% tidyr::unnest() ## When simplify is TRUE
        
        effect_size2_df$outcome <- nn
        effect_size2_df$type <- "kruskal-wallis"
        effect_size2_df$variables <- numeric_integer_vars
        
      }
      
      effect_size_df <- dplyr::bind_rows(effect_size1_df, effect_size2_df) %>%
        tibble::remove_rownames() %>%
        dplyr::left_join(attribute, 
                         by = c("variables" = "variable")
                         )
      
      return(effect_size_df)
      
    }, simplify = FALSE
    )
    
    } else if (all(by_vars %in% numeric_integer_vars == "TRUE")) { 
      
      sapply(by_vars[by_vars %in% numeric_integer_vars], function(x) {
        nn <- x
        
        effect_size3 <- sapply(factor_character_vars, function(y) {
          res.aov <- aov(as.formula(paste(nn, "~", y)), data = df_new)
          rstatix::eta_squared(res.aov)
        }, simplify = FALSE
        )
        
        effect_size3 <- do.call("rbind", effect_size3)
        
        effect_size3_df <- data.frame(outcome = nn, effsize = unname(effect_size3), type = "eta_squared"
                                      ,variables = factor_character_vars
                                      )
        
        effect_size4 <- df_new %>%
          dplyr::select(all_of(numeric_integer_vars)) %>%
          rstatix::cor_test(
            vars = nn,
            vars2 = numeric_integer_vars[!numeric_integer_vars %in% nn]
          ) %>%
          dplyr::rename(outcome = var1, variables = var2)
        
        effect_size4_df <- effect_size4
        effect_size4_df$type <- "correlation"
        
        effect_size_df <- dplyr::bind_rows(effect_size3_df, effect_size4_df) %>%
          tibble::remove_rownames() %>%
          dplyr::left_join(attribute, 
                           by = c("variables" = "variable")
                           )
        
        return(effect_size_df)
        
        }, simplify = FALSE
        )
      
       } else if ( all(by_vars %in% factor_character_vars == "FALSE") && all(by_vars %in% numeric_integer_vars == "FALSE")) { 
        print(paste0("Variable name provided in by_vars doesn't exist in data frame or has no unique elements or ",
                     "or is not of factor/character/logical/numeric/integer type. ", 
                     "Provide a variable name that exists in data frame, has unique elements and is of class ",
                     "factor/character/logical/numeric/integer"))
         }
  
  out
  
  } else if (length(factor_character_vars) > 1 && length(numeric_integer_vars) == 1) {
    out <- if (all(by_vars %in% factor_character_vars == "TRUE")) {
      sapply(by_vars[by_vars %in% factor_character_vars], function(x) {
        nn <- x
        
        level_by_vars <- length(unique(df_new[[nn]]))
        
        effect_size1 <- sapply(factor_character_vars[!factor_character_vars %in% nn], function(y) {
          rstatix::cramer_v(df_new[[y]], df_new[[nn]])
        }, simplify = TRUE
        )
        
        effect_size1_df <- data.frame(outcome = nn, effsize = effect_size1, type = "cramer_v"
                                      ,variables = factor_character_vars[!factor_character_vars %in% nn]
        )
        
        if (level_by_vars == 2) {
          
          effect_size2 <- sapply(numeric_integer_vars, function(y) {
            df_new %>%
              rstatix::wilcox_effsize(as.formula(paste(y, "~", nn)))
          }, simplify = FALSE
          )
          
          effect_size2_df <- data.table::rbindlist(effect_size2)
          #effect_size2_output <- data.frame(t(effect_size2)) ## When simplify is TRUE
          #effect_size2_df <- effect_size2_output %>% tidyr::unnest() ## When simplify is TRUE
          
          effect_size2_df$outcome <- nn
          effect_size2_df$type <- "wilcoxon"
          effect_size2_df$variables <- numeric_integer_vars
        } else if (level_by_vars > 2) {
          effect_size2 <- sapply(numeric_integer_vars, function(y) {
            df_new %>%
              rstatix::kruskal_effsize(as.formula(paste(y, "~", nn)))
          }, simplify = FALSE
          )
          
          effect_size2_df <- data.table::rbindlist(effect_size2)
          #effect_size2_output <- data.frame(t(effect_size2)) ## When simplify is TRUE
          #effect_size2_df <- effect_size2_output %>% tidyr::unnest() ## When simplify is TRUE
          
          effect_size2_df$outcome <- nn
          effect_size2_df$type <- "kruskal-wallis"
          effect_size2_df$variables <- numeric_integer_vars
          
        }
        
        effect_size_df <- dplyr::bind_rows(effect_size1_df, effect_size2_df) %>%
          tibble::remove_rownames() %>%
          dplyr::left_join(attribute, 
                           by = c("variables" = "variable")
          )
        
        return(effect_size_df)
        
      }, simplify = FALSE
      )
      
    } else if (all(by_vars %in% numeric_integer_vars == "TRUE")) { 
      
      sapply(by_vars[by_vars %in% numeric_integer_vars], function(x) {
        nn <- x
        
        effect_size3 <- sapply(factor_character_vars, function(y) {
          res.aov <- aov(as.formula(paste(nn, "~", y)), data = df_new)
          rstatix::eta_squared(res.aov)
        }, simplify = FALSE
        )
        
        effect_size3 <- do.call("rbind", effect_size3)
        
        effect_size3_df <- data.frame(outcome = nn, effsize = unname(effect_size3), type = "eta_squared"
                                      ,variables = factor_character_vars
                                      )
        
        effect_size_df <- effect_size3_df %>%
          tibble::remove_rownames() %>%
          dplyr::left_join(attribute, 
                           by = c("variables" = "variable")
                           )
        
        return(effect_size_df)
        
      }, simplify = FALSE
      )
      
    } else if ( all(by_vars %in% factor_character_vars == "FALSE") && all(by_vars %in% numeric_integer_vars == "FALSE")) { 
      print(paste0("Variable name provided in by_vars doesn't exist in data frame or has no unique elements or ",
                   "or is not of factor/character/logical/numeric/integer type. ", 
                   "Provide a variable name that exists in data frame, has unique elements and is of class ",
                   "factor/character/logical/numeric/integer"))
    }
    
    out
    
    } else if (length(factor_character_vars) == 1 && length(numeric_integer_vars) > 1) {
      out <- if (all(by_vars %in% factor_character_vars == "TRUE")) {
        sapply(by_vars[by_vars %in% factor_character_vars], function(x) {
          nn <- x
          
          level_by_vars <- length(unique(df_new[[nn]]))
          
          if (level_by_vars == 2) {
            
            effect_size2 <- sapply(numeric_integer_vars, function(y) {
              df_new %>%
                rstatix::wilcox_effsize(as.formula(paste(y, "~", nn)))
            }, simplify = FALSE
            )
            
            effect_size2_df <- data.table::rbindlist(effect_size2)
            #effect_size2_output <- data.frame(t(effect_size2)) ## When simplify is TRUE
            #effect_size2_df <- effect_size2_output %>% tidyr::unnest() ## When simplify is TRUE
            
            effect_size2_df$outcome <- nn
            effect_size2_df$type <- "wilcoxon"
            effect_size2_df$variables <- numeric_integer_vars
          } else if (level_by_vars > 2) {
            effect_size2 <- sapply(numeric_integer_vars, function(y) {
              df_new %>%
                rstatix::kruskal_effsize(as.formula(paste(y, "~", nn)))
            }, simplify = FALSE
            )
            
            effect_size2_df <- data.table::rbindlist(effect_size2)
            #effect_size2_output <- data.frame(t(effect_size2)) ## When simplify is TRUE
            #effect_size2_df <- effect_size2_output %>% tidyr::unnest() ## When simplify is TRUE
            
            effect_size2_df$outcome <- nn
            effect_size2_df$type <- "kruskal-wallis"
            effect_size2_df$variables <- numeric_integer_vars
            
          }
          
          effect_size_df <- effect_size2_df %>%
            tibble::remove_rownames() %>%
            dplyr::left_join(attribute, 
                             by = c("variables" = "variable")
                             )
          
          return(effect_size_df)
          
        }, simplify = FALSE
        )
        
      } else if (all(by_vars %in% numeric_integer_vars == "TRUE")) { 
        
        sapply(by_vars[by_vars %in% numeric_integer_vars], function(x) {
          nn <- x
          
          effect_size3 <- sapply(factor_character_vars, function(y) {
            res.aov <- aov(as.formula(paste(nn, "~", y)), data = df_new)
            rstatix::eta_squared(res.aov)
          }, simplify = FALSE
          )
          
          effect_size3 <- do.call("rbind", effect_size3)
          
          effect_size3_df <- data.frame(outcome = nn, effsize = unname(effect_size3), type = "eta_squared"
                                        ,variables = factor_character_vars
                                        )
          
          effect_size4 <- df_new %>%
            dplyr::select(all_of(numeric_integer_vars)) %>%
            rstatix::cor_test(
              vars = nn,
              vars2 = numeric_integer_vars[!numeric_integer_vars %in% nn]
              ) %>%
            dplyr::rename(outcome = var1, variables = var2)
          
          effect_size4_df <- effect_size4
          effect_size4_df$type <- "correlation"
          
          effect_size_df <- dplyr::bind_rows(effect_size3_df, effect_size4_df) %>%
            tibble::remove_rownames() %>%
            dplyr::left_join(attribute, 
                             by = c("variables" = "variable")
                             )
          
          return(effect_size_df)
          
        }, simplify = FALSE
        )
        
      } else if ( all(by_vars %in% factor_character_vars == "FALSE") && all(by_vars %in% numeric_integer_vars == "FALSE")) { 
        print(paste0("Variable name provided in by_vars doesn't exist in data frame or has no unique elements or ",
                     "or is not of factor/character/logical/numeric/integer type. ", 
                     "Provide a variable name that exists in data frame, has unique elements and is of class ",
                     "factor/character/logical/numeric/integer"))
      }
      
      out
      
      } else if (length(factor_character_vars) > 1 && length(numeric_integer_vars) == 0) {
        out <- if (all(by_vars %in% factor_character_vars == "TRUE")) {
          sapply(by_vars[by_vars %in% factor_character_vars], function(x) {
            nn <- x
            
            effect_size1 <- sapply(factor_character_vars[!factor_character_vars %in% nn], function(y) {
              rstatix::cramer_v(df_new[[y]], df_new[[nn]])
            }, simplify = TRUE
            )
            
            effect_size1_df <- data.frame(outcome = nn, effsize = effect_size1, type = "cramer_v"
                                          ,variables = factor_character_vars[!factor_character_vars %in% nn]
            )
            
            effect_size_df <- effect_size1_df %>%
              tibble::remove_rownames() %>%
              dplyr::left_join(attribute, 
                               by = c("variables" = "variable")
              )
            
            return(effect_size_df)
            
          }, simplify = FALSE
          )
          
        } else if (all(by_vars %in% factor_character_vars == "FALSE")) { 
          print(paste0("Variable name provided in by_vars doesn't exist in data frame or has no unique elements or ",
                       "is not of factor/character/logical type. ",
                       "Provide a variable name that exists in data frame, has unique elements and is of class ",
                       "factor/character/logical"))
        }
        
        out
        
        } else if (length(factor_character_vars) == 0 && length(numeric_integer_vars) > 1) {
          out <- if (all(by_vars %in% numeric_integer_vars == "TRUE")) { 
            
            sapply(by_vars[by_vars %in% numeric_integer_vars], function(x) {
              nn <- x
              
              effect_size4 <- df_new %>%
                dplyr::select(all_of(numeric_integer_vars)) %>%
                rstatix::cor_test(
                  vars = nn,
                  vars2 = numeric_integer_vars[!numeric_integer_vars %in% nn]
                ) %>%
                dplyr::rename(outcome = var1, variables = var2)
              
              effect_size4_df <- effect_size4
              effect_size4_df$type <- "correlation"
              
              effect_size_df <- effect_size4_df %>%
                tibble::remove_rownames() %>%
                dplyr::left_join(attribute, 
                                 by = c("variables" = "variable")
                )
              
              return(effect_size_df)
              
            }, simplify = FALSE
            )
            
          } else if (all(by_vars %in% numeric_integer_vars == "FALSE")) { 
            print(paste0("Variable name provided in by_vars doesn't exist in data frame or has no unique elements or ",
                         "is not of numeric/integer type. ",
                         "Provide a variable name that exists in data frame, has unique elements and is of class ",
                         "numeric/integer"))
          }
          
          out
          
          } else if (length(factor_character_vars) == 1 && length(numeric_integer_vars) == 1) {
            out <- if (all(by_vars %in% factor_character_vars == "TRUE")) {
              sapply(by_vars[by_vars %in% factor_character_vars], function(x) {
                nn <- x
                
                level_by_vars <- length(unique(df_new[[nn]]))
                
                if (level_by_vars == 2) {
                  
                  effect_size2 <- sapply(numeric_integer_vars, function(y) {
                    df_new %>%
                      rstatix::wilcox_effsize(as.formula(paste(y, "~", nn)))
                  }, simplify = FALSE
                  )
                  
                  effect_size2_df <- data.table::rbindlist(effect_size2)
                  #effect_size2_output <- data.frame(t(effect_size2)) ## When simplify is TRUE
                  #effect_size2_df <- effect_size2_output %>% tidyr::unnest() ## When simplify is TRUE
                  
                  effect_size2_df$outcome <- nn
                  effect_size2_df$type <- "wilcoxon"
                  effect_size2_df$variables <- numeric_integer_vars
                } else if (level_by_vars > 2) {
                  effect_size2 <- sapply(numeric_integer_vars, function(y) {
                    df_new %>%
                      rstatix::kruskal_effsize(as.formula(paste(y, "~", nn)))
                  }, simplify = FALSE
                  )
                  
                  effect_size2_df <- data.table::rbindlist(effect_size2)
                  #effect_size2_output <- data.frame(t(effect_size2)) ## When simplify is TRUE
                  #effect_size2_df <- effect_size2_output %>% tidyr::unnest() ## When simplify is TRUE
                  
                  effect_size2_df$outcome <- nn
                  effect_size2_df$type <- "kruskal-wallis"
                  effect_size2_df$variables <- numeric_integer_vars
                  
                }
                
                effect_size_df <- effect_size2_df %>%
                  tibble::remove_rownames() %>%
                  dplyr::left_join(attribute, 
                                   by = c("variables" = "variable")
                                   )
                
                return(effect_size_df)
                
              }, simplify = FALSE
              )
              
            } else if (all(by_vars %in% numeric_integer_vars == "TRUE")) { 
              
              sapply(by_vars[by_vars %in% numeric_integer_vars], function(x) {
                nn <- x
                
                effect_size3 <- sapply(factor_character_vars, function(y) {
                  res.aov <- aov(as.formula(paste(nn, "~", y)), data = df_new)
                  rstatix::eta_squared(res.aov)
                }, simplify = FALSE
                )
                
                effect_size3 <- do.call("rbind", effect_size3)
                
                effect_size3_df <- data.frame(outcome = nn, effsize = unname(effect_size3), type = "eta_squared"
                                              ,variables = factor_character_vars
                                              )
                
                effect_size_df <- effect_size3_df %>%
                  tibble::remove_rownames() %>%
                  dplyr::left_join(attribute, 
                                   by = c("variables" = "variable")
                                   )
                
                return(effect_size_df)
                
              }, simplify = FALSE
              )
              
            } else if ( all(by_vars %in% factor_character_vars == "FALSE") && all(by_vars %in% numeric_integer_vars == "FALSE")) { 
              print(paste0("Variable name provided in by_vars doesn't exist in data frame or has no unique elements or ",
                           "or is not of factor/character/logical/numeric/integer type. ", 
                           "Provide a variable name that exists in data frame, has unique elements and is of class ",
                           "factor/character/logical/numeric/integer"))
            }
            
            out
            
            } else if (length(factor_character_vars) == 1 && length(numeric_integer_vars) == 0 ) {
              print(paste0("Data frame with only one factor/character/logical column and no numeric/integer column. ",
                           "Data frame with atleast 2 columns needed for effect size calculation"))
              
              } else if (length(factor_character_vars) == 0 && length(numeric_integer_vars) == 1 ) {
                print(paste0("Data frame with only one numeric/integer column and no factor/character/logical column. ",
                             "Data frame with atleast 2 columns needed for effect size calculation"))

                } else if (length(factor_character_vars) == 0 && length(numeric_integer_vars) == 0 ) {
                  print(c("Data frame is empty. Data frame with atleast 2 columns needed for effect size calculation"))
  
                  }
  
  }

