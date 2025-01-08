library(dplyr)
library(readxl)
library(gtsummary)
library(flextable)
library(labelled)
library(sjlabelled)

working_directory

## read all sheets in excel file
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename) #List all sheets in an excel spreadsheet
  out <- base::lapply(sheets, function(x) {
    readxl::read_excel(filename, 
                       sheet = x, 
                       col_types = NULL, 
                       na = "", 
                       trim_ws = TRUE
                       )
  }
  ) 
  base::names(out) <- sheets
  out
}

## Setting gt summary theme
my_gtsummary_theme <- gtsummary::set_gtsummary_theme(
  list(
    ## round large p-values to three places
    #"pkgwide-fn:pvalue_fun" = function(x) gtsummary::style_pvalue(x, digits = 3),
    ## report mean (sd) and n (percent) as default stats in `tbl_summary()`
    #"tbl_summary-fn:percent_fun" = function(x) gtsummary::style_percent(x, digits = 1), 
    ## less than 10% are rounded to digits + 1 places
    #"tbl_summary-str:continuous_stat" = "{mean} ({sd})",
    "style_number-arg:big.mark" = ""
    #"tbl_summary-str:categorical_stat" = "{n} ({p}%)" #"{n} / {N} ({p}%)"
  )
)

### Setting `Compact` theme
gtsummary_compact_theme <- gtsummary::theme_gtsummary_compact()

## descriptive summary tables
descriptive_table <- 
  function(df, foot_note = "", caption = "", ci=FALSE, include, mean_vars = NULL, sum_vars = NULL,
           flex_table = TRUE, categorical_proportion_digits = 1, continous_digits = 1,
           statistic_categorical = "{n} ({p}%)") {
    df <- (df %>%
             mutate(across(where(is.character), sjlabelled::as_factor))
           )
    summ <- if (is.null(mean_vars) & is.null(sum_vars)) {
      gtsummary::tbl_summary(df
                             , include = any_of(include)
                             , type = list(
                               all_dichotomous() ~ "categorical"
                               ,all_continuous() ~ "continuous2"
                             )
                             , statistic = list(
                               all_continuous(continuous2 = TRUE) ~ c(
                               "{mean} ({sd})",
                               "{median} ({p25}, {p75})",
                               "{min}, {max}" ),
                               all_categorical() ~ statistic_categorical
                               )
                             , digits = list(all_continuous(continuous2 = TRUE) ~ continous_digits, 
                                             all_categorical() ~ c(0, categorical_proportion_digits)
                             )
                             , percent = "column" #"column", "row", or "cell"
                             , missing = "ifany" #list missing data separately #ifany #no #always
                             , missing_text = "Missing"
      ) 
    } else {
      gtsummary::tbl_summary(df
                             , include = any_of(include)
                             , type = list(
                               any_of(mean_vars) ~ "continuous2"
                               ,any_of(sum_vars) ~ "continuous"
                               ,all_dichotomous() ~ "categorical"
                               ,c(all_continuous(), -any_of(sum_vars)) ~ "continuous2" 
                             )
                             , statistic = list(
                               any_of(sum_vars) ~ "{sum}",
                               all_continuous2() ~ c(
                                 "{mean} ({sd})",
                                 "{median} ({p25}, {p75})",
                                 "{min}, {max}" ),
                               all_categorical() ~ statistic_categorical
                               )
                             , digits = list(any_of(sum_vars) ~ continous_digits, 
                                             all_continuous2() ~ continous_digits, 
                                             all_categorical() ~ c(0, categorical_proportion_digits)
                             )
                             , percent = "column" #"column", "row", or "cell"
                             , missing = "ifany" #list missing data separately #ifany #no #always
                             , missing_text = "Missing"
      )
    }
    
    summ1 <- if (ci == TRUE) {
      summ %>%
        add_ci(conf.level = 0.95, # add columns with confidence interval
               statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
                                all_continuous() ~ "{conf.low} - {conf.high}"
               ),
               style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
                                all_continuous() ~ style_sigfig
               )
        )
    } else { summ }
    
    summ2 <- summ1 %>% 
      modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n}"
                    # update the column header
      ) %>% 
      bold_labels() %>%
      italicize_levels() %>% 
      add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
             # add column with total number of non-missing observations 
      ) %>% 
      modify_footnote(all_stat_cols() ~ foot_note) %>%
      modify_caption(caption)
    
    summ3 <- if (flex_table == TRUE) { 
      summ2 %>%
        gtsummary::as_flex_table() 
      # as_kable_extra() covert gtsummary object to knitrkable object. 
      #as_flex_table() maintains identation, footnotes, spanning headers
    } else {
      summ2
      }
    
    summ3
    
  }

## inferential summary tables
inferential_table <- 
  function(df, foot_note = "", caption = "", by_vars , percent = "column", include , ci=FALSE, 
           mean_vars = NULL, sum_vars = NULL, overall = FALSE, p_value = TRUE, flex_table = TRUE,
           categorical_proportion_digits = 1, continous_digits = 1, statistic_categorical = "{n} ({p}%)") {
    out <- lapply(by_vars, function(x){
      df <- (df %>%
               mutate(across(where(is.character), sjlabelled::as_factor))
      )
      df <- df[!is.na(df[[x]]),]
      index_by <- df[[x]]
      levels_by <- if (class(index_by) == "factor") {nlevels(index_by)
      } else {
        length(unique(index_by))
      }
      label_by <- if (is.null(labelled::var_label(index_by))) {x
      } else {
        labelled::var_label(index_by)
      }
      
      summ <- if (is.null(mean_vars) & is.null(sum_vars)) {
        gtsummary::tbl_summary(df
                               , by = any_of(x)
                               , include = any_of(include)
                               , type = list(
                                 all_dichotomous() ~ "categorical"
                                 ,all_continuous() ~ "continuous2"
                                 )
                               , statistic = list(
                                 all_continuous(continuous2 = TRUE) ~ c(
                                 "{mean} ({sd})",
                                 "{median} ({p25}, {p75})",
                                 "{min}, {max}" ),
                                 all_categorical() ~ statistic_categorical
                                 )
                               , digits = list(all_continuous(continuous2 = TRUE) ~ continous_digits,
                                               all_categorical() ~ c(0, categorical_proportion_digits))
                               , percent = percent #"column", "row", or "cell"
                               , missing = "ifany" #list missing data separately #ifany #no #always
                               ,missing_text = "Missing"
        ) 
      } else {
        gtsummary::tbl_summary(df
                               , by = any_of(x)
                               , include = any_of(include)
                               , type = list(
                                 any_of(mean_vars) ~ "continuous2"
                                 ,any_of(sum_vars) ~ "continuous"
                                 ,all_dichotomous() ~ "categorical"
                                 ,c(all_continuous(), -any_of(sum_vars)) ~ "continuous2"
                                 )
                               , statistic = list(any_of(sum_vars) ~ "{sum}",
                                                  all_continuous2() ~ c(
                                                    "{mean} ({sd})",
                                                    "{median} ({p25}, {p75})",
                                                    "{min}, {max}" ),
                                                  all_categorical() ~ statistic_categorical
                                                  )
                               , digits = list(any_of(sum_vars) ~ continous_digits, 
                                               all_continuous2() ~ continous_digits, 
                                               all_categorical() ~ c(0, categorical_proportion_digits)
                               )
                               , percent = percent #"column", "row", or "cell"
                               , missing = "ifany" #list missing data separately #ifany #no #always
                               , missing_text = "Missing"
        )
      }
      
      summ1 <- if (ci == TRUE) {
        summ %>%
          add_ci(method = list(all_categorical() ~ "wilson", all_continuous() ~ "t.test"),
                 conf.level = 0.95, # add columns with confidence interval
                 statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
                                  all_continuous() ~ "{conf.low} - {conf.high}"
                 ),
                 style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
                                  all_continuous() ~ style_sigfig
                 ),
                 pattern = NULL #"{stat} / ({ci})"
          ) 
      } else { 
        summ 
      }
      
      summ2 <- if (p_value == TRUE) {
        summ1 %>% 
          add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
                test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
          ) %>%
          bold_p(t= 0.05)
      } else { summ1 }
      
      summ3 <- if (overall == TRUE) {
        summ2 %>% 
          add_overall()
      } else { summ2 }
      
      summ4 <- summ3 %>% 
        modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n}"
                      # update the column header
        ) %>% 
        bold_labels() %>%
        italicize_levels() %>%
        add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
               # add column with total number of non-missing observations
        ) %>% 
        modify_spanning_header(paste0("stat_", seq(levels_by)) ~ paste0("**", label_by, "**")
                               #all_stat_cols() ~ paste0("**", label_by, "**")
        ) %>% 
        modify_footnote(all_stat_cols() ~ foot_note) %>%
        modify_caption(caption)
      
      summ5 <- if (flex_table == TRUE) { 
        summ4 %>%
          gtsummary::as_flex_table() 
        # as_kable_extra() covert gtsummary object to knitrkable object. 
        #as_flex_table() maintains identation, footnotes, spanning headers
      } else {
        summ4
      }
      
      summ5
      
    })
    out
  }

## inferential strata summary tables
inferential_strata_table <- 
  function(df, foot_note = "", caption = "", strata_vars, by_vars, percent = "column", include, ci=FALSE,
           p_value = TRUE, mean_vars = NULL, sum_vars = NULL, flex_table = TRUE, categorical_proportion_digits = 1,
           continous_digits = 1, statistic_categorical = "{n} ({p}%)") {
    out <- lapply( by_vars, function(y){
      df <- (df %>%
               mutate(across(where(is.character), sjlabelled::as_factor))
      )
      df <- df[!is.na(df[[y]]) & !is.na(df[[strata_vars]]),]
      index_strata <- df[[strata_vars]]
      label_strata <- if (is.null(labelled::var_label(index_strata))) {strata_vars
      } else {
        labelled::var_label(index_strata)
      }
      index_by <- df[[y]]
      levels_by <- if (class(index_by) == "factor") {nlevels(index_by)
      } else {
        length(unique(index_by))
      }
      label_by <- if (is.null(labelled::var_label(index_by))) {y
      } else {
        labelled::var_label(index_by)
      }
      
      summ2 <- if (is.null(mean_vars) & is.null(sum_vars)) {
        summ1 <- if (ci == TRUE) {
          summ <- if (p_value == TRUE) {
            tbl_strata(df,
                       strata = any_of(strata_vars),
                       .tbl_fun = ~ .x %>% 
                         gtsummary::tbl_summary(  by = any_of(y)
                                                  , include = any_of(include)
                                                  , type = list(
                                                    all_dichotomous() ~ "categorical"
                                                    ,all_continuous() ~ "continuous2"
                                                  )
                                                  , statistic = list(
                                                    all_continuous(continuous2 = TRUE) ~ c(
                                                    "{mean} ({sd})",
                                                    "{median} ({p25}, {p75})",
                                                    "{min}, {max}" ),
                                                    all_categorical() ~ statistic_categorical
                                                    )
                                                  , digits = list(all_continuous(continuous2 = TRUE) ~ continous_digits,
                                                                  all_categorical() ~ c(0, categorical_proportion_digits)
                                                  )
                                                  , percent = percent #"column", "row", or "cell"
                                                  , missing = "ifany" #list missing data separately #ifany #no #always
                                                  ,missing_text = "Missing"
                         ) %>% 
                         modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n}"
                                       # update the column header
                         ) %>%
                         bold_labels() %>%
                         italicize_levels() %>%
                         add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
                                # add column with total number of non-missing observations
                         ) %>%
                         add_ci(method = list(all_categorical() ~ "wilson", all_continuous() ~ "t.test"),
                                conf.level = 0.95, # add columns with confidence interval
                                statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
                                                 all_continuous() ~ "{conf.low} - {conf.high}"
                                ),
                                style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
                                                 all_continuous() ~ style_sigfig
                                ),
                                pattern = NULL #"{stat} / ({ci})"
                         ) %>% 
                         add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
                               test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
                         ) %>%
                         bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
                         modify_spanning_header(paste0("stat_", seq(levels_by)) ~ paste0("**", label_by, "**")
                                                #all_stat_cols() ~ paste0("**", label, "**")
                         ) %>% 
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption)
                       , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                       )
          } else {
            tbl_strata(df,
                       strata = any_of(strata_vars),
                       .tbl_fun = ~ .x %>% 
                         gtsummary::tbl_summary(  by = any_of(y)
                                                  , include = any_of(include)
                                                  , type = list(
                                                    all_dichotomous() ~ "categorical"
                                                    ,all_continuous() ~ "continuous2"
                                                  )
                                                  , statistic = list(
                                                    all_continuous(continuous2 = TRUE) ~ c(
                                                    "{mean} ({sd})",
                                                    "{median} ({p25}, {p75})",
                                                    "{min}, {max}" ),
                                                    all_categorical() ~ statistic_categorical
                                                  )
                                                  , digits = list(all_continuous(continuous2 = TRUE) ~ continous_digits,
                                                                  all_categorical() ~ c(0, categorical_proportion_digits)
                                                  )
                                                  , percent = percent #"column", "row", or "cell"
                                                  , missing = "ifany" #list missing data separately #ifany #no #always
                                                  ,missing_text = "Missing"
                         ) %>% 
                         modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n}"
                                       # update the column header
                         ) %>%
                         bold_labels() %>%
                         italicize_levels() %>%
                         add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
                                # add column with total number of non-missing observations
                         ) %>%
                         add_ci(method = list(all_categorical() ~ "wilson", all_continuous() ~ "t.test"),
                                conf.level = 0.95, # add columns with confidence interval
                                statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
                                                 all_continuous() ~ "{conf.low} - {conf.high}"
                                ),
                                style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
                                                 all_continuous() ~ style_sigfig
                                ),
                                pattern = NULL #"{stat} / ({ci})"
                         ) %>% 
                         modify_spanning_header(paste0("stat_", seq(levels_by)) ~ paste0("**", label_by, "**")
                                                #all_stat_cols() ~ paste0("**", label, "**")
                         ) %>% 
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption)
                       , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                       )
          }
          summ
        } else {
          summ <- if (p_value == TRUE) {
          tbl_strata(df,
                     strata = any_of(strata_vars),
                     .tbl_fun = ~ .x %>% 
                       gtsummary::tbl_summary(  by = any_of(y)
                                                , include = any_of(include)
                                                , type = list(
                                                  all_dichotomous() ~ "categorical"
                                                  ,all_continuous() ~ "continuous2"
                                                )
                                                , statistic = list(
                                                  all_continuous(continuous2 = TRUE) ~ c(
                                                  "{mean} ({sd})",
                                                  "{median} ({p25}, {p75})",
                                                  "{min}, {max}" ),
                                                  all_categorical() ~ statistic_categorical
                                                )
                                                , digits = list(all_continuous(continuous2 = TRUE) ~ continous_digits,
                                                                all_categorical() ~ c(0, categorical_proportion_digits)
                                                )
                                                , percent = percent #"column", "row", or "cell"
                                                , missing = "ifany" #list missing data separately #ifany #no #always
                                                ,missing_text = "Missing"
                       ) %>% 
                       modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n}"
                                     # update the column header
                       ) %>%
                       bold_labels() %>%
                       italicize_levels() %>%
                       add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
                              # add column with total number of non-missing observations
                       ) %>% 
                       add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
                             test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
                       ) %>%
                       bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
                       modify_spanning_header(paste0("stat_", seq(levels_by)) ~ paste0("**", label_by, "**")
                                              #all_stat_cols() ~ paste0("**", label, "**")
                       ) %>% 
                       modify_footnote(all_stat_cols() ~ foot_note) %>%
                       modify_caption(caption)
                     , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                     )
          } else {
            tbl_strata(df,
                       strata = any_of(strata_vars),
                       .tbl_fun = ~ .x %>% 
                         gtsummary::tbl_summary(  by = any_of(y)
                                                  , include = any_of(include)
                                                  , type = list(
                                                    all_dichotomous() ~ "categorical"
                                                    ,all_continuous() ~ "continuous2"
                                                  )
                                                  , statistic = list(
                                                    all_continuous(continuous2 = TRUE) ~ c(
                                                    "{mean} ({sd})",
                                                    "{median} ({p25}, {p75})",
                                                    "{min}, {max}" ),
                                                    all_categorical() ~ statistic_categorical
                                                  )
                                                  , digits = list(all_continuous(continuous2 = TRUE) ~ continous_digits,
                                                                  all_categorical() ~ c(0, categorical_proportion_digits)
                                                  )
                                                  , percent = percent #"column", "row", or "cell"
                                                  , missing = "ifany" #list missing data separately #ifany #no #always
                                                  ,missing_text = "Missing"
                         ) %>% 
                         modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n}"
                                       # update the column header
                         ) %>%
                         bold_labels() %>%
                         italicize_levels() %>%
                         add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
                                # add column with total number of non-missing observations
                         ) %>% 
                         modify_spanning_header(paste0("stat_", seq(levels_by)) ~ paste0("**", label_by, "**")
                                                #all_stat_cols() ~ paste0("**", label, "**")
                         ) %>% 
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption)
                       , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                       )
          }
          summ
        }
        summ1
        } else {
          summ1 <- if (ci == TRUE) {
            summ <- if (p_value == TRUE) {
              tbl_strata(df,
                         strata = any_of(strata_vars),
                         .tbl_fun = ~ .x %>% 
                           gtsummary::tbl_summary(  by = any_of(y)
                                                    , include = any_of(include)
                                                    , type = list(
                                                      any_of(mean_vars) ~ "continuous2"
                                                      ,any_of(sum_vars) ~ "continuous"
                                                      ,all_dichotomous() ~ "categorical"
                                                      ,c(all_continuous(), -any_of(sum_vars)) ~ "continuous2"
                                                    )
                                                    , statistic = list(any_of(sum_vars) ~ "{sum}",
                                                                       all_continuous2() ~ c(
                                                                         "{mean} ({sd})",
                                                                         "{median} ({p25}, {p75})",
                                                                         "{min}, {max}" ),
                                                                       all_categorical() ~ statistic_categorical
                                                                       )
                                                    , digits = list(any_of(sum_vars) ~ continous_digits,
                                                                    all_continuous2() ~ continous_digits,
                                                                    all_categorical() ~ c(0, categorical_proportion_digits))
                                                    , percent = percent #"column", "row", or "cell"
                                                    , missing = "ifany" #list missing data separately #ifany #no #always
                                                    ,missing_text = "Missing"
                           ) %>% 
                           modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n}"
                                         # update the column header
                           ) %>%
                           bold_labels() %>%
                           italicize_levels() %>%
                           add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
                                  # add column with total number of non-missing observations
                           ) %>%
                           add_ci(method = list(all_categorical() ~ "wilson", all_continuous() ~ "t.test"),
                                  conf.level = 0.95, # add columns with confidence interval
                                  statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
                                                   all_continuous() ~ "{conf.low} - {conf.high}"
                                  ),
                                  style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
                                                   all_continuous() ~ style_sigfig
                                  ),
                                  pattern = NULL #"{stat} / ({ci})"
                           ) %>% 
                           add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
                                 test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
                           ) %>%
                           bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
                           modify_spanning_header(paste0("stat_", seq(levels_by)) ~ paste0("**", label_by, "**")
                                                  #all_stat_cols() ~ paste0("**", label, "**")
                           ) %>% 
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption)
                         , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                         )
            } else {
              tbl_strata(df,
                         strata = any_of(strata_vars),
                         .tbl_fun = ~ .x %>% 
                           gtsummary::tbl_summary(  by = any_of(y)
                                                    , include = any_of(include)
                                                    , type = list(
                                                      any_of(mean_vars) ~ "continuous2"
                                                      ,any_of(sum_vars) ~ "continuous"
                                                      ,all_dichotomous() ~ "categorical"
                                                      ,c(all_continuous(), -any_of(sum_vars)) ~ "continuous2"
                                                    )
                                                    , statistic = list(any_of(sum_vars) ~ "{sum}",
                                                                       all_continuous2() ~ c(
                                                                         "{mean} ({sd})",
                                                                         "{median} ({p25}, {p75})",
                                                                         "{min}, {max}" ),
                                                                       all_categorical() ~ statistic_categorical
                                                                       )
                                                    , digits = list(any_of(sum_vars) ~ continous_digits,
                                                                    all_continuous2() ~ continous_digits,
                                                                    all_categorical() ~ c(0, categorical_proportion_digits))
                                                    , percent = percent #"column", "row", or "cell"
                                                    , missing = "ifany" #list missing data separately #ifany #no #always
                                                    ,missing_text = "Missing"
                           ) %>% 
                           modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n}"
                                         # update the column header
                           ) %>%
                           bold_labels() %>%
                           italicize_levels() %>%
                           add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
                                  # add column with total number of non-missing observations
                           ) %>%
                           add_ci(method = list(all_categorical() ~ "wilson", all_continuous() ~ "t.test"),
                                  conf.level = 0.95, # add columns with confidence interval
                                  statistic = list(all_categorical() ~ "{conf.low}% - {conf.high}%",
                                                   all_continuous() ~ "{conf.low} - {conf.high}"
                                  ),
                                  style_fun = list(all_categorical() ~ purrr::partial(style_sigfig, scale = 100, digits = 3),
                                                   all_continuous() ~ style_sigfig
                                  ),
                                  pattern = NULL #"{stat} / ({ci})"
                           ) %>% 
                           modify_spanning_header(paste0("stat_", seq(levels_by)) ~ paste0("**", label_by, "**")
                                                  #all_stat_cols() ~ paste0("**", label, "**")
                           ) %>% 
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption)
                         , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                         )
            }
            summ
          } else {
            summ <- if (p_value == TRUE) {
            tbl_strata(df,
                       strata = any_of(strata_vars),
                       .tbl_fun = ~ .x %>% 
                         gtsummary::tbl_summary(  by = any_of(y)
                                                  , include = any_of(include)
                                                  , type = list(
                                                    any_of(mean_vars) ~ "continuous2"
                                                    ,any_of(sum_vars) ~ "continuous"
                                                    ,all_dichotomous() ~ "categorical"
                                                    ,c(all_continuous(), -any_of(sum_vars)) ~ "continuous2"
                                                  )
                                                  , statistic = list(any_of(sum_vars) ~ "{sum}",
                                                                     all_continuous2() ~ c(
                                                                       "{mean} ({sd})",
                                                                       "{median} ({p25}, {p75})",
                                                                       "{min}, {max}" ),
                                                                     all_categorical() ~ statistic_categorical
                                                                     )
                                                  , digits = list(any_of(sum_vars) ~ continous_digits,
                                                                  all_continuous2() ~ continous_digits,
                                                                  all_categorical() ~ c(0, categorical_proportion_digits))
                                                  , percent = percent #"column", "row", or "cell"
                                                  , missing = "ifany" #list missing data separately #ifany #no #always
                                                  ,missing_text = "Missing"
                         ) %>% 
                         modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n}"
                                       # update the column header
                         ) %>%
                         bold_labels() %>%
                         italicize_levels() %>%
                         add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
                                # add column with total number of non-missing observations
                         ) %>%
                         add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),
                               test.args = all_tests("fisher.test") ~ list(simulate.p.value=TRUE)
                         ) %>%
                         bold_p(t= 0.05) %>% # bold p-values under a given threshold (default 0.05)
                         modify_spanning_header(paste0("stat_", seq(levels_by)) ~ paste0("**", label_by, "**")
                                                #all_stat_cols() ~ paste0("**", label, "**")
                         ) %>% 
                         modify_footnote(all_stat_cols() ~ foot_note) %>%
                         modify_caption(caption)
                       , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
                       )
            } else {
              tbl_strata(df,
                         strata = any_of(strata_vars),
                         .tbl_fun = ~ .x %>% 
                           gtsummary::tbl_summary(  by = any_of(y)
                                                    , include = any_of(include)
                                                    , type = list(
                                                      any_of(mean_vars) ~ "continuous2"
                                                      ,any_of(sum_vars) ~ "continuous"
                                                      ,all_dichotomous() ~ "categorical"
                                                      ,c(all_continuous(), -any_of(sum_vars)) ~ "continuous2"
                                                    )
                                                    , statistic = list(any_of(sum_vars) ~ "{sum}",
                                                                       all_continuous2() ~ c(
                                                                         "{mean} ({sd})",
                                                                         "{median} ({p25}, {p75})",
                                                                         "{min}, {max}" ),
                                                                       all_categorical() ~ statistic_categorical
                                                                       )
                                                    , digits = list(any_of(sum_vars) ~ continous_digits,
                                                                    all_continuous2() ~ continous_digits,
                                                                    all_categorical() ~ c(0, categorical_proportion_digits))
                                                    , percent = percent #"column", "row", or "cell"
                                                    , missing = "ifany" #list missing data separately #ifany #no #always
                                                    ,missing_text = "Missing"
                           ) %>% 
                           modify_header(label = "**Variables**", all_stat_cols() ~ "**{level}**\n N = {n}"
                                         # update the column header
                           ) %>%
                           bold_labels() %>%
                           italicize_levels() %>%
                           add_n( statistic = "{N_nonmiss}", col_label = "**n**", last = FALSE, footnote = FALSE
                                  # add column with total number of non-missing observations
                           ) %>% 
                           modify_spanning_header(paste0("stat_", seq(levels_by)) ~ paste0("**", label_by, "**")
                                                  #all_stat_cols() ~ paste0("**", label, "**")
                           ) %>% 
                           modify_footnote(all_stat_cols() ~ foot_note) %>%
                           modify_caption(caption)
                         , .header = paste(paste0("**", label_strata, "**"), "**{strata}**, N = {n}")
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
