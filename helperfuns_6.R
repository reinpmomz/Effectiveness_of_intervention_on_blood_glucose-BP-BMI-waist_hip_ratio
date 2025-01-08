library(dplyr)
library(stringr)
library(tidyselect)
library(tibble)

working_directory

#Coalesce Multiple Columns in svenhalvorson/SvenR

coalesce_multi = function(
    df,
    pattern = stringr::fixed('.'),
    noisy = TRUE
){
  
  stopifnot(
    "`df` must be a data.frame" = checkmate::test_data_frame(df),
    "pattern must be a character" = checkmate::test_character(pattern)
  )
  
  # first figure out what columns should be coalesced:
  colname_df = stringr::str_split_fixed(
    string = colnames(df),
    pattern = pattern,
    n = 2
  )
  
  colnames(colname_df) = c('prefix', 'suffix')
  
  colname_df = colname_df |>
    tibble::as_tibble() |>
    dplyr::mutate(
      colname = colnames(df)
    ) |>
    dplyr::group_by(prefix) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()
  
  if(nrow(colname_df) == 0){
    warning('No columns coalesced by coalesce_multi')
    return(df)
  } else{
    
    for(column_prefix in unique(colname_df[['prefix']])){
      
      sub_df = dplyr::filter(colname_df, prefix == column_prefix)
      
      if(noisy){
        message(
          paste0(
            'Coalescing c(',
            paste(sub_df[['colname']], collapse = ', '),
            ') into ',
            column_prefix,
            '\n\n'
          )
        )
      }
      
      new_col = df |>
        dplyr::select(
          tidyselect::all_of(sub_df[['colname']])
        ) |>
        as.list()
      
      drop_cols = setdiff(sub_df[['colname']], column_prefix)
      
      df[[column_prefix]] = dplyr::coalesce(!!!new_col)
      df = dplyr::select(df, -tidyselect::all_of(drop_cols))
      
    }
    
  }
  
  df
  
}

