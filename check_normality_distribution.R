library(dplyr)
library(labelled)
library(fitdistrplus)
library(rstatix)
library(grDevices)
library(writexl)

working_directory

if (length(outcome_vars)>0) {
  
  normality_test <- sapply(outcome_vars, function(x) {
    nn <- x
    df_new <- df_analysis %>%
      tidyr::drop_na(any_of(nn))
    
    label <- labelled::var_label(df_new[[nn]])
    
# Normality of Data - Visual Methods

## discrete is TRUE, represented distributions are the Poisson, negative binomial and normal.
## discrete is FALSE, represented distributions are uniform, normal, logistic, lognormal, beta and gamma

# Save as PNG using png() and dev.off()

    grDevices::png(filename = base::file.path(output_Dir, paste0("distribution_plot_",nn, ".png")),
                   height = 7,
                   width = 10,
                   units = "in",
                   res = 300,
                   bg = "white"
                   )

    distribution_type_visual <- fitdistrplus::descdist(as.numeric(df_new[[nn]]),
                                                       discrete = FALSE, 
                                                       boot = 500, 
                                                       method = "unbiased", #"sample"
                                                       graph = TRUE,
                                                       print = FALSE
                                                       )

# Close the PNG device
    grDevices::dev.off()

# Normality of Data - Normality test
## There are several methods for normality test such as Kolmogorov-Smirnov (K-S) normality test and Shapiro-Wilk’s test.

## Shapiro-Wilk’s method is widely recommended for normality test and it provides better power than K-S. 
## It is based on the correlation between the data and the corresponding normal scores.

    normality_test <- dplyr::bind_rows(
      df_new %>%
        rstatix::shapiro_test(vars = nn) %>%
        dplyr::mutate(type = "shapiro-wilk normality test"
                      , result = if_else(p < 0.05, "significant difference from normal distribution",
                                         "no significant difference from normal distribution")
                      ),
        as.data.frame(do.call("cbind", distribution_type_visual)
                   ) %>%
        dplyr::mutate(variable = nn,
                      name = label)
        )

  }, simplify = FALSE
  )
  
  writexl::write_xlsx(normality_test,
                      path = base::file.path(output_Dir, "normality_test.xlsx" )
                      )

} else {
  print(paste0("No normality checks done"))
}

