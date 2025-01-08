library(dplyr)
library(lubridate)
library(labelled)
library(tibble)
library(forcats)

working_directory

df_clean_followup <- df_raw_followup %>% 
  dplyr::left_join(unique_id,
                   by = c("wdf_q1_4", "wdf_q1_5", "wdf_q1_6")) %>%
  dplyr::rename(wdf_q3_2a = wdf_q3_2
                , wdf_q5_6 = wdf_q5_61,
                , wdf_q3_32_dup = wdf_q3_32
                , wdf_q3_36_dup = wdf_q3_36
                , wdf_q3_37a_dup = wdf_q3_37a
                , wdf_q3_37b_dup = wdf_q3_37b
                , wdf_q3_38_dup = wdf_q3_38
                , wdf_q3_42_dup = wdf_q3_42
                , wdf_q3_43a_dup = wdf_q3_43a
                , wdf_q3_43b_dup = wdf_q3_43b
                , wdf_q3_44_dup = wdf_q3_44
                , wdf_q3_47a_dup = wdf_q3_47a
                , wdf_q3_47b_dup = wdf_q3_47b
                ) %>%
  dplyr::rename_with(~paste0(.x,"_followup")) %>%
  dplyr::left_join(df_clean_baseline %>%
                     dplyr::select(wdf_q1_6, wdf_q1_4, wdf_q2_2, wdf_q2_4, wdf_q2_5, wdf_q2_6, wdf_q2_7a, wdf_q2_7b, wdf_q2_7c,
                                   wdf_q2_7d, wdf_q2_7e, wdf_q2_7f, wdf_q2_7g, wdf_q2_7sp, wdf_q2_8a, wdf_q2_8b, wdf_q2_8bsp,
                                   wdf_q2_8c, wdf_q2_8csp, wdf_q2_8d, wdf_q2_8dsp, wdf_q2_8e, wdf_q2_8esp, wdf_q2_8f, wdf_q2_8fsp,
                                   wdf_q2_8g, wdf_q2_8h, wdf_q2_8i, wdf_q2_8isp, wdf_q2_10, wdf_q2_12, wdf_q2_13, wdf_q2_14,
                                   wdf_q2_15a, wdf_q2_15b, wdf_q2_15c, wdf_q2_15d, wdf_q2_15e, wdf_q2_15f, wdf_q2_16a, wdf_q2_16b,
                                   wdf_q2_16c, wdf_q2_16d, wdf_q2_16e, wdf_q2_16f, wdf_q2_16g, wdf_q2_16h, wdf_q2_16i, wdf_q2_17a,
                                   wdf_q2_18a, wdf_q3_4, wdf_q3_5a, wdf_q3_5b, wdf_q3_9, wdf_q3_15, wdf_q5_5a, wdf_q5_5b, wdf_q1_2
                                   ), 
                   by = c("wdf_q1_6_followup" = "wdf_q1_6", "wdf_q1_4_followup" = "wdf_q1_4")
                   ) %>%
  dplyr::mutate( wdf_q2_2_followup = wdf_q2_2, #Filling column with baseline data
                 , wdf_q2_3b_followup = round(time_length(interval(wdf_q2_2_followup, wdf_q1_2_followup),
                                                        unit = "year"),2
                                              ) #calculate time length in years, months, weeks, days
                 , wdf_q2_3a_followup = if_else(wdf_q2_3b_followup<1, "Months", "Years")
                 , wdf_q2_3b_followup = if_else(wdf_q2_3b_followup == 0.54, 6.48,
                                                if_else(wdf_q2_3b_followup == 0.73, 8.76,
                                                        if_else(wdf_q2_3b_followup == 0.78, 9.36,
                                                                if_else(wdf_q2_3b_followup == 0.84, 10.08,
                                                                        if_else(wdf_q2_3b_followup == 0.90, 10.80,
                                                                                if_else(wdf_q2_3b_followup == 0.96,
                                                                                        11.52, wdf_q2_3b_followup
                                                                                        )
                                                                                )
                                                                        )
                                                                )
                                                        )
                                                )
                 , wdf_q2_4_followup = wdf_q2_4 #Filling column with baseline data
                 , wdf_q2_10_followup = if_else(wdf_q2_9_followup == "Yes" & is.na(wdf_q2_10_followup), wdf_q2_10,
                                                wdf_q2_10_followup
                                                ) #Filling column with baseline data
                 , wdf_q2_11b_followup = if_else(is.na(wdf_q2_11b_followup), round(time_length(interval(wdf_q2_10_followup, wdf_q1_2_followup),
                                                                                               unit = "year"),2), 
                                                 wdf_q2_11b_followup
                                                 ) #calculate time length in years, months, weeks, days
                 , wdf_q2_11a_followup = if_else(is.na(wdf_q2_11a_followup) & wdf_q2_11b_followup<1, "Months",
                                                 if_else(is.na(wdf_q2_11a_followup) & wdf_q2_11b_followup>= 1, "Years",
                                                         wdf_q2_11a_followup
                                                         )
                                                 )
                 , wdf_q2_11b_followup = if_else(wdf_q2_11b_followup == 0.81, 9.72,
                                                 if_else(wdf_q2_11b_followup == 0.88, 10.56, 
                                                         if_else(wdf_q2_11b_followup == 0.92, 11.04, 
                                                                 if_else(wdf_q2_11b_followup == 0.96, 11.52, wdf_q2_11b_followup
                                                                         )
                                                                 )
                                                         )
                                                 )
                 , across(c(wdf_q2_3a_followup, wdf_q2_11a_followup), ~ factor(.x, levels = c("Days", "Weeks", "Months", "Years"))
                          ) #columns to factor
                 , across(c(wdf_q2_3b_followup, wdf_q2_11b_followup ), ~ round(.x, 0)) #round column values 
                 , wdf_q2_12_followup = if_else(wdf_q2_9_followup == "No", NA,
                                                if_else(is.na(wdf_q2_12_followup), wdf_q2_12, wdf_q2_12_followup
                                                        )
                                                ) #Filling column with baseline data
                 , wdf_q2_13_followup = if_else(is.na(wdf_q2_13_followup), wdf_q2_13, wdf_q2_13_followup
                                                ) #Filling column with baseline data
                 , wdf_q2_14_followup = if_else(is.na(wdf_q2_14_followup), wdf_q2_14, wdf_q2_14_followup
                                                ) #Filling column with baseline data
                 , wdf_q2_15a_followup = if_else(is.na(wdf_q2_15a_followup), wdf_q2_15a, wdf_q2_15a_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_15b_followup = if_else(is.na(wdf_q2_15b_followup), wdf_q2_15b, wdf_q2_15b_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_15c_followup = if_else(is.na(wdf_q2_15c_followup), wdf_q2_15c, wdf_q2_15c_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_15d_followup = if_else(is.na(wdf_q2_15d_followup), wdf_q2_15d, wdf_q2_15d_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_15e_followup = if_else(is.na(wdf_q2_15e_followup), wdf_q2_15e, wdf_q2_15e_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_15f_followup = if_else(is.na(wdf_q2_15f_followup), wdf_q2_15f, wdf_q2_15f_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_16a_followup = if_else(is.na(wdf_q2_16a_followup), wdf_q2_16a, wdf_q2_16a_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_16b_followup = if_else(is.na(wdf_q2_16b_followup), wdf_q2_16b, wdf_q2_16b_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_16c_followup = if_else(is.na(wdf_q2_16c_followup), wdf_q2_16c, wdf_q2_16c_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_16d_followup = if_else(is.na(wdf_q2_16d_followup), wdf_q2_16d, wdf_q2_16d_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_16e_followup = if_else(is.na(wdf_q2_16e_followup), wdf_q2_16e, wdf_q2_16e_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_16f_followup = if_else(is.na(wdf_q2_16f_followup), wdf_q2_16f, wdf_q2_16f_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_16g_followup = if_else(is.na(wdf_q2_16g_followup), wdf_q2_16g, wdf_q2_16g_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_16h_followup = if_else(is.na(wdf_q2_16h_followup), wdf_q2_16h, wdf_q2_16h_followup
                                                 ) #Filling column with baseline data
                 , wdf_q2_16i_followup = if_else(is.na(wdf_q2_16i_followup), wdf_q2_16i, wdf_q2_16i_followup
                                                 ) #Filling column with baseline data
                 , wdf_q3_5a_followup = wdf_q3_5a #duplicating from baseline data
                 , wdf_q3_5b_followup = wdf_q3_5b #duplicating from baseline data
                 , wdf_q3_5b_followup = wdf_q3_5b_followup + round(time_length(interval(wdf_q1_2, wdf_q1_2_followup), unit = "year"),0
                                                                   ) #calculate time length in years, months, weeks, days
                 , wdf_q3_17a_followup = ifelse(is.na(wdf_q3_17a_followup) | wdf_q3_17a_followup == 2, "No",
                                                wdf_q3_17a_followup
                                                ) #Replace NA and 2 to No
                 , wdf_q3_17a_followup = factor(wdf_q3_17a_followup, levels = c("Yes", "No"))
                 , across(c(wdf_q3_2a_followup, wdf_q3_3_followup, wdf_q3_7_followup, wdf_q3_8_followup, wdf_q3_14_followup,
                            wdf_q3_17b_followup:wdf_q3_17g_followup, wdf_q3_33a_followup:wdf_q3_33g_followup, wdf_q3_35a_followup:wdf_q3_35g_followup,
                            wdf_q3_37a_dup_followup:wdf_q3_37g_followup, wdf_q3_39a_followup:wdf_q3_39f_followup, wdf_q3_41a_followup:wdf_q3_41h_followup,
                            wdf_q3_43a_dup_followup:wdf_q3_43g_followup, wdf_q3_45a_followup:wdf_q3_45k_followup,
                            wdf_q3_47a_dup_followup:wdf_q3_47i_followup), ~ fct_na_value_to_level(.x, level = "No")
                          ), #Replace NA to Factor No
                 , wdf_q5_5a_followup = wdf_q5_5a #duplicating 1st reading height from baseline data
                 , wdf_q5_5b_followup =  wdf_q5_5b #duplicating 2nd reading height from baseline data
                 , wdf_q2_5_followup = wdf_q2_5 #duplicating from baseline data
                 , wdf_q2_6_followup = wdf_q2_6 #duplicating from baseline data
                 , wdf_q2_7a_followup = wdf_q2_7a #duplicating from baseline data
                 , wdf_q2_7b_followup = wdf_q2_7b #duplicating from baseline data
                 , wdf_q2_7c_followup = wdf_q2_7c #duplicating from baseline data
                 , wdf_q2_7d_followup = wdf_q2_7d #duplicating from baseline data
                 , wdf_q2_7e_followup = wdf_q2_7e #duplicating from baseline data
                 , wdf_q2_7f_followup = wdf_q2_7f #duplicating from baseline data
                 , wdf_q2_7g_followup = wdf_q2_7g #duplicating from baseline data
                 , wdf_q2_7sp_followup = wdf_q2_7sp #duplicating from baseline data
                 , wdf_q2_8a_followup = wdf_q2_8a #duplicating from baseline data
                 , wdf_q2_8b_followup = wdf_q2_8b #duplicating from baseline data
                 , wdf_q2_8bsp_followup = wdf_q2_8bsp #duplicating from baseline data
                 , wdf_q2_8c_followup = wdf_q2_8c #duplicating from baseline data
                 , wdf_q2_8csp_followup = wdf_q2_8csp #duplicating from baseline data
                 , wdf_q2_8d_followup = wdf_q2_8d #duplicating from baseline data
                 , wdf_q2_8dsp_followup = wdf_q2_8dsp #duplicating from baseline data
                 , wdf_q2_8e_followup = wdf_q2_8e #duplicating from baseline data
                 , wdf_q2_8esp_followup = wdf_q2_8esp #duplicating from baseline data
                 , wdf_q2_8f_followup = wdf_q2_8f #duplicating from baseline data
                 , wdf_q2_8fsp_followup = wdf_q2_8fsp #duplicating from baseline data
                 , wdf_q2_8g_followup = wdf_q2_8g #duplicating from baseline data
                 , wdf_q2_8h_followup = wdf_q2_8h #duplicating from baseline data
                 , wdf_q2_8i_followup = wdf_q2_8i #duplicating from baseline data
                 , wdf_q2_8isp_followup = wdf_q2_8isp #duplicating from baseline data
                 , wdf_q2_17a_followup = wdf_q2_17a #duplicating from baseline data
                 , wdf_q2_18a_followup = wdf_q2_18a #duplicating from baseline data
                 , wdf_q3_4_followup = wdf_q3_4 #duplicating from baseline data
                 , wdf_q3_9_followup = wdf_q3_9 #duplicating from baseline data
                 , wdf_q3_15_followup = wdf_q3_15 #duplicating from baseline data
                 ) %>% 
  dplyr::mutate_if(is.factor, ~fct_drop(.x #, only = c("")
                                 ) #drop unused factor levels
                   ) %>%
  dplyr::select(wdf_q1_2_followup:wdf_q5_11_followup, study_id_followup, wdf_q5_5a_followup:wdf_q3_15_followup) %>%
  dplyr::mutate(wdf_q1_4_age_followup = round(time_length(difftime(wdf_q1_2_followup, wdf_q1_4_followup, units = "auto"),
                                                   unit = "year"),2
                                              ) #creating age column
                , wdf_q1_4_age_group_followup = ifelse(wdf_q1_4_age_followup < 51, "50 and below",
                                                     ifelse(wdf_q1_4_age_followup  < 61, "51-60 years", "61 years and above" 
                                                            )
                                                     ) #creating age group column
                , wdf_q1_4_age_group_followup = factor(wdf_q1_4_age_group_followup,
                                                       levels = c("50 and below", "51-60 years", "61 years and above")
                                                       ) #factor age group column
                , wdf_q2_3a_b_years_followup = round(time_length(as.period(str_to_lower(wdf_q2_3a_followup))*(as.numeric(wdf_q2_3b_followup)),
                                                               unit = "year"),3
                                                     ) #creating duration of diabetes in years column
                , wdf_q2_3a_b_years_group_followup = ifelse(wdf_q2_3a_b_years_followup < 5, "Below 5 years",
                                                            ifelse(wdf_q2_3a_b_years_followup  < 10, "5-9 years", "10 years and above" 
                                                                   )
                                                            ) #creating duration of diabetes in years group column
                , wdf_q2_3a_b_years_group_followup = factor(wdf_q2_3a_b_years_group_followup, 
                                                            levels = c("Below 5 years", "5-9 years", "10 years and above")
                                                            ) #factor duration of diabetes in years group column
                , wdf_q2_11a_b_years_followup = round(time_length(as.period(str_to_lower(wdf_q2_11a_followup))*(as.numeric(wdf_q2_11b_followup)),
                                                                  unit = "year"),3
                                                      ) #creating duration of hypertension in years column
                , wdf_q2_11a_b_years_group_followup = ifelse(wdf_q2_11a_b_years_followup < 5, "Below 5 years",
                                                             ifelse(wdf_q2_11a_b_years_followup  < 10, "5-9 years", "10 years and above" 
                                                                    )
                                                             ) #creating duration of hypertension in years group column
                , wdf_q2_11a_b_years_group_followup = factor(wdf_q2_11a_b_years_group_followup, 
                                                             levels = c("Below 5 years", "5-9 years", "10 years and above")
                                                             ) #factor duration of hypertension in years group column
                , wdf_q3_5a_b_years_followup = round(time_length(as.period(str_to_lower(wdf_q3_5a_followup))*(as.numeric(wdf_q3_5b_followup)),
                                                        unit = "year"), 3
                                                     ) #creating duration of how long ago did you stop smoking in years
                , wdf_q5_1_2_3a_followup = round(rowMeans(across(c(wdf_q5_1a_followup,wdf_q5_2a_followup,wdf_q5_3a_followup)),
                                                          na.rm = TRUE),0
                                                 ) #creating average Systolic BP column
                , wdf_q5_1_2_3b_followup = round(rowMeans(across(c(wdf_q5_1b_followup,wdf_q5_2b_followup,wdf_q5_3b_followup)),
                                                        na.rm = TRUE),0
                                                 ) #creating average Diastolic BP column
                , wdf_q5_5a_b_followup = round(rowMeans(across(c(wdf_q5_5a_followup, wdf_q5_5b_followup)),
                                                      na.rm = TRUE)/100,2
                                               ) #creating average height in meters column
                , wdf_q5_6_7_followup = round(rowMeans(across(c(wdf_q5_6_followup, wdf_q5_7_followup)),
                                                     na.rm = TRUE),1
                                              ) #creating average weight in meters column
                , wdf_q5_5_6_7_bmi_followup = round(wdf_q5_6_7_followup/((wdf_q5_5a_b_followup)^2),2
                                                    ), #creating BMI column
                , wdf_q5_5_6_7_bmi_group_followup = ifelse(wdf_q5_5_6_7_bmi_followup <18.5, "Underweight (<18.5)",
                                                         ifelse(wdf_q5_5_6_7_bmi_followup <25, "Normal (18.5–24.9)",
                                                                ifelse(wdf_q5_5_6_7_bmi_followup <30, "Overweight (25–29.9)",
                                                                       "Obese (>=30)"
                                                                       )
                                                                )
                                                         ) #Creating BMI group column
                , wdf_q5_5_6_7_bmi_group_followup = factor(wdf_q5_5_6_7_bmi_group_followup, 
                                                           levels = c("Normal (18.5–24.9)", "Underweight (<18.5)", 
                                                                      "Overweight (25–29.9)", "Obese (>=30)")
                                                           ) #factor BMI group column
                , wdf_q5_8a_b_waist_hip_ratio_followup = round(wdf_q5_8a_followup/wdf_q5_8b_followup,2
                                                               ) #creating waist hip ratio column
                , wdf_q5_8a_b_waist_hip_ratio_group_followup = 
                  ifelse(wdf_q1_5_followup == "Female" & wdf_q5_8a_b_waist_hip_ratio_followup < 0.85, "Normal (Male<0.90; Female<0.85)",
                         ifelse(wdf_q1_5_followup == "Male" & wdf_q5_8a_b_waist_hip_ratio_followup < 0.90, "Normal (Male<0.90; Female<0.85)",
                                "At Risk (Male>=0.90; Female>=0.85)")), #creating waist hip ratio group column
                , wdf_q5_8a_b_waist_hip_ratio_group_followup = factor(wdf_q5_8a_b_waist_hip_ratio_group_followup,
                                                                      levels = c("Normal (Male<0.90; Female<0.85)",
                                                                                 "At Risk (Male>=0.90; Female>=0.85)")
                                                                      ) #factor waist hip ratio group column
                , wdf_q5_10_group_followup = ifelse(wdf_q5_10_followup <3.9, "Low (<3.9)",
                                                  ifelse(wdf_q5_10_followup <=5.6, "Normal (3.9-5.6)",
                                                         ifelse(wdf_q5_10_followup <7, "At Risk (5.7-6.9)", "High (>=7)"
                                                                )
                                                         )
                                                  ) #Creating blood glucose group column
                , wdf_q5_10_group_followup = factor(wdf_q5_10_group_followup,
                                                    levels = c("Low (<3.9)", "Normal (3.9-5.6)", "At Risk (5.7-6.9)", "High (>=7)")
                                                    ) #factor blood glucose group column
                , wdf_q2_17b_c_d_followup = ifelse(wdf_q2_17b_followup == "Yes" | wdf_q2_17c_followup == "Yes" | wdf_q2_17d_followup == "Yes",
                                                     "Yes", "No"
                                                    )
                , wdf_q2_17e_19d_followup = if_else(wdf_q2_17e_followup == "Yes" | wdf_q2_19d_followup == "Yes", "Yes", "No"
                                                    )
                , wdf_q2_19f_q5_0c_followup = if_else(wdf_q2_19f_followup == "Yes" | wdf_q5_0c_followup == "Yes", "Yes", "No"
                                                      )
                , wdf_q2_7a_b_c_followup = if_else(wdf_q2_7a_followup == "Yes" | wdf_q2_7b_followup == "Yes" | wdf_q2_7c_followup == "Yes", "Yes", "No"
                                          )
                , wdf_q2_7a_b_c_d_followup = if_else(wdf_q2_7a_b_c_followup == "Yes" & wdf_q2_7d_followup == "Yes", "Yes", "No"
                                          )
                , wdf_q2_9_17a_followup = if_else(wdf_q2_9_followup == "Yes" | wdf_q2_17a_followup == "Yes", "Yes", "No"
                                       )
                , diabetes_medication_type_followup = if_else(wdf_q2_7a_b_c_followup == "Yes" & wdf_q2_7d_followup == "No", "Tablets only",
                                                              if_else(wdf_q2_7d_followup == "Yes" & wdf_q2_7a_b_c_followup == "No", "Insulin Injections only",
                                                                      if_else(wdf_q2_7a_b_c_followup == "Yes" & wdf_q2_7d_followup == "Yes", "Tablets with Insulin Injections",
                                                                              "None"
                                                                              )
                                                                      )
                                                              )
                , diabetes_medication_followup = if_else(diabetes_medication_type_followup == "None", "No", "Yes")
                , across(c(wdf_q2_17b_c_d_followup, wdf_q2_17e_19d_followup, wdf_q2_19f_q5_0c_followup,
                           wdf_q2_7a_b_c_followup, wdf_q2_7a_b_c_d_followup, wdf_q2_9_17a_followup, diabetes_medication_followup
                           ), ~ factor(.x, levels = c("No", "Yes"))
                         )
                , diabetes_medication_type_followup = factor(diabetes_medication_type_followup,
                                                             levels = c("None", "Tablets only","Insulin Injections only",
                                                                        "Tablets with Insulin Injections")
                                                             )
                , across(c(wdf_q3_32_dup_followup, wdf_q3_33a_followup, wdf_q3_33c_followup, wdf_q3_34_followup,
                           wdf_q3_35a_followup, wdf_q3_35c_followup, wdf_q3_36_dup_followup, wdf_q3_37b_dup_followup,
                           wdf_q3_37c_followup, wdf_q3_37e_followup, wdf_q3_37f_followup, wdf_q3_38_dup_followup,
                           wdf_q3_39b_followup, wdf_q3_39c_followup, wdf_q3_39e_followup, wdf_q3_40_followup,
                           wdf_q3_41b_followup, wdf_q3_41c_followup, wdf_q3_41e_followup, wdf_q3_41g_followup,
                           wdf_q3_42_dup_followup, wdf_q3_43a_dup_followup, wdf_q3_43c_followup, wdf_q3_44_dup_followup,
                           wdf_q3_45b_followup, wdf_q3_45c_followup, wdf_q3_45e_followup, wdf_q3_45f_followup,
                           wdf_q3_45g_followup, wdf_q3_45h_followup, wdf_q3_45i_followup, wdf_q3_45j_followup,
                           wdf_q3_46_followup, wdf_q3_47a_dup_followup, wdf_q3_47c_followup, wdf_q3_47e_followup,
                           wdf_q3_47g_followup
                           ), ~ fct_rev(.x)
                         ) #Reverse factor levels Yes/No to No/Yes
                , wdf_q1_7_new_followup = forcats::fct_recode(wdf_q1_7_followup, !!!new_levels_location)
                , wdf_q1_7_new_followup = forcats::fct_relevel(wdf_q1_7_new_followup, "Dandora", after = 0L
                                                               ) #move level to 1st position
                ) %>%
  labelled::set_variable_labels(!!!new_labels_followup[names(new_labels_followup) %in% names(.)]
                                ) %>% #labeling variables from data dictionary
  labelled::set_variable_labels( #creating labels for new variables
    wdf_q1_4_age_followup = "Age (years)",
    wdf_q1_4_age_group_followup = "Age grouped",
    wdf_q2_3a_b_years_followup = "Duration of diabetes (years)",
    wdf_q2_3a_b_years_group_followup = "Duration of diabetes grouped",
    wdf_q2_11a_b_years_followup = "Duration of high BP (years)",
    wdf_q2_11a_b_years_group_followup = "Duration of high BP grouped",
    wdf_q3_5a_b_years_followup = "How long ago did you stop smoking daily? (in years)",
    wdf_q5_1_2_3a_followup = "Systolic Blood pressure (mm Hg)",
    wdf_q5_1_2_3b_followup = "Diastolic Blood pressure (mm Hg)",
    wdf_q5_5a_b_followup = "Height (metres)",
    wdf_q5_6_7_followup = "Weight (kgs)", 
    wdf_q5_5_6_7_bmi_followup = "BMI (kg/m2)",
    wdf_q5_5_6_7_bmi_group_followup = "BMI (kg/m2) grouped",
    wdf_q5_8a_b_waist_hip_ratio_followup = "Waist-to-hip ratio",
    wdf_q5_8a_b_waist_hip_ratio_group_followup = "Waist-to-hip ratio grouped",
    wdf_q5_10_group_followup = "Blood glucose (mmol/L) grouped",
    wdf_q2_17b_c_d_followup = "Diagnosed with cardiovascular diseases",
    wdf_q2_17e_19d_followup = "Kidney Complications",
    wdf_q2_19f_q5_0c_followup = "Abdominal or pedal edema complications",
    wdf_q2_7a_b_c_followup = "Diabetes medication - Tablets",
    wdf_q2_7a_b_c_d_followup = "Diabetes medication - Tablets with Insulin Injections",
    wdf_q2_9_17a_followup= "Hypertension",
    diabetes_medication_type_followup = "Diabetes medication type",
    diabetes_medication_followup = "Taking diabetes medication",
    wdf_q1_7_new_followup = "Location"
  ) %>%
  dplyr::rename_with( ~gsub("_followup", "", .x))



