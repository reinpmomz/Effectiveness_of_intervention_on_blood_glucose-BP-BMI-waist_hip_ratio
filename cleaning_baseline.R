library(dplyr)
library(lubridate)
library(labelled)
library(tibble)
library(forcats)

working_directory

#creating unique id from names of participants from baseline and followup data

unique_id <- dplyr::bind_rows(df_raw_baseline%>% dplyr::select(wdf_q1_4, wdf_q1_5, wdf_q1_6),
                              df_raw_followup%>% dplyr::select(wdf_q1_4, wdf_q1_5, wdf_q1_6)
                              ) %>%
  distinct(wdf_q1_4, wdf_q1_5, wdf_q1_6)%>%
  mutate(study_id = 1:n())%>%
  labelled::set_variable_labels( #relabeling specific variables
    study_id = "Created respondent id"
    )

df_clean_baseline <- df_raw_baseline %>% 
  dplyr::left_join(unique_id,
                   by = c("wdf_q1_4", "wdf_q1_5", "wdf_q1_6")) %>%
  dplyr::rename(wdf_q3_2a = wdf_q3_2) %>%
  dplyr::mutate(wdf_q2_2 = if_else(wdf_q2_1 == "Yes" & is.na(wdf_q2_2),
                                   wdf_q1_2- (as.period(str_to_lower(wdf_q2_3a))*(as.numeric(wdf_q2_3b))), wdf_q2_2
                                   ), #cleaning when you were diagnosed with diabetes
                wdf_q1_9 = if_else(wdf_q1_9 == "B", "Baseline", wdf_q1_9),
                wdf_q1_9 = factor(wdf_q1_9, levels = c("Baseline", "Six month follow-up", "One year follow-up")),
                wdf_q5_5b = if_else(wdf_q5_5a == 161.1 & wdf_q5_5b == 61, 161.1, wdf_q5_5b
                                    ), #cleaning 2nd reading height
                wdf_q1_4_age = round(time_length(difftime(wdf_q1_2, wdf_q1_4, units = "auto"), unit = "year"),2
                                     ), #creating age column
                wdf_q1_4_age_group = if_else(wdf_q1_4_age < 51, "50 and below",
                                             if_else(wdf_q1_4_age  < 61, "51-60 years", "61 years and above" )
                                             ), #creating age group
                wdf_q1_4_age_group = factor(wdf_q1_4_age_group, levels = c("50 and below", "51-60 years", "61 years and above")
                                            ), #factor age group 
                wdf_q2_3a_b_years = round(time_length(as.period(str_to_lower(wdf_q2_3a))*(as.numeric(wdf_q2_3b)), unit = "year")
                                          ,3
                                          ), #creating duration of diabetes in years
                wdf_q2_3a_b_years_group = if_else(wdf_q2_3a_b_years < 5, "Below 5 years",
                                                  if_else(wdf_q2_3a_b_years  < 10, "5-9 years", "10 years and above" )
                                                  ), #creating duration of diabetes in years group
                wdf_q2_3a_b_years_group = factor(wdf_q2_3a_b_years_group,
                                                 levels = c("Below 5 years", "5-9 years", "10 years and above")
                                                 ), #factor duration of diabetes in years group
                wdf_q2_11a = if_else(wdf_q2_11b == 98, "Months", wdf_q2_11a ), #cleaning
                wdf_q2_11a = factor(wdf_q2_11a, levels = c("Days", "Weeks", "Months", "Years")), #factor cleaned
                wdf_q2_11a_b_years = round(time_length(as.period(str_to_lower(wdf_q2_11a))*(as.numeric(wdf_q2_11b)),
                                                       unit = "year"),3
                                           ), #creating duration of hypertension in years
                wdf_q2_11a_b_years_group = if_else(wdf_q2_11a_b_years < 5, "Below 5 years",
                                                   if_else(wdf_q2_11a_b_years  < 10, "5-9 years", "10 years and above" )
                                                   ), #creating duration of hypertension in years group
                wdf_q2_11a_b_years_group = factor(wdf_q2_11a_b_years_group,
                                                  levels = c("Below 5 years", "5-9 years", "10 years and above")
                                                  ), #factor duration of hypertension in years group
                across(c(wdf_q2_19a:wdf_q2_19g), ~ fct_collapse(.x, "No" = c("No", "Dont Know"))),
                wdf_q3_5a_b_years = round(time_length(as.period(str_to_lower(wdf_q3_5a))*(as.numeric(wdf_q3_5b)),
                                                      unit = "year"), 3
                                          ), #creating duration of how long ago did you stop smoking in years
                wdf_q3_19 = if_else(wdf_q3_18 == 0, 0, wdf_q3_19), #Replace NA with 0
                across(c(wdf_q3_27, wdf_q3_28), ~if_else(wdf_q3_26 == "No", 0, .x)
                       ), #Replace NA with 0
                wdf_q3_29a = if_else(wdf_q3_29 == "No", fct_na_value_to_level(wdf_q3_29a, level = "Never"), wdf_q3_29a),
                across(c(wdf_q3_31a, wdf_q3_31b, wdf_q3_31c, wdf_q3_31e, wdf_q3_31f), ~if_else(wdf_q3_30 == "Never", 0, .x)
                       ), #Replace NA with 0
                across(c(wdf_q3_33, wdf_q3_34a, wdf_q3_34b), ~if_else(wdf_q3_32 == "No", 0, .x)), #Replace NA with 0
                wdf_q3_34a_b_hours = round(time_length(hours(as.numeric(wdf_q3_34a))+minutes(as.numeric(wdf_q3_34b)),
                                                       unit = "hour"), 2
                                           ), #creating duration of Time spent doing vigorous-intensity activities hrs
                across(c(wdf_q3_36, wdf_q3_37a, wdf_q3_37b), ~if_else(wdf_q3_35 == "No", 0, .x)
                       ), #Replace NA with 0
                wdf_q3_37a_b_hours = round(time_length(hours(as.numeric(wdf_q3_37a))+minutes(as.numeric(wdf_q3_37b)),
                                                       unit = "hour"), 2
                                           ), #creating duration of Time spent doing moderate-intensity activities hrs
                across(c(wdf_q3_39, wdf_q3_40a, wdf_q3_40b), ~if_else(wdf_q3_38 == "No", 0, .x)
                       ), #Replace NA with 0
                wdf_q3_40a_b_hours = round(time_length(hours(as.numeric(wdf_q3_40a))+minutes(as.numeric(wdf_q3_40b)),
                                                       unit = "hour"), 2
                                           ), #creating duration of Time spent walking or cycling hrs
                across(c(wdf_q3_42, wdf_q3_43a, wdf_q3_43b), ~if_else(wdf_q3_41 == "No", 0, .x)
                       ), #Replace NA with 0
                wdf_q3_43a_b_hours = round(time_length(hours(as.numeric(wdf_q3_43a))+minutes(as.numeric(wdf_q3_43b)),
                                                       unit = "hour"), 2
                                           ), #creating duration of Time spent on vigorous-intensity sports hrs
                across(c(wdf_q3_45, wdf_q3_46a, wdf_q3_46b), ~if_else(wdf_q3_44 == "No", 0, .x)
                       ), #Replace NA with 0
                wdf_q3_46a_b_hours = round(time_length(hours(as.numeric(wdf_q3_46a))+minutes(as.numeric(wdf_q3_46b)),
                                                       unit = "hour"), 2
                                           ), #creating duration of Time spent on moderate-intensity sports hrs
                wdf_q3_47a_b_hours = round(time_length(hours(as.numeric(wdf_q3_47a))+minutes(as.numeric(wdf_q3_47b)),
                                                       unit = "hour"), 2
                                           ), #creating duration of Time spent sitting or reclining hrs
                wdf_q3_47a_b_hours_group = if_else(wdf_q3_47a_b_hours <4, "Less than 4 hrs",
                                                   if_else(wdf_q3_47a_b_hours <9, "4-8 hrs", "9hrs and more")
                                                   ),
                wdf_q3_47a_b_hours_group = factor(wdf_q3_47a_b_hours_group,
                                                  levels = c("Less than 4 hrs", "4-8 hrs", "9hrs and more")
                                                  ),
                wdf_q3_48_group = if_else(wdf_q3_48 < 7, "Less than 7 hours", "7 and more"),
                wdf_q3_48_group = factor(wdf_q3_48_group, levels = c("Less than 7 hours", "7 and more")
                                         ),
                wdf_q5_1_2_3a = round(rowMeans(across(c(wdf_q5_1a,wdf_q5_2a,wdf_q5_3a)), na.rm = TRUE),0
                                      ), #creating average Systolic BP
                wdf_q5_1_2_3b = round(rowMeans(across(c(wdf_q5_1b,wdf_q5_2b,wdf_q5_3b)), na.rm = TRUE),0
                                      ), #creating average Diastolic BP
                wdf_q5_5a_b = round(rowMeans(across(c(wdf_q5_5a, wdf_q5_5b)), na.rm = TRUE)/100,2
                                    ), #creating average height in meters
                wdf_q5_6_7 = round(rowMeans(across(c(wdf_q5_6, wdf_q5_7)), na.rm = TRUE),1
                                   ), #creating average weight in meters
                wdf_q5_5_6_7_bmi = round(wdf_q5_6_7/((wdf_q5_5a_b)^2),2
                                         ), #creating BMI
                wdf_q5_5_6_7_bmi_group = if_else(wdf_q5_5_6_7_bmi <18.5, "Underweight (<18.5)",
                                                 if_else(wdf_q5_5_6_7_bmi <25, "Normal (18.5–24.9)",
                                                         if_else(wdf_q5_5_6_7_bmi <30, "Overweight (25–29.9)", "Obese (>=30)"))
                                                 ), #Creating BMI group
                wdf_q5_5_6_7_bmi_group = factor(wdf_q5_5_6_7_bmi_group, levels = c("Normal (18.5–24.9)", "Underweight (<18.5)",
                                                                                   "Overweight (25–29.9)", "Obese (>=30)")
                                                ), #factor BMI group
                wdf_q5_8a_b_waist_hip_ratio = round(wdf_q5_8a/wdf_q5_8b ,2), #creating waist hip ratio
                wdf_q5_8a_b_waist_hip_ratio_group = if_else(wdf_q1_5== "Female" & wdf_q5_8a_b_waist_hip_ratio < 0.85,
                                                            "Normal (Male<0.90; Female<0.85)",
                                                            if_else(wdf_q1_5== "Male" & wdf_q5_8a_b_waist_hip_ratio < 0.90,
                                                                    "Normal (Male<0.90; Female<0.85)",
                                                                    "At Risk (Male>=0.90; Female>=0.85)")
                                                            ), #creating waist hip ratio group
                wdf_q5_8a_b_waist_hip_ratio_group = factor(wdf_q5_8a_b_waist_hip_ratio_group,
                                                           levels = c("Normal (Male<0.90; Female<0.85)",
                                                                      "At Risk (Male>=0.90; Female>=0.85)")
                                                           ), #factor waist hip ratio group
                wdf_q5_10_group = if_else(wdf_q5_10 <3.9, "Low (<3.9)",
                                          if_else(wdf_q5_10 <=5.6, "Normal (3.9-5.6)",
                                                  if_else(wdf_q5_10 <7, "At Risk (5.7-6.9)", "High (>=7)"))
                                          ), #Creating blood glucose group
                wdf_q5_10_group = factor(wdf_q5_10_group, levels = c("Normal (3.9-5.6)", "Low (<3.9)",
                                                                     "At Risk (5.7-6.9)", "High (>=7)")
                                         ), #factor blood glucose group
                across(where(is.factor),  ~fct_drop(.x )), #drop unused factor levels
                across(c(wdf_q3_2a, wdf_q3_3, wdf_q3_4, wdf_q3_7, wdf_q3_8, wdf_q3_9), ~fct_na_value_to_level(.x, level = "No")
                       ), #Replace NA to No
                wdf_q3_13 = if_else(wdf_q3_10 == "No", NA, wdf_q3_13), #Replace condition 9 to NA
                wdf_q3_14 = fct_collapse(wdf_q3_14, No = c("No", "9") ), #factor collapse column
                wdf_q3_15 = fct_collapse(wdf_q3_15, Yes = c("Yes", "9")), #factor collapse column
                across(c(wdf_q3_17a, wdf_q3_17b, wdf_q3_17c, wdf_q3_17d, wdf_q3_17e, wdf_q3_17f), ~ as.character(.x)
                       ), #Change columns to character
                across(c(wdf_q3_17a, wdf_q3_17b, wdf_q3_17c, wdf_q3_17d, wdf_q3_17e, wdf_q3_17f), 
                       ~ if_else(wdf_q3_15 == "Yes" & .x == "9", NA, .x)
                       ), #Replace condition 9 to NA
                across(c(wdf_q3_17a, wdf_q3_17b, wdf_q3_17c, wdf_q3_17d, wdf_q3_17e, wdf_q3_17f), 
                       ~ factor(.x, levels = c("Yes", "No", "9")) 
                       ), #convert Column to Factor
                across(c(wdf_q3_17a, wdf_q3_17b, wdf_q3_17c, wdf_q3_17d, wdf_q3_17e, wdf_q3_17f),
                       ~ fct_collapse(.x, No = c("No", "9") ) 
                       ), #Factor collapse column
                across(c(wdf_q3_31a, wdf_q3_31b, wdf_q3_31c, wdf_q3_31e, wdf_q3_31f), ~ na_if(.x, 99)), #convert 99 to NA
                wdf_q4_10 = fct_collapse(wdf_q4_10, 'Very Unhappy' = c("Very Unhappy", "6")),#factor collapse column
                wdf_q2_17b_c_d = if_else(wdf_q2_17b == "Yes" | wdf_q2_17c == "Yes" | wdf_q2_17d == "Yes", "Yes", "No"),
                wdf_q2_17e_19d = if_else(wdf_q2_17e == "Yes" | wdf_q2_19d == "Yes", "Yes", "No"),
                wdf_q2_19f_q5_0c = if_else(wdf_q2_19f == "Yes" | wdf_q5_0c == "Yes", "Yes", "No"),
                across(c(wdf_q4_1:wdf_q4_10), ~ fct_rev(.x)
                       ), #Reverse factor levels for QOL questions
                wdf_q2_7a_b_c = if_else(wdf_q2_7a == "Yes" | wdf_q2_7b == "Yes" | wdf_q2_7c == "Yes", "Yes", "No"
                                        ),
                wdf_q2_7a_b_c_d = if_else(wdf_q2_7a_b_c == "Yes" & wdf_q2_7d == "Yes", "Yes", "No"
                                          ),
                wdf_q2_9_17a = if_else(wdf_q2_9 == "Yes" | wdf_q2_17a == "Yes", "Yes", "No"),
                diabetes_medication_type = if_else(wdf_q2_7a_b_c == "Yes" & wdf_q2_7d == "No", "Tablets only",
                                                   if_else(wdf_q2_7d == "Yes" & wdf_q2_7a_b_c == "No", "Insulin Injections only",
                                                           if_else(wdf_q2_7a_b_c == "Yes" & wdf_q2_7d == "Yes", "Tablets with Insulin Injections",
                                                                   "None"
                                                                   )
                                                           )
                                                   ),
                diabetes_medication = if_else(diabetes_medication_type == "None", "No", "Yes"),
                across(c(wdf_q2_7a_b_c, wdf_q2_7a_b_c_d, wdf_q2_17b_c_d, wdf_q2_17e_19d, wdf_q2_19f_q5_0c, 
                         wdf_q2_9_17a, diabetes_medication), ~ factor(.x, levels = c("No", "Yes"))
                       ),
                across(c(wdf_q1_10, wdf_q2_1, wdf_q2_7a:wdf_q2_7g, wdf_q2_8a, wdf_q2_8b, wdf_q2_8c, wdf_q2_8d, wdf_q2_8e,
                         wdf_q2_8f, wdf_q2_8g:wdf_q2_8i, wdf_q2_9, wdf_q2_15a:wdf_q2_15f, wdf_q2_16a, wdf_q2_16b, wdf_q2_16c,
                         wdf_q2_16d, wdf_q2_16e, wdf_q2_16f, wdf_q2_16g:wdf_q2_16i, wdf_q2_17a, wdf_q2_17b, wdf_q2_17c,
                         wdf_q2_17d, wdf_q2_17e, wdf_q2_17f, wdf_q2_17g, wdf_q2_17h, wdf_q2_17i, wdf_q2_19a:wdf_q2_19g,
                         wdf_q3_1:wdf_q3_4, wdf_q3_6:wdf_q3_10, wdf_q3_14,wdf_q3_15, wdf_q3_17a:wdf_q3_17f,
                         wdf_q3_32, wdf_q3_35, wdf_q3_38, wdf_q3_41, wdf_q3_44, #physical activity
                         wdf_q5_0a:wdf_q5_0c, wdf_q5_4
                         ), ~ fct_rev(.x)
                       ), #Reverse factor levels Yes/No to No/Yes
                diabetes_medication_type = factor(diabetes_medication_type,
                                                  levels = c("None", "Tablets only","Insulin Injections only",
                                                             "Tablets with Insulin Injections" 
                                                  )
                                                  ),
                overall_qol_raw = rowMeans(across(c(wdf_q4_1:wdf_q4_10), ~ as.numeric(.x)), na.rm = TRUE),
                overall_qol_raw = if_else(rowSums(across(c(wdf_q4_1:wdf_q4_10), ~is.na(.x))) > round(0.2*10,0),
                                          NA, overall_qol_raw 
                                          ), #If more than 20% of total columns have missing, replace with NA
                overall_qol_transformed = round((overall_qol_raw-1)*(1/4),3),
                wdf_q1_7_new = forcats::fct_recode(wdf_q1_7, !!!new_levels_location),
                wdf_q1_7_new = forcats::fct_relevel(wdf_q1_7_new, "Dandora", after = 0L) #move level to 1st position
                ) %>%
  labelled::set_variable_labels(!!!new_labels_baseline[names(new_labels_baseline) %in% names(.)]) %>% #labeling variables from data dictionary 
  labelled::set_variable_labels( #creating labels for new variables
    wdf_q1_4_age = "Age (years)",
    wdf_q1_4_age_group = "Age grouped",
    wdf_q2_3a_b_years = "Duration of diabetes (years)",
    wdf_q2_3a_b_years_group = "Duration of diabetes grouped",
    wdf_q2_11a_b_years = "Duration of high BP (years)",
    wdf_q2_11a_b_years_group = "Duration of high BP grouped",
    wdf_q3_5a_b_years = "How long ago did you stop smoking daily? (in years)",
    wdf_q3_34a_b_hours = "Time spent doing vigorous-intensity activities in hrs",
    wdf_q3_37a_b_hours = "Time spent doing moderate-intensity activities in hrs",
    wdf_q3_40a_b_hours = "Time spent walking or cycling in hrs",
    wdf_q3_43a_b_hours = "Time spent on vigorous-intensity sports in hrs",
    wdf_q3_46a_b_hours = "Time spent on moderate-intensity sports in hrs",
    wdf_q3_47a_b_hours = "Time spent sitting or reclining in hrs",
    wdf_q3_47a_b_hours_group = "Time spent sitting or reclining in hrs grouped",
    wdf_q3_48_group = "Hours spent on sleeping grouped",
    wdf_q5_1_2_3a = "Systolic Blood pressure (mm Hg)",
    wdf_q5_1_2_3b = "Diastolic Blood pressure (mm Hg)",
    wdf_q5_5a_b = "Height (metres)",
    wdf_q5_6_7 = "Weight (kgs)", 
    wdf_q5_5_6_7_bmi = "BMI (kg/m2)",
    wdf_q5_5_6_7_bmi_group = "BMI (kg/m2) grouped",
    wdf_q5_8a_b_waist_hip_ratio = "Waist-to-hip ratio",
    wdf_q5_8a_b_waist_hip_ratio_group = "Waist-to-hip ratio grouped",
    wdf_q5_10_group = "Blood glucose (mmol/L) grouped",
    wdf_q2_17b_c_d = "Diagnosed with cardiovascular diseases",
    wdf_q2_7a_b_c = "Diabetes medication - Tablets",
    wdf_q2_7a_b_c_d = "Diabetes medication - Tablets with Insulin Injections",
    wdf_q2_9_17a = "Hypertension",
    wdf_q2_17e_19d = "Kidney Complications",
    wdf_q2_19f_q5_0c = "Abdominal or pedal edema complications",
    diabetes_medication_type = "Diabetes medication type",
    diabetes_medication = "Taking diabetes medication",
    overall_qol_raw = "Overall QOL Raw Score",
    overall_qol_transformed = "Overall QOL transformed Score",
    wdf_q1_7_new = "Location"
  )
  
