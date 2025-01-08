# Effectiveness_of_intervention_on_blood_glucose-BP-BMI-waist_hip_ratio
Effectiveness of intervention program on changes in blood glucose, blood pressure, body mass index and waist-to-hip ratio of patients with diabetes in a resource-limited setting in Kenya.

## Summary



## Setup

We are assuming you have `R Software` and `Rstudio IDE` installed. If not you can download and install [**R software**](https://www.r-project.org/) then followed by [**RStudio/Posit IDE**](https://posit.co/download/rstudio-desktop/).

## Data

The data used for analysis can be accessed through [**APHRC Microdata portal**](https://microdataportal.aphrc.org/index.php/catalog/75) upon proper approvals.

- **Data used for analysis:** `wdfbaseline_cleaned.dta` and `wdffollowup_cleaned.dta`


## Materials

- The `wdf_baseline_followup_recode_file.xlsx` file contains: 
    
    1. MetaData about study in the _study_title_ sheet.
    
    2. Categorization of the locations in the _location_ sheet.
    
    3. Data dictionary of the baseline data in the _rename_vars_baseline_ sheet.
    
    4. Data dictionary of the follow-up data in the _rename_vars_followup_ sheet.
   
    5. _selected_vars_ sheet that contains merged original and new created variables during data cleaning. Guide on variables
    to be analyzed and visualized.
    
    6. _drop_selected_vars_ sheet for dropped variables that won't be used in modelling.
    
    7. _model_params_ sheet with set parameters for imputation and correlation.
    
   
## Run

After cloning the repository or downloading the ZIP, you also need the data files (**Data used for analysis**) in the _data_ sub-folder of _Effectiveness_of_intervention_on_blood_glucose-BP-BMI-waist_hip_ratio_ folder.

Open `Rstudio` then set your working directory to the _Effectiveness_of_intervention_on_blood_glucose-BP-BMI-waist_hip_ratio_ folder. 

- Copy the below code to run all files at once in Rstudio

```
source("main.R")

```
- To run individual files, open the `main.R` script, and run from the beginning.
