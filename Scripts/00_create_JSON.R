# S Basics ----------------------------------------------------------------
if(! require("rjson")){
  install.packages("rjson")
}
library("rjson")

list <-  list(
  needed_script = c("dplyr", "data.table", "ez", "ggplot2", "nlme", "lme4", "languageR", "lmerTest"),
  dir_script = getwd(),
  min_RT = 300,
  max_RT = 3000,
  IQR_prod = 1.5,
  correct_only = FALSE, # Si quremeos solo las repuestas correctas
  Ratio_response_Block = 0.8,
  n_per_sex_men = 50,
  n_per_sex_women = 50,
  Subject_2_Study = FALSE, # Seleccionamos solo los que cumplen los criterios
  Out_rt_300_3000 = TRUE, # Quitamos los de min y los de max, pero o se cambian o se quitan
  Change_rt_300_3000 = FALSE, # Cambiamos los de min y los de max por el valor max o el min, pero o se cambian o se quitan
  Out_IQR = FALSE,
  Error_plus_Subject_block_600 = TRUE, # Los dos que vienen solo puede ser uno TRUE, son tratamientos de error.
  Error_plus_Subject_600 = FALSE,
  Only_attribute = FALSE, # Nos quedamos solo con los atributos
  final_var_anova = "log" # RT, log o z
)

writeLines(text = toJSON(list), con = "Data/00_Initial.json")

source("Scripts/03_Prepare_EzAnova.R")
source("Scripts/04_ezANOVA.R")
