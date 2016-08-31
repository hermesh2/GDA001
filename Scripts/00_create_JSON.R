# S Basics ----------------------------------------------------------------
if(! require("rjson")){
  install.packages("rjson")
}
library("rjson")

list <-  list(
  needed_script = c("dplyr", "data.table", "ez", "ggplot2"),
  dir_script = getwd(),
  min_RT = 300,
  max_RT = 3000,
  IQR_prod = 1.5,
  Ratio_response_Block = 0.7,
  n_per_sex_men = 50,
  n_per_sex_women = 71,
  Subject_2_Study = TRUE,
  Out_rt_300_3000 = TRUE,
  Out_IQR = FALSE,
  Subject_out = c(102) # Falla
)

writeLines(text = toJSON(list), con = "Data/00_Initial.json")
