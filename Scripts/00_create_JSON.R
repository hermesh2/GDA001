# S Basics ----------------------------------------------------------------
if(! require("rjson")){
  install.packages("rjson")
}
library("rjson")

list <-  list(
  needed_script = c("dplyr", "data.table"),
  dir_script = getwd()
)

writeLines(text = toJSON(list), con = "Data/00_Initial.json")
