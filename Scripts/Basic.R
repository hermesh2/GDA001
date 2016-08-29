# S Basics ----------------------------------------------------------------
if(! require("rjson")){
  install.packages("rjson")
}
library("rjson")

list_script <- fromJSON(readLines(con = "Data/00_Initial.json") ) 
needed_script <- list_script$needed_script
lapply(X = needed_script, FUN = function(x){
  if( !x %in% installed.packages() ){
    install.packages(x)
  }
  library(x, character.only = TRUE)
})

dir_script <- list_script$dir_script
setwd(dir = dir_script)
rm(list = ls());gc()
# E Basics ----------------------------------------------------------------
