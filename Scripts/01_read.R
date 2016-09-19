# Ojo he borrado los datos porque ocupaban mucho en Dropbox

# S Basics ----------------------------------------------------------------
if(! require("rjson")){
  install.packages("rjson")
}
library("rjson")

list_script <- fromJSON(readLines(con = "Data/00_Initial.json") ) 
needed_script <- list_script$needed_script
lapply(X = needed_script, FUN = function(x){
  print(x)
  if( !x %in% installed.packages() ){
    install.packages(x)
  }
  library(x, character.only = TRUE)
})


dir_script <- 
  list_script$dir_script

setwd(dir = dir_script)
rm(list = ls());gc()
# E Basics ----------------------------------------------------------------



# S read ------------------------------------------------------------------
list_data <- 
  lapply(as.list(dir(path = "Data")) , function(x){
    file_name <- 
      x %>% file.path("Data",  .)
    if (   file.size(file_name ) > 210000 ){
      cat("\n=============================================\n" ,
          x,
          "\n=============================================\n" )
       x <- file_name %>%
        fread(sep = ",", header = TRUE, stringsAsFactors = TRUE, verbose = TRUE, 
              dec = ".", encoding = "UTF-8", showProgress = TRUE, data.table = TRUE)
    }
    return(x)
  })

names(list_data) <- dir(path = "Data")
  

list_data  <- list_data [ 
  (1: length(list_data) )[ !  unlist( 
    lapply( X = list_data, FUN = function(x){
      x %>%  nrow() %>%  is.null()    
      }) 
    ) ] ]
list_data  <- list_data [ (1: length(list_data) )[ !  unlist(
  lapply( X = list_data, FUN = function(x){
    ncol(x) != 244
  } ) 
  ) ] ]


# check the number of rows
# x <- list_data %>%  lapply( FUN = function(x){
#   if(nrow(x) != 128){
#     print("1")
#   }
#   print(nrow(x))
# })
# rm(x)
# list_data[[1]] %>%  summary

data <- data.frame()
Times <- data.frame( matrix(data = NA, ncol = 3, nrow =  length( list_data) ) )

for( i in 1: length( list_data)){
  x <- list_data[[i]]
  x$Subject <- 
    names( list_data )[i] %>% gsub( "subject-", "" , . ) %>% gsub( ".csv", "" , . , fixed = TRUE)
  x$target <- x$target %>% as.character() %>% gsub(" ", "", . ) %>%  factor
  x %>% 
    filter( block != "training") %>% 
    select( Subject,	avg_rt, correct, response , correct_response, block,
            response_time, response_time_bienvenida, sexo, target, tipo, total_response_time, height, width, pareja ) -> data_Aux
  print("==================================================================")
  data_Aux$avg_rt <- data_Aux$avg_rt %>% as.character %>%   as.numeric
  data_Aux$response_time <- data_Aux$response_time %>%  as.character %>% as.numeric
  data_Aux$total_response_time <- data_Aux$total_response_time %>%  as.character %>% as.numeric
  data_Aux$response_time_bienvenida <- data_Aux$response_time_bienvenida %>%  as.character %>% as.numeric
  

  (data_Aux$total_response_time[ nrow(data_Aux)]/100/60) %>% round(2) %>% paste( "Task Time", . ,"minits") %>% print
  Times[ i, 1] <- data_Aux$Subject[1]
  
  Times[ i, 2] <- (data_Aux$total_response_time[ nrow(data_Aux)]/100/60) %>% round(2) 
  data_Aux$total_response_time <- rm()
  print("==================================================================")
  (data_Aux$response_time_bienvenida[ nrow(data_Aux)]/100/60) %>% round(2) %>% paste("Welcome Time",. ,"minits") %>% print
  Times[ i, 3] <- (data_Aux$response_time_bienvenida[ nrow(data_Aux)]/100/60) %>% round(2) 
  data_Aux$response_time_bienvenida <- rm()
  print("==================================================================")
  if( nrow(data) != 0){
    data <- rbind( data, data_Aux)
  }else{
    data <-data_Aux
  }
  
}
names(Times) <- c( "Subject","Task_time", "Instructions_time")
# rm(x, data_Aux, i, list_data, list_data_files);gc()
# E read data -------------------------------------------------------------


# S Checks ----------------------------------------------------------------
# summary(data)
data$height <-rm()
data$width <- rm()
data$tipo <-data$tipo %>% factor() 
data$tipo %>% table
data$tipo[data$tipo == "SEX"] <- "SEXO"
data$tipo <- data$tipo %>%  factor
data$target %>%  table
data$target <- data$target %>%  factor
data$correct <- data$correct %>%  as.character %>% as.numeric
# E Checks ----------------------------------------------------------------


# S save ------------------------------------------------------------------
# save(data, file = "RData/00_read_prepare_data.RData")
# E save ------------------------------------------------------------------

Times
