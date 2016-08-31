rm(list = ls());gc()

# S Basics ----------------------------------------------------------------
if(! require("rjson")){
  install.packages("rjson")
}
library("rjson")

list_script <- fromJSON(readLines(con = "Data/00_Initial.json") ) 

lapply(X = list_script$needed_script, FUN = function(x){
  print(x)
  if( !x %in% installed.packages() ){
    install.packages(x)
  }
  library(x, character.only = TRUE)
})

setwd(dir = list_script$dir_script)
# rm(list = ls());gc()

sessionInfo()
Sys.info()
# E Basics ----------------------------------------------------------------


# S load data -------------------------------------------------------------
load("RData/00_read_prepare_data.RData")
# E load data -------------------------------------------------------------


# S Prepare data ----------------------------------------------------------
data$block2 <- ifelse(test = data$block %in% c(1, 3), yes = "Sex-FB", no = "Help-FB") %>%  factor
data$Subject <- data$Subject %>%  as.character %>%  gsub("(1)", "", ., fixed = TRUE) %>% 
  gsub("(2)", "", ., fixed = TRUE) %>%  factor # Clean the repeatd files. Looks better in this way.
# Check the correctnes of the blocks 
# The following rule is correct? Yes OK
# ifelse(test = data$block %in% c(1, 3), yes = "Sex-FB", no = "Help-FB") %>%  factor
# n <- 1
# block <- 4
# data %>% filter(block == block & tipo == "SEXO") %>%  head(n) %>%  select(correct_response, block2)
# data %>% filter(block == block & tipo == "HELP") %>%  head(n) %>%  select(correct_response, block2)
# data %>% filter(block == block & tipo == "FACEBOOK") %>%  head(n) %>%  select(correct_response, block2)
# table(data$correct_response, data$tipo, data$block2) # Tiene que estar pareado OK
# rm(n, block);gc()
# table(data$sexo) / (nrow(data) / length( unique( data$Subject)) ) 
# (nrow(data) / length( unique( data$Subject)) )  Computes the number of trials
# Particpants
# Mujer Hombre 
# 80     59 
# (nrow(data) / length( unique( data$Subject)) )  Computes the number of trials

number_trial_per_block <- nrow(data) / length( unique( data$Subject)) / 4 # Is divided by four because we have four blocks
data$trial_order <- 1: number_trial_per_block  
rm( number_trial_per_block);gc()

# Check that the firs is the change block trial
# data[ c( FALSE, data$block2[1: (nrow(data) -1  )] == data$block2[2: (nrow(data)   )] ) , c("trial_order")] %>%  table
# data[ c( FALSE, data$block2[1: (nrow(data) -1  )] != data$block2[2: (nrow(data)   )] ) , c("trial_order")] %>%  table

# Check the firs trial hipothesis
# mean( data$response_time[  data$trial_order == 1] ) /
#   mean( data$response_time[  data$trial_order != 1] ) # Los primeros trial son 46% más lentos que el resto
# x <- by(data= data$response_time, INDICES = data$Subject, function(x){( x - mean(x) ) / sd(x) }  ) %>% unlist
# seleccion_uno  <- ifelse( data$trial_order == 1, yes = "uno", no = "no")  == "uno"
# by ( x[seleccion_uno ] ,  data$Subject[seleccion_uno ], mean) %>% mean
# by ( x[seleccion_uno ] ,  data$Subject[seleccion_uno ], mean) %>% sd
# 
# seleccion_no  <- ifelse( data$trial_order == 1, yes = "uno", no = "no")  == "no"
# by ( x[seleccion_no ] ,  data$Subject[seleccion_no ], mean) %>% mean
# by ( x[seleccion_no ] ,  data$Subject[seleccion_no ], mean) %>% sd
#  rm( number_trial_per_block );gc()

# Select the data table without the first trial
dataDT <- data[ data$trial_order != 1, c("Subject", "correct", "block", "sexo", "target", "tipo", "block2", "response_time")] %>% 
  data.table
dataDT %>%  summary

######################################
#### OUT data checks
######################################
# by( data = dataDT$response_time, INDICES = dataDT$Subject, hist)
# dataDT$correct %>% table %>%  prop.table
# (dataDT$response_time <= list_script$min_RT ) %>%  table %>%  prop.table # Miramos los que tienen menos 300 msec
# (dataDT$response_time >= list_script$max_RT ) %>%  table %>%  prop.table # Miramos los que tienen mas 3000 msec

dataDTcorrect <- dataDT[ dataDT$correct == 1, ] # Only the correct answer
# (dataDTcorrect$response_time <= list_script$min_RT ) %>%  table %>%  prop.table # Miramos los que tienen menos 300 msec
# (dataDTcorrect$response_time >= list_script$max_RT ) %>%  table %>%  prop.table # Miramos los que tienen mas 3000 msec

######################################
#### Compute range matrix
######################################
Ranges <- dataDTcorrect$response_time %>%  by(data = ., INDICES = dataDTcorrect$Subject, FUN = function(x){ 
  list( min = median(x) - list_script$IQR_prod * IQR(x) , max = median(x)  + list_script$IQR_prod * IQR(x))
  }) # Compute the ranges

Ranges <- 
  data.frame( 
    Subject = NA,
    min = Ranges %>% lapply(FUN = function(x){
      x$min
      }) %>% unlist ,
    max = Ranges %>% lapply(FUN = function(x){
      x$max
      }) %>%  unlist )
Ranges$Subject <- 
  row.names(Ranges)  %>%  factor 
row.names(Ranges) <- rm()

######################################
#### Out people with less than 80% ratio
######################################
CheckSubjects <- dataDT[, list( Subject = Subject[1], sexo = sexo[1], mean_correct = mean(correct)),
                        by = paste(dataDT$Subject, dataDT$block) ]
CheckSubjects$Subject[ CheckSubjects$mean_correct < list_script$Ratio_response_Block ] %>% table
CheckSubjects[ CheckSubjects$mean_correct < list_script$Ratio_response_Block ,]
SubjectsFailAccuracyRatio <- 
  CheckSubjects$Subject[ CheckSubjects$mean_correct < list_script$Ratio_response_Block] %>% unique

# Table for computing the bad responders
# Numero de hombres y de mujeres que no cumplen el 80% correctas en la tarea
# SubjectsOutDT$sexo %>%  table 
# Mujer Hombre 
# 9      9 
# SubjectsOutDT
# Subject   sexo
# 1:     102 Hombre
# 2:     108 Hombre
# 3:     110 Hombre
# 4:     118 Hombre
# 5:     120 Hombre
# 6:     128 Hombre
# 7:     141 Hombre
# 8:      28  Mujer
# 9:       2 Hombre
# 10:      31  Mujer
# 11:      39  Mujer
# 12:      41  Mujer
# 13:      55  Mujer
# 14:      69  Mujer
# 15:      76 Hombre
# 16:      77  Mujer
# 17:      83  Mujer
# 18:      96  Mujer
# E Prepare data ----------------------------------------------------------



# S filters ---------------------------------------------------------------
######################################
#### Selected Per Study
######################################
set.seed(123456)
Subject_2_Study <- c( dataDTcorrect$Subject[ dataDTcorrect$sexo == "Mujer" &  
                                               !(dataDTcorrect$Subject %in% SubjectsFailAccuracyRatio)] %>%
                        unique %>%  as.character %>% 
                        sample( size = list_script$n_per_sex_women, replace = FALSE) ,
                      dataDTcorrect$Subject[ dataDTcorrect$sexo == "Hombre" & 
                                               !(dataDTcorrect$Subject %in% SubjectsFailAccuracyRatio)] %>%
                        unique %>% as.character  %>% 
     sample( size = list_script$n_per_sex_men, replace = FALSE)
)
dataDTcorrect$Subject_2_Study <- ifelse( test = dataDTcorrect$Subject %in% Subject_2_Study, 
                                         yes = 1, no = 0)
######################################
#### 300 msec
######################################
dataDTcorrect$rt_minor_300 <- 
  ifelse(test = dataDTcorrect$response_time < list_script$min_RT, yes = 1, no = 0)

######################################
#### 3000 msec
######################################
dataDTcorrect$rt_bigger_3000 <- 
  ifelse(test = dataDTcorrect$response_time > list_script$max_RT, yes = 1, no = 0)

######################################
#### Inside the range
######################################
dataDTcorrect <- merge( x = dataDTcorrect , y = Ranges, by = "Subject", all.x = TRUE, all.y = FALSE)
dataDTcorrect$Out_IQR <- ifelse(test = dataDTcorrect$response_time < dataDTcorrect$min | dataDTcorrect$response_time > dataDTcorrect$max,
       yes = 1, no =  0 )
# E filters ---------------------------------------------------------------






# S selection -------------------------------------------------------------

if( list_script$Subject_2_Study == TRUE){
  nrow(dataDTcorrect) %>% print
  dataDTcorrect <- dataDTcorrect[ dataDTcorrect$Subject_2_Study == 1, ]
  nrow(dataDTcorrect) %>% print
}

if( list_script$Out_rt_300_3000 == TRUE){ 
  (dataDTcorrect$rt_minor_300 != 1 &
    dataDTcorrect$rt_bigger_3000 != 1) %>%  table %>% print
    dataDTcorrect <- dataDTcorrect[ dataDTcorrect$rt_minor_300 != 1 &
                                    dataDTcorrect$rt_bigger_3000 != 1, ]
}


if( list_script$Out_IQR == TRUE){ 
  (dataDTcorrect$Out_IQR != 1 )%>%  table %>% print
  dataDTcorrect <- dataDTcorrect[ dataDTcorrect$Out_IQR != 1, ]
}
# E selection -------------------------------------------------------------


# S Prepare Matrix --------------------------------------------------------

dataDT_ez <- dcast( formula = Subject ~  block2 + tipo ,data = dataDTcorrect , fun = function(x){
  mean(x, na.rm = TRUE)
  } , value.var="response_time")
dataDT_ez <- melt(data = dataDT_ez, id.vars="Subject")
dataDT_ez$Block <- dataDT_ez$variable %>%  as.character %>%  strsplit( "_") %>% 
  lapply(FUN = function(x){x[1]}) %>%  unlist
dataDT_ez$Type <- dataDT_ez$variable %>%  as.character %>%  strsplit( "_") %>%
  lapply(FUN = function(x){x[2]}) %>%  unlist


dataSubjectSexo <- 
  dataDT[ , list(Sex = sexo[1]), by =Subject ]
dataDT_ez <- merge(x = dataDT_ez, y = dataSubjectSexo, by = "Subject", all.x = TRUE, all.y = FALSE )
dataDT_ez$Block <- dataDT_ez$Block %>%  factor
# Numero de elemetnos por condicion
# (dcast( formula = Subject ~  block2 + tipo ,data = dataDTcorrect , fun = function(x){
#   length(x)
# } , value.var="response_time")[, -1, with = FALSE]< 7 ) %>%  table
# dcast( formula = Subject ~  block2 + tipo ,data = dataDTcorrect , fun = function(x){
#   length(x)
# } , value.var="response_time")

######################################
#### Checkeo el suejto 102 falla todas en la condición  data$block2 == "Help-FB" & data$tipo == "FACEBOOK"
######################################
# data[ data$Subject == 102 & data$block2 == "Help-FB" , ]
# Ranges[ Ranges$Subject ==  102,]
# 
# 
# dataDTcorrect[ dataDTcorrect$Subject == 102, c("tipo",  "block2")]
# data[ data$Subject == 102, c("tipo",  "block2")]
# data[ data$Subject == 102, c("tipo")] %>%  paste( data[ data$Subject == 102, c(  "block2")]) %>%  table
# data[ data$Subject == 102 & data$block2 == "Help-FB" & data$tipo == "FACEBOOK",]
# dataDT[ dataDT$Subject == 102 & dataDT$block2 == "Help-FB" & dataDT$tipo == "FACEBOOK",]
# CheckSubjects[ CheckSubjects$Subject == 102, ]

# E Prepare Matrix --------------------------------------------------------




save(dataDT_ez, file = "RData/03_Prepare_EzAnova.RData")
CheckSubjects
SubjectsFailAccuracyRatio
Subject_2_Study