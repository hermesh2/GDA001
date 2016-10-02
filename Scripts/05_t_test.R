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
source(file = "Scripts/Functions/PlotMeanCh.R")
source(file = "Scripts/Functions/t.test.Comparison.Function.Ch.R")



sessionInfo()
Sys.info()
# E Basics ----------------------------------------------------------------


# S load data -------------------------------------------------------------
load("RData/04_ezANOVA.RData")
# E load data -------------------------------------------------------------


# S t.test ----------------------------------------------------------------
# dataDT_t_test <-
#   dataDTcorrect[ ,list(  value = mean(log(response_time))  ) , by = .(Subject, sexo)]
# setnames( x = dataDT_t_test, old = "sexo", new = "Sex")
# t.test.Comparison.Function.Ch(data = dataDT_t_test %>% data.frame, StringResponse = "value", StringFactor = "Sex")[ , -1]

dataDT_t_test <-
  dataDTcorrect[ ,list(  value = mean(log(response_time)) ) , by = .(Subject, block2) ]
setnames( x = dataDT_t_test, old = "block2", new = "Block")
t.test.Comparison.Function.Ch(data = dataDT_t_test %>% data.frame, StringResponse = "value", StringFactor = "Block", paired = TRUE)[ , -1]
t.test.Comparison.Function.Ch(data = dataDT_t_test %>% data.frame, StringResponse = "value", StringFactor = "Block")[ , -1]

# dataDT_t_test <-
#   dataDTcorrect[ ,list(  value = mean(log(response_time))  ) , by = .(Subject, pareja) ]
# setnames( x = dataDT_t_test, old = "pareja", new = "Couple")
# t.test.Comparison.Function.Ch(data = dataDT_t_test %>% data.frame, StringResponse = "value", StringFactor = "Couple")[ , -1]



# dataDTcorrect$Factor <- paste( dataDTcorrect$sexo, dataDTcorrect$block2) %>%  as.factor
# dataDT_t_test <-
#   dataDTcorrect[ ,list(  value = mean(log(response_time)) ) , by = .(Subject, Factor) ]
# t.test.Comparison.Function.Ch(data = dataDT_t_test %>% data.frame, StringResponse = "value", StringFactor = "Factor")[ , -1]
# 
# 
# dataDTcorrect$Factor <- paste( dataDTcorrect$sexo, dataDTcorrect$pareja) %>%  as.factor
# dataDT_t_test <-
#   dataDTcorrect[ ,list(  value = mean(log(response_time)) ) , by = .(Subject, Factor) ]
# t.test.Comparison.Function.Ch(data = dataDT_t_test %>% data.frame, StringResponse = "value", StringFactor = "Factor")[ , -1]
# 
# 
# dataDTcorrect$Factor <- paste( dataDTcorrect$sexo, dataDTcorrect$block2) %>%  as.factor
# dataDT_t_test <-
#   dataDTcorrect[ ,list(  value = mean(log(response_time)) ) , by = .(Subject, Factor) ]
# t.test.Comparison.Function.Ch(data = dataDT_t_test %>% data.frame, StringResponse = "value", StringFactor = "Factor")[ , -1]
# 
# 
# dataDTcorrect$Factor <- paste( dataDTcorrect$pareja, dataDTcorrect$block2) %>%  as.factor
# dataDT_t_test <-
#   dataDTcorrect[ ,list(  value = mean(log(response_time)) ) , by = .(Subject, Factor) ]
# t.test.Comparison.Function.Ch(data = dataDT_t_test %>% data.frame, StringResponse = "value", StringFactor = "Factor")[ , -1]


dataDTcorrect$Factor <- paste( dataDTcorrect$sexo, dataDTcorrect$pareja, dataDTcorrect$block2) %>%  as.factor
dataDT_t_test <-
  dataDTcorrect[ ,list(  value = mean(log(response_time)) ) , by = .(Subject, Factor) ]
t.test.Comparison.Function.Ch(data = dataDT_t_test %>% data.frame, StringResponse = "value", StringFactor = "Factor", paired = TRUE)[ , -1]
t.test.Comparison.Function.Ch(data = dataDT_t_test %>% data.frame, StringResponse = "value", StringFactor = "Factor")[ , -1]

# E t.test ----------------------------------------------------------------



# S t test D --------------------------------------------------------------
# dataDT_t_test <-
#   dataDT_ez_D[ ,list(  value = mean(value)  ) , by = .(Subject, Sex)]
# t.test.Comparison.Function.Ch(data = dataDT_ez_D %>% data.frame, StringResponse = "value", StringFactor = "Sex")[ , -1]
# 
# dataDT_t_test <-
#   dataDT_ez_D[ ,list(  value = mean(value)  ) , by = .(Subject, Couple)]
# t.test.Comparison.Function.Ch(data = dataDT_ez_D %>% data.frame, StringResponse = "value", StringFactor = "Couple")[ , -1]


dataDT_ez_D$Factor <- paste( dataDT_ez_D$Sex, dataDT_ez_D$Couple)
dataDT_t_test <-
  dataDT_ez_D[ ,list(  value = mean(value) ) , by = .(Subject, Factor) ]
t.test.Comparison.Function.Ch(data = dataDT_t_test %>% data.frame, StringResponse = "value", StringFactor = "Factor", paired = TRUE)[ , -1]
t.test.Comparison.Function.Ch(data = dataDT_t_test %>% data.frame, StringResponse = "value", StringFactor = "Factor")[ , -1]

?t.test
dataDT_t_test[ , list( D = mean(value)) , by =.( Factor)]
# E t test D --------------------------------------------------------------