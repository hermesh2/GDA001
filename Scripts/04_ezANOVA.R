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
load("RData/03_Prepare_EzAnova.RData")
# E load data -------------------------------------------------------------



# S Anova -----------------------------------------------------------------
dataDT_ez$variable <- dataDT_ez$variable %>%  as.character %>%  gsub("_z", "", x = .) %>%  gsub("_log", "", x = .) %>%  factor
dataDT_ez_Anova <- dataDT_ez[  
  dataDT_ez$measure == list_script$measure &
    dataDT_ez$variable != "D" &
    dataDT_ez$variable != "Help-FB_SEXO" & # Esto se puede mantener aunque sea la D
    dataDT_ez$variable != "Sex-FB_HELP" &
    dataDT_ez$variable != "Sex-FB_HELP_NoInterf",  ]

ezANOVA(
  data = dataDT_ez_Anova
  , dv = list("value")
  , wid = .("Subject")
  , within = .("Block")
  , between = .("Sex", "Couple")
) %>% print


dataDT_ez_D <- dataDT_ez[
  dataDT_ez$measure == "D" ,  ]
ezANOVA(
  data = dataDT_ez_D
  , dv = list("value")
  , wid = .("Subject")
  # , within = .("Block")
  , between = .("Sex", "Couple")
) %>% print
# E Anova -----------------------------------------------------------------

# by(  data = dataDT_ez_Anova$value, paste(dataDT_ez_Anova$Sex, dataDT_ez_Anova$Block), mean)
# 

# S Comparison ------------------------------------------------------------
dataDT_ez_D <- dataDT_ez[
  dataDT_ez$measure == "D" ,  ]

by(data = dataDT_ez_D$value, INDICES =  paste0( dataDT_ez_D$Sex, dataDT_ez_D$Couple), FUN = mean  )
by(data = dataDT_ez_Anova$value, INDICES =  paste0( dataDT_ez_Anova$Sex, dataDT_ez_Anova$Couple , dataDT_ez_Anova$Block), FUN = mean  )
# E Comparison ------------------------------------------------------------


# S plots -----------------------------------------------------------------
plotMeanCh(response = dataDT_ez_Anova$value,
           factor2 = dataDT_ez_Anova$Block %>%  as.character %>% factor,  
           factor1 = 
             paste(dataDT_ez_Anova$Sex, dataDT_ez_Anova$Couple, sep = "-") %>%  as.character %>% factor,
           error.bars = "se", pch = 15:16 , lty = 1, xlab = "Gender-Couple", ylab = paste(list_script$measure, "RT"),
           main = "Reaction times (log(RT) msecs" )
grid()

dataDT_ez_Anova$value %>%  as.character %>% factor %>%  length()
paste(dataDT_ez_Anova$Sex, dataDT_ez_Anova$Couple, sep = "-")%>%  as.character %>% factor %>%  length()

plotMeanCh(response = dataDT_ez_D$value, 
           factor1 =dataDT_ez_D$Sex, factor2 =  dataDT_ez_D$Couple,
           error.bars = "se", pch = 15:16 , lty = 1, xlab = "Gender", ylab = "D-measuere",
           main = "D-Measure" )
grid()
# E plots -----------------------------------------------------------------


# S t.test ----------------------------------------------------------------
t.test.Comparison.Function.Ch(data = dataDT_ez_Anova %>% data.frame, StringResponse = "value", StringFactor = "Sex")
t.test.Comparison.Function.Ch(data = dataDT_ez_Anova %>% data.frame, StringResponse = "value", StringFactor = "Block")
t.test.Comparison.Function.Ch(data = dataDT_ez_Anova %>% data.frame, StringResponse = "value", StringFactor = "Couple")



dataDT_ez_Anova$Factor <- paste( dataDT_ez_Anova$Sex, dataDT_ez_Anova$Block)
t.test.Comparison.Function.Ch(data = dataDT_ez_Anova %>% data.frame, StringResponse = "value", StringFactor = "Factor")

dataDT_ez_Anova$Factor <- paste( dataDT_ez_Anova$Sex, dataDT_ez_Anova$Couple)
t.test.Comparison.Function.Ch(data = dataDT_ez_Anova %>% data.frame, StringResponse = "value", StringFactor = "Factor")

dataDT_ez_Anova$Factor <- paste( dataDT_ez_Anova$Sex, dataDT_ez_Anova$Block)
t.test.Comparison.Function.Ch(data = dataDT_ez_Anova %>% data.frame, StringResponse = "value", StringFactor = "Factor")

dataDT_ez_Anova$Factor <- paste( dataDT_ez_Anova$Couple, dataDT_ez_Anova$Block)
t.test.Comparison.Function.Ch(data = dataDT_ez_Anova %>% data.frame, StringResponse = "value", StringFactor = "Factor")

dataDT_ez_Anova$Factor <- paste( dataDT_ez_Anova$Sex , dataDT_ez_Anova$Couple, dataDT_ez_Anova$Block)
t.test.Comparison.Function.Ch(data = dataDT_ez_Anova %>% data.frame, StringResponse = "value", StringFactor = "Factor")



t.test.Comparison.Function.Ch(data = dataDT_ez_D %>% data.frame, StringResponse = "value", StringFactor = "Sex")
t.test.Comparison.Function.Ch(data = dataDT_ez_D %>% data.frame, StringResponse = "value", StringFactor = "Couple")

dataDT_ez_D$Factor <- paste( dataDT_ez_D$Sex, dataDT_ez_D$Couple)
t.test.Comparison.Function.Ch(data = dataDT_ez_D %>% data.frame, StringResponse = "value", StringFactor = "Factor")





