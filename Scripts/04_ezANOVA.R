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
load("RData/03_Prepare_EzAnova.RData")
# E load data -------------------------------------------------------------



# S Anova -----------------------------------------------------------------
dataDT_ez$variable <- dataDT_ez$variable %>%  as.character %>%  gsub("_z", "", x = .) %>%  gsub("_log", "", x = .) %>%  factor
dataDT_ez_Anova <- dataDT_ez[  
  dataDT_ez$measure == list_script$measure &
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
dataDT_ez_D <- dataDT_ez[
  dataDT_ez$measure == "D" ,  ]

by(data = dataDT_ez_D$value, INDICES =  paste0( dataDT_ez_D$Sex, dataDT_ez_D$Couple), FUN = mean  )
by(data = dataDT_ez_Anova$value, INDICES =  paste0( dataDT_ez_Anova$Sex, dataDT_ez_Anova$Couple , dataDT_ez_Anova$Block), FUN = mean  )

