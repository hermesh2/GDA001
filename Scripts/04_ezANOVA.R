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
490
dataDT_ez_Anova <- dataDT_ez[  dataDT_ez$variable != "Help-FB_SEXO" &
                                 dataDT_ez$variable != "Sex-FB_HELP" ,  ]
ezANOVA(
  data = dataDT_ez_Anova
  , dv = list("value")
  , wid = .("Subject")
  , within = .("Block")
  # , between = .("Sex")
)
# E Anova -----------------------------------------------------------------

sexo <- "Hombre"
t.test( dataDT_ez_Anova$value[dataDT_ez_Anova$Sex == sexo] ~ dataDT_ez_Anova$Block[dataDT_ez_Anova$Sex == sexo]  )
t.test( dataDT_ez_Anova$value ~ dataDT_ez_Anova$Block )
          