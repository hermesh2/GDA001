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
  dataDT_ez$measure == list_script$final_var_anova &
    dataDT_ez$variable != "Help-FB_SEXO" &
    dataDT_ez$variable != "Sex-FB_HELP" ,  ]

ezANOVA(
  data = dataDT_ez_Anova
  , dv = list("value")
  , wid = .("Subject")
  , within = .("Block")
  , between = .("Sex")
) %>% print
# E Anova -----------------------------------------------------------------

# sexo <- "Hombre"
# t.test( dataDT_ez_Anova$value[dataDT_ez_Anova$Sex == sexo] ~ dataDT_ez_Anova$Block[dataDT_ez_Anova$Sex == sexo]  )
# t.test( dataDT_ez_Anova$value ~ dataDT_ez_Anova$Block )
# 
# dataDT_ez_Anova$value
# lmod <- lmer(formula = value ~ Block * Sex+ (1| Subject) ,verbose = TRUE,  data = dataDT_ez_Anova)
# # Error in pvals.fnc(lmod, nsim = 10^4) : 
# #   MCMC sampling is no longer supported by lme4.
# # For p-values, use the lmerTest package, which provides
# # functions summary() and anova() which give p-values of
# # various kinds.
# lmod %>%  summary
# lmod %>%  anova
# 
# 
# by(  data = dataDT_ez_Anova$value, dataDT_ez_Anova$variable, mean)
# 
# by(  data = dataDT_ez_Anova$value, dataDT_ez_Anova$Block, mean)
# # 
# by(  data = dataDT_ez_Anova$value, paste(dataDT_ez_Anova$Sex, dataDT_ez_Anova$Block), mean)
# by(  data = dataDT_ez$value, paste(dataDT_ez$Sex, dataDT_ez$Block, dataDT_ez$Type), mean)
# 
# dataDT_ez
by(  data = dataDT_ez_Anova$value, paste(dataDT_ez_Anova$Sex, dataDT_ez_Anova$Block), mean)


( mean( dataDT_ez_Anova$value[ dataDT_ez_Anova$Block == "Sex-FB" ]   )  -
  mean( dataDT_ez_Anova$value[ dataDT_ez_Anova$Block == "Help-FB" ]   ) ) / 
    sd( dataDT_ez_Anova$value   ) %>% print

( mean( dataDT_ez_Anova$value[ dataDT_ez_Anova$Block == "Sex-FB" &  dataDT_ez_Anova$Sex == "Hombre"]   )  -
  mean( dataDT_ez_Anova$value[ dataDT_ez_Anova$Block == "Help-FB" &  dataDT_ez_Anova$Sex == "Hombre"]   ) ) / 
  sd( dataDT_ez_Anova$value [dataDT_ez_Anova$Sex == "Hombre"]  )  %>% print

( mean( dataDT_ez_Anova$value[ dataDT_ez_Anova$Block == "Sex-FB" &  dataDT_ez_Anova$Sex == "Mujer"]   )  -
  mean( dataDT_ez_Anova$value[ dataDT_ez_Anova$Block == "Help-FB" &  dataDT_ez_Anova$Sex == "Mujer"]   ) ) / 
  sd( dataDT_ez_Anova$value [dataDT_ez_Anova$Sex == "Mujer"]  )  %>% print
