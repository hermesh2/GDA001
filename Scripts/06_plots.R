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



# S Prepare Data ----------------------------------------------------------
dataDT_ez_Anova$Sex %>% as.numeric %>% factor(labels = c("W", "M") ) %>%  table(dataDT_ez_Anova$Sex)
dataDT_ez_Anova$Sex <- dataDT_ez_Anova$Sex %>% as.numeric %>% factor(labels = c("W", "M") )

dataDT_ez_Anova$Couple %>% as.numeric %>% factor(labels = c("NR", "R") ) %>%  table(dataDT_ez_Anova$Couple)
dataDT_ez_Anova$Couple <- dataDT_ez_Anova$Couple %>% as.numeric %>% factor(labels = c("NR", "R") )
# E Prepare Data ----------------------------------------------------------

# S plots -----------------------------------------------------------------
if( list_script$save_plots_06 == TRUE){
  tiff(filename = "Results/OliveiraFig1.tiff", width = 20, height = 15, units = "cm", res = 1600 )
}

par( las = 1 )
plotMeanCh(response = dataDT_ez_Anova$value, 
           Xlim = c(0.75, 4.25), cex.leg = 1, position.leg = "topleft",PLOTBOX = TRUE, 
           factor2 = dataDT_ez_Anova$Block %>%  as.character %>% factor,  
           factor1 = 
             paste(dataDT_ez_Anova$Sex, dataDT_ez_Anova$Couple, sep = "-") %>%  as.character %>% factor,
           error.bars = "se", pch = 15:16 , lty = 1, xlab = "Sex-Relationship Status", ylab = paste(list_script$measure, "RT (ms)"),
           main = "Reaction times" )
grid()
if( list_script$save_plots_06 == TRUE){
  dev.off()
}


# dataDT_ez_Anova$value %>%  as.character %>% factor %>%  length()
# paste(dataDT_ez_Anova$Sex, dataDT_ez_Anova$Couple, sep = "-")%>%  as.character %>% factor %>%  length()
if( list_script$save_plots_06 == TRUE){
  tiff(filename = "Results/D_Measure.tiff", width = 20, height = 10, units = "cm", res = 1600 )
}
par( las = 1, bg= "transparent" )
plotMeanCh(response = dataDT_ez_D$value, 
           factor1 =dataDT_ez_D$Sex, factor2 =  dataDT_ez_D$Couple,
           error.bars = "se", pch = 15:16 , lty = 1, xlab = "Gender", ylab = "D-measuere",
           main = "D-Measure" )
grid()
if( list_script$save_plots_06 == TRUE){
  dev.off()
}
  # E plots -----------------------------------------------------------------
