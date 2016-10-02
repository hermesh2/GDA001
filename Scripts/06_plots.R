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
