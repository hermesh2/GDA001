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

sessionInfo()
Sys.info()
# E Basics ----------------------------------------------------------------


# S load data -------------------------------------------------------------
load("RData/00_read_prepare_data.RData")
data$target <- paste(data$target, data$block, sep = "-")
# E load data -------------------------------------------------------------


# S change the format to with ---------------------------------------------
# We change the format for the rt analisys by item
data <- data %>% data.table()
data[ ,Subject := factor( Subject)]

####### By Subject
dataWideSubject <-
  data[ data$correct == 1,
        list(
          RT = mean(response_time),
          SD = sd(response_time),
          Accuracy = mean(correct),
          Sex = sexo[1]
        ),
        by = Subject]

dataWideSubject_RTError <-
  data[ data$correct == 0,
        list(
          RT_Error = mean(response_time),
          SD_ERROR_SD = sd(response_time)
        ),
        by = Subject]

dataWideSubject <- merge(x = dataWideSubject, y = dataWideSubject_RTError, by = "Subject", all.x = TRUE, all.y = FALSE)
dataWideSubject

###### By type
dataWideItem <-
  data[ data$correct == 1,
        list(
          RT = mean(response_time),
          SD = sd(response_time),
          Accuracy = mean(correct), 
          tipo = tipo[1]
          
        ),
        by = target]

dataWideItem_RTError <-
  data[ data$correct == 0,
        list(
          RT_Error = mean(response_time, na.rm = TRUE),
          SD_ERROR_SD = sd(response_time)
        ),
        by = target]

dataWideItem <- merge(x = dataWideItem, y = dataWideItem_RTError, by = "target", all.x = TRUE, all.y = FALSE)
dataWideItem


##### By Item Men 
dataWideItem_Men <-
  data[ data$sexo == "Hombre" & data$correct == 1,
        list(
          RT = mean(response_time),
          SD = sd(response_time),
          Accuracy = mean(correct), 
          tipo = tipo[1]
          
        ),
        by = target]

dataWideItem_Men_RTError <-
  data[ data$sexo == "Hombre" & data$correct == 0,
        list(
          RT_Error = mean(response_time, na.rm = TRUE),
          SD_ERROR_SD = sd(response_time)
        ),
        by = target]

dataWideItem_Men <- merge(x = dataWideItem_Men, y = dataWideItem_Men_RTError, by = "target", all.x = TRUE, all.y = FALSE)
dataWideItem_Men

###### By Item Women
dataWideItem_Women <-
  data[ data$sexo == "Mujer" & data$correct == 1,
        list(
          RT = mean(response_time),
          SD = sd(response_time),
          Accuracy = mean(correct), 
          tipo = tipo[1]
          
        ),
        by = target]

dataWideItem_Women_RTError <-
  data[ data$sexo == "Mujer" & data$correct == 0,
        list(
          RT_Error = mean(response_time, na.rm = TRUE),
          SD_ERROR_SD = sd(response_time)
        ),
        by = target]

dataWideItem_Women <- merge(x = dataWideItem_Women, y = dataWideItem_Women_RTError, by = "target", all.x = TRUE, all.y = FALSE)
dataWideItem_Women


# E change the format to with ---------------------------------------------

# S plot ------------------------------------------------------------------
dat <- dataWideItem_Men

dat$target2 <- paste(dat$tipo %>% abbreviate(minlength = 1), dat$target, sep = ".")  
p <- ggplot(dat, aes(x= target2, y= RT, colour= tipo)) + 
  geom_errorbar(aes(ymin=RT-SD, ymax=RT+SD), width=.1) +
  geom_line() +
  geom_point()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) +
  ggplot2::ggtitle("Men RT")
p
jpeg(filename = "Results/Men_RT.jpeg")
p
dev.off()


p <-ggplot(dat, aes(x= target2, y= Accuracy, colour= tipo)) +
  geom_line() +
  geom_point()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) +
  ggplot2::ggtitle("Men Accuracy")
p
jpeg(filename = "Results/Men_Accuracy.jpeg")
p
dev.off()


p <-ggplot(dat, aes(x= target2, y= RT_Error, colour= tipo)) + 
  geom_errorbar(aes(ymin= RT_Error  -SD_ERROR_SD, ymax= RT_Error +SD_ERROR_SD), width=.1) +
  geom_line() +
  geom_point()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) +
  ggplot2::ggtitle("Men RT ERROR")
p
jpeg(filename = "Results/Men_RT_ERROR.jpeg")
p
dev.off()










dat <- dataWideItem_Women

dat$target2 <- paste(dat$tipo %>% abbreviate(minlength = 1), dat$target, sep = ".")  
p <-ggplot(dat, aes(x= target2, y= RT, colour= tipo)) + 
  geom_errorbar(aes(ymin=RT-SD, ymax=RT+SD), width=.1) +
  geom_line() +
  geom_point()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) +
  ggplot2::ggtitle("Women RT")
p
jpeg(filename = "Results/Women_RT.jpeg")
p
dev.off()


p <-ggplot(dat, aes(x= target2, y= Accuracy, colour= tipo)) + 
  geom_line() +
  geom_point()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) +
  ggplot2::ggtitle("Women Accuracy")
p
jpeg(filename = "Results/Women_Accuracy.jpeg")
p
dev.off()

p <-ggplot(dat, aes(x= target2, y= RT_Error, colour= tipo)) + 
  geom_errorbar(aes(ymin= RT_Error  -SD_ERROR_SD, ymax= RT_Error +SD_ERROR_SD), width=.1) +
  geom_line() +
  geom_point()+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) +
  ggplot2::ggtitle("Women RT ERROR")
p
jpeg(filename = "Results/Women_RT_ERROR.jpeg")
p
dev.off()

# E plot ------------------------------------------------------------------

write.table(x = dataWideSubject, sep = ",", dec = ".", row.names = FALSE, fileEncoding = "UTF-8", file =  "Results/dataSubject.csv")

dat_Aux <- dataWideItem_Women$target %>% strsplit(split = "-") %>% unlist() %>% matrix(ncol = 2, byrow = 2) %>% data.frame()
names(dat_Aux) <- c("Target", "Block")
dataWideItem_Women <-cbind( dataWideItem_Women, dat_Aux)
write.table(x = dataWideItem_Women, sep = ",", dec = ".", row.names = FALSE, fileEncoding = "UTF-8", file =  "Results/data_Item_Men.csv")

dat_Aux <- dataWideItem_Men$target %>% strsplit(split = "-") %>% unlist() %>% matrix(ncol = 2, byrow = 2) %>% data.frame()
names(dat_Aux) <- c("Target", "Block")
dataWideItem_Men <-cbind( dataWideItem_Women, dat_Aux)
write.table(x = dataWideItem_Men, sep = ",", dec = ".", row.names = FALSE, fileEncoding = "UTF-8", file =  "Results/data_Item_Women.csv")
