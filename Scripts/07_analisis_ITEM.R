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



# S Item anlysis ----------------------------------------------------------
Corr_results <- function(data, tipo_char, response_char = "response_time") { 
  eval(  
    parse(text =  
            paste0( 
                "Corr_matrix <- data[ tipo == \"Interf\"  & tipo3 == \"", tipo_char,
                "\",  list(response_char = mean( ", response_char , ") ), by = .(target,Subject )]"  
                )
              ) 
        )
  
  Corr_matrix_2 <- 
    dcast(data = Corr_matrix, formula = Subject ~  target, fun.aggregate = mean, value.var = "response_char")
  
  
  return( list( cor( Corr_matrix_2[ , -1, with =FALSE] ), 
                psych::corr.test(Corr_matrix_2[ , -1, with =FALSE], ci = TRUE, alpha = 0.95  )
                )
  )
}

Corr_results(data = dataDTcorrect[ correct == 1, ], tipo_char = "FACEBOOK")
Corr_results(data = dataDTcorrect[ correct == 1, ], tipo_char = "HELP")
Corr_results(data = dataDTcorrect[ correct == 1, ], tipo_char = "SEXO")

Corr_results(data = dataDTcorrect, tipo_char = "FACEBOOK")
Corr_results(data = dataDTcorrect, tipo_char = "HELP")
Corr_results(data = dataDTcorrect, tipo_char = "SEXO")

Corr_results(data = dataDTcorrect, tipo_char = "FACEBOOK", response_char = "correct")
Corr_results(data = dataDTcorrect, tipo_char = "HELP", response_char = "correct")
Corr_results(data = dataDTcorrect, tipo_char = "SEXO", response_char = "correct")
# E Item anlysis ----------------------------------------------------------



# E mean ------------------------------------------------------------------
t_test_func <- function(data, tipo_char, response_char = "response_time") {
  # data <- dataDTcorrect
  # tipo_char <- "FACEBOOK"
  
  eval(  
    parse(text =  
            paste0( 
              "Mean_matrix <- data[ tipo == \"Interf\"  & tipo3 == \"", tipo_char,
              "\",  list(response_char = mean( ", response_char , ") ), by = .(target,Subject )]"  
            )
    ) 
  )
  # Mean_matrix <- data[ tipo == "Interf" & tipo3 == tipo_char, list(response_time = mean(response_time) ), by = .(target,Subject )]
  dataDT_t_test <- Mean_matrix
  return( list( 
    Mean_matrix[ , list( mean(response_char )) , by = target ], 
    Mean_matrix[ , list( sd(response_char )) , by = target ], 
    t.test.Comparison.Function.Ch(data = dataDT_t_test %>% data.frame, StringResponse = "response_char", StringFactor = "target")[ , -1]
    )
  )
}

t_test_func(data = dataDTcorrect[ correct == 1, ] , tipo_char = "FACEBOOK")
t_test_func(data = dataDTcorrect[ correct == 1, ] , tipo_char = "HELP")
t_test_func(data = dataDTcorrect[ correct == 1, ] , tipo_char = "SEXO")

t_test_func(data = dataDTcorrect , tipo_char = "FACEBOOK")
t_test_func(data = dataDTcorrect , tipo_char = "HELP")
t_test_func(data = dataDTcorrect , tipo_char = "SEXO")

# dataDTcorrect$response_time <- dataDTcorrect$response_time %>%  log
# t_test_func(data = dataDTcorrect , tipo_char = "FACEBOOK")
# t_test_func(data = dataDTcorrect , tipo_char = "HELP")
# t_test_func(data = dataDTcorrect , tipo_char = "SEXO")

t_test_func(data = dataDTcorrect , tipo_char = "FACEBOOK", response_char = "correct")
t_test_func(data = dataDTcorrect , tipo_char = "HELP", response_char = "correct")
t_test_func(data = dataDTcorrect , tipo_char = "SEXO", response_char = "correct")
# E mean ------------------------------------------------------------------



# S plot ------------------------------------------------------------------
plot_mean_bars <- function(dat = dataDTcorrect, tipo_char) {
  dat  <-
    dataDTcorrect[ tipo3 == tipo_char ,
                   list(
                     MEAN = mean(response_time, na.rm = TRUE),
                     SD = sd(response_time)
                   ),
                   by = .(target, tipo3) ]
  dat$tipo <- 
    dat$tipo3
  factor <- ""
  dat$target2 <- paste(dat$tipo %>% abbreviate(minlength = 1), dat$target, sep = ".")  
  p <- ggplot(dat, aes(x= target, y= MEAN, colour= tipo)) + 
    geom_errorbar(aes(ymin=MEAN-SD, ymax=MEAN+SD), width=.1) +
    geom_line() +
    geom_point()+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) +
    ggplot2::ggtitle( paste0( "MEAN" , factor ) )
  p
}


plot_mean_bars_correct <- function(dat = dataDTcorrect, tipo_char) {
  dat  <-
    dataDTcorrect[ tipo3 == tipo_char ,
                   list(
                     MEAN = mean(correct, na.rm = TRUE),
                     SD = sd(correct)
                   ),
                   by = .(target, tipo3) ]
  dat$tipo <- 
    dat$tipo3
  factor <- ""
  dat$target2 <- paste(dat$tipo %>% abbreviate(minlength = 1), dat$target, sep = ".")  
  p <- ggplot(dat, aes(x= target, y= MEAN, colour= tipo)) + 
    geom_errorbar(aes(ymin=MEAN-SD, ymax=MEAN+SD), width=.1) +
    geom_line() +
    geom_point()+
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0)) +
    ggplot2::ggtitle( paste0( "Error" , factor ) )
  return( list( p , dat) )
}

plot_mean_bars(dat = dataDTcorrect[ correct == 1, ], tipo_char = "FACEBOOK")
plot_mean_bars(dat = dataDTcorrect[ correct == 1, ], tipo_char = "HELP")
plot_mean_bars(dat = dataDTcorrect[ correct == 1, ], tipo_char = "SEXO")

plot_mean_bars(dat = dataDTcorrect, tipo_char = "FACEBOOK")
plot_mean_bars(dat = dataDTcorrect, tipo_char = "HELP")
plot_mean_bars(dat = dataDTcorrect, tipo_char = "SEXO")


plot_mean_bars_correct(dat = dataDTcorrect, tipo_char = "FACEBOOK")
plot_mean_bars_correct(dat = dataDTcorrect, tipo_char = "HELP")
plot_mean_bars_correct(dat = dataDTcorrect, tipo_char = "SEXO")
# S plot ------------------------------------------------------------------