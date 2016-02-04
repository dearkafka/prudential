######################################## Prudential_control.R ######################################
SYSG_PROJ_LIB              <- "/home/rstudio/prudentialpj/"
SYSG_SYSTEM_DIR            <- paste0(SYSG_PROJ_LIB,"")
SYSG_INPUT_DIR             <- paste0(SYSG_PROJ_LIB,"input")
SYSG_OUTPUT_DIR            <- paste0(SYSG_PROJ_LIB,"output")
SYSG_OUTPUT_MODELING_DIR   <- paste0(SYSG_PROJ_LIB,"modeling")


source(paste0(SYSG_SYSTEM_DIR,"Prudential_functions.R"))

# P   - run data preparation including read data , prepare modelling , evaluation and prediction data + new features
# DP - Data Preparation , ME - Modelling and Evaluation , P - Prediction and Submission
SYS_RUN_MODE               <- "DP" 

library(readr)
library(stringr)
library(plyr)
library(dplyr)
library(mlr)
library(caret)
library(Metrics)
library(caretEnsemble)
library(Hmisc)


create_log_entry("", "Starting run ....................................","SF")

if (!SYS_RUN_MODE %in% c("DP","ME","P"))
  stop(simpleError("Illegal SYS_RUN_MODE :" , SYS_RUN_MODE))

############################################ DATA PREPARATION ############################################
# Run to prepare data and save data locally
if (SYS_RUN_MODE == "DP") {
  run_data_preparation()
} else if (SYS_RUN_MODE == "ME") {
  run_model_evaluation()
} else if (SYS_RUN_MODE == "P") {
  run_prediction()
} else stop("Unsupported SYS_RUN_MODE")

create_log_entry("","Finished run ....................................","SF")


run_data_preparation <- function()       ################## Data Preparation ##############
{
  source(paste0(SYSG_SYSTEM_DIR,"Prudential_functions.R"))
  create_log_entry("", "Data preparation started .....","SF")
  perform_data_preparation()
  invisible(gc())
  create_log_entry("", "Data preparation finished ..... ","SF")
}

run_mode_evaluation <- function()        ################## Model Evaluation #############
{

SYS_ALGORITHM_ID            <- "XGBC"
# SYS_ALGORITHM_ID              <- "XGBLC"
# SYS_ALGORITHM_ID            <- "GBM"
# SYS_ALGORITHM_ID            <- "GLMNET"
# SYS_ALGORITHM_ID            <- "EMM"
# SYS_ALGORITHM_ID            <- "EMF"

SYS_CS_MODE                  <- "CW" #, "N" , "S"


source(paste0(SYSG_SYSTEM_DIR,"Prudential_functions.R"))
source(paste0(SYSG_SYSTEM_DIR,"modeling_functions.R"))
create_log_entry("", "Model evaluation started .....","SF")
# Prepare model evaluation input data
  create_log_entry("", "Model evaluation load data started","SF")  
  if (exists("me_data"))  rm(me_data)
  setwd(SYSG_SYSTEM_DIR)
  me_data                <- get(load("me_input_data.rda"))
  create_log_entry("","Model evaluation load data finsihed","SF")

# Initialize parallel framework  
    closeAllConnections()
    library(doMC)
    registerDoMC(cores=4)

  ma_model_id             <- paste0("MA_","#",SYS_ALGORITHM_ID,"#",format(Sys.time(), "%Y-%m-%d %H_%M_%S"))
  me_classification_model <- perform_model_assessment(me_data,ma_model_id,SYS_CS_MODE)
  invisible(gc())
  #closeAllConnections()

create_log_entry("", "Model evaluation finished ..... ","SF")

}

source(paste0(SYSG_SYSTEM_DIR,"\\Prudential_functions.R"))
run_prediction <- function()        ################## Prediction #############
{

#   ma_run_id      <- paste0("MA_","#",SYS_ALGORITHM_ID,"#",format(Sys.time(), "%Y-%m-%d %H_%M_%S"))
#   opt_parameters <- NULL
#   # SYS_ALGORITHM_ID = "XGBC"
#   opt_parameters <- data.frame(rbind(opt_parameters,c(2500,10,0.05)))
#   names(opt_parameters) <- c("nrounds","max_depth", "eta")
#   setwd(SYSG_OUTPUT_MODELING_DIR)
#   save(opt_parameters, file = paste0("OM_",ma_run_id,".rda"))
  ma_run_id <- "MA_#XGBC#2016-02-04 18_59_10"

  create_log_entry("", "Starting prediction on data","SF")
  opt_model_id <- paste0("MODEL_","#",SYS_ALGORITHM_ID,"#",format(Sys.time(), "%Y-%m-%d %H_%M_%S"))
  if (exists("me_data"))  rm(me_data)
  setwd(SYSG_SYSTEM_DIR)
  me_data                <- get(load("me_input_data.rda"))
  if (exists("classification_model"))         rm(classification_model)
  setwd(SYSG_OUTPUT_MODELING_DIR)
  classification_model   <- get(load(paste0(ma_run_id,".rda")))
  
  source(paste0(SYSG_SYSTEM_DIR,"Prudential_functions.R"))
  create_p_model(opt_model_id,me_data,classification_model)
  
 
  if (exists("p_data"))                 rm(p_data)
  if (exists("opt_classification_model")) rm(opt_classification_model)
  invisible(gc())
  setwd(SYSG_SYSTEM_DIR)
  p_data                 <- get(load("p_input_data.rda"))
  setwd(SYSG_OUTPUT_MODELING_DIR)
  p_classification_model <- get(load(paste0(opt_model_id,".rda")))
  
  
  prediction_data        <- perform_p_prediction(opt_classification_model, p_data , opt_model_id)
  invisible(gc())
  
  
}









