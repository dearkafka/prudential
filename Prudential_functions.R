
perform_data_understanding <- function()
{

  
library(reshape2)
library(plyr)
library(ggplot2)
  
 dim(train_input)  
 summary(train_input)
 
 # Check dynamic data
 
 summary(train_input[,medical_history_features])
 
 dynamic_data_features <- c(medical_history_features[c(3:9,11:14,16:23,25:31,33:41)],"Response")
 
 plot_input_data            <- train_input[,dynamic_data_features]
 plot_input_data$Response   <- factor(plot_input_data$Response)
 
 # contruct new data
 
 plot_input_data$MHN_6m7m8 <- plot_input_data$Medical_History_6*plot_input_data$Medical_History_7*plot_input_data$Medical_History_8
 plot_input_data$MHN_26m27m28 <- plot_input_data$Medical_History_26*plot_input_data$Medical_History_27*plot_input_data$Medical_History_28

 
 # Plot original data
 medical_history_mean       <- ddply(plot_input_data, .(Response) , numcolwise(median))

 plot_data                   <- melt(medical_history_mean, id.var = "Response")
 
 plot_data$variable          <- str_replace(plot_data$variable, "Medical_History", "MH")
 
 plot_original_data          <- plot_data[!str_detect(plot_data$variable,"MHN"),]
 
 
 p1 <- ggplot(data=plot_original_data, aes(x=variable, y=value, group = Response, colour = Response)) +
   geom_line() + geom_point( size=4, shape=21, fill="white")
 
 p1
 
 plot_new_data              <- plot_data[str_detect(plot_data$variable,"MHN"),]
 
 p2 <- ggplot(data=plot_new_data, aes(x=variable, y=value, group = Response, colour = Response)) +
   geom_line() + geom_point( size=4, shape=21, fill="white")
 
 p2
 
 

 # Diagnostic plot
 
 
 
 
}

perform_data_preparation <- function()
{
  library(readr)
  library(stringr)
  library(dplyr)
  
  #READ TRAIN DATA
  setwd(SYSG_INPUT_DIR)
  train_input                 <- read_csv("train.csv")
  test_input                  <- read_csv("test.csv")
  
  SYS_TARGET_NAME            <- "Response"
  
  ############# Processing train data input  ###############################
  
  create_log_entry("", "Prepare train data started","SF")
  
  employment_info_features         <- names(train_input)[str_detect(names(train_input), "^Employment_Info")]
  insured_info_features            <- names(train_input)[str_detect(names(train_input), "^InsuredInfo")]
  insurance_history_features       <- names(train_input)[str_detect(names(train_input), "^Insurance_History")]
  family_hist_features             <- names(train_input)[str_detect(names(train_input), "^Family_Hist")]
  
  medical_history_features         <- names(train_input)[str_detect(names(train_input), "^Medical_History")]
  medical_history_features_dropped <- c("Medical_History_10","Medical_History_24","Medical_History_32")
  medical_history_features_missing <- c("Medical_History_1","Medical_History_15")
  medical_history_features         <- setdiff(medical_history_features,c(medical_history_features_dropped))
  
  medical_keyword_features         <- names(train_input)[str_detect(names(train_input), "^Medical_Keyword")]

  features_select <- c("Product_Info_1","Product_Info_2","Product_Info_3","Product_Info_4",
                       "Product_Info_5","Product_Info_6", "Product_Info_7",
                       "Ins_Age","Ht","Wt","BMI",
                        employment_info_features,
                        insured_info_features, 
                        insurance_history_features , 
                        family_hist_features, 
                        medical_history_features,
                        medical_keyword_features)
  
  # Assume same order and quantity of rows
  
  me_input_target_data     <- train_input$Response
  train_input1             <- data.frame(train_input[,features_select])
  
  vbef_data                <- create_vbfe(train_input1,medical_keyword_features)
  
  train_input2             <- data.frame(train_input1,vbef_data)
  
  replace_origin_features  <- c("Medical_History_6","Medical_History_7","Medical_History_8",
                                "Medical_History_26","Medical_History_27","Medical_History_28")
    
  process_missing_features <- c(employment_info_features,"Insurance_History_5",family_hist_features,
                                medical_history_features_missing,"Family_Hist_2_3")
  
  new_missing_data <- process_missing_data(train_input2[,process_missing_features])
  
  # replace_features <- c(replace_origin_features,process_missing_features)
  
  replace_features <- c(process_missing_features)
  
  train_input3 <- data.frame(train_input2[,!(names(train_input2) %in% replace_features)],
                             new_missing_data,
                             Response=me_input_target_data)
  
  me_input_data <- data.frame(train_input3)

  create_log_entry("", "Prepare train data finished","SF")
  
  # summary(me_input_data)
  # str(me_input_data)
  
  ########### Processing test data input   #################################### 
  create_log_entry("", "Prepare test data started","SF")
  # summary(test_input)
  # str(test_input)
  
  vbef_data             <- create_vbfe(test_input,medical_keyword_features)
  
  test_input1           <- data.frame(test_input,vbef_data)
  
  new_missing_data      <- process_missing_data(test_input1[,process_missing_features])
  
  test_input2           <- data.frame(test_input1[,!(names(test_input1) %in% process_missing_features)],
                                      new_missing_data)

  p_features_select     <- c("Id", setdiff(names(me_input_data),c("Response")))
  p_input_data          <- data.frame(test_input2[,p_features_select])
  
  create_log_entry("", "Prepare test data finished","SF")
  
  for (f in names(me_input_data)) {
    if (class(me_input_data[[f]])=="character") {
      levels              <- unique(c(as.character(me_input_data[[f]]), as.character(p_input_data[[f]])))
      me_input_data[[f]]  <- factor(me_input_data[[f]], levels=levels)
      p_input_data[[f]]   <- factor(p_input_data[[f]],  levels=levels)
    }
  }
  
  # summary(p_input_data)
  # str(p_input_data)
  
  
  setwd(SYSG_SYSTEM_DIR)
  save(me_input_data, file = paste0("me_input_data.rda"))
  save(p_input_data, file = paste0("p_input_data.rda"))
  
  
}

process_missing_data <- function (input_data)
{
    
    missing_features <- c("Employment_Info_1","Employment_Info_2","Employment_Info_3","Employment_Info_4",
                          "Employment_Info_5","Employment_Info_6","Insurance_History_5","Family_Hist_2",
                          "Family_Hist_3","Family_Hist_4","Family_Hist_5","Medical_History_1",
                          "Medical_History_15","Family_Hist_2_3")
    new_input_data <- input_data[,missing_features]
    
    new_input_data <- apply(input_data,2,function(x) {
      median_x <- median(x,na.rm = TRUE)
      new_x <- ifelse(is.na(x),median_x,x)
    })
    
    
    return (new_input_data)
}

create_vbfe <- function(input_data,medical_keyword_features)
{
  
  Product_Info_2  <- input_data$Product_Info_2
  
  Product_Info_2L <- substr(Product_Info_2, 1 , 1)
  Product_Info_2N <- as.integer(substr(Product_Info_2, 2 , 2))
  
  MHN_6m7m8    <- input_data$Medical_History_6*input_data$Medical_History_7*input_data$Medical_History_8
  MHN_26m27m28 <- input_data$Medical_History_26*input_data$Medical_History_27*input_data$Medical_History_28
  
  # Bi gram features on set of medical keyword 
  bi_gram_features <- c("Medical_Keyword_3", "Medical_Keyword_38", "Medical_Keyword_23",
                        "Medical_Keyword_37","Medical_Keyword_15",
                        "Medical_Keyword_2","Medical_Keyword_32","Medical_Keyword_6",
                        "Medical_Keyword_20","Medical_Keyword_33")
  bi_gram_comb     <- t(combn(bi_gram_features,2))
  bi_gram_data     <- apply(bi_gram_comb , 1 , function (x) {
    i_bi_gram_data <- input_data[[x[1]]]*input_data[[x[2]]]
  })
  bi_gram_data_names     <- paste0(bi_gram_comb[,1],"_",str_split_fixed(bi_gram_comb[,2],"_",3)[,3])
  colnames(bi_gram_data) <- bi_gram_data_names
  
  medical_keyword_sum     <- rowSums(input_data[,medical_keyword_features])

  Family_Hist_2_3 <- rep(NA,dim(input_data)[1])
  
  Family_Hist_2_3 <- ifelse(input_data$Family_Hist_2>=0 & is.na(input_data$Family_Hist_3), 1, Family_Hist_2_3)
  
  Family_Hist_2_3 <- ifelse(input_data$Family_Hist_3>=0 & is.na(input_data$Family_Hist_2), 0, Family_Hist_2_3)
  
  Dummy <- rep(1,dim(input_data)[1])
  
  vbfe_data <- data.frame(Product_Info_2L,Product_Info_2N,MHN_6m7m8,MHN_26m27m28,bi_gram_data,
                          Medical_Keyword_Sum = medical_keyword_sum)
  
  vbfe_data <- data.frame(Product_Info_2L,Product_Info_2N , Family_Hist_2_3 = Family_Hist_2_3)
  
  
}

perform_model_assessment <- function (me_input_data,ma_model_id,cs_mode)
{
  
  library(caret)
  library(Metrics)
  
  source(paste0(SYSG_SYSTEM_DIR,"modeling_functions.R"))

  set.seed(998)
  m_indexes    <- createDataPartition(me_input_data$Response , p = .85, list = FALSE)
  
  # Prepare weighted data
  class.weights      <- c(1,1,1,1,1,1,1,1)
  weigthed_data_idxs <- prepare_class_weigthed_data(m_indexes,
                                                    me_input_data$Response,
                                                    class.weights,seq(1,8,1),cs_mode)
  
  
  m_input_data       <- me_input_data[weigthed_data_idxs,]  
  
  e_input_data       <- me_input_data[-m_indexes,]
  
  classification_formula <- as.formula(paste("Response" ,"~",
                                             paste(names(me_input_data)[!names(me_input_data)=='Response'],collapse="+")))
  
  # Initialize model assesment objects
  start_time            <- NULL
  end_time              <- NULL
  classification_model  <- NULL
  opt_param_description <- NULL
  assesment_grid        <- NULL
  start_time            <- proc.time()
  
  
  # Greed for parameter evaluation
  if (grepl("XGBC",ma_model_id)) {
    xgbc_tuneGrid   <- expand.grid( nrounds   = seq(500,1000, length.out = 2) , 
                                    eta       = seq(0.01,0.05, length.out = 3) , 
                                    max_depth = seq(10,12, length.out = 2) ,
                                    gamma = 0,               
                                    colsample_bytree = 1,    
                                    min_child_weight = 240)
    
    xgbc_tuneGrid   <- expand.grid( nrounds   = 500 , eta = 0.03         , max_depth = 9 ,
                                    gamma = 0       , colsample_bytree = 1 , min_child_weight = 200)
    
    assesment_grid <- xgbc_tuneGrid
  }
  
  if (grepl("GBM",ma_model_id)) {
    gbm_tuneGrid   <- expand.grid(interaction.depth = seq(1,3, length.out = 3),
                                  n.trees = seq(301,501, length.out = 3),
                                  shrinkage = seq(0.02,0.05, length.out = 3) , n.minobsinnode = 5)
#     gbm_tuneGrid   <- expand.grid(interaction.depth = 1, n.trees = 101 ,
#                                   shrinkage = 0.5 , n.minobsinnode = 5)
    
    assesment_grid <- gbm_tuneGrid
  }
  
  if (grepl("SVMR",ma_model_id)) {
    svmr_tuneGrid   <- expand.grid(sigma = seq(0.01,0.03, length.out = 3),C = 2^seq(1,10, length.out = 10))
    svmr_tuneGrid   <- expand.grid(sigma = 0.1 , C = 100)
    
    assesment_grid <- svmr_tuneGrid
  }
  
  if (grepl("SVML",ma_model_id)) {
    svml_tuneGrid   <- expand.grid(C = 2^seq(1,10, length.out = 10))
    svml_tuneGrid   <- expand.grid(C = 100)
    
    assesment_grid <- svml_tuneGrid
  }
  
  if (grepl("GLMNET",ma_model_id)) {
    glmnet_tuneGrid   <- expand.grid(alpha = c(0,1),lambda = seq(0.1,0.3, length.out = 3))
    glmnet_tuneGrid   <- expand.grid(alpha = 0.5 ,lambda = 0.005)
    
    assesment_grid <- glmnet_tuneGrid
  }
  
  emm_tuneGrid_list <- NULL;
  if (grepl("EMM",ma_model_id)) {
    emm_tuneGrid                       <- expand.grid(emm=1)
    emm_tuneGrid_list$xgbc1_tuneGrid   <- expand.grid( nrounds   = 200,eta = 0.05, max_depth = 10,
                                          gamma = 0, colsample_bytree = 1, min_child_weight = 5)
    emm_tuneGrid_list$gbm2_tuneGrid    <- expand.grid(interaction.depth = 1, n.trees = 101 ,
                                          shrinkage = 0.5 , n.minobsinnode = 5)
    assesment_grid <- emm_tuneGrid
  }
  
  
  if (grepl("EMF",ma_model_id)) {
    emf_tuneGrid                       <- expand.grid(emf=1)
    
    emf_tuneGrid_list <- NULL
    emf_tuneGrid_list_XGBC   <- expand.grid( nrounds   = 200,eta = 0.05, max_depth = 10,
                                                       gamma = 0, colsample_bytree = 1, min_child_weight = 5)
#     emf_tuneGrid_list$GBM    <- expand.grid(interaction.depth = 1, n.trees = 101 ,
#                                                       shrinkage = 0.5 , n.minobsinnode = 5)
    assesment_grid <- emf_tuneGrid_list_XGBC
    
    
  }
  
  
  SYS_CV_NFOLDS <- 5
  # CVreps  <- 4
  
  
  #Index for the trainControl()
  set.seed(1045481)
  tr_index <- createFolds(m_input_data$Response, k=SYS_CV_NFOLDS)
  #Seeds for the trainControl()
  set.seed(1056)
  tr_seeds <- vector(mode = "list", length = SYS_CV_NFOLDS + 1)
  for(i in 1:SYS_CV_NFOLDS) tr_seeds[[i]] <- sample.int(1000, dim(assesment_grid)[1] + SYS_CV_NFOLDS)
  set.seed(1056)
  tr_seeds[[SYS_CV_NFOLDS+1]] <- sample.int(1000, 1)
  
  ma_control <- trainControl(method          = "cv",
                             number          = SYS_CV_NFOLDS,
                             index           = tr_index,
                             seeds           = tr_seeds,
                             summaryFunction = QWKE,
                             allowParallel   = TRUE , 
                             verboseIter     = TRUE,
                             savePredictions = TRUE)
  
  ############################################################# MODEL CREATION #####################################
  
  create_log_entry("",paste0(ma_model_id ," Model Assesment started"),"SF")
  create_log_entry(names(assesment_grid),assesment_grid,"F")
  
  if (grepl("XGBC",ma_model_id)) { 
    xgbc_model <- train( classification_formula , data = m_input_data , method = "xgbTree", 
                         metric   ="QWKE" , trControl = ma_control, tuneGrid = assesment_grid , 
                         objective           = 'reg:linear',
                         nthread             = 2)
    
    classification_model$ma_model_id            <- ma_model_id
    classification_model$model$M                <- xgbc_model
    classification_model$model$M$opt_param      <- xgbc_model$bestTune
    
    opt_param_description$opt_param <- classification_model$model$M$opt_param
  }
  if (grepl("GBM",ma_model_id)) { 
    gbm_model <- train(classification_formula , data = m_input_data , method = "gbm", 
                       metric = "QWKE" , trControl = ma_control, tuneGrid = assesment_grid,
                       maximize = FALSE)
    
    classification_model$ma_model_id            <- ma_model_id
    classification_model$model$M                <- gbm_model
    classification_model$model$M$opt_param      <- gbm_model$bestTune
    
    opt_param_description$opt_param <- classification_model$model$M$opt_param
  }
  if (grepl("SVMR",ma_model_id)) { 
    svmr_model <- train(classification_formula , data = m_input_data , method = "svmRadial", 
                       metric = "QWKE" , trControl = ma_control, tuneGrid = assesment_grid,
                       maximize = FALSE)

    classification_model$ma_model_id            <- ma_model_id
    classification_model$model$M                <- svmr_model
    classification_model$model$M$opt_param      <- svmr_model$bestTune
    
    opt_param_description$opt_param <- classification_model$model$M$opt_param
  }
  
  if (grepl("SVML",ma_model_id)) { 
    svml_model <- train(classification_formula , data = m_input_data , method = "svmLinear", 
                        metric = "QWKE" , trControl = ma_control, tuneGrid = assesment_grid,
                        maximize = FALSE)

    classification_model$ma_model_id            <- ma_model_id
    classification_model$model$M                <- svml_model
    classification_model$model$M$opt_param      <- svml_model$bestTune
    
    opt_param_description$opt_param <- classification_model$model$M$opt_param
  }
  if (grepl("GLMNET",ma_model_id)) { 
    glmnet_model <- train(classification_formula , data = m_input_data , method = "glmnet", 
                        metric = "QWKE" , trControl = ma_control, tuneGrid = assesment_grid,
                        maximize = FALSE)

    classification_model$ma_model_id            <- ma_model_id
    classification_model$model$M                <- glmnet_model
    classification_model$model$M$opt_param      <- glmnet_model$bestTune
    
    opt_param_description$opt_param <- classification_model$model$M$opt_param
  }
  
  if (grepl("EMM",ma_model_id)) { 
    emm_model_list <- caretList (
      classification_formula , data = m_input_data , 
      trControl=ma_control, metric = "QWKE",
     tuneList=list(
      xgbc1  = caretModelSpec(method='xgbTree', tuneGrid=emm_tuneGrid_list$xgbc1_tuneGrid , objective = 'reg:linear'),
      gbm2   = caretModelSpec(method='gbm',     tuneGrid=emm_tuneGrid_list$gbm2_tuneGrid , verbose = FALSE)
      )
    )
    # emm_model <- caretEnsemble(emm_model_list,optFUN = QWKE)
    emm_model <- caretEnsemble(emm_model_list)

    classification_model$ma_model_id            <- ma_model_id
    classification_model$M                      <- emm_model
    classification_model$model                  <- emm_model$models
    classification_model$weights                <- emm_model$weights
    
    opt_param_description                       <- sapply(classification_model$model, "[[", "bestTune")
    opt_param_description$weights               <- classification_model$weights
  }
  
  if (grepl("EMF",ma_model_id)) { 
    
    emf_model <- NULL
    emf_algorithms <- c("XGBC")
    emf_feature_subsets <- list(
      fs1 <- c(1:15),
      fs2 <- c(11:20)
    )
    
    # a <- which(names(me_input_data) %in% c("Family_Hist_5","Employment_Info_4"))
    
    ensemble_space <- expand.grid(alg = emf_algorithms,fs = emf_feature_subsets)

    emf_model$ma_model_id            <- ma_model_id
    
    emf_model_list <- apply(ensemble_space, 1, function(x) {
      fsx  <- unlist(x$fs)
      algx <- unlist(x$alg)
      subset <- m_input_data[,c(names(m_input_data)[fsx],"Response")]

      i_assesment_grid <- assesment_grid
      classification_formula <- as.formula(paste("Response" ,"~",
                                                  paste(names(subset)[!names(subset)=='Response'],collapse="+")))
      i_emf_model <- train(  classification_formula , data = subset , method = "xgbTree", 
                           metric              ="QWKE" , trControl = ma_control, tuneGrid = i_assesment_grid , 
                           objective           = 'reg:linear',
                           nthread             = 2)
      i_emf_model$fs        <- fsx
      i_emf_model$alg       <- algx
      i_emf_model$opt_param <- i_emf_model$bestTune
      i_emf_model
    })

    # sapply(mylist,function(x) x[2])
    names(emf_model_list) <- paste0("M_",1:dim(ensemble_space)[1])
    emf_model$model       <- emf_model_list
    
    perf_metric <- "QWKE"
    perf_data <- sapply(emf_model$model, function (x) {
      x$results[[perf_metric]]
    })
    
    weights <- round(perf_data / sum(perf_data),2)
    
    emf_model$model$weights <- weights
    
    classification_model <- emf_model

    opt_param_description$alg                      <- lapply(emf_model_list, "[[", "alg")
    opt_param_description$fs                       <- lapply(emf_model_list, "[[", "fs")
    opt_param_description$opt_param                <- lapply(emf_model_list, "[[", "opt_param")
    opt_param_description$weights                  <- classification_model$model$weights
    
  }
  
  end_time <- proc.time() ; runtime <- round(as.numeric((end_time - start_time)[3]),2)
  
  create_log_entry(paste0(ma_model_id , " Model Assesment finished : " , runtime),"","SF")

  create_log_entry("Optimal parameters : ", opt_param_description ,"SF")
  
  featureImp <- create_feature_importance_data(classification_model,ma_model_id)
 
  # Create predictions based on evaluation data
  create_log_entry(" Evaluation prediction started ..... ","","SF")
  
  e_prediction_data <- create_e_prediction_data(classification_model, e_input_data , ma_model_id)
  
  classification_model$epd <- e_prediction_data
  
  setwd(SYSG_OUTPUT_MODELING_DIR)
  save(classification_model, file = paste0(ma_model_id,".rda"))
  
  create_log_entry(" Evaluation prediction finished ..... ","","SF")
  
}

QWKE <- function(data,lev = NULL, model = NULL) {
  prediction_values  <- process_prediction_values(data$pred)
  err  <- ScoreQuadraticWeightedKappa(data$obs,prediction_values)
  names(err) <- "QWKE"
  err
}


create_feature_importance_data <- function(classification_model,ma_model_id)
{
  
  create_log_entry(" Feature Importance evaluation started ..... ","","SF")
  if(!grepl("EMM",ma_model_id) & !grepl("EMF",ma_model_id)) {
    # Output feature importance based on modelling data
    importance_data_obj <- varImp(classification_model$model$M,scale = FALSE)$importance
    importance_data     <- data.frame(Var = rownames(importance_data_obj),Imp = importance_data_obj$Overall,stringsAsFactors=FALSE)
  } 
  if (grepl("EMM",ma_model_id)) {
    # Output feature importance based on modelling data
    importance_data_obj <- varImp(classification_model$M,scale = TRUE)
    importance_data     <- data.frame(importance_data_obj,stringsAsFactors=FALSE)
  }
  
  # Importance is presented for each partial model
  if (grepl("EMF",ma_model_id)) 
  {
      ens_classification_models <- classification_model$model[names(classification_model$model)[grepl("M_", names(classification_model$model))]]

      importance_data     <- sapply(ens_classification_models , function (x) {
      importance_data_obj <- varImp(x,scale = FALSE)$importance
      importance_data     <- data.frame(Var = rownames(importance_data_obj),
                                        Imp = importance_data_obj$Overall,stringsAsFactors=FALSE)
    })
  }
  
  create_log_entry(paste0(ma_model_id , " Feature Importance : "),"","F")
  create_log_entry(names(importance_data),head(importance_data,200),"F")
  create_log_entry(" Feature Importance evaluation finished ..... ","","SF")
  
}

perform_feature_selection <- function(input_data)
{
  
  library(caret)
  
  
  ctrl <- rfeControl(functions = lmFuncs,
                     method = "repeatedcv",
                     repeats = 5,
                     verbose = FALSE)
  
  
  predictors(lmProfile)
}

create_evaluation_diagnostic <- function(ma_run_id)
{
  library(ggplot2)
  ma_run_id      <- "MA_#XGBC#2016-01-08 00_56_47"

  prediction_data                <- classification_model$epd
  
  prediction_data$resid  <- prediction_data$obs - prediction_data$pred
  
  summary_cnt <- ddply(prediction_data, c("pred", "resid"), summarise, N=length(obs))
  
  prediction_data <- inner_join(prediction_data , summary_cnt)
  
  p1<-ggplot(summary_cnt, aes(pred,resid))+geom_point(aes(size=N), na.rm=TRUE)
  p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<- p1 + scale_x_continuous(breaks = seq(1, 8, 1))
  p1<- p1 + scale_y_continuous(breaks = seq(-8, 8, 1))
  p1<-p1+xlab("Predicted")+ylab("Residuals")
  p1<-p1+ggtitle("Predicted vs Residuals Plot")+theme_bw()
  
  p1
  
}

# Function predicts model on evaluation data and output AUC to log
create_e_prediction_data <- function (classification_model, e_input_data , ma_run_id)
{
  
  prediction_values  <- create_prediction_values(classification_model, e_input_data , ma_run_id)
  
  prediction_data <- data.frame(pred = prediction_values,obs = e_input_data$Response)
  
  QWKE <- QWKE(prediction_data)

  create_log_entry(paste0(ma_run_id , " Evaluation : " , QWKE),"","SF")
  
  return(prediction_data)
}


# Create final model using optimal parameters tuned by caret + non-tunable parameters after manual evaluation
# Use all train data set
create_p_model <- function (opt_model_id,me_input_data,classification_model)
{
  
  opt_classification_model <- NULL
  
  classification_formula <- as.formula(paste("Response" ,"~",
                                             paste(names(me_input_data)[!names(me_input_data)=='Response'],collapse="+")))
  
  set.seed(2223)
  p_seeds <- vector(mode = "list", length = 1)
  p_seeds[[1]] <- sample.int(1000, 1)
  
  m_control <- trainControl(method          = "none",
                            seeds           = p_seeds,
                            allowParallel   = TRUE , 
                            verboseIter     = TRUE)
  
  create_log_entry(paste0(opt_model_id ," Optimal Model Creation started : "),"","SF")

  start_time <- proc.time()
  
  if (grepl("XGBC",opt_model_id)) { 
    opt_parameters <- classification_model$model$M$opt_param
    xgbc_model <- train( classification_formula , data = me_input_data , method = "xgbTree", 
                         trControl = m_control, tuneGrid = opt_parameters , 
                         objective           = 'reg:linear',
                         nthread             = 6)
    
    opt_classification_model$p_model_id <- opt_model_id
    opt_classification_model$model$M <- xgbc_model
  }
  if (grepl("GBM",opt_model_id)) { 
    gbm_model <- train(classification_formula , data = me_input_data , method = "gbm", 
                       trControl = m_control, tuneGrid = opt_parameters)
    opt_classification_model <- gbm_model
  }
  
  if (grepl("SVMR",opt_model_id)) { 
    svmr_model <- train(classification_formula , data = me_input_data , method = "svmRadial", 
                        trControl = m_control, tuneGrid = opt_parameters)
    opt_classification_model <- svmr_model
  }
  if (grepl("SVML",opt_model_id)) { 
    svml_model <- train(classification_formula , data = me_input_data , method = "svmLinear", 
                        trControl = m_control, tuneGrid = opt_parameters)
    opt_classification_model <- svml_model
  }
  if (grepl("GLMNET",opt_model_id)) { 
    glmnet_model <- train(classification_formula , data = me_input_data , method = "glmnet", 
                          trControl = m_control, tuneGrid = opt_parameters)
    opt_classification_model <- glmnet_model
  }
  
  if (grepl("EMM",opt_model_id)) { 
    
    #Index for the trainControl()
    set.seed(1045481)
    p_index <- createFolds(me_input_data$Response, k = 2)
    #Seeds for the trainControl()
    set.seed(1056)
    p_seeds <- vector(mode = "list", length = 2 + 1)
    for(i in 1:2) p_seeds[[i]] <- sample.int(1000, 1 + 2)
    set.seed(1056)
    p_seeds[[2+1]] <- sample.int(1000, 1)
    
    m_control <- trainControl(method          = "cv",
                               number          = 2,
                               index           = createFolds(me_input_data$Response, k=2),
                               seeds           = p_seeds,
                               summaryFunction = QWKE,
                               allowParallel   = TRUE , 
                               verboseIter     = TRUE,
                               savePredictions = TRUE)
    
    emm_model_list <- caretList (
      classification_formula , data = me_input_data , 
      trControl=m_control, 
      tuneList=list(
        xgbc1  = caretModelSpec(method='xgbTree', metric = "QWKE" , tuneGrid=classification_model$M$models$xgbc1$bestTune , objective = 'reg:linear'),
        gbm2   = caretModelSpec(method='gbm',     metric = "QWKE" , tuneGrid=classification_model$M$models$gbm2$bestTune, verbose = FALSE)
      )
    )
    # emm_model <- caretEnsemble(emm_model_list,optFUN = QWKE)
    # Weights are learned from the begining and aren't transfered from previous modeling stage
    emm_model <- caretEnsemble(emm_model_list)
    # emm_model <- caretEnsemble(emm_model_list,optFUN = greedOptQWKE)
    
    opt_classification_model$p_model_id     <- opt_model_id
    opt_classification_model$M              <- emm_model
    opt_classification_model$model          <- emm_model$models
    opt_classification_model$weights        <- emm_model$weights
  }
  
  if (grepl("EMF",opt_model_id)) { 

    ens_classification_models <- classification_model$model[grepl("M_",names(classification_model$model))]
    
    emf_model_list <- lapply(ens_classification_models, function(x) {
      fsx  <- unlist(x$fs)
      algx <- unlist(x$alg)
      subset <- me_input_data[,c(names(m_input_data)[fsx],"Response")]
      
      i_assesment_grid <- x$opt_param
      classification_formula <- as.formula(paste("Response" ,"~",
                                                 paste(names(subset)[!names(subset)=='Response'],collapse="+")))
      i_emf_model <- train(  classification_formula , data = subset , method = "xgbTree", 
                           metric              ="QWKE" , trControl = m_control, tuneGrid = i_assesment_grid , 
                           objective           = 'reg:linear',
                           nthread             = 2)
      i_emf_model$fs        <- fsx
      i_emf_model$alg       <- algx
      i_emf_model$opt_param <- emf_model$bestTune
      i_emf_model
    })
    names(emf_model_list)                   <- names(ens_classification_models)
    opt_classification_model$p_model_id     <- opt_model_id
    opt_classification_model$model          <- emf_model_list
    opt_classification_model$model$weights  <- classification_model$model$weights

  }
  
  end_time <- proc.time() ; runtime <- round(as.numeric((end_time - start_time)[3]),2)
  
  save(opt_classification_model, file = paste0(opt_model_id,".rda"))
  
  create_log_entry(paste0(opt_model_id , " Optimal Model Creation finished : " , runtime),"","SF")
  
}

# Function predicts model on prediction/submission data
perform_p_prediction <- function (classification_model,p_input_data,opt_model_id)
{
  
  prediction_values         <- create_prediction_values(classification_model,p_input_data,opt_model_id)
  
  prediction_data           <- data.frame(Id = p_input_data$Id,Response = prediction_values , row.names = NULL)
  
  setwd(SYSG_OUTPUT_DIR)
  options(scipen=10)
  write.csv(prediction_data, file = paste0("submission_", opt_model_id ,".csv"), row.names = FALSE)
  options(scipen=0)
  create_log_entry("Finished prediction on data","","SF")

}

# Function creates predictions with given classification model and input data
create_prediction_values <- function (classification_model, p_input_data , m_run_id)
{
  prediction_values  <- NULL
  
  if (!grepl("EMM",m_run_id) & !grepl("EMF",m_run_id))
  {
    classification_model <- classification_model$model$M
    prediction_values    <- predict(classification_model,p_input_data , type = "raw")
    prediction_values    <- process_prediction_values(prediction_values)
    
  }
  if (grepl("EMF",m_run_id)) 
  {
    weights <- classification_model$model$weights
    ens_classification_models <- classification_model$model[grepl("M_",names(classification_model$model))]
    
    prediction_values     <- sapply(ens_classification_models , function (x) {
    prediction_values     <- predict(x,p_input_data , type = "raw")
    })
    # Assumimg regression
    prediction_values <- process_prediction_values(prediction_values%*%weights)
  }
  if (grepl("EMM",m_run_id)) 
  {
    classification_model <- classification_model$M
    prediction_values    <- predict(classification_model,newdata = p_input_data)
    prediction_values    <- process_prediction_values(prediction_values)
  }
  
  return (prediction_values)
}

process_prediction_values <- function (prediction_values)
{
  prediction_values1 <- findInterval (prediction_values , c(-999,2,2.8,3.2,4,4.67,5.75,6.5,999))
  
  prediction_values1  <- round(prediction_values)
  prediction_values2 <- ifelse(prediction_values1 < 1,1, prediction_values1)
  prediction_values3 <- ifelse(prediction_values2 > 8,8, prediction_values2)
  
  return (prediction_values3)
}

create_log_entry <- function(message_title = "", message , log_mode)
  
{
  current_library <- getwd()
  
  setwd(SYSG_SYSTEM_DIR)
  
  if (regexpr("S",log_mode)>0) {
    print(paste0(Sys.time(), " : " , message_title) , row.names = FALSE)
    print(message , row.names = FALSE)
  }
  
  if (regexpr("F",log_mode)>0) {
      write(message_title , "log.txt", append = TRUE)
    if (class(message)=="data.frame")
      write.table(message , "log.txt", append = TRUE,col.names = FALSE ,  row.names = FALSE , quote = FALSE , sep = "\t") 
    else write.table(paste0(Sys.time(), " : " , message) , "log.txt", append = TRUE, col.names = FALSE ,  row.names = FALSE , quote = FALSE,sep = ",")
  }
  
  setwd(current_library)
  
}  


