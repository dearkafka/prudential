
#' Generate simple repetition of instances in by meaning of indexes according class.weights
#' Support 1:x.y weigthening formulation . Data is repeated x times and y fraction is sampled
#' Input data is row indexes of data used in code calling to this function
#' So in final dataset there are in x.y of instance with minor class
#' Use dependency between weights and in data class proportions to adjust the weight toward 50:50 ratio
#' 
#' @param    target_feature_vector - Vector of target feature
#' @param    class.weights         - Target values weights in 1:x.y form and in order of target values in target_values
#' @return   target_values         - Enumeration of target values  
#' @example

#' target_feature_vector <- ("C1","C1","C2","C2","C2","C1")
#' class.weights         <- (1,1.5)
#' target_values         <- ("C1","C2")
#' 
#' Repetition : seq_along(target_feature_vector) <- (1,2,3,4,5,6)
#' Sampling   : With probability 0.5 index 3 ("C2") is replicated
#'              With probability 0.5 index 4 ("C2") is replicated
#'              With probability 0.5 index 5 ("C2") is replicated
#' So in weighted dataset data indexes can be presented as follows : (1,2,3,3,4,4,5,6)

# 
# target_feature_vector <- metadata$TFV[modeling_sample_data]
# class.weights         <- class.weights
# target_values         <- target_values

prepare_class_weigthed_data <- function (m_indexes,target_feature_vector,class.weights,target_values,cs_mode)
{
  
  if(cs_mode == "N")
    return (m_indexes)
  
  wdata_idxs <- NULL
  
  if(cs_mode == "CW") {
    wdata_idxs <- unlist(sapply(seq_along(m_indexes),
                                function (t) {
                                  class_w       <- class.weights[target_values == target_feature_vector[t]]
                                  class_w_int   <- as.integer(class_w) 
                                  class_w_fract <- ifelse(runif(1) < (class_w - class_w_int),1,0)
                                  wdata_idxs    <- c(wdata_idxs,rep(m_indexes[t],class_w_int+class_w_fract))}))
  }
  
  return (wdata_idxs)
  
}

# data <- data.frame(prediction_values = c(1,2,3,4,5,6.5,4,4,4,4,6,8) , 
#                    actual_values = c(1,2.3,4,5,6,7,6,7,7,7,8,7))
# 
# create_prediction_bounds(data$prediction_values,data$actual_values)

create_prediction_bounds <- function (prediction_values,actual_values)
{
  library(Hmisc)
  SQWKfun = function(x = seq(1.5, 7.5, by = 1), data) {
    preds   = data$prediction_values
    actuals = data$actual_values 
    cuts = c(min(preds), x[1], x[2], x[3], x[4], x[5], x[6], x[7], max(preds))
    preds = as.numeric(Hmisc::cut2(preds, cuts))
    err = Metrics::ScoreQuadraticWeightedKappa(preds, actuals , 1, 8)
    return(-err)
  }
  
  optBounds = optim(seq(1.5, 7.5, by = 1), SQWKfun, data = data.frame(prediction_values,actual_values) , 
                    method = "Nelder-Mead",control = list(maxit = 1000))
  return(optBounds$par)
}

