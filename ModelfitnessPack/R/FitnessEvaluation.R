#' Evaluate the fitness of different models
#'
#' Calculate the fit statistics of models, given the predicted values and observaed values.
#'
#' @param y A vector of observed values
#' @param p a matrix of predicted values
#' @param r a vector of naive values
#' @param fits a vector of fit statistics, including RMSE, MAD, RMSLE, MAPE, and MEAPE
#'
#' @return A matrix where each column corresponds with one of the 
#' fit statistics, and each row corresponds to a model.
#' @author Luwei YING
#' @note Users can choose which statistics to calculate.
#' @examples
#' x <- seq(1, 100, by=1)
#' y <- seq(1, 100, by=1) +  rnorm(100, 0, 1)
#' p <- matrix(predict(lm(y ~ x)), ncol=1)
#' r <- rep(50.5, 100)
#' FitStats(y=y, p=p, r=r, fits=c("rmse", "mad", "mrae"))
#' @rdname FitnessEvaluation
#' @export
FitStats <- function(y, p, r = NULL, 
                     fits){
  
  # Define the error term e, which is a matrix of the same numbers of colums and rows 
  # with the matrix of the predicted values. Subtract the observed values from each 
  # columns of the predicted values, respectively.
  error <- apply(p, 2, function(p) abs(p-y))
  # Delete all the NAs in the error matrix.
  e <- apply(error, 2, function(error) na.omit(error))
  
  # To prevent an infinite result in next step when y=0, add a tiny number to it.
  y[y==0 & !is.na(y)] <- y[y==0 & !is.na(y)] + 0.001
  # Define the absolute percentage error, do it from one column to another.
  a <- apply(error, 2, function(error) (error/abs(y)) * 100)
  # Delete all the NAs in the absolute percentage error matrix.
  a <- apply(a, 2, function(a) na.omit(a))
  
  # After ommitting all the NAs, the e matrix and a matrix are still of the same 
  # number of rows. We can check this by nrow(e)==nrow(a). Record that row number 
  # for later calculation.
  n <- nrow(e)
  
  # Before we calculate the fitness statistics, construct the output matrix for 
  # later use. Name the rows with the names of models.
  # Output <- matrix(1:ncol(Pn), nrow = ncol(Pn), ncol = 1)
  # rownames(Output) <- colnames(P)
  Output <- NULL
  
  # Calculate the fitness statistics
  # RMSE
  if("rmse" %in% fits){
    RMSE <- apply(e, 2, function(e) sqrt(sum(e^2)/n))
    Output <- cbind(Output, RMSE)
  }
  
  # MAD
  if("mad" %in% fits){
    MAD <- apply(e, 2, function(x) median(e))
    Output <- cbind(Output, MAD)
  }
  
  # RMSLE
  if("rmsle" %in% fits){
    # Delete all the NAs in the p matrix, and store the remaining values in a new matrix.
    p_complete <- apply(p, 2, function(p) na.omit(p))
    # Find the corresponding observed values in y.
    y_complete <- as.matrix(y[!is.na(p[ ,1])])
    # After ommitting all the NAs, the p_complete matrix and y_complete matrix should have
    # the same number of rows. We can check this by nrow(y_complete) == nrow(p_complete).
    
    # Remove the numbers less than or equal to -1 in p_complete and y_complete. Notice h=that 
    # if there is one in either of the two, we also need to delete the correponding value in 
    # the other.
    for(i in 1:nrow(p_complete)){
      if (min(p_complete[i, ]) < 0 | y_complete[i, ] < 0){
        y_comp <- as.matrix(y_complete[-i, ])
        p_comp <- p_complete[-i, ]
      } else {
        y_comp <- as.matrix(y_complete)
        p_comp <- p_complete
      }
    }
    RMSLE <- apply(p_comp, 2, function(p_comp) sqrt(sum((log(p_comp+1)-log(y_comp+1))^2)/n))
    Output <- cbind(Output, RMSLE)
  }
  
  # MAPE
  if("mape" %in% fits){
    MAPE <- apply(a, 2, function(a) sum(a)/n)
    Output <- cbind(Output, MAPE)
  }
    
  # MEAPE
  if("meape" %in% fits){
    MEAPE <- apply(a, 2, function(a) median(a))
    Output <- cbind(Output, MEAPE)
  }
  
  # MRAE
  if("mrae" %in% fits){
    # check if the naive forcast is proveded
    if(is.null(r)){
      warning("r must be specified for MRAE calculation.")
    } else {
      # Find b
      b <- as.matrix(abs(r-y))
      b <- as.matrix(b[!is.na(error[ ,1])])
      # Add a tiny number to b=0 to avoid infinite in the next step.
      b[b==0 & !is.na(b)] <- b[b==0 & !is.na(b)]+0.001  
      # Calculate MRAE
      MRAE <- apply(e, 2, function(e) median(e/b))
      Output <- cbind(Output, MRAE)
    }
  }
  print(Output)
}
