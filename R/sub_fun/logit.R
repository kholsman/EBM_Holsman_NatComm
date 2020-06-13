#'
#' calculate the logit of a number
#' @param x
#' @examples 
#' logit(.3)
logit<-function(x){log(x/(1-x))}