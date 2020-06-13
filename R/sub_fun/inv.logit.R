#'calculate the inverse logit of a number
#' @param x input 
#' @examples 
#' inv.logit(3)
inv.logit<-function(x){exp(x)/(1+exp(x))}