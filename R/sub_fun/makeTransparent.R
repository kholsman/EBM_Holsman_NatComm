#'-------------------------------------
#'  makeTransparent
#'-------------------------------------
#' This function makes any color transparent
#' @param someColor  # color 
#' @param alpha      # transparency 0-240
makeTransparent<-function(someColor, alpha=100)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}