#-------------------------------------
# GGplot_aclimCEATTLE
#-------------------------------------
#' PLot ACLIM CEATTLE projections covariates using GGplot
#'
#' @param dat is a data.frame with t=date and dat=data to plot
#' @param plotSet is the columns of dat that you want to plot (col=1 is the date, 2:ncol are options)
#' @param lwdd vector of the line widths; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param ltyy vector of the line types; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param ylab y axis label
#' @param xlab x axis label
#' @param title title for the graph
#' @param coll vector colors for each line; length of the projection simulation set + 1 for hindcast (dim of dat)
#' @param esnmCol colors for each ensemble polygon
#' @param esnmSet is a list with the set for each ensemble groups
#' @param alpha vector of transparencies of the ensemble mean; corresponds to number quantiles
#' @param add0line Add a horizontal line at 0?
#' @keywords Temperature, plot, data, ACLIM
#' @export 
#' @examples 
#  
#577

#  "###########################################################"  

