#' @title Weight of Evidence based segmentation of a variable
#'
#' @description This function applies the binning generated from woe_binning to new data
#'
#' @param df A data frame. The variable names and types need to be identical to the ones passed to woe_binning
#'
#' @param woe_object Output object from woe_binning function
#'
#' @return Input data frame is returned with two new columns - bin & woe
#'
#' @examples library(smbinning)
#' \dontrun{woe_object <- woe_binning(smbsimdf1, "cbs1", "fgood", initial_bins = 10)
#' out <- apply_woe(smbsimdf1, woe_object)
#' #Above example to create and apply woe segmentation}
#'
#'@export apply_woe
#'

apply_woe <- function(df, woe_object){
   ### Do name check of list, list should have 5 elements. Then check df has those variables and are of the same class & values
   # Check df is a data frame
   if (!is.data.frame(df)) {
     return("Data not a data.frame")
   }
   if (!identical(names(woe_object), c("variable", "dv", "breaks", "woe", "IV"))) {
     stop("Incorrect parameter specification. Parameter woe_object should be generated from woe_binning function")
   }
   # Check variables are present in the data frame
   if (is.na(which(names(df) == woe_object$variable)[1]) ) {
     stop("Parameters variable/dv not found in the datatset.")   
   }
   # Check class of input arguments
   if (!class(df[, woe_object$variable]) %in% c("numeric", "integer")) {
     stop(paste0("Incorrect parameter specification. Column ", woe_object$variable, " is not numeric/integer.") )
   }
   df$woe <- 0
   df[which(is.na(df[,woe_object$variable])),"woe"] <- woe_object$woe$woe[which(is.na(woe_object$woe$bin))]
   len <- woe_object$breaks[-1]
   df$ind <- 0
   for(i in 1:length(len)){
     df[which(df[,woe_object$variable] <= len[i] & df$ind == 0),"woe"] <- woe_object$woe$woe[i]
     df$ind[which(df[,woe_object$variable] <= len[i])] <- 1
   }
   df$ind <- NULL
   names(df)[ncol(df)] <- paste0(woe_object$variable, "_woe")   
   return(df)
 }
