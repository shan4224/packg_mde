#' Finding mode of a variable
#'
#'
#' This function find the mode of a categorical variable . If the variable is not
#' categorical it displays a warning message .
#'
#'
#' @param     x a vector containing values of a variable .
#' @param     na.rm  default value to include NA values. It can be set to TRUE to exclude NA values.
#' @return    a list [1] modal values [2] number of modes in the variable .
#' @author    Shan
#' @details
#' This function find the mode of a categorical variable. It check whether there are
#' NA values in the vector. There is option to include or exclude the NA values.
#' Default is na.rm=FALSE . If the variable is not categorical it displays a warning
#' message.
#'
#' @seealso \code{unique}      \code{is.na}
#' @export
#'
#'
#' @examples
#' x <- c(2,3,4,4,7,4,9)
#' xm <- mode(x)




mde <- function(x, na.rm=FALSE){

    if(na.rm){
        x = x[!is.na(x)]
    }

    if(class(x)!="factor"){
        warning("Its not a factor variable !!. Modes are displayed for factor variable.")
    }

    unx <- unique(x)
    len <- length(unx)

    freq <- rep(0,len)
    for ( i in seq_len(len)){
        if (is.na(unx[i])){
            freq[i] <-  sum(is.na(x))
        }

        else{
            freq[i] <-  sum(x==unx[i], na.rm=TRUE)
        }
    }

    mdval <-  if((max(freq)==min(freq))|(max(freq)==min(freq) & freq[1]==1)){
        ("No Mode. All class type are equal in numbers.")} else{
            unx[which(freq==max(freq))]
        }
    mdlen <- ifelse(((max(freq)==min(freq))|(max(freq)==min(freq) & freq[1]==1)),"NULL",sum(length(which(freq==max(freq)))))

    return(list("mode value"= mdval, "number of mode"= mdlen))

}

