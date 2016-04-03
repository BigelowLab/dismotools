#' Given a Raster* object and point locations compute the AUC
#'
#' @export
#' @seealso \code{auc_vector}
#' @param R a Raster* layer object
#' @param xy either a data.frame or matrix of coordinates (lon, lat)
#'    if r has multiple layers then xy should also have column 'layer'
#' @param ... further arguments for \code{auc_vector}
#' @return a list of 
#' \itemize{
#'  \item{a vector of fpa (fractional predicted area)}
#'  \item{a vector of sensitivity (1-omission rate)}
#'  \item{area AUC}
#'  }
auc_raster <- function(R, xy, ...){

    if (nlayers(R) > 1){
        v <- layers_extractPoints(R, xy)
    } else {
        v <- raster::extract(R, xy)
    }
    auc_vector(raster::as.vector(R), v, ...)
}

#' Compute AUC values ala presence-only data
#' 
#' @export
#' @param f a vector of forecasted ranked suitability values, NAs will be removed
#' @param v a vector of forecasted values where there are presences, NAs will be removed
#' @param thr a vector of threshold values
#' @return a list of 
#' \itemize{
#'  \item{fpa, a vector of fpa (fractional predicted area)}
#'  \item{sensitive, a vector of sensitivity (1-omission rate)}
#'  \item{area, AUC}
#'  }
auc_vector <- function(f, v, thr = seq(from = 1, to = 0, by = -0.001)){
    f <- f[!is.na(f)]
    fn <- length(f)
    v <- v[!is.na(v)]
    vn <- length(v)
    x <- rep(0, length(thr))
    y <- x
    for (i in seq_along(x)){
        ix <- sum(f > thr[i])
        x[i] <- ix/fn
        iy <- sum(v > thr[i])
        y[i] <- iy/vn
    }    
    
    list(fpa = x, sensitivity = y,
        area = sum(diff(x) * (y[2:length(y)]+ y[1:length(y)-1])/2) )
}
