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
#'  \item{fpa, a vector of fractional predicted area}
#'  \item{sensitive,a vector of sensitivity (1-omission rate)}
#'  \item{area, scalar AUC value}
#'  }
auc_raster <- function(R, xy, ...){
    v <- layers_extractPoints(R, xy)
    auc_vector(as.vector(raster::values(R)), v, ...)
}

#' Compute AUC values ala presence-only data
#'
#' @export
#' @param f a vector of forecasted ranked suitability values, NaNs and NAs will be removed
#' @param v a vector of forecasted values where there are presences, NaNs and NAs will be removed
#' @param thr a vector of threshold values
#' @param method character, if 'fast' (the default) use the fast implementation
#' @return a list of
#' \itemize{
#'  \item{fpa, a vector of fpa (fractional predicted area)}
#'  \item{sensitive, a vector of sensitivity (1-omission rate)}
#'  \item{area, AUC}
#'  }
auc_vector <- function(f, v, thr = seq(from = 1, to = 0, by = -0.001),
                       method = 'fast'){

    if (tolower(method[1]) == 'fast') return(auc_vector_fast(f,v,thr = thr))

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

#' Compute AUC values ala presence-only data using a fast algorithm.
#'
#' This function gains speed using \code{Rfast::Sort()} and \code{base::findInterval()}
#'
#' @export
#' @param f a vector of forecasted ranked suitability values, NaNs and NAs will be removed
#' @param v a vector of forecasted values where there are presences, NaNs and NAs will be removed
#' @param thr a vector of threshold values
#' @return a list of
#' \itemize{
#'  \item{fpa, a vector of fpa (fractional predicted area)}
#'  \item{sensitive, a vector of sensitivity (1-omission rate)}
#'  \item{area, AUC}
#'  }
auc_vector_fast <- function(f, v, thr = seq(from = 1, to = 0, by = -0.001)){

    f   <- f[is.finite(f)]
    f   <- Rfast::Sort(f, na.last = NA)
    fn  <- length(f)
    v   <- v[is.finite(v)]
    v   <- Rfast::Sort(v, na.last = NA)
    vn  <- length(v)
    x   <- rep(0, length(thr))
    y   <- x

    fix <- findInterval(thr, f)
    vix <- findInterval(thr, v)
    x   <- (fn - fix)/fn
    y   <- (vn - vix)/vn
    a   <- sum(diff(x) * (y[2:length(y)] + y[1:length(y)-1])/2)

    list(fpa = x,
         sensitivity = y,
         area = a)
}
