#' Compute the predictor response(s) for a maxent model. 
#' 
#' @export
#' @param x maxent model output
#' @param var character, one or more variables to compute response (defaults to all). See \code{\link[dismo]{response}}
#' @param N numeric, the number of rows in the output.  Responses are interpolated at this number of intervals.
#' @param at see \code{\link[dismo]{response}}
#' @param data see \code{\link[dismo]{response}}
#' @param fun see \code{\link[dismo]{response}}
#' @param expand see \code{\link[dismo]{response}}
#' @param range see \code{\link[dismo]{response}}
#' @return tibble of sampled predictors and responses 
maxent_response <- function(x , 
                            var=NULL, 
                            at=median, 
                            range='pa', 
                            expand=10, 
                            data=NULL, 
                            fun=predict,
                            N = 100) {
  if (FALSE){
    x = dismotools::read_maxent("/mnt/ecocast/projectdata/students/kenny/habobis/versions/v2/v2.101/model")
    var=NULL
    data=NULL
    fun=predict
    at=median
    range='pa'
    expand=10 
    N = 100
  }
  stopifnot(range %in% c('p', 'pa'))
  if (is.null(data)) {
    data <- x@presence
    if (range == 'pa' & x@hasabsence) {
      data <- rbind(data, x@absence)
    }
  }
  cn <- colnames(data)
  if (is.null(var)) {
    var <- cn
  }
  if (is.numeric(var)) {
    var <- cn[var]
  }
  if (length(var)==1) {
    # ?
  }
  var <- var[var %in% cn]
  if (length(var) == 0) { 
    stop('var not found')	
  }
  
  compute_response(x, var, at, data, cn, expand, fun, N)
  
}


#' Compute maxent response
#' @param x model
#' @param var char, variable names
#' @param at see \code{\link[dismo]{response}}
#' @param data see \code{\link[dismo]{response}}
#' @param cn char, column names
#' @param expand see \code{\link[dismo]{response}}
#' @param fun see \code{\link[dismo]{response}}
#' @param N numeric, defines number of interpolates (rows) 
compute_response <- function(x, var, at, data, cn, expand, fun, N) {
  f <- sapply(data, is.factor)
  notf <- !f
  m <- matrix(nrow=1, ncol=ncol(data))
  
  if (is.null(at)) {
    m <- data
    nrm <- nrow(m)
  } else if (is.function(at)) {
    if (sum(notf) > 0) {
      m[notf] <- as.numeric(apply(data[,notf,drop=FALSE], 2, at))
    } 
    if (sum(f) > 0) {
      m[f] <- as.numeric(apply(data[,f,drop=FALSE], 2, raster::modal))
    }
    m <- matrix(m, nrow=N, ncol=length(m), byrow=TRUE)
    colnames(m) <- cn
  } else {
    at <- at[cn]
    m <- as.vector(at)
    #m <- matrix(m, nrow=1)
    m <- matrix(m, nrow=N, ncol=length(m), byrow=TRUE)
    colnames(m) <- names(at)
  }
  m <- data.frame(m)
  
  RR <- lapply(var,
    function(vr){
      i <- which(cn==vr)
      if (is.null(at)) {
        nr <- ifelse(length(var)==1, N * 0.25, N * 0.1)
        v <- data[,i]
        if (is.factor(v)) {
          v <- as.numeric(levels(v))				
          fact <- TRUE
        } else {
          fact <- FALSE
          r <- range(v)
          expand <- round(abs(expand))
          v <- (r[1]-expand) + 0:(nr-1) * (r[2]-r[1] + 2*expand)/(nr-1)
        }
        
        mm <- m[rep(1:nrm, length(v)), ]
        mm[, vr] <- rep(v, each=nrm)
        p <- fun(x, mm)
        pd <- cbind(v, colMeans(matrix(p, nrow=nrm), na.rm=TRUE))
        
      } else {
        nr <- N
        v <- data[,i]
        if (is.factor(v)) {
          v <- as.numeric(levels(v))
          v <- rep(v, ceiling(N/length(v)))
          v <- v[seq_len(N)]	
          fact <- TRUE
        } else {
          fact <- FALSE
          r <- range(v)
          expand <- round(abs(expand))
          v <- (r[1]-expand) + 0:(nr-1) * (r[2]-r[1] + 2*expand)/(nr-1)
        }
        
        mm <- m
        mm[, vr] <- v
        p <- fun(x, mm)
        pd <- cbind(mm[, vr], p)
      }
      colnames(pd) <- c(vr, paste0(vr, ".response"))
      pd |>
        dplyr::as_tibble()
    }) |>
    dplyr::bind_cols()
}
