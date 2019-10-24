#' Converts point location to cell number.
#'
#' This function is essentially a wrapper around the \code{raster::cellFrom*()} functions.
#'
#' @export
#' @param R a multilayer Raster* object
#' @param pts location info for points must have at least one of the following
#'  combinations of columns
#'  \itemize{
#'      \item{'cell' if present this is returned}
#'      \item{'row' and 'col'}
#'      \item{'lon' and 'lat'}
#'      \item{'x' and 'y' assumed to be the same as 'lon' and 'lat'}
#'  }
#' @return vector or cell numbers
cellFromPts <- function(R, pts){

    if (!inherits(R, 'BasicRaster')) stop("Input R must be a Raster* class")
    if (!is.data.frame(pts) && !is.matrix(pts))
        stop("Input pts must be data.frame or matrix")
    if (inherits(pts, 'tbl_df')) pts <- as.data.frame(pts, stringsAsFactors = FALSE)
    # setup
    nm <- colnames(pts)
    cell <- NULL

    # easy
    if ('cell' %in% nm) cell <- pts[['cell']]

    # row/col is the next easiest
    if (!is.null(cell)){
        if (all(c('row', 'col') %in% nm)){
            cell <- raster::cellFromRowCol(R, pts[['row']], pts[['col']])
        }
    }

    # real coordinates provided
    if (is.null(cell)){
        ix <- which(nm %in% c("x", "lon"))[1]
        if (length(ix)> 0){
            iy <- which(nm %in% c("y", "lat"))[1]
            if (length(iy) > 0) cell <- raster::cellFromXY(R, pts[,c(ix,iy)])
        }
    }

    if (is.null(cell)) warning("unable to compute cell - returning NULL")
    return(cell)
}

#' Extract values from a multilayer Raster*
#'
#' @export
#' @param R a single or multilayer Raster* object
#' @param pts location info for points to be extracted. Must be a data frame
#'  or matrix with either set of the following columns. Note that layer (or z)
#'  may be either a layer index number or layer names.
#' \itemize{
#'      \item{'cell' and 'layer' (or 'z')}
#'      \item{'row', 'col' and 'layer' (or 'z')}
#'      \item{'lon', 'lat' and 'layer' (or 'z')}
#'      \item{'x', 'y' and 'layer' (or 'z')}
#'  }
#' @return a vector of values
layers_extractPoints <- function(R, pts){

    if (!inherits(R, 'BasicRaster')) stop("Input R must be a Raster* class")

    nl <- as.integer(raster::nlayers(R))
    if (nl == 1L) return(raster::extract(R, pts))

    nc <- as.integer(raster::ncell(R))
    ny <- raster::nrow(R)
    nx <- raster::ncol(R)

    if (!(is.data.frame(pts) || is.matrix(pts)))
        stop("pts must be data.frame or matrix")
    if (inherits(pts, 'tbl_df')) pts <- as.data.frame(pts, stringsAsFactors = FALSE)

    iz <- which(names(pts) %in% c("layer", "z"))[1]
    if (length(iz) == 0) stop("pts must have 'layer' or 'z' column")
    layer <- pts[,iz]
    if (is.character(layer)) layer <- match(layer, names(R))

    cell <- cellFromPts(R, pts)

    index <- cell + (layer-1) * nc

    raster::getValues(R)[index]
}

#' Select N non-NA random points from a mulitlayer Raster* object
#'
#' @export
#' @param R a multilayer Raster* object
#' @param pts xyz values for presence points [lon, lat, layer] - these
#'  locations are avoided when sampling. Ignored if NULL.   This must have
#'  either set of the following columns.  Note that layer (or z)
#'  may be either a layer index number or layer names.
#' \itemize{
#'      \item{'cell' and 'layer' (or 'z')}
#'      \item{'row', 'col' and 'layer' (or 'z')}
#'      \item{'lon', 'lat' and 'layer' (or 'z')}
#'      \item{'x', 'y' and 'layer' (or 'z')}
#'  }
#' @param N the number of points to select
#' @return data.frame of sampled points with the following
#'  \itemize{
#'      \item{lon}
#'      \item{lat}
#'      \item{col}
#'      \item{row}
#'      \item{cell}
#'      \item{layer}
#'      \item{value}
#'  }
layers_randomPoints <- function(R, pts = NULL, N = 1000){

    if (!inherits(R, 'BasicRaster')) stop("Input R must be a Raster* class")
    if (!is.null(pts)){
        if (!(is.data.frame(pts) || is.matrix(pts)))
            stop("pts must be data.frame or matrix")
        if (inherits(pts, 'tbl_df')) pts <- as.data.frame(pts, stringsAsFactors = FALSE)
        if (ncol(pts) < 3) stop("pts must have at least three columns")
        if (!any(c("lon", "x", "col") %in% names(pts)))
            stop("pts must have 'col', 'lon' or 'x' column")
        if (!any(c("lat", "y", "row") %in% names(pts)))
            stop("pts must have 'row', 'lat' or 'y' column")
        iz <- which(names(pts) %in% c("layer", "z"))[1]
        if (length(iz) == 0) stop("pts must have 'layer' or 'z' column")
    }
    # get all of the data as a vector - storage is 1,2,3 across rows
    # starting form upper left
    v <- raster::getValues(R)
    nc <- as.integer(raster::ncell(R))
    nl <- as.integer(raster::nlayers(R))
    ny <- raster::nrow(R)
    nx <- raster::ncol(R)


    # if pts are present, then we flag those in vector 'v'
    if (!is.null(pts)){
        if (is.character(pts[,iz])){
            z <- match(pts[,iz], names(R))
        } else {
            z <- match(pts[,iz], 1:nl)
        }
        #col <- colFromX(R, pts[, ix])
        #row <- rowFromY(R, pts[, iy])
        #cell <- cellFromRowCol(R, row, col)
        cell <- cellFromPts(R, pts)
        # this is the number of layer steps 'extra'
        bump <- 0:(nl-1L) * nc
        index <- cell + bump[z]
        v[index] <- NA
    }
    # create a dummy index
    aix <- seq.int(from = 1L, to = (nc * nl))
    # determine where NAs occur
    nna <- !is.na(v)
    if (sum(nna) < N) stop("N exceeds number of available values to select")
    # sample the *indices*
    s <- sort(sample(aix[nna], N))
    # convert to cells, layers, rows, columns, and xy
    cell <- ((s-1L) %% nc) + 1L
    layer <- ((s-1L) %/% nc) + 1L
    rc <- raster::rowColFromCell(R, cell)
    xy <- raster::xyFromCell(R, cell)

    x <- data.frame(
        lon = raster::xFromCol(R, rc[,'col']),
        lat = raster::yFromRow(R, rc[,'row']),
        row = rc[,'row'],
        col = rc[,'col'],
        cell = cell,
        layer = layer,
        value = v[cell + (layer-1)*nc],
        stringsAsFactors = TRUE)
    invisible(x)
}
