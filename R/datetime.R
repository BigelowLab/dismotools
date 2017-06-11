#' Compute a season from POSIXct or Date values
#'
#' @export
#' @param x POSIXct or Date vector
#' @param lut charcater vector of length 12.  x is converted to month number
#'  and is then used as a index into this look up table
#' @return charcater vector of season names, same length as x
date_to_seasonname <- function(x,
    lut = c(
        'DJF', 'DJF',
        'MAM', 'MAM', 'MAM',
        'JJA', 'JJA', 'JJA',
        'SON', 'SON', 'SON',
                      'DJF')){
    lut[lubridate::month(x)]
}


#' Compute a season start from POSIXct or Date values
#'
#' @export
#' @param x POSIXct or Date vector
#' @param lut charcater vector of length 12.  x is converted to month number
#'  and is then used as a index into this look up table
#' @return POSIXct date that starts the season a date belongs to
date_to_seasonstart <- function(x){
    r <- range(x)
    s <- seq_season(from = r[1], to = r[2])
    s[findInterval(x, s)]
}


#' Compute the prior season specified for each provided date. 
#'
#' Consider a spring (MAM) input date, the prior MAM will be 1 year before,
#'  while the prior DJF will be just three months before.
#' 
#' @export
#' @param x POSIXct or Date vector
#' @return tibble with 5 columns of POSIXct vectors
#'  \itemize{
#'      \item{x original input dates}
#'      \item{DJF prior winter start dates}
#'      \item{MAM prior spring start dates}
#'      \item{JJA prior summer start dates}
#'      \item{SON prior autumn start dates}
#' }
date_to_seasonprior <- function(x = tickdata::read_obs()$date){
    
    # get the range of dates, and dial the first one back on year
    r <- range(x)
    r[1] <- r[1] - 366*24*60*60
    # get the season starts that cover the same range
    S <- seq_season(from = r[1], to = r[2])
    S_name <- date_to_seasonname(S)
    fS_name <- as.factor(S_name)
    SS <- split(S,fS_name)
    
    x_czn <- date_to_seasonstart(x)
    
    czn <- c("DJF", "MAM", "JJA", "SON")
    names(czn) <- czn
    
    X <- lapply(czn,
        function(cname){
            ix <- findInterval(x, SS[[cname]])
            y <- SS[[cname]][ix]
            iy <- y == x_czn
            if (any(iy)) y[iy] <- SS[[cname]][ix[iy]-1]
            y
        })
    
    X <- tibble::as.tibble(X)
    tibble::add_column(X, x, .before = 1)   
}


#' Generate a sequence of POSIXct by season start dates
#' 
#' @export
#' @param from POSIXct, the starting date
#' @param to POSIXct, the ending date
#' @return POSIXct sequence
seq_season <- function(
    from = as.POSIXct("2003-12-01 00:00:00", tz = "UTC"),
    to = namtools::today()){
    
    fy <- lubridate::year(from[1])
    fm <- lubridate::month(from[1])
    # note there are 12 entries
    from <- switch(fm[1],
        lubridate::ymd_hms(paste0(fy-1,"-12-01 00:00:00"), tz = "UTC", quiet = TRUE),
        lubridate::ymd_hms(paste0(fy-1,"-12-01 00:00:00"), tz = "UTC", quiet = TRUE),
        lubridate::ymd_hms(paste0(fy,"-03-01 00:00:00"), tz = "UTC", quiet = TRUE),
        lubridate::ymd_hms(paste0(fy,"-03-01 00:00:00"), tz = "UTC", quiet = TRUE),
        lubridate::ymd_hms(paste0(fy,"-03-01 00:00:00"), tz = "UTC", quiet = TRUE),
        lubridate::ymd_hms(paste0(fy,"-06-01 00:00:00"), tz = "UTC", quiet = TRUE),
        lubridate::ymd_hms(paste0(fy,"-06-01 00:00:00"), tz = "UTC", quiet = TRUE),
        lubridate::ymd_hms(paste0(fy,"-06-01 00:00:00"), tz = "UTC", quiet = TRUE),
        lubridate::ymd_hms(paste0(fy,"-09-01 00:00:00"), tz = "UTC", quiet = TRUE),
        lubridate::ymd_hms(paste0(fy,"-09-01 00:00:00"), tz = "UTC", quiet = TRUE),
        lubridate::ymd_hms(paste0(fy,"-09-01 00:00:00"), tz = "UTC", quiet = TRUE),
        lubridate::ymd_hms(paste0(fy,"-12-01 00:00:00"), tz = "UTC", quiet = TRUE))
        
    seq(from = from[1], to = to[1], by = '3 months')      
}


#' Compute today's date as POSIXct
#'
#' @export
#' @param tz character time zone
#' @return POSIXct
today <- function(tz = 'UTC'){
    as.POSIXct(paste(Sys.Date(), "00:00:00"),tz = tz)
}

#' Compute the next n day
#'
#' @export
#' @param x POSIXct date as a starting date, generally \code{today()}
#' @param n the number of days ahead (or behind if negative)
#' @return POSIXct
tomorrow <- function(x = today(), n = 1){
    x + (n * 86400)
}

#' Compute the prior n day
#'
#' @export
#' @param x POSIXct date as a starting date, generally \code{today()}
#' @param n the number of days prior (or ahead if positive)
#' @return POSIXct
yesterday <- function(x = today(), n = -1){
    tomorrow(x, n = n)
}