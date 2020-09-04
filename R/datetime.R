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
  if (missing(x))  x <- Sys.Date()
  lut[lubridate::month(x)]
}

#' Compute a season start from POSIXct or Date values
#'
#' @export
#' @param x POSIXct or Date vector
#' @return POSIXct or Date that starts the season a date belongs to
date_to_seasonstart <- function(x){
  if (missing(x)) x <- Sys.Date()
  r <- range(x)
  s <- seq_season(from = r[1], to = r[2])
  s[findInterval(x, s)]
}

#' Convert to as POSIXct to an 8D week number (1-46) or 7D week number (1-52)
#'
#' @export
#' @param x POSIXct or Date vector
#' @param week_length numeric, the number of days per week
#' @return numeric 1-46 or 1-52 week number
date_to_week <- function(x, week_length = c(8,7)[1]){
  if (missing(x)) x <- Sys.Date()
  J <- as.numeric(format(x, "%j"))
  (J-1) %/% week_length + 1
}

#' Convert a Date, character, or POSIXlt to POSIXct
#'
#' @export
#' @param x vector of Date objects
#' @param time character time of day, defaults to '00:00:00'
#' @param tz character time zone, defaults to 'UTC'
#' @param format for character inputs, the format defaults to 'YYYY-mm-dd HH:MM:SS'
#' @return POSIXct equivalent of input
as_POSIXct <- function(x, time = '00:00:00', tz = 'UTC', format = '%Y-%m-%d %H:%M:%S'){
    if (inherits(x, 'POSIXct')) return(x)
    if (inherits(x, 'Date')) x <- as.POSIXct(sprintf("%s %s", format(x, '%Y-%m-%d'), time), tz = tz)
    if (inherits(x, 'POSIXlt')) x <- as.POSIXct(x)
    if (inherits(x, 'character')) x <- as.POSIXct(x, format = format, tz = tz)
    return(x)
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
date_to_seasonprior <- function(x){
  if (missing(x)) x <- Sys.Date()

    # get the range of dates, and dial the first one back on year
    r <- range(x)
    if (inherits(x, "POSIXt")){
      r[1] <- r[1] - 366*24*60*60
    } else {
      r[1] <- r[1] - 366
    }
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

#' Generate a sequence of POSIXct or Date by season start dates
#'
#' @export
#' @param from POSIXct or Date, the starting date
#' @param to POSIXct or Date, the ending date
#' @return POSIXct or Date sequence
seq_season <- function(
    from = as.Date("2003-01-01"),
    to = Sys.Date()){

   if (inherits(from, "POSIXt")){
     to = as_POSIXct(to)

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
   } else {
     to <- as.Date(to)
     fy <- lubridate::year(from[1])
     fm <- lubridate::month(from[1])
     # note there are 12 entries
     from <- switch(fm[1],
                    lubridate::ymd(paste0(fy-1,"-12-01"),quiet = TRUE),
                    lubridate::ymd(paste0(fy-1,"-12-01"), quiet = TRUE),
                    lubridate::ymd(paste0(fy,"-03-01"), quiet = TRUE),
                    lubridate::ymd(paste0(fy,"-03-01"), quiet = TRUE),
                    lubridate::ymd(paste0(fy,"-03-01"), quiet = TRUE),
                    lubridate::ymd(paste0(fy,"-06-01"), quiet = TRUE),
                    lubridate::ymd(paste0(fy,"-06-01"), quiet = TRUE),
                    lubridate::ymd(paste0(fy,"-06-01"), quiet = TRUE),
                    lubridate::ymd(paste0(fy,"-09-01"), quiet = TRUE),
                    lubridate::ymd(paste0(fy,"-09-01"), quiet = TRUE),
                    lubridate::ymd(paste0(fy,"-09-01"), quiet = TRUE),
                    lubridate::ymd(paste0(fy,"-12-01"), quiet = TRUE))
   }

  seq(from = from[1], to = to[1], by = '3 months')
}

#' Compute today's date as POSIXct
#'
#' @export
#' @param x Date or POSIXct or charcater date to be converted with as_POSIXct()
#' @param form the form of the date returned, possible a formatting pattern
#' @return POSIXct or Date
today <- function(x,
  form = c('POSIXct', "Date", "%Y-%m-%d", 'week', 'season')[2]){
  if (missing(x)) x <- Sys.Date()
  switch(form[1],
         'week' = date_to_week(x),
         'season' = date_to_seasonstart(x),
         "Date"   = x,
         'POSIXct' = as_POSIXct(x),
         format(x, form[1]))
}

#' Compute the next n day
#'
#' @export
#' @param x Date or POSIXct, starting date, generally \code{today()}
#' @param n the number of days ahead (or behind if negative)
#' @return POSIXct or Date
tomorrow <- function(x, n = 1){
  if (missing(x)) x <- Sys.Date()
  if (inherits(x, "Date")){
    x + n
  } else {
    x + (n * 86400)
  }
}


#' Compute the prior n day.
#'
#' A wrapper around \code{tomorrow()}
#'
#' @export
#' @param x POSIXct date as a starting date, generally \code{today()}
#' @param n the number of days prior (or ahead if positive)
#' @return POSIXct
yesterday <- function(x, n = -1){
  if (missing(x)) x <- Sys.Date()
  if (inherits(x, "Date")){
    x + n
  } else {
    x + (n * 86400)
  }
}


#' Compute the doy window given the day of year and window size
#'
#' Given doy 100 with a window of c(-5,5) will yield -95,-96,...104,105
#'
#' @export
#' @param x the day of year (should be 1-366 or 001-366)
#' @param w the window as 2 elements [days before, days after]
#' @param MAX the highest possible day number
#' @return numeric doy vector
doy_window <- function(x = 1, w = c(-5,5), MAX = 366){
    newday <- as.numeric(x) + seq(from = w[1], to = w[2])
    ix <- newday < 1
    if (any(ix)) newday[ix] = MAX + newday[ix]
    ix <- newday > MAX
    if (any(ix)) newday[ix] = newday[ix] - MAX
    newday
}


#' Compute the union of a list of vectors
#'
#' @export
#' @param x a list of zero or more vectors
#' @return a vector of the union or NULL is the input is empty
munion <- function(x){
    if (!is.list(x)) stop("input must be a list")
    n <- length(x)
    if (n == 0) return(NULL)
    if (n == 1) return(x[[1]])

    s <- x[[1]]
    for (i in 2:n) s <- union(s, x[[i]])
    s
}

#' Compute the intersection of a list of vectors
#'
#' @export
#' @param x a list of zero or more vectors
#' @return a vector of the intersection or NULL is the input is empty
mintersect <- function(x){
    if (!is.list(x)) stop("input must be a list")
    n <- length(x)
    if (n == 0) return(NULL)
    if (n == 1) return(x[[1]])

    s <- x[[1]]
    for (i in 2:n) s <- intersect(s, x[[i]])
    s
}

