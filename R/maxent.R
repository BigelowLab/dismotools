#' Read a summary file for a model family run - generally consists of a CSV file
#' with one row per model generated
#'
#' @export
#' @param filename the filename with path
#' @return either NULL or a tibble
read_model_summary <- function(filename = "model_summary.csv"){
  if (!file.exists(filename)){
    warning("summary file not found:", filename)
    r <- NULL
  } else {
    r <- suppressMessages(readr::read_csv(filename))
  }
  r
}


#' Plot a three panel summary: contributions, AUC and counts
#'
#' @export
#' @param x a tibble of summary data
#' @param version character, the model verison identifier
#' @param fauc tibble or NULL, if not NULL then
#'        include the FAUC in the middle panel
#' @param model_name text, by default 'model' but can be anything you prefer
#' @param count_legend text, location for legend in right hand counts plot
#' @param auc_legend text, location for legend in middle AUC/FAUC plot
#' @return nothing
plot_model_summary <- function(x = read_model_summary(),
                               version = "v0.000",
                               fauc = NULL,
                               model_name = "model",
                               auc_legend = "topright",
                               count_legend = "topleft"){
  opar <- par(no.readonly = TRUE)
  on.exit( par(opar) )

  has_shannon <- ("shannon" %in%  colnames(x))

  col <- c(auc = "#000000",
           fauc = "#0000FF",
           shannon = "#CD6600",
           bkg = "#0000FF",
           pres = "#000000")
  lwd <- c(auc = 2,
           fauc = NA,
           shannon = 1,
           pres = 2,
           bkg= 1)
  pch <- c(auc = NA,
           fauc = 19,
           shannon = NA)
  legend_loc <- "bottomright"
  inset = inset=c(0,1)

  auc <- x$auc
  id <- basename(x$path)
  m <- as.matrix(x %>% dplyr::select(-.data$path, -.data$auc, -.data$p_count,
                                     -.data$b_count, -.data$shannon))
  rownames(m) <- id
  xx <- seq_len(ncol(m))
  yy <- ids <- as.numeric(id)
  rids <- range(ids)


  par(mfrow = c(1,3), mar = par("mar") + c(2,0,0,0))


  image(xx, yy, t(m),
        ylim = rids,
        breaks = seq(from = 0, to = 90, length = 10),
        zlim = c(0,100),
        main = sprintf("model contributions, %s", version[1]),
        xlab = '',
        ylab = model_name[1],
        xaxt = "n", yaxt = "n",
        col = RColorBrewer::brewer.pal(9, "Oranges"))
  axis(1, at = seq_len(ncol(m)), colnames(m), las = 2)


  pretty_id <- pretty(ids)
  axis(2, at = pretty_id, TRUE, las = 2)

  if (has_shannon) {
    xrange <- c(0, 1.0)
  } else {
    xrange <- c(0.5,1)
  }
  if (!is.null(fauc)){
    plot(auc, ids, typ = 'l',
         lwd = lwd['auc'],
         col = col['auc'],
         xlim = xrange,
         ylim = range(yy),
         yaxt = "n",
         yaxs = 'i',
         xlab = ifelse(has_shannon, 'AUC and Shannon index', "AUC"),
         ylab = model_name[1],
         main = ifelse(has_shannon, 'Model AUC and Shannon index', "Model AUC"))
    if (has_shannon) lines(x$shannon, ids, typ = 'l',
                           lwd = lwd['shannon'],
                           col = col['shannon'])
    fauc <- fauc %>%
      dplyr::mutate(ids = ids)
    points(fauc$fauc, fauc$ids,
           col = col['fauc'],
           pch = pch['fauc'])
    if (has_shannon){
      #legend("bottomright", c("group A", "group B"), pch=c(1,2), lty=c(1,2),
      #inset=c(0,1), xpd=TRUE, horiz=TRUE, bty="n"
      #)
      legend(
        legend_loc,
        xpd = TRUE,
        horiz = TRUE,
        inset = inset,
        bg = "transparent",
        bty = "n",
        legend = c("model", "prediction", "Shannon"),
        lwd = lwd,
        pch = pch,
        col = col)
    } else {
      legend(
        legend_loc,
        xpd = TRUE,
        horiz = TRUE,
        inset = inset,
        bg = "transparent",
        bty = "n",
        legend = c("model", "prediction"),
        lwd = lwd[c("auc", 'fauc')],
        pch = pch[c("auc", "fuac")],
        col = col[c("auc", "fuac")] )
    }


  } else {

    plot(auc, ids, typ = 'l', lwd = 2,
         xlim = xrange,
         ylim = range(yy),
         yaxt = "n",
         yaxs = 'i',
         xlab = ifelse(has_shannon, 'AUC and Shannon index', "AUC"),
         ylab = model_name[1],
         main = ifelse(has_shannon, 'Model AUC and Shannon index', "Model AUC"))
    if (has_shannon) lines(x$shannon, ids, typ = 'l',
                           lwd = lwd['shannon'],
                           col = col['shannon'])
    if (has_shannon){
      legend(
        legend_loc,
        xpd = TRUE,
        horiz = TRUE,
        inset = inset,
        bg = "transparent",
        bty = "n",
        legend = c("AUC",  "Shannon"),
        lwd = lwd[c("auc", "shannon")],
        pch = pch[c("auc", "shannon")],
        col = col[c("auc", "shannon")] )
    } else {
      #legend(auc_legend[1],
      #       bg = "transparent",
      #       bty = "n",
      #       legend = "auc",
      #       lwd = lwd["auc"],
      #       pch = pch["auc"],
      #       col = col["auc"] )
    }
  }  # shannon
  axis(2, at = pretty_id, TRUE, las = 2)

  pn <- x$p_count
  bn <- x$b_count
  xlim <- c(0, max(pmax(pn,bn)))
  plot(pn, ids, typ = "l",
       lwd = lwd[['pres']],
       col = col[['pres']],
       xlim = xlim,
       ylim = range(yy),
       yaxt = "n",
       yaxs = 'i',
       xlab = 'count',
       ylab = model_name[1],
       main = 'Counts')
  lines(bn, ids,
        lwd = lwd[['bkg']],
        col = col[['bkg']])
  axis(2, at = pretty(ids), pretty(id), las = 2)
  legend(
    legend_loc,
    xpd = TRUE,
    horiz = TRUE,
    inset = inset,,
    bg = "transparent",
    bty = "n",
    legend = c("presence", "background"),
    lwd = lwd[c("pres", "bkg")],
    col = col[c("pres", "bkg")])
  invisible(NULL)
}



#' Checks if a dismo model ran successfully
#'
#' @export
#' @param model a model object such as MaxEnt, CliMap
#' @return logical. If model ran successfully?
model_successful <- function(model){

  classname <- class(model)
  ok <- FALSE
  if ("MaxEnt" %in% classname){
    ok <- is.numeric(slot(model, "results"))
  } else {
    stop("class not known yet:", classname)
  }

  return(ok)
}


#' Write a maxent summary file
#'
#' @export
#' @param x list or data frame of maxent summary
#' @param filename optional filename to write to, by default file.path(x$path, "summary.txt")
#'        or file.path(x$path, "summary.csv") if input is a data.frame
#' @return the original list or data frame
maxent_write_summary <- function(x, filename = NULL){

    if (inherits(x, "data.frame")){
        if (is.null(filename)) filename = file.path(x$path, "summary.csv")
        x = readr::write_csv(x,filename )
    } else {
        if (is.null(filename)) filename = file.path(x$path, "summary.txt")
        conn = file(filename, open = 'wt')
        cat("[summary]\n", file = conn)
        cat(sprintf("path: %s",x$path), "\n", file = conn)
        cat(sprintf("auc: %0.4f",x$auc), "\n", file = conn)
        cat(sprintf("predictors: %s", paste(x$predictors, collapse = " ")), "\n",
            file = conn)
        cat(sprintf("presence count: %i", x$p_count), "\n", file = conn)
        cat(sprintf("background count: %i", x$b_count), "\n", file = conn)
        cat("contributions:\n", file = conn)
        write.csv(x$contrib, row.names = TRUE, file = conn)
        close(conn)
    }
    x
}

#' Summarize a maxent model
#'
#' @export
#' @param x a MaxEnt model or a list of them.
#' @param fmt character either 'list' or 'dataframe' or 'tibble' (same as 'dataframe')
#'    Defines the format of the output.  If x is a list and fmt is 'list' then
#'    as list is returned, otherwise if x is a lists then a data.frame or tibble is returned.
#' @param save_summary logical, if TRUE then save to a text file as summary.txt
#'        or summary.csv  Ignored if x is a list
#' @param ... further arguments for maxent_write_summary, ignored if x is a list
#' @return a list with summary information
#' \itemize{
#'  \item{path name of the model}
#'  \item{auc AUC value}
#'  \item{predictors vector of predictor names}
#'  \item{contrib data.frame of contributions by each predictor}
#'  \item{p_count number of presence points}
#'  \item{b_cound number of background points}
#'  }
#'  OR a tibble (data.frame) with the same info in one row
maxent_summary <- function(x,
    fmt = c("list", "dataframe", "tibble")[3],
    save_summary = FALSE, ...){

    if (inherits(x, 'list')){
      xx <- lapply(x, maxent_summary, fmt = fmt, save_summary = FALSE)
      r <- switch(tolower(fmt[1]),
        "list" = xx,
        dplyr::bind_rows(xx))
      return(r)
    }

    path = slot(x, "path")
    auc = maxent_get_results(x,"auc")
    predictors = maxent_get_varnames(x)
    p_count <- nrow(slot(x, "presence"))
    b_count <- nrow(slot(x, "absence"))
    contrib = t(maxent_get_results(x,"contribution")[,1])
    shannon <- shannon_index(contrib)
    if (tolower(fmt[1]) == 'list'){
        r = list(
            path = path,
            auc = auc,
            predictors = predictors,
            p_count = p_count,
            b_count = b_count,
            shannon = shannon,
            contrib = contrib)
    } else {
        colnames(contrib) <- predictors
        contrib <- dplyr::as_tibble(as.data.frame(contrib))
        r = dplyr::tibble(path, auc, p_count, b_count, shannon) %>%
            dplyr::bind_cols(contrib)
    }
    if (save_summary) ok = maxent_write_summary(r, ...)
    r
}

#' Given a list of maxent models prepare a data frame of results
#'
#' @export
#' @param x a list of one or more maxent models
#' @param include character vector fo one or more components to include in the result
#'  - ignored for now
#' @return data.frame or NULL  Results for 'empty' models are excluded
maxent_assemble_results <- function(x,
    include = c("auc", "contribution")){

    nm <- sapply(x, function(x) basename(slot(x, "path")))
    auc <- sapply(x, maxent_get_results, 'auc')
    null <- sapply(auc, is.null)
    contrib <- lapply(x, maxent_get_results, "contribution")
    contrib <- as.data.frame(do.call(rbind, contrib[!null]), stringsAsFactors = FALSE)
    colnames(contrib) <- gsub(".contribution", "", colnames(contrib), fixed = TRUE)
    data.frame(
        name = nm[!null],
        auc = unlist(auc[!null]),
        contrib,
        stringsAsFactors = FALSE)
}


#' Read a dismo::MaxEnt model saved in a directory.
#'
#' @export
#' @param path fully qualified path to where the MaxEnt model is saved
#'  If multiple paths are provided then a list of models is returned.
#'  You just have to keep track of this yourself so you aren't surprised.
#' @return a MaxEnt model object, a list of Maxent models or NULL
read_maxent <- function(path){

   if (length(path) > 1) {
        x <- lapply(path, read_maxent)
        names(x) <- basename(path)
        return(x)
    }

   # read the lambdas file 'species.lambdas' - no processing required
   # @return a character vector
   me_read_lambdas <- function(filename='species.lambdas'){
      if (!file.exists(filename)) return(NULL)
      readLines(filename)
   }

   # read the contents of 'maxentResults.csv'
   # @return a matrix
   me_read_results <- function(filename = 'maxentResults.csv'){
      if (!file.exists(filename)) return(NULL)
      x <- utils::read.csv(filename, row.names = 1, stringsAsFactors = FALSE)
      t(as.matrix(x))
   }

   # read the contents of the 'presence' file
   # @param drop_cols names or indices of columns to drop, or NULL to drop none
   # @return a data.frame with one predictor per column
   me_read_presence <- function(filename = 'presence',
      drop_cols = c("species", "x", "y")){
      if (!file.exists(filename)) return(NULL)
      x <- utils::read.csv(filename, stringsAsFactors = FALSE)
      if (!is.null(drop_cols)) for (i in drop_cols) x[,i] <- NULL
      x
   }

   # read the contents of the 'absence' file
   # @param drop_cols names or indices of columns to drop, or NULL to drop none
   # @return a data.frame with one predictor per column
   me_read_absence <- function(filename = 'absence',
      drop_cols = c("species", "x", "y")){
      if (!file.exists(filename)) return(NULL)
      me_read_presence(filename, drop_cols = drop_cols)
   }

   if (!file.exists(path[1])) return(NULL)
   orig_wd <- setwd(path[1])
   lambdas <- me_read_lambdas()
   results <- me_read_results()
   presence <- me_read_presence()
   absence <- me_read_absence()
   #X <- methods::new('MaxEnt')
   X <- readRDS(system.file("emptyMaxent.rds", package = 'dismotools'))
   if (!is.null(lambdas)) methods::slot(X, 'lambdas') <- lambdas
   if (!is.null(results)) methods::slot(X, 'results') <- results
   methods::slot(X, 'path') <- path
   methods::slot(X, 'html') <- file.path(path, 'maxent.html')
   if (!is.null(presence)) methods::slot(X, 'presence') <- presence
   if (!is.null(absence)) methods::slot(X, 'absence') <- absence
   methods::slot(X, 'hasabsence') <- !is.null(absence)
   setwd(orig_wd)
   invisible(X)
}

#' Read MaxEnt results from a text file
#'
#'
#' @export
#' @param filename the fully qualified filename to read
#' @return a matrix of results as per MaxEnt@results
maxent_read_results <- function(filename){
   stopifnot(file.exists(filename[1]))
   t(as.matrix(utils::read.csv(filename[1], stringsAsFactors = FALSE)))
}

#' Retrieve the names of the input variables of a maxent model
#'
#' @export
#' @param object a MaxEnt model
#' @return characater vector of variable names or NULL
maxent_get_varnames <- function(object){
   stopifnot(inherits(object, 'MaxEnt'))
   if (is.null(slot(object, "presence")) || (nrow(slot(object, "presence"))==0)) return(NULL)
   colnames(slot(object, "presence"))
}

#' Retrieve the results of a MaxEnt model by name
#'
#' @export
#' @param object a MaxEnt model
#' @param name the result name
#' @return numeric, matrix or vector depending upon what is requested or NULL
#' @examples
#' \dontrun{
#'  auc <- maxent_get_results(object, 'AUC')
#'  contrib <- maxent_get_results(object, 'contribution')
#' }
maxent_get_results <- function(object, name){
   stopifnot(inherits(object, 'MaxEnt'))
   if (is.null(slot(object, "results")) || is.null(rownames(slot(object, "results"))) ) return(NULL)
   nm <- tolower(name[1])
   if (nm == 'contribution'){
      vn <- maxent_get_varnames(object)
      x <- (slot(object, "results"))[paste0(vn,".contribution"), ,drop = FALSE]
      names(x) <- vn
   } else if (nm == 'importance'){
      vn <- maxent_get_varnames(object)
      x <- (slot(object, "results"))[paste0(vn,".permutation.importance"),]
      names(x) <- vn
   } else if(grepl("auc", tolower(name[1]), fixed = TRUE)){
      x <- (slot(object, "results"))['Training.AUC',]
   } else{
      x <- (slot(object, "results"))[name,]
      names(x) <- name
   }
   x
}


#' Compute a Shannon index for diversity of equitability
#'
#' @export
#' @param x table or matrix of observations.  If a matrix then, like a table,
#'        columns are variables and rows are records.
#' @param type character, one of "proportion", "counts", etc.
#' @param equitability logical, if TRUE (default) convert diversity to equitability which is
#'        just the normalized (0-1) diversity index
#' @param base numeric, the base of the logarithm
#' @param invert character, if \code{equitabilty} is \code{TRUE} then
#'        then invert using one-minus-equitability or 1/equitability. Ignored
#'        if \code{NA}
#' @param ... arguments for the diversity index function
#' @return vector of diversity index values - one element for each record (row of input)
shannon_index <- function(x,
                            type = c("proportion", "counts")[1],
                            equitability = TRUE,
                            base = exp(1),
                            invert = c(NA, "reciprocal", "one-minus")[1],
                            ...){

  if (!inherits(x, "matrix")) x <- as.matrix(x)
  s <- switch(tolower(type[1]),
         'counts' = shannon_counts_index(x, base = base, ...),
         'proportion' = shannon_proportion_index(x, base = base, ...))
  if (equitability){
    s <- s/log(ncol(x))
    s <- switch(tolower(as.character(invert[1])),
                "reciprocal" = 1/s,
                "one-minus" = 1 - s,
                s)
  }
  s
}


#' Compute Shannon Index froma a table of counts
#'
#' @seealso NIST \href{https://www.itl.nist.gov/div898/software/dataplot/refman2/auxillar/shannon.htm}{entry}
#' @param x table or matrix of observations.  If a matrix then, like a table,
#'        columns are variables and rows are records.
#' @param base numeric, the base of the logarithm
#' @return vector of Shannon index - one element for each record
shannon_counts_index <- function(x, base = exp(1)){
  if (!inherits(x, "matrix")) x <- as.matrix(x)
  apply(x, 1,
    function(y){
      ix <- y <= 0
      n <- sum(y)
      ln <- y * log(y, base = base)
      ln[ix] <- 0
      (n*log(n, base - base) - sum(ln))/n
    })
}

#' Compute the Shannon index from a table of contributions
#'
#' @seealso Wikipedia \href{https://en.wikipedia.org/wiki/Diversity_index#Shannon_index}{entry}
#' @export
#' @param x table or matrix of observations.  If a matrix then, like a table,
#'        columns are variables and rows are records.
#' @param base numeric, the base of the logarithm
#' @param offset numeric, a small value to prevent taking the log of zero. If \code{offset} is
#'        NA or less than or equal to zero then zero-valued elements are removed.
#'        if \code{offset} greater than zero, then zero-valued elements in x are
#'        replaced with this value.
#' @return vector of Shannon index - one element for each record
shannon_proportion_index <- function(x, base = exp(1), offset = c(1e-5, NA)[1]){

  if (!inherits(x, "matrix")) x <- as.matrix(x)
  apply(x, 1,
    function(y) {
      if (is.na(offset[1]) || (offset[1] <= 0)){
        y <- y[y>0]
      } else {
        y[y <= 0] <- offset[1]
      }
      y <- y/sum(y)
      0 - sum( y * log(y + offset, base = base) )
      })
}
