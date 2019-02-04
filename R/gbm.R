#' Extract a simple table of info from a single gbm.object
#'
#' @export
#' @param x a gbm.object
#' @return a tibble of summary results
extract_gbm <- function(x){
    gbm.x = x$gbm.call$gbm.x
    r <- dplyr::tibble(
        n_input  = x$nTrain,
        tc = x$gbm.call$tree.complexity,
        lr = x$gbm.call$learning.rate,
        n_tree = x$n.trees,
        bag_f = x$bag.fraction,
        auc = x$self.statistics$discrimination
    )
    for (n in gbm.x) r[[n]] = x$contributions[n, 'rel.inf']
    r
}

#' Given a list of one or more gbm.objects, retrieve a tibble that
#' summarizes the key diagnostic elements.
#'
#' @export
#' @param x a list of one or more gbm.objects
#' @return a tibble of the summary values
assemble_gbm <- function(x){

    if (!inherits(x, 'list')) x <- list(x)
    nm <- names(x)
    if (is.null(nm)) nm <- as.character(seq_along(x))
    rr <- lapply(x, extract_gbm)
    dplyr::bind_rows(rr) %>%
        tibble::add_column(name = nm, .before = 1)
}


#' Plot a gbm.object
#'
#' @export
#' @param x a gbm.object
#' @param plot.folds logical, if TRUE show the folds on the plot
#' @param name the name of the model to include in the plot, if NULL then
#'          use x$gbm.call$gbm.y
#' @return NULL invisibly
plot_gbm <- function(x, plot.folds = FALSE, name = NULL){
    if (is.null(name)) name <- x$gbm.call$gbm.y
    y.min <- min(x$cv.values - x$cv.loss.ses)
    y.max <- max(x$cv.values + x$cv.loss.ses)
    if (plot.folds) {
        y.min <- min(x$cv.loss.matrix)
        y.max <- max(x$cv.loss.matrix)
    }
    plot(x$trees.fitted, x$cv.values, type = "l", ylab = "holdout deviance",
         xlab = "no. of trees", ylim = c(y.min, y.max), lwd = 2)
    abline(h = min(x$cv.values), col = 2)
    lines(x$trees.fitted, x$cv.values + x$cv.loss.ses, lty = 2)
    lines(x$trees.fitted, x$cv.values - x$cv.loss.ses, lty = 2)
    if (plot.folds) {
        for (i in 1:nrow(x$cv.loss.matrix)) {
            lines(x$trees.fitted, x$cv.loss.matrix[i, ], lty = 3)
        }
    }
    abline(v = x$n.trees, col = 3)
    title(sprintf("%s, tc = %i, lr = %0.3f",
                  name, x$gbm.call$tree.complexity, x$gbm.call$learning.rate))
    invisible(NULL)
}
