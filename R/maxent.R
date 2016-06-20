#' Read a dismo::MaxEnt model saved in a directory. 
#' 
#' @export
#' @param path fully qualified path to where the MexEnt model is saved
#' @return a MaxEnt model object or NULL
read_maxent <- function(path){
   # remove this if we move this fucntion to a package the depends upon dismo
   stopifnot(require(dismo))
   
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
      x <- read.csv(filename, row.names = 1, stringsAsFactors = FALSE)
      t(as.matrix(x))
   }
   
   # read the contents of the 'presence' file
   # @param drop_cols names or indices of columns to drop, or NULL to drop none
   # @return a data.frame with one predictor per column
   me_read_presence <- function(filename = 'presence', 
      drop_cols = c("species", "x", "y")){
      if (!file.exists(filename)) return(NULL)
      x <- read.csv(filename, stringsAsFactors = FALSE)
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
   X <- new('MaxEnt')
   if (!is.null(lambdas)) slot(X, 'lambdas') <- lambdas
   if (!is.null(results)) slot(X, 'results') <- results
   slot(X, 'path') <- path
   slot(X, 'html') <- file.path(path, 'maxent.html')
   if (!is.null(presence)) slot(X, 'presence') <- presence
   if (!is.null(absence)) slot(X, 'absence') <- absence
   slot(X, 'hasabsence') <- !is.null(absence)
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
   t(as.matrix(read.csv(filename[1], stringsAsFactors = FALSE)))
}

#' Retrieve the names of the input variables of a maxent model
#'
#' @export
#' @param object a MaxEnt model
#' @return characater vector of variable names 
maxent_get_varnames <- function(object){
   stopifnot(inherits(object, 'MaxEnt'))
   colnames(object@presence)
}

#' Retrieve the results of a MaxEnt model by name
#'
#' @export
#' @param object a MaxEnt model
#' @param name the result name
#' @return numeric, matrix or vector depending upon what is requested
#' @examples
#' \dontrun{
#'  auc <- maxent_get_results(object, 'AUC')
#'  contrib <- maxent_get_results(object, 'contribution')
#' }
maxent_get_results <- function(object, name){
   stopifnot(inherits(object, 'MaxEnt'))
   nm <- tolower(name[1])
   if (nm == 'contribution'){
      vn <- maxent_get_varnames(object)
      x <- object@results[paste0(vn,".contribution"),]
   } else if (nm == 'importance'){
      vn <- maxent_get_varnames(object)
      x <- object@results[paste0(vn,".permutation.importance"),]
   } else if(nm == 'auc'){
      x <- object@results['Training.AUC',]
   } else{
      x <- object@results[name,]
   }
   x
}
