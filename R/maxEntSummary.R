#' Read one or more maxent models stored in a path
#'
#' @export
#' @param version_path path to directory containing one or more 
#'   maxent model directories
#' @param drop_empty logical if TRUE drop empty models
#' @return a list of zero or more maxent models 
maxent_assemble <- function(version_path, drop_empty=TRUE){
 
  if (!dir.exists(version_path[1])) stop("path not found:", version_path[1])
 
   # find and read in maxEnt models
  model_list <-list.dirs(path = version_path, recursive = FALSE)
  if (length(model_list) == 0) return(list())

  names(model_list) <- basename(model_list)
  
  model_list <- try(lapply(model_list,dismotools::read_maxent))
  if (inherits(model_list, 'try-error')) stop("unknown error reading maxents")
  
  # filter unsucessful models
  if (drop_empty){
    model_sucess <- sapply(model_list, model_sucessful)
    model_list <- model_list[model_sucess]   
  }
  invisible(model_list)
    
} 
  
#' checks if a dismo model ran sucessfully 
#'
#' @export
#' @param model a model object such as MaxEnt, CliMap
#' @return logical. If model ran sucessfully?
model_sucessful <- function(model){
  
  classname <- class(model)
  ok <- FALSE
  if ("MaxEnt" %in% classname){
    ok <- is.numeric(model@results)
  } else {
    stop("class not known yet:", classname)
  }
  
  return(ok)
}


#' Creates dataset of results of a set of maxEnt models. 
#' Dataset contains AUC and contributors of the set of maxEnt models
#' Drops models that have failed, then writes dataset to csv
#' 
#' @export
#' @param models one or more maxEnt models
#' @param filename absolute path to desired csv file
#' @return dataframe
maxent_create_resultset <- function(models, filename){
  if (missing(filename)) stop("filename is required")
  # gather data
  auc <- sapply(models, dismotools::maxent_get_results, 'auc') 
  contributor_list <- lapply(models, dismotools::maxent_get_results, 'contribution')
  
  # create dataframe
  cleaned.df <- as.data.frame(do.call(rbind, contributor_list))
  names(cleaned.df) <- sub(".contribution", "", names(cleaned.df), fixed = TRUE)
  model_names <- names(auc)
  cleaned.df = cbind(model_names,auc, cleaned.df, stringsAsFactors=FALSE)
  
  # writeout data to .csv file
  write.csv(file=filename, x=cleaned.df,row.names = FALSE)
  
  return(cleaned.df)
  
}
