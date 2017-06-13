#' write a cleaned csv of a version of maxEnt models 
#' report AUC and all contributing factors 
#'
#' @export
#' @param version_path absolute path to the folder holding the version of models
cleaned_results <- function(version_path){
 
  # find and read in maxEnt models
  model_list <-list.dirs(path = version_path, recursive = FALSE)
  model_list <- lapply(model_list,dismotools::read_maxent)
  
  
  # filter out failed models
  cntr=0
  filtered_list=list()
  for (model in model_list) {
    if(model_sucessful(model)) {
      filtered_list[cntr] = model
      cntr=cntr+1
    }
    
  } 
  

  #create new dataframe 
  auc_list <- lapply(filtered_list,dismotools::maxent_get_results,"AUC")
  contributor_matrix <- lapply(filtered_list, dismotools::maxent_get_results,"contribution")
  
 cleaned.df <-as.data.frame(rbind(contributor_matrix[[1]]))

  # add contributors to dataframe
  for (i in 2:length(contributor_matrix)){
    cleaned.df = rbind(cleaned.df,contributor_matrix[[i]])
  }
  
  # add AUC to dataframe
  cleaned.df$auc = auc_list
 
 # write out cleaned csv
  write.csv(file=version_path, x=cleaned.df)

    
}


#' checks if a maxEnt model ran sucessfully 
#'
#' @export
#' @param model a maxEnt model object
#' @return logical. If model ran sucessfully?
model_sucessful <- function(model){
  return( is.numeric(model@results))
}  


# Test Code 
cleaned_results("/opt/data/tickcast/versions/v2/v2.00/model")
