#' Create a gif animation using rasterVis::levelplot() and ImageMagick's convert
#'
#' This is Unix/Linix centric.
#'
#' @export
#' @seealso \url{http://www.imagemagick.org/Usage/anim_basics/}
#' @seealso \url{http://imagemagick.org/script/command-line-processing.php}
#' @param R a multi-layer raster object
#' @param ... further arguments for rasterVis::levelplot like 
#'   \itemize{
#'     \item{margin}
#'     \item{at}
#'     \item{par.settings}
#'  }
#' @param other_layers a list of one or more latticeExtra::layer or list of layers
#' @param output_filename the name of the output file, by default 'animation.gif'
#' @param convert_extra character of extra conver arguments.  Defaults to 
#'  "-loop 0 -delay = 50" for infinte looping and 0.5s per frame
#' @return the numeric value returned by convert
animate_gif <- function(R, ..., 
    other_layers = NULL, 
    width = 480, height = 480,
    output_filename = file.path(getwd(), "animation.gif"),
    tmppath = tempfile(pattern = 'animation'), 
    convert_extra = "-loop 0 -delay 50"){  

  if(require(rasterVis) == FALSE) stop("rasterVis package must be installed")
  if (nchar(Sys.which("convert")) == 0) stop("convert command not found")
  
  if (!file.exists(tmppath)){
    ok <- dir.create(tmppath)
    if (!ok) stop("Unable to create temporary path:", tmppath)
  }
  
  nm <- names(R)
  fname <- sprintf("%i.png", 1:length(nm))
  ofile <- file.path(tmppath, fname)
  
  for (i in seq_along(ofile)){
    plot_obj <- rasterVis::levelplot(R[[i]], main = nm[i], ...)
    if (!is.null(other_layers)) {
        if (!inherits(other_layers, 'list')) other_layers = list(other_layers)
        for (ol in other_layers) plot_obj <- plot_obj + ol
    }
    png(ofile[i], width = width, height = height)
    print(plot_obj)
    dev.off()
  }
  
  # move to the directory
  origdir <- setwd(tmppath)

  # craft the convert file iterator "1.png", "2.png", ...
  cat("converting to ", output_filename,"\n")
  inputs <- paste0("%d.png[1-", length(nm), "]")
  CMD <- paste("convert", inputs, convert_extra, output_filename)
    
  # send the command to the shell
  return_value <- system(CMD)   
  
  # tidy - remove files and return user to original location
  setwd(origdir)
  if (return_value == 0) {
    ok <- system(paste("rm -rf", tmppath))
  }
  return(return_value)
}