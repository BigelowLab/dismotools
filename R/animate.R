#' Create a gif animatation using ImageMagick's convert
#'
#' @export
#' @seealso \url{http://www.imagemagick.org/Usage/anim_basics/}
#' @seealso \url{http://imagemagick.org/script/command-line-processing.php}
#' @param FF charcater vector of PNG filenames
#' @param output_filename the name of the output file, by default 'animation.gif'
#' @param convert_extra character of extra conver arguments.  Defaults to
#'  "-loop 0 -delay = 50" for infinte looping and 0.5s per frame
#' @return the numeric value returned by convert
images_to_gif <- function(FF,
    output_filename = "animation.gif",
    convert_extra = "-loop 0 -delay 50"){


    if (nchar(Sys.which("convert")) == 0) stop("convert command not found")


    cat(FF, sep = "\n", file = "images.txt")
    CMD <- paste("convert @images.txt", convert_extra, output_filename)

    # send the command to the shell
    return_value <- system(CMD)
    return(return_value)
}




#' Create a gif animation using rasterVis::levelplot() and ImageMagick's convert
#'
#' This is Unix/Linux centric.
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
#' @param width numeric image width in pixels
#' @param height image height in pixels
#' @param tmppath string, temporary working directory to use
#' @param other_layers a list of one or more latticeExtra::layer or list of layers
#' @param output_filename the name of the output file, by default 'animation.gif'
#' @param convert_extra character of extra conver arguments.  Defaults to
#'  "-loop 0 -delay = 50" for infinte looping and 0.5s per frame
#' @return the numeric value returned by convert
animate_gif <- function(R, ...,
    other_layers = NULL,
    width = 480, height = 480,
    output_filename = file.path(".", "animation.gif"),
    tmppath = tempfile(pattern = 'animation'),
    convert_extra = "-loop 0 -delay 50"){


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
    grDevices::png(ofile[i], width = width, height = height)
    print(plot_obj)
    grDevices::dev.off()
  }

  # move to the directory
  origdir <- setwd(tmppath)

  ofile = basename(output_filename)
  # craft the convert file iterator "1.png", "2.png", ...
  cat("converting to ", ofile,"\n")
  ofile = file.path(getwd(), ofile)
  inputs <- paste0("%d.png[1-", length(nm), "]")
  CMD <- paste("convert", inputs, convert_extra, ofile)

  # send the command to the shell
  return_value <- system(CMD)

  # tidy - remove files and return user to original location
  setwd(origdir)
  ok = file.rename(ofile, output_filename)

  if (return_value == 0) {
    ok <- system(paste("rm -rf", tmppath))
  }
  return(return_value)
}
