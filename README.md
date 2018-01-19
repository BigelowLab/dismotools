# dismotools
R: extra tools for the raster and dismo packages.

### Requirements

+ [R](https://www.r-project.org/)
+ [raster](https://cran.r-project.org/web/packages/raster/index.html) package for geographic data analysis and modeling.
+ [dismo](https://cran.r-project.org/web/packages/dismo/index.html) package for species distribution modeling.

#### Installation
It's easy to install using Hadley Wickham's [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```r
library(devtools)
install_github('BigelowLab/dismotools')
```

### Usage

Maxent tools - reading from disk, getting model elements, assemblies of models

```r
m = read_maxent("/path/to/maxent/directory")
ok = model_successful(m)
auc = maxent_get_results(m, 'AUC')
contrib = maxent_get_results(m, 'contribution')
mm = lapply(list_of_maxent_directories, maxent_assemble_results)
```

Raster tools using a stack or brick of layers `RR`

```r
random_locations = layers_randomPoints(RR, pts = points_to_avoid, N = number to select)
extracted_points = layers_extractPoints(RR, pts_to_extract)
animateGif(RR, ...)
auc = auc_raster(RR, xy = locations, ...)
```

Raster tools using a single layer `R`

```r
auc = auc_raster(R, xy = locations, ...)
```

Date and time

```r
x = today()
tomorrow(x, n = +ndays)
yesterday(x, n = -ndays)
date_to_seasonname(x)
date_to_seasonstart(x)
date_to_week(x, week_length = 7|8|etc)
date_to_seasonprior(x)
seq_season(from = , to = )
```
