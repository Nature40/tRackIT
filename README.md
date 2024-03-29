# The tRackIT R-package

The tRackIT R-Package provides functionalities for the processing of data recorded in local automatic radio-tracking studies. It is specifically tailored to data recorded with one of the sensors from the [tRackIT ecosystem](https://dx.doi.org/10.18420/informatik2021-035) (tRackIT-Stations, BatRack), but can also be used for other systems. The functionalities provided in the package cover project and individual management, raw signal data processing and the generation of high-level information such as the [calculation of locations]( https://doi.org/10.1111/2041-210X.13294) and the [classification of behavioral states](https://doi.org/10.1111/2041-210X.14037)  based on pattern in the recorded vhf-signals. It provides a default data structure to guarantee easy exchangeability of data and analysis scripts between scientists. For a detailed guide please go to the package [github-page](https://nature40.github.io/tRackIT/). The latest release can be found here [![DOI](https://zenodo.org/badge/519220457.svg)](https://zenodo.org/badge/latestdoi/519220457).


## Getting startet

The package uses functionalities from the [telemetr](https://github.com/barryrowlingson/telemetr) R-Package developed by Barry Rowlingson. It provides all methods for the localization of a transmitter described in [this article](https://doi.org/10.2307/1268030) using fortran in the background. To make use of the dependencies however, some adjustments to the package had to be conducted, which is why the version used in the tRackIT R-package is hostet under the Nature40 github account. Before the tRackIT package can be installed, please install the telemtr package as follows:

```
library(remotes)

Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")

remotes::install_github("Nature40/telemetr")
```

We also make use of very fast c++ based [rolling windows](https://github.com/andrewuhl/RollingWindow) which are not hostet on cran, yet. Please install the package as follows:

```
devtools::install_github("andrewuhl/RollingWindow")
```


Now you can install the tRackIT R-package

```
devtools::install_github("Nature40/tRackIT")

```

## Test data, models and tutorials

To check out the functionalities of the package using the package vignette, we recommend to download the [test data](https://data.uni-marburg.de/handle/dataumr/172) and [trained models]( https://doi.org/10.17192/fdr/79) for activity classification. Models need to be unzipped and stored in the extdata folder of the installed tRackIT-package. We also we provide the following [tutorials](https://github.com/Nature40/tRackIT/tree/main/rmd) describing the workflow for [model tuning and evaluation](https://nature40.github.io/tRackIT_activity_classification_model_tuning_and_evaluation/) for activity classification. You can also check the [reproducible script](https://nature40.github.io/tRackIt_activity_ecological_case_study/) for the case study analysis shown in the paper.



 


