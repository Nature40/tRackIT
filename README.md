# The tRackIT R-package

The tRackIT R-Package provides functionalities for the processing of data recorded in local automatic radio-tracking studies. It is specifically tailored to data recorded with one of the sensors from the [tRackIT ecosystem](https://dx.doi.org/10.18420/informatik2021-035) (tRackIT-Stations, BatRack), but can also be used for other systems. The functionalities provided in the package cover project and individual management, raw signal data processing and the generation of high-level information such as the [calculation of locations]( https://doi.org/10.1111/2041-210X.13294) and the [classification of behavioral states](https://doi.org/10.1101/2022.03.22.485147)  based on pattern in the recorded vhf-signals. It provides a default data structure to guarantee easy exchangeability of data and analysis scripts between scientists. 

## Getting startet

The package uses functionalities from the[telemetr](https://github.com/barryrowlingson/telemetr) R-Package developed by Barry Rowlingson. It provides all methods for the localization of a transmitter described in [this article](https://doi.org/10.2307/1268030) using fortran in the background. To make use of the dependencies however, some adjustments to the package had to be conducted, which is why the version used in the tRackIT R-package is hostet under the Nature40 github account. Before the tRackIT package can be installed, please install the telemtr package as follows:

```
library(remotes)

Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")

remotes::install_github("Nature40/telemetr")
```

then

```
devtools::install_github("Nature40/tRackIT")

```

## Test data and models

To check out the functionalities of the package, we recommend to download the test data and trained models for activity classification. They are also used in the package vignette. 



 


