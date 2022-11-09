# rswipl: Embed SWI-Prolog into an R package

The purpose of this package is to embed SWI-Prolog into an R library,
such that other packages can link to the SWI-Prolog runtime without the need
to install the program on their computer. Use cases include people who
cannot install programs, including the CRAN server.

This R package is *not* meant to be used directly. Please use the R package
`rolog` instead. Install this package if you do not have the administrative
privilege to install SWI-Prolog on your computer.

## License

This R package is distributed under FreeBSD simplified license. SWI-Prolog is 
distributed under its own license (BSD-2).

## Installation

Please use R version >= 4.2. The package is on CRAN, it can be installed using 
`install.packages("rswipl")` from the R environment. The current sources can be
installed using 

`install.packages('remotes')`

`remotes::install_github('mgondan/rswipl')`

Please note that under Windows, you need the RTools42 build system. 
