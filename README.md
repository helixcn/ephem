# Welcome to the homepage of ephem

# What is it?
It is an R packages for creating some figures that can be used in an astronomical almanac

# Installation

As ephem depends on skycalc, which is a wrapper package of the population AA+ library written in Cpp (http://www.naughter.com/aa.html), and need compilation, you must have Rtools (https://cran.r-project.org/bin/windows/Rtools/) installed.

Meanwhile, the R package devtools must also be installed. 

## To install devtools: 
```R
install
```

## To install ephem and its dependencies
```R
devtools::install_github("helixcn/moonsun")
devtools::install_github("helixcn/skycalc")
devtools::install_github("helixcn/ephem")
```

# Note
THIS PACKAGE HAS NOT BEEN PEER REVIEWED, USE AT YOUR OWN RISK.

If you have any questions or comments, please feel free to send an email to the package maintainer Dr. Jinlong Zhang .

# Citation

Jinlong Zhang (2016). ephem: Creating figures for
  astronomical almanac. R package version 0.X.X.
  https://github.com/helixcn/ephem
  
  
  
