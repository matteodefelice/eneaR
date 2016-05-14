# eneaR
This package contains the R functions most used in the ENEA Climate Impacts & Modelling laboratory to deal with climate data. 

Please **be careful** this package is still under-development. 
You can install the package using `devtools` with `devtools::install_github("matteodefelice/eneaR")`

- `spag_plot`: a very trivial function useful for a quick plot of a matrix containing the members of an ensemble
- `pretty_labels`: a function useful to provide nice labels for discrete intervals
- `plot_field_discrete`: this is the function that everyone needs when working with climate data in R. Based on ggplot2, this function gives you the possibility to create a beautiful plot of gridded data with smoothing, dotting (e.g. to show the significance), font size and line width
- `getEqualFreqBreaks`: it splits a vector into breaks with the same size
- `mySave`: replace your `save` with `mySave` to attach metadata to your saved R data
- `getTSfromSHP`: a very advanced function that aggregates gridded data on European NUTS regions (from ADM0 to ADM2) in time-series. 
- `means_along`: a very naive function to perform the mean on the n-th dimension of an array avoiding `apply`

