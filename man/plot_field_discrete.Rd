\name{plot_field_discrete}
\alias{plot_field_discrete}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot gridded data with discrete intervals. 
}
\description{
Display geo-gridded data. 
}
\usage{
plot_field_discrete(x, lon, lat, lonlim = 'auto', latlim = 'auto', labels = c(), breaks, cscale = "Spectral", varname = "x", title = c(), mask = NULL, siglev = NULL, plot_only_sig = F, smooth = F, smooth_factor = 5, smooth_theta = 0.5, lineWidth = 0.5, dotSize = 0.5, GRID_STEP = 10, FONT_SIZE = 18)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The data field to plot. It can be a matrix (suggested) or a data frame. The matrix should have longitudinal data on the rows, otherwise the function will try to transpose the matrix giving a warning message. The data frame instead should have three columns with longitude, latitude and the data to plot. 
}
  \item{lon}{
Longitude vector in the range -180, 180. If the longitude is provided in the 0, 360 range it will be transformed with a warning message. 
}
  \item{lat}{
Latitude vector. 
}
  \item{lonlim}{
Plot longitude range.
}
  \item{latlim}{
Plot latitude range.
}
  \item{labels}{
A character vector of labels associated to the breaks. The length of this vector should be equals to the length of breaks + 2 (the two extremes). If the length of the labels vector is zero, the function `pretty_labels` is used on the breaks vector.
}
  \item{breaks}{
A vector with the break values. The breaks are considered "open" at both sides, i.e. -Inf and Inf are added at the extremes. 
}
  \item{cscale}{
The colorscale, it may be the name of a colorbrewer palette (see http://colorbrewer2.org/) or a vector containing the colors used. Spectral palette is used as default.
}
  \item{varname}{
Name of the plotted variables. It will appear into the legend. 
}
  \item{title}{
Title of the plot. 
}
  \item{mask}{
Data field to be used to mask the data for significance levels. It has the same dimensions of x. 
}
  \item{siglev}{
Level of significance to show. All the points where mask <= siglev will be dotted. 
}
  \item{plot_only_sig}{
Instead of displaying dots over the grid points that satisfy the condition mask < siglev, only those points will be shown leaving the rest of the data as NA. 
}
  \item{smooth}{
Apply a spatial smoothing to the data to make it easier to read. 
}
  \item{smooth_factor}{
The coefficient defining how much the shown grid will be finer than the original.
}
  \item{smooth_theta}{
The `theta` factor of `fields::image.smooth`
}
  \item{lineWidth}{
%%     ~~Describe \code{lineWidth} here~~
}
  \item{dotSize}{
%%     ~~Describe \code{dotSize} here~~
}
  \item{GRID_STEP}{
%%     ~~Describe \code{GRID_STEP} here~~
}
  \item{FONT_SIZE}{
%%     ~~Describe \code{FONT_SIZE} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

data(ncep)
p = plot_field_discrete(z, lon, lat, breaks = c(5e-5, 1e-4))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
