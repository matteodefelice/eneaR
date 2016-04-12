\name{plot_field_discrete}
\alias{plot_field_discrete}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot gridded discrete data
}
\description{
Display geo-gridded data.
}
\usage{
plot_field_discrete(x, lon, lat, lonlim = c(-180, 180), latlim = c(-70, 70), labels, breaks, cscale = "Spectral", varname = "x", title = c(), mask = NULL, siglev = NULL, plot_only_sig = F, smooth = F, smooth_factor = 5, smooth_theta = 0.5, lineWidth = 0.5, dotSize = 0.5, GRID_STEP = 10, FONT_SIZE = 18)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{lon}{
%%     ~~Describe \code{lon} here~~
}
  \item{lat}{
%%     ~~Describe \code{lat} here~~
}
  \item{lonlim}{
%%     ~~Describe \code{lonlim} here~~
}
  \item{latlim}{
%%     ~~Describe \code{latlim} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{cscale}{
%%     ~~Describe \code{cscale} here~~
}
  \item{varname}{
%%     ~~Describe \code{varname} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{mask}{
%%     ~~Describe \code{mask} here~~
}
  \item{siglev}{
%%     ~~Describe \code{siglev} here~~
}
  \item{plot_only_sig}{
%%     ~~Describe \code{plot_only_sig} here~~
}
  \item{smooth}{
%%     ~~Describe \code{smooth} here~~
}
  \item{smooth_factor}{
%%     ~~Describe \code{smooth_factor} here~~
}
  \item{smooth_theta}{
%%     ~~Describe \code{smooth_theta} here~~
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

## The function is currently defined as
function (x, lon, lat, lonlim = c(-180, 180), latlim = c(-70,
    70), labels, breaks, cscale = "Spectral", varname = "x",
    title = c(), mask = NULL, siglev = NULL, plot_only_sig = F,
    smooth = F, smooth_factor = 5, smooth_theta = 0.5, lineWidth = 0.5,
    dotSize = 0.5, GRID_STEP = 10, FONT_SIZE = 18)
{
    if (length(latlim) > 0 && length(lonlim) > 0 && (diff(lonlim) *
        diff(latlim)) < 2500) {
        wmap = readShapeSpatial(system.file("borders", "TM_WORLD_BORDERS-0.3.shp",
            package = "enear"))
    }
    else {
        wmap = readShapeSpatial(system.file("borders", "TM_WORLD_BORDERS_SIMPL-0.3.shp",
            package = "enear"))
    }
    wmap = fortify(wmap)
    if (any(lon > 180)) {
        lon[lon > 180] = lon[lon > 180] - 360
    }
    if (length(unique(diff(lon))) > 1) {
        lon = round(lon, digits = 3)
    }
    if (length(unique(diff(lat))) > 1) {
        lat = round(lat, digits = 3)
    }
    if (is.array(x) && length(dim(x)) > 2) {
        stop("you cannot plot an array with more than two dimensions")
    }
    if (smooth && is.data.frame(x)) {
        warning("Smoothing procedure on data frames is not implemented")
        smooth = F
    }
    if (smooth) {
        library(fields)
        library(akima)
        z = image.smooth(x, theta = smooth_theta)
        z = bicubic.grid(x = lon, y = lat, z = z$z, xlim = lonlim,
            ylim = latlim, dx = mean(diff(lon))/smooth_factor,
            dy = mean(diff(lat))/smooth_factor)
        x = z$z
        lon = z$x
        lat = z$y
    }
    if (!is.data.frame(x)) {
        if (dim(x)[1] != length(lon)) {
            if (dim(x)[2] == length(lon)) {
                warning("Latitude and longitude vectors look swapped, data field will be transposed")
                x = t(x)
            }
            else {
                stop("Dimensions of x are not consistent with the lat-lon vectors provided")
            }
        }
        dd = melt(x)
        dd[, 1] = lon[dd[, 1]]
        dd[, 2] = lat[dd[, 2]]
        names(dd) = c("lon", "lat", "x")
    }
    else {
        dd = x
    }
    dd$orig_x = dd$x
    dd$x = cut(dd$x, breaks = c(-Inf, breaks, Inf), labels = labels)
    if (length(lonlim) > 0) {
        dd = subset(dd, lon >= lonlim[1] & lon <= lonlim[2])
        wmap = subset(wmap, long >= lonlim[1] & long <= lonlim[2])
    }
    if (length(latlim) > 0) {
        dd = subset(dd, lat >= latlim[1] & lat <= latlim[2])
        wmap = subset(wmap, lat >= latlim[1] & lat <= latlim[2])
    }
    if (!is.null(mask)) {
        mask = mask > siglev
        ij_dots = which(mask == FALSE, arr.ind = T)
        lon_dots_all = lon[ij_dots[, 2]]
        lat_dots_all = lat[ij_dots[, 1]]
        int_dots = 1
        lon_dots = lon_dots_all[seq(1, length(lon_dots_all),
            by = int_dots)]
        lat_dots = lat_dots_all[seq(1, length(lat_dots_all),
            by = int_dots)]
        ij_data <- data.frame(lon_dots, lat_dots)
        names(ij_data) = c("lon", "lat")
        ij_data$sig = 1
        dd = left_join(dd, ij_data)
    }
    if (!plot_only_sig) {
        g = ggplot() + geom_raster(data = dd, aes(x = lon, y = lat,
            fill = x), alpha = 0.8)
        if (!is.null(mask)) {
            g = g + geom_point(data = ij_data, aes(lon_dots,
                lat_dots), size = dotSize, alpha = 0.75, stroke = 0,
                shape = 16)
        }
    }
    else {
        g = ggplot() + geom_raster(data = filter(dd, sig == 1),
            aes(x = lon, y = lat, fill = x), alpha = 0.8)
    }
    g = g + geom_path(data = wmap, aes(x = long, y = lat, group = group),
        size = lineWidth)
    if (length(cscale) == 1) {
        cscale = brewer.pal(length(labels), cscale)
    }
    g = g + scale_fill_manual(name = varname, values = cscale,
        drop = F)
    g = g + ggtitle(title)
    g = g + theme_bw()
    g = g + scale_x_continuous(expand = c(0, 0), breaks = seq(-180,
        180, by = GRID_STEP), limits = lonlim)
    g = g + scale_y_continuous(expand = c(0, 0), breaks = seq(-180,
        180, by = GRID_STEP), limits = latlim)
    g = g + theme(text = element_text(size = FONT_SIZE), panel.border = element_rect(colour = "black",
        size = 2, fill = NA), panel.grid.major = element_line(colour = "black",
        size = 0.25))
    g = g + xlab("Longitude") + ylab("Latitude")
    return(g)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
