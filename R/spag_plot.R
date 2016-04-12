spag_plot <- function(y, DIM = 1, lwd = 1, alpha_value = 0.5, col = "black") {


    if (length(dim(y)) != 2) {
        stop("Wrong number of dimensions")
    }
    if (DIM == 2) {
        y = t(y)
    }
    for (i in seq(1, dim(y)[1])) {
        if (i == 1) {
            plot(y[i, ], ylim = range(y), lwd = lwd, col = alpha(col, alpha_value), type = "l")
        } else {
            lines(y[i, ], lwd = lwd, col = alpha(col, alpha_value))
        }
    }
}
