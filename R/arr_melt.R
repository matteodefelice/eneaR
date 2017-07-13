arr_melt <- function(a, drop = FALSE) {
  axes <- lapply(dim(a), function(n) seq_len(n))
  if (drop) {
    ones <- unlist(lapply(axes, length)) == 1L
    if (sum(ones) < 1) stop("no non-degenerate dimensions, use drop = FALSE")
    axes <- axes[!ones]
  }
  
  total_prod <- prod(dim(a))
  nm <- c("row", "col", letters)[seq_along(axes)]
  tib <- tibble::tibble(data = as.vector(a))
  prod_dims <- 1
  for (i in seq_along(axes)) {
    nd <- dim(a)[i]
    tib[[nm[i]]] <- rep(axes[[i]], each = prod_dims, length.out = total_prod)
    prod_dims <- prod_dims * nd
  }
  tib
}