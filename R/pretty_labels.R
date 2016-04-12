pretty_labels = function(breaks) {
  labels = rep(NA, length(breaks)+1)
  labels[1] = paste0('<', breaks[1])
  for (i in 1:(length(breaks)-1)) {
    labels[i+1] = paste0(breaks[i], ',', breaks[i+1])
  }
  labels[length(breaks)+1] = paste0('>', breaks[length(breaks)])
  return(labels)
}