getEqualFreqBreaks <- function(vector, nbins, na.rm = F, remove_outliers = NA) {
  if (na.rm) {
    vector = vector[!is.na(vector)]
  }
  if (!is.na(remove_outliers)) {
    to_remove = abs(vector) > (sd(vector, na.rm = T) * remove_outliers)
    vector = vector[!to_remove]
  }
  len = length(vector)
  hist_bins = max(c(5, min(round(len / 25), 200)))
  hist_data = hist(vector, breaks = hist_bins, plot = F)
  cumsum_interval = round(sum(hist_data$counts) / (nbins ))
  cumsum_breaks   = seq(cumsum_interval - 1, len - cumsum_interval + 1, cumsum_interval)
  breaks = rep(NA, length(cumsum_breaks))
  for (i in 1:length(cumsum_breaks)) {
    breaks[i] =hist_data$breaks[which.min(abs(cumsum(hist_data$counts) - cumsum_breaks[i])) + 1]
  }
  return(unique(breaks))
}