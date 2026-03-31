#Purpose: Rescale the impaired world with the parameters from the perfect world data
#The second function is for the Noise impairment. It rescales the mean only because noise will blow up the variance, don't want to recorrect that
#And we don't want any rescaling for implausible values



rescale_with_clean_params <- function(x_impaired, scaling_params) {
  (x_impaired - scaling_params["mean"]) / scaling_params["sd"]
}

# for random noise only
rescale_mean_only <- function(x_impaired, scaling_params) {
  x_impaired - scaling_params["mean"]
}

# implausible values doesn't get rescaled