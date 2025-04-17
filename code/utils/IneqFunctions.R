library(hutils)
library(tidyverse)
StatsGini <- function(x, w = rep(1, length(x))){
  # This functions returns the Gini index of a given vector and its respective weights.
  n <- length(x)
  wxsum <- sum(w * x)
  wsum <- sum(w)
  sxw <- order(x, w)
  sx <- w[sxw] * x[sxw]
  sw <- w[sxw]
  pxi <- vector(mode = "numeric", length = n)
  pci <- vector(mode = "numeric", length = n)
  pxi <- cumsum(sx) / wxsum
  pci <- cumsum(sw) / wsum
  G <- 0.0
  for (i in 2:n){
    G <- G - (pci[i] * pxi[i - 1] - pxi[i] * pci[i - 1] )
  }
  return(G)
}

Top_Aprop <- function(x, w = rep(1,length(x)),centile){
  # Appropriation of national income, assets, etc. by the top x%
  centiles = weighted_ntile(x, w, 100)
  df <- data.frame(income = x, weights = w, centiles = centiles)
  top <- df %>% filter(centiles>=centile)
  aprop <- sum(((top$income)*(top$weights)))/sum(x*w)
  return(aprop)
}
Top_Aprop_thousandth <- function(x, w = rep(1,length(x)),centile){
  # Appropriation of national income, assets, etc. by the top x%
  centiles = weighted_ntile(x, w, 1000)
  df <- data.frame(income = x, weights = w, centiles = centiles)
  top <- df %>% filter(centiles>=centile)
  aprop <- sum(((top$income)*(top$weights)))/sum(x*w)
  return(aprop)
}
Bottom_Aprop <- function(x, w = rep(1,length(x)),centile){
  # Appropriation of national income, assets, etc. by the bottom x%
  centiles = weighted_ntile(x, w, 100)
  df <- data.frame(income = x, weights = w, centiles = centiles)
  top <- df %>% filter(centiles<=centile)
  aprop <- sum(((top$income)*(top$weights)))/sum(x*w)
  return(aprop)
}