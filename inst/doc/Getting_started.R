## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width = 6)
knitr::opts_chunk$set(fig.height = 6)
library(treestats)

## ----list---------------------------------------------------------------------
list_statistics()

## ----colless------------------------------------------------------------------
phy <- ape::rphylo(n = 100, birth = 1, death = 0.1)

treestats::colless(phy)

## ----colless2-----------------------------------------------------------------
treestats::colless(phy, normalization = "yule")

## ----all_stats----------------------------------------------------------------
all_stats <- calc_all_stats(phy)

## ----all_stats3---------------------------------------------------------------
balance_stats <- calc_topology_stats(phy)
unlist(balance_stats)

