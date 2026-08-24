## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
require(TreeSim)
require(ggplot2)
require(tibble)
require(tidyr)
require(dplyr)

## ----simulate_trees-----------------------------------------------------------
found <- c()
found_min <- c()
found_max <- c()
for (tree_size in ceiling(10^seq(1, 3, length.out = 10))) {
  cat(tree_size, "\n")
  stats <- c()

  # more replicates for smaller trees:
  num_repl <- min(100, ceiling(5 + 1000 / tree_size))
  speciation <- 0.5

  # calculate the expected crown age for the relevant tree size:
  max_t <- (1 / speciation) * log(tree_size / 2)

  for (r in 1:num_repl) {
    tree <- TreeSim::sim.bd.taxa.age(n = tree_size,
                                     numbsim = 1,
                                     lambda = speciation,
                                     mu = 0,
                                     age = max_t,
                                     mrca = TRUE)[[1]]
    stats <- rbind(stats, treestats::calc_all_stats(tree, normalize = TRUE))
  }
  stats2 <- apply(stats, 2, mean)

  min_stats <- apply(stats, 2, function(x) return(quantile(x, 0.025)))
  max_stats <- apply(stats, 2, function(x) return(quantile(x, 0.975)))

  found <- rbind(found, stats2)
  found_min <- rbind(found_min, min_stats)
  found_max <- rbind(found_max, max_stats)
}

found <- as_tibble(found)
found_min <- as_tibble(found_min)
found_max <- as_tibble(found_max)

## ----collect_information------------------------------------------------------
norm_info <- read.table("https://raw.githubusercontent.com/thijsjanzen/treestats-scripts/main/datasets/normalize.txt", header = TRUE) # nolint

found2 <- found |>
  tidyr::gather(key = "statistic", value = "mean", -number_of_lineages)



found_min2 <- found_min |>
  tidyr::gather(key = "statistic", value = "min", -number_of_lineages)
found_max2 <- found_max |>
  tidyr::gather(key = "statistic", value = "max", -number_of_lineages)

found3 <- left_join(found2, norm_info,
                    by = join_by(statistic))
found3 <- left_join(found3, found_min2,
                    by = join_by(statistic, number_of_lineages))
found3 <- left_join(found3, found_max2,
                    by = join_by(statistic, number_of_lineages))

## ----yule_plots---------------------------------------------------------------
found3 |>
  filter(normalize == TRUE & type == "Yule") |>
  ggplot(aes(x = number_of_lineages, y = mean)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "yellow", alpha = 0.2) +
  geom_point(col = "blue", alpha = 0.3) +
  geom_line(col = "gold") +
  scale_x_log10() +
  facet_wrap(~statistic, scales = "free_y", ncol = 3) +
  theme_minimal() +
  xlab("Extant tree size") +
  ggtitle("Statistics normalized by the Yule expectation")

## ----size_plots---------------------------------------------------------------
found3 |>
  filter(normalize == TRUE & type == "Tips") |>
  ggplot(aes(x = number_of_lineages, y = mean)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "yellow", alpha = 0.2) +
  geom_point(col = "blue", alpha = 0.3) +
  geom_line(col = "gold") +
  scale_x_log10() +
  facet_wrap(~statistic, scales = "free_y", ncol = 4) +
  theme_minimal() +
  xlab("Extant tree size") +
  ggtitle("Statistics normalized by tree size")

## ----no_norm------------------------------------------------------------------
found3 |>
  filter(normalize == FALSE) |>
  ggplot(aes(x = number_of_lineages, y = mean)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "yellow", alpha = 0.2) +
  geom_point(col = "blue", alpha = 0.3) +
  geom_line(col = "gold") +
  scale_x_log10() +
  facet_wrap(~statistic, scales = "free_y", ncol = 5) +
  theme_minimal() +
  xlab("Extant tree size") +
  ggtitle("Statistics not normalized")

