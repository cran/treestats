## ----setup, include=FALSE-----------------------------------------------------
required <- c("ape", "pheatmap")
if (!all(unlist(lapply(required,
                       function(pkg) requireNamespace(pkg, quietly = TRUE)))))
  knitr::opts_chunk$set(eval = FALSE)

knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width = 6)
knitr::opts_chunk$set(fig.height = 6)



## ----read_trees---------------------------------------------------------------
focal_trees <- ape::read.tree(file = "https://raw.githubusercontent.com/thijsjanzen/treestats-scripts/main/datasets/phylogenies/fracced/birds.trees")  # nolint

## ----calc_sum_stats-----------------------------------------------------------
all_stats <- c()
for (i in seq_along(focal_trees)) {
  focal_stats <- treestats::calc_all_stats(focal_trees[[i]])
  all_stats <- rbind(all_stats, focal_stats)
}
all_stats <- as.data.frame(all_stats)

## ----plot_size----------------------------------------------------------------
hist(all_stats$number_of_lineages)

## ----corr---------------------------------------------------------------------
cor.dist <- cor(all_stats)
diag(cor.dist) <- NA
heatmap(cor.dist)

## ----plot with tree size------------------------------------------------------
opar <- par()
par(mfrow = c(3, 3))
for (stat in c("area_per_pair", "colless", "eigen_centrality",
               "four_prong", "max_betweenness", "max_width",
               "mntd", "sackin", "wiener")) {
  if (stat != "number_of_lineages") {
    x <- all_stats[, colnames(all_stats) == "number_of_lineages"]
    y <- all_stats[, colnames(all_stats) == stat]
    plot(y ~ x, xlab = "Tree size", ylab = stat, pch = 16)
  }
}
par(opar)

## ----correct_corr-------------------------------------------------------------
tree_size <- all_stats[, colnames(all_stats) == "number_of_lineages"]

for (i in seq_len(nrow(cor.dist))) {
  for (j in seq_len(ncol(cor.dist))) {
    stat1 <- rownames(cor.dist)[i]
    stat2 <- colnames(cor.dist)[j]
    x <- all_stats[, colnames(all_stats) == stat1]
    y <- all_stats[, colnames(all_stats) == stat2]

    a1 <- lm(x ~ tree_size)
    a2 <- lm(y ~ tree_size)
    new_cor <- cor(a1$residuals, a2$residuals)
    cor.dist[i, j] <- new_cor
  }
}
diag(cor.dist) <- NA
heatmap(cor.dist)

## ----nicer plot, out.width="100%"---------------------------------------------
if (requireNamespace("pheatmap")) pheatmap::pheatmap(cor.dist)

