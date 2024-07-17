## ----setup, include=FALSE-----------------------------------------------------
required <- c("abcrf", "ape", "DDD")
if (!all(unlist(lapply(required,
                       function(pkg) requireNamespace(pkg, quietly = TRUE)))))
  knitr::opts_chunk$set(eval = FALSE)


knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width = 6)
knitr::opts_chunk$set(fig.height = 6)
library(abcrf)

## ----training_simple----------------------------------------------------------
num_points <- 100
test_data <- matrix(nrow = num_points,
                    ncol = 1 + length(treestats::list_statistics()))
for (r in 1:num_points) {
  b <- runif(1)
  focal_tree <- ape::rphylo(n = 100, birth = b, death = 0)
  focal_stats <- treestats::calc_all_stats(focal_tree)
  test_data[r, ] <- c(b, focal_stats)
}
colnames(test_data) <- c("birth", names(focal_stats))
test_data <- as.data.frame(test_data)

## ----abcrf_simple-------------------------------------------------------------
forest <- regAbcrf(birth ~ ., test_data, ntree = 100)

## ----test_simple--------------------------------------------------------------
test_tree <- ape::rphylo(n = 100, birth = 0.5, death = 0)
test_stats <- treestats::calc_all_stats(test_tree)
test_stats <- as.data.frame(t(test_stats))

predict(forest, test_stats, test_data)

## ----plot_simpl2--------------------------------------------------------------
densityPlot(forest, test_stats, test_data)

## ----plot_simple--------------------------------------------------------------
plot(forest)

## ----sim_ddd------------------------------------------------------------------
new_test_data <- cbind(test_data, 1)
for (r in 1:num_points) {
  b <- runif(1)
  focal_tree <- DDD::dd_sim(pars = c(b, 0, 130), age = 26.5)$tes
  # too small trees will yield NA values
  while (length(focal_tree$tip.label) < 4)
    focal_tree <- DDD::dd_sim(pars = c(b, 0, 130), age = 26.5)$tes
  focal_stats <- treestats::calc_all_stats(focal_tree)
  new_test_data <- rbind(new_test_data, c(b, focal_stats, 2))
}

for_abc <- new_test_data[, 2:ncol(new_test_data)]
colnames(for_abc) <- c(names(focal_stats), "model")
for_abc$model <- as.factor(for_abc$model)

forest <- abcrf::abcrf(model ~ ., data = for_abc, ntree = 100)

## ----plot_abcrf_model_select--------------------------------------------------
plot(forest, for_abc)

## ----predict_model_select-----------------------------------------------------
to_test <- c()
for (i in 1:5) {
  focal_tree <- ape::rphylo(n = 100, birth = 0.5, death = 0)
  to_test <- rbind(to_test, treestats::calc_all_stats(focal_tree))
}

for (i in 1:5) {
  focal_tree <- DDD::dd_sim(pars = c(0.5, 0, 130), age = 26.5)$tes
  # too small trees will yield NA values
  while (length(focal_tree$tip.label) < 4)
    focal_tree <- DDD::dd_sim(pars = c(0.5, 0, 130), age = 26.5)$tes
  to_test <- rbind(to_test, treestats::calc_all_stats(focal_tree))
}
to_test <- as.data.frame(to_test)
predict(forest, to_test, for_abc)

