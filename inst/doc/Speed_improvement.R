## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(fig.width = 6)
knitr::opts_chunk$set(fig.height = 6)
speed_data <- read.table("https://raw.githubusercontent.com/thijsjanzen/treestats-scripts/main/Figure_S3_S4/timings.txt", header = TRUE)  # nolint


## ----out.width="100%", echo=FALSE---------------------------------------------
knitr::include_graphics("https://github.com/thijsjanzen/treestats/blob/main/layout/Figure_S3.png?raw=true") # nolint

## ----summarise_data, echo=FALSE-----------------------------------------------
res <- c()
for (x in unique(speed_data$treestatsfunction)) {
  a <- subset(speed_data, speed_data$treestatsfunction == x &
                          speed_data$ntips == 1000 &
                          speed_data$method == "treestats")
  res <- rbind(res, c(x, mean(a$time)))
}
res <- data.frame("treestats_function" = res[, 1],
                     "time" = res[, 2])
res$time <- as.numeric(res$time)

res2 <- res[order(res$time), ]


## ----out.width="100%", echo = FALSE-------------------------------------------
opar <- par(no.readonly = TRUE)
par(mar = c(8, 4, 4, 4))
barplot(res2$time, names.arg = res2$treestats_function, las = 2,
        cex.names = 0.4, ylab = "Time", log = "y")
par(opar)

## ----out.width="100%", echo=FALSE---------------------------------------------
knitr::include_graphics("https://github.com/thijsjanzen/treestats/blob/main/layout/Figure_S4.png?raw=true") # nolint

