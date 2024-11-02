library(ggplot2)

setwd("/Users/alex/documento/github/r/etnobotanica-workshop/")

raw_data <- read.delim("data/raw_data.csv")

p <- ggplot(df, aes(x, y)) + geom_point()

ggsave("../figs/fig_scatterplot.png")
