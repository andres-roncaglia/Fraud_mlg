library(ggplot2)
library(dplyr)
library(tidyr)


# data <- read.csv("Data/Base.csv")
# set.seed(2024)
# 
# 
# filas <- createDataPartition(data$fraud_bool, p = 0.3)[[1]]
# 
# x <- slice(data, filas)
# 
# write.csv(x, file = "Data/Base_resumida.csv")


data <- read.csv("Data/Base_resumida.csv")

glimpse(data)

data |> ggplot() +
  geom_bar(aes(x = fraud_bool))

unique(data$source)


table(data$prev_address_months_count)

