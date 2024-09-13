library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)


# data <- read.csv("Data/Base.csv")
# set.seed(2024)
# 
# 
# filas <- createDataPartition(data$fraud_bool, p = 0.3)[[1]]
# 
# x <- slice(data, filas)
# 
# write.csv(x, file = "Data/Base_resumida.csv")


data <- read.csv("Data/Base_resumida.csv") |> 
  mutate(
    income = factor(income)
  )

glimpse(data)

data |> ggplot() +
  geom_bar(aes(x = fraud_bool))

unique(data$source)
# geom_bar(aes(, fill = "skyblue") +


table(data$prev_address_months_count)
# income vs fraud
data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = income, y = (..count..)/sum(..count..))+
  geom_bar()

# name_email_similarity vs fraud

data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = name_email_similarity)+
  geom_histogram(bins = 15, color = "black", fill = "firebrick4")

data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = name_email_similarity)+
  geom_histogram(bins = 15, color = "black", fill = "firebrick4")


data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = name_email_similarity)+
  geom_density(fill = "firebrick4", color = "black")

data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = name_email_similarity)+
  geom_density(fill = "firebrick4", color = "black")


# Probar combinar las dos densidades con transparecia (el facking andrew)

data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = payment_type, y = (..count..)/sum(..count..))+
  geom_bar(color = "black", fill = "firebrick4")

data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = payment_type, y = (..count..)/sum(..count..))+
  geom_bar()


# score de riesgo vs fraude

data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = has_other_cards, y = after_stat(count)/sum(after_stat(count)))+
  geom_bar(fill = "firebrick4", color = "black")+
  scale_y_continuous(limits = c(0,1))


data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = has_other_cards, y = after_stat(count)/sum(after_stat(count)))+
  geom_bar(fill = "firebrick4", color = "black")+
  scale_y_continuous(limits = c(0,1))

data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = foreign_request, y = after_stat(count)/sum(after_stat(count)))+
  geom_bar(fill = "firebrick4", color = "black")+
  scale_y_continuous(limits = c(0,1))


data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = foreign_request, y = after_stat(count)/sum(after_stat(count)))+
  geom_bar(fill = "firebrick4", color = "black")+
  scale_y_continuous(limits = c(0,1))



# limite de credito propuesto vs fraude


data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = proposed_credit_limit)+
  geom_density(fill = "firebrick4", color = "black")


data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = proposed_credit_limit)+
  geom_histogram(bins = 15, color = "black", fill = "firebrick4")



# Dominio gratis vs fraude

data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = email_is_free, y = after_stat(count)/sum(after_stat(count)))+
  geom_bar(fill = "firebrick4", color = "black")+
  scale_y_continuous(limits = c(0,1))


