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
    income = factor(income),
    proposed_credit_limit_cat = case_when(proposed_credit_limit == 190 ~ 200,
                                          proposed_credit_limit == 200 ~ 200,
                                          proposed_credit_limit == 210 ~ 200,
                                          proposed_credit_limit == 490 ~ 500,
                                          proposed_credit_limit == 500 ~ 500,
                                          proposed_credit_limit == 510 ~ 500,
                                          proposed_credit_limit == 990 ~ 1000,
                                          proposed_credit_limit == 1000 ~ 1000,
                                          proposed_credit_limit == 1500 ~ 1500,
                                          proposed_credit_limit == 1900 ~ 2000,
                                          proposed_credit_limit == 2000 ~ 2000,
                                          T ~ 2000))

data <- data |> 
  mutate(
    proposed_credit_limit_cat = factor(proposed_credit_limit_cat),
    has_other_cards = factor(has_other_cards),
    foreign_request = factor(foreign_request),
    phone_home_valid = factor(phone_home_valid),
    phone_mobile_valid = factor(phone_mobile_valid),
    email_is_free = factor(email_is_free),
    keep_alive_session = factor(keep_alive_session)
  )

glimpse(data)

data |> ggplot() +
  geom_bar(aes(x = fraud_bool))

# Tabla de la variable respuesta (individuos)
table(data$fraud_bool)

# Conjunto de datos desbalanceados

unique(data$source)
# geom_bar(aes(, fill = "skyblue") +


table(data$prev_address_months_count)

# income vs fraud
(data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = income, y = (..count..)/sum(..count..))+
  geom_bar() +
  scale_y_continuous(limits = c(0,0.45)) +
  ggtitle("No fraude"))+
(data |> 
   filter(fraud_bool == 1) |> 
   ggplot() +
   aes(x = income, y = (..count..)/sum(..count..))+
   geom_bar() +
   scale_y_continuous(limits = c(0,0.45)) +
   ggtitle("Fraude"))
  
# Se puede observar que las personas que cometieron fraude tienden a tener un ingreso anual registrado mayor. La distribución tiene una mayor asimetría a la izquierda

# name_email_similarity vs fraud

(data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = name_email_similarity, y = ..density..)+
  geom_histogram(bins = 15, color = "black", fill = "firebrick4") +
  scale_y_continuous(limits = c(0,2.25)) +
  ggtitle("No fraude")) +
(data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = name_email_similarity, y = ..density..)+
  geom_histogram(bins = 15, color = "black", fill = "firebrick4") +
  scale_y_continuous(limits = c(0,2.25)) +
  ggtitle("Fraude"))

# Se puede observar que las personas que cometen fraude suelen en general tener una menor similitud entre el nombre y el email.

# data |> 
#   filter(fraud_bool == 1) |> 
#   ggplot() +
#   aes(x = name_email_similarity)+
#   geom_density(fill = "firebrick4", color = "black")
# 
# data |> 
#   filter(fraud_bool == 0) |> 
#   ggplot() +
#   aes(x = name_email_similarity)+
#   geom_density(fill = "firebrick4", color = "black", adjust = 1.5)


# Probar combinar las dos densidades con transparecia (el facking andrew)

(data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = payment_type, y = (..count..)/sum(..count..))+
  geom_bar(color = "black", fill = "firebrick4")+
  scale_y_continuous(limits = c(0,0.4)) +
  ggtitle("No fraude")) +
(data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = payment_type, y = (..count..)/sum(..count..))+
  geom_bar(color = "black", fill = "firebrick4") +
  scale_y_continuous(limits = c(0,0.4)) +
  ggtitle("Fraude"))

# Se puede observar que las personas que cometen fraude en general parecen pagar menos con el método AA y más con el método AC que las personas que no cometen fraude.

# score de riesgo vs fraude

(data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = has_other_cards, y = after_stat(count)/sum(after_stat(count)))+
  geom_bar(fill = "firebrick4", color = "black")+
  scale_y_continuous(limits = c(0,1))+
  ggtitle("No fraude")) +
(data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = has_other_cards, y = after_stat(count)/sum(after_stat(count)))+
  geom_bar(fill = "firebrick4", color = "black")+
  scale_y_continuous(limits = c(0,1))+
  ggtitle("Fraude"))

# Se puede observar que las personas que cometen fraude parecen no tener otra tarjeta en el mismo banco

(data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = foreign_request, y = after_stat(count)/sum(after_stat(count)))+
  geom_bar(fill = "firebrick4", color = "black")+
  scale_y_continuous(limits = c(0,1))+
  ggtitle("No fraude")) +
 (data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = foreign_request, y = after_stat(count)/sum(after_stat(count)))+
  geom_bar(fill = "firebrick4", color = "black")+
  scale_y_continuous(limits = c(0,1))+
  ggtitle("Fraude"))

# Se puede observar que las personas que cometen fraude, parece hacer más solicitudes del exterior aunque la diferencia parece ser sutil

table(data$fraud_bool, data$foreign_request)

# limite de credito propuesto vs fraude


data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = proposed_credit_limit_cat)+
  geom_density(fill = "firebrick4", color = "black")


(data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = proposed_credit_limit_cat, y = after_stat(count)/sum(after_stat(count)))+
  geom_bar(color = "black", fill = "firebrick4")+
  scale_y_continuous(limits = c(0,0.65))+
  ggtitle("No fraude")) +
(data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = proposed_credit_limit_cat, y = after_stat(count)/sum(after_stat(count)))+
  geom_bar(color = "black", fill = "firebrick4") +
  scale_y_continuous(limits = c(0,0.65))+
  ggtitle("Fraude"))

table(data$fraud_bool, data$proposed_credit_limit_cat)

# Se puede observar que las personas que cometen fraude parecen pedir limites de créditos más altos que aquellos que no cometen fraude

# Dominio gratis vs fraude

(data |> 
  filter(fraud_bool == 0) |> 
  ggplot() +
  aes(x = email_is_free, y = after_stat(count)/sum(after_stat(count)))+
  geom_bar(fill = "firebrick4", color = "black")+
  scale_y_continuous(limits = c(0,1))+
  ggtitle("No fraude"))+
(data |> 
  filter(fraud_bool == 1) |> 
  ggplot() +
  aes(x = email_is_free, y = after_stat(count)/sum(after_stat(count)))+
  geom_bar(fill = "firebrick4", color = "black")+
  scale_y_continuous(limits = c(0,1))+
  ggtitle("Fraude"))

# Se puede observar que las personas que cometen fraude parecen tener una mayor proporción de emails con dominios gratuitos.

# Score de riesgo vs Fraude

(data |> 
    filter(fraud_bool == 0) |> 
    ggplot() +
    aes(x = credit_risk_score, y = after_stat(count)/sum(after_stat(count)))+
    geom_histogram(fill = "firebrick4", color = "black", bins = 20)+
    scale_y_continuous(limits = c(0, 0.2)) +
    scale_x_continuous(limits = c(-200, 400)) +
    ggtitle("No fraude")) +
(data |> 
    filter(fraud_bool == 1) |> 
    ggplot() +
    aes(x = credit_risk_score, y = after_stat(count)/sum(after_stat(count)))+
    geom_histogram(fill = "firebrick4", color = "black", bins = 20)+
    scale_y_continuous(limits = c(0, 0.2)) +
    scale_x_continuous(limits = c(-200, 400)) +
    ggtitle("Fraude"))

# Se puede observar que la distribución del score de riesgo para las personas que cometen fraude es simetrica y centrada alrededor de 200, mientras que 
# la distribucion del score de riesgo para las personas que no cometen fraude parece ser más asimétrica y tener una media menor.

# Edad del cliente vs Fraude 
# (data |> 
#     filter(fraud_bool == 0) |> 
#     ggplot() +
#     aes(x = customer_age, y = after_stat(count)/sum(after_stat(count)))+
#     geom_bar(fill = "firebrick4", color = "black")+
#     scale_y_continuous(limits = c(0, 0.35)) +
#     ggtitle("No fraude")) +
# (data |> 
#     filter(fraud_bool == 1) |> 
#     ggplot() +
#     aes(x = customer_age, y = after_stat(count)/sum(after_stat(count)))+
#     geom_bar(fill = "firebrick4", color = "black")+
#     scale_y_continuous(limits = c(0, 0.35)) +
#     ggtitle("Fraude"))

# Interesante (Mantener la sesión iniciada vs Fraude)
(data |> 
    filter(fraud_bool == 0) |> 
    ggplot() +
    aes(x = keep_alive_session, y = after_stat(count)/sum(after_stat(count)))+
    geom_bar(fill = "firebrick4", color = "black")+
    scale_y_continuous(limits = c(0,1)) +
    ggtitle("No fraude")) +
  (data |> 
     filter(fraud_bool == 1) |> 
     ggplot() +
     aes(x = keep_alive_session, y = after_stat(count)/sum(after_stat(count)))+
     geom_bar(fill = "firebrick4", color = "black")+
     scale_y_continuous(limits = c(0,1)) +
     ggtitle("Fraude"))
