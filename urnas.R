# carrega pacotes
library(tidyverse)
library(janitor)
library(data.table)
library(ggplot2)

votos_urnas <- fread("Dados/VOTOS_T1E2_presidenciais_2022.csv") %>%
  clean_names()

# cria variável urna nova ou velha (se 2020 é nova)
# e cira variável regiao

votos_urnas <- votos_urnas %>%
  mutate(urna_nova = as.numeric(grepl("UE2020", log_modelo)),
         regiao = ifelse(uf %in% c("AL", "BA", "CE", "MA", "RN", "PB", "PE", "PI", "SE"), "NE",
                         ifelse(uf %in% c("SP", "MG", "RJ", "ES"), "SE",
                                ifelse(uf %in% c("RS", "SC", "PR"), "SUL", 
                                       ifelse(uf %in% c("GO", "MS", "MT", "DF", "TO"), "CO", "Norte"))))) 
  
# inspeciona os dados
glimpse(votos_urnas)

# Média de votos no 2t por urna
votos_urnas %>%
  group_by(log_modelo) %>%
  summarise(t2_media_13 = mean(t2qt13),
            t2_media_22 = mean(t2qt22))

# percentual de urna por uf

urna_uf <- votos_urnas %>%
  group_by(uf, log_modelo) %>%
  summarise(freq = n()) %>%
  mutate(total_uf = sum(freq),
         perc = round(freq/total_uf, 2))

# histograma por urna nova ou antigas
votos_urnas %>%
    ggplot(aes(t1qt22)) + geom_histogram() + facet_grid(~urna_nova)

# histograma por uf
votos_urnas %>%
  ggplot(aes(t1qt22)) + geom_histogram() + facet_grid(uf~urna_nova, scales = "free_y")

# histograma por regiao e urna velha ou nova
votos_urnas %>%
  ggplot(aes(t1qt22)) + geom_density() + facet_grid(regiao ~ urna_nova, scales = "free_y")


# Média  por região e urna velha ou nova
votos_urnas %>%
  group_by(regiao, urna_nova) %>%
  summarise(qtde = n()) %>%
  mutate(total_regiao = sum(qtde),
         perc = round(qtde/total_regiao, 2))


votos_urnas %>%
  group_by(regiao, urna_nova) %>%
  summarise(media_aptos = mean(t1qtaptos),
            mediana_aptos = median(t1qtaptos))


votos_urnas %>%
  summarise(media_aptos = mean(t1qtaptos),
            mediana_aptos = median(t1qtaptos))


# abstenção

# histograma por regiao e urna velha ou nova
votos_urnas %>%
  ggplot(aes(t1qtabst)) + geom_density() + facet_grid(regiao ~ urna_nova, scales = "free_y")


            