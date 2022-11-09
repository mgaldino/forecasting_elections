# pacotes
# https://dadosabertos.tse.jus.br/dataset/resultados-2014
library(tidyverse)
library(electionsBR)
library(janitor)
library(data.table)
library(rstanarm)
library(rstan)

options(mc.cores = parallel::detectCores())
# importando dados de 2014

setwd("/home/mgaldino/Downloads/votacao_candidato_munzona_2014")
df14 <- fread("votacao_candidato_munzona_2014_BRASIL.csv", encoding = "Latin-1") %>%
  clean_names() %>%
  dplyr::filter(ds_cargo == "Presidente")

glimpse(df14)

saveRDS(df14, file="vote_mun_zona_14.rds")

setwd("/home/mgaldino/Downloads/votacao_candidato_munzona_2018")
vote_2018 <- fread("votacao_candidato_munzona_2018_BRASIL.csv",  encoding="Latin-1") %>%
  clean_names() %>%
  dplyr::filter(ds_cargo == "Presidente")

saveRDS(vote_2018, file="vote_mun_zona_18.rds")
# arrumando os dados

# selecionando quem foi pro 2o turno

# 2018
cand_18_2o_turno <- vote_2018 %>%
  dplyr::filter(nr_turno == 2) %>%
  distinct(nm_urna_candidato,.keep_all = T)

# liste de candidatos no 2o turno da eleição
cand_2o_turno_18 <- cand_18_2o_turno$nm_urna_candidato

# 2014
cand_14_2o_turno <- df14 %>%
  dplyr::filter(nr_turno == 2) %>%
  distinct(nm_urna_candidato,.keep_all = T)

cand_2o_turno_14 <- cand_14_2o_turno$nm_urna_candidato

# calculando votos dos dois candidatos e totais
# 2018
vote_2018_lideres <- vote_2018 %>%
  group_by(nr_turno, sg_uf, nm_municipio, cd_municipio, nr_zona, nm_urna_candidato) %>%
  summarise(votacao = sum(qt_votos_nominais), .groups= "drop") %>%
  group_by(nr_turno, sg_uf, nm_municipio, cd_municipio, nr_zona) %>%
  mutate(total_validos = sum(votacao)) %>%
  group_by(nm_urna_candidato) %>%
  mutate(perc_valido = votacao/total_validos) %>%
  dplyr::filter(nm_urna_candidato %in% cand_2o_turno_18)

# 2014
vote_2014_lideres <- df14 %>%
  group_by(nr_turno, sg_uf, nm_municipio, cd_municipio, nr_zona, nm_urna_candidato) %>%
  summarise(votacao = sum(qt_votos_nominais), .groups= "drop") %>%
  group_by(nr_turno, sg_uf, nm_municipio, cd_municipio, nr_zona) %>%
  mutate(total_validos = sum(votacao)) %>%
  group_by(nm_urna_candidato) %>%
  mutate(perc_valido = votacao/total_validos) %>%
  dplyr::filter(nm_urna_candidato %in% cand_2o_turno_14)

# colocando os dados em formato wide, para regressão
# renomeando variáveis

voto_zona_14 <- vote_2014_lideres %>%
  pivot_wider(id_cols = c(sg_uf, cd_municipio, nm_municipio, nr_zona),
              names_from=c(nr_turno,nm_urna_candidato), values_from = c(votacao, total_validos)) %>%
  clean_names() %>%
  mutate(perc_lider_1t = votacao_1_dilma/total_validos_1_dilma,
         perc_lider_2t = votacao_2_dilma/total_validos_2_dilma - .0001,
         perc_runnerup_1t = votacao_1_aecio_neves/total_validos_1_dilma,
         sg_uf = as.factor(sg_uf)) %>%
  select(sg_uf, cd_municipio, nr_zona, perc_lider_1t, perc_lider_2t, perc_runnerup_1t)

# rodando o modelo para 14
fit14 <- stan_lmer(perc_lider_2t ~ perc_lider_1t + perc_runnerup_1t + (1| sg_uf), data = voto_zona_14, seed = 12345, iter = 2500, chains=8)
prior_summary(fit14) 

coef(fit14)
pp_check(fit14)


# preditiva posterior
# criando banco de dados de 22 para prever 2t
vote_22_zona_limpo <- vote_22_zona %>%
  select(valido_1t, valido1t_pt, sg_uf, nr_zona, nm_municipio, cd_municipio, total) %>%
  filter(!is.na(valido_1t))

newdata <- vote_22_zona_limpo %>%
  select(-total, -nr_zona, -nm_municipio, -cd_municipio)

y_rep <- as_tibble(t(posterior_predict(fit3, newdata))) %>%
  clean_names()

# 1k da preditiva posterior
n <- 1000
minha_amostra <- sample(1:length(y_rep), n)

vote_22_zona_full <- bind_cols(vote_22_zona_limpo, y_rep[,minha_amostra])
# calcula os votos válidos de cada uma das 1k previsões da posterior preditiva

vote_22_zona_full1 <- vote_22_zona_full %>%
  mutate(across(paste("v",minha_amostra, sep=""), ~ .*total),) %>% # 6240
  ungroup() %>%
  summarise(voto_total = sum(total),
            across(paste("v",minha_amostra, sep=""), sum, .names = "total_{.col}"),
            across(paste("total_v",minha_amostra, sep=""), ~ ./voto_total, .names = "perc_{.col}")) %>%
  select(starts_with("perc"))

vec_bolso <- unlist(vote_22_zona_full1)

# resumo das previsões
summary(vec_bolso)

# Ic 2,5% e 97,5%
round(quantile(vec_bolso, c(.025, .975)), 3)



