

# Bibliotecas -------------------------------------------------------------

library(readxl)
library(tidyverse)

# Dados e Tratamento ------------------------------------------------------

# lendo o conjunto de dados
soja_produtiv <- read_excel("Dados/Dados - Produtividade em Latossolo e Plintossolo - 2018 a 2023.xlsx")

# renomenando as colunas trocando os espacos por _
names(soja_produtiv) <- gsub("\\s", "_", names(soja_produtiv))

dados_soja <- soja_produtiv |>
  # remove acentos dos valores das covariaveis
  mutate_at(vars(Local, Textura_do_solo), ~stringi::stri_trans_general(.,"Latin-ASCII")) |>
  # trasnforma character em fator
  mutate_if(is.character, as.factor) |>
  mutate(GM = as.factor(GM))

attach(dados_soja)

# Análise Descritiva ------------------------------------------------------

# estudando a quantidade de observações
table(Local)
table(Ano) # estudo desbalanceado no tempo
table(Solo)
min(table(Cultivar)); max(table(Cultivar))
table(Tecnologia)
table(GM)
table(Ciclo)
table(Textura_do_solo)

#### GRAFICO DE DENSIDADE POR COVARIAVEL ####

##### Densidade por ANO #####
sample_size = dados_soja %>% group_by(Ano) %>% summarize(num=n())

dados_soja %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Ano, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha, fill=Ano)) +
  geom_violin(width=1.4) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Ano") +
  xlab("")

##### Densidade por LOCAL #####
sample_size = dados_soja %>% group_by(Local) %>% summarize(num=n())

dados_soja %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Local, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha, fill=Local)) +
  geom_violin(width=1.4) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Local de Plantio") +
  xlab("")

##### Densidade por tipo de Solo #####
sample_size = dados_soja %>% group_by(Solo) %>% summarize(num=n())

dados_soja %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Solo, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha)) +
  geom_violin(width=1.4, aes(fill = Solo)) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Solo") +
  xlab("")

##### Densidade por ANO e SOLO #####
sample_size = dados_soja %>% group_by(Ano, Solo) %>% summarize(num=n())

dados_soja %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Ano, "\n", Solo, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha)) +
  geom_violin(width=1.4, aes(fill = Solo)) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Ano e Solo") +
  xlab("")

##### Densidade por Grupo de Maturidade #####
sample_size = dados_soja %>% group_by(GM) %>% summarize(num=n())

dados_soja %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(GM, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha)) +
  geom_violin(width=1.4) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Cultivar") +
  xlab("")

##### Densidade por GM e SOLO #####
sample_size = dados_soja %>% group_by(GM, Solo) %>% summarize(num=n())

dados_soja %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(GM, "\n", Solo, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha)) +
  geom_violin(width=1.4, aes(fill = Solo)) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Ano e Solo") +
  xlab("")

##### Densidade por Cultivar #####
sample_size = dados_soja %>% group_by(Cultivar) %>% summarize(num=n())

dados_soja %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(Cultivar, "\n", "n=", num)) %>%
  ggplot(aes(x=myaxis, y=kgha)) +
  geom_violin(width=1.4) +
  # scale_fill_viridis(discrete = TRUE) +
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Densidade da produtividade de Soja por Cultivar") +
  xlab("")


### GRAFICO DE PERFIL MEDIO DA COVARIAVEL ####

##### perfil médio do Tipo de Solo ####
ggplot(data = dados_soja, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(fun.y = "mean", geom = "line", aes(linetype = Solo, colour = Solo)) +
  scale_x_continuous(breaks = unique(Ano))

##### perfil médio do Grupo de Maturidade ####
ggplot(data = dados_soja, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(fun.y = "mean", geom = "line", aes(linetype = GM, colour = GM)) +
  scale_x_continuous(breaks = unique(Ano))

##### perfil médio da Textura do solo ####
ggplot(data = dados_soja, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(fun.y = "mean", geom = "line", aes(linetype = Textura_do_solo, colour = Textura_do_solo)) +
  scale_x_continuous(breaks = unique(dados_soja$Ano)) + ylim(range(kgha))

##### perfil médio do Local de Plantio ####
ggplot(data = dados_soja, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(fun.y = "mean", geom = "line", aes(linetype = Local, colour = Local)) +
  scale_x_continuous(breaks = unique(dados_soja$Ano))

##### perfil médio do Ciclo ####
ggplot(data = dados_soja, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(fun.y = "mean", geom = "line", aes(linetype = factor(Ciclo), colour = factor(Ciclo))) +
  scale_x_continuous(breaks = unique(Ano))

##### perfil médio da Tecnologia ####
ggplot(data = dados_soja, mapping = aes(x = Ano, y = kgha)) +
  stat_summary(fun.y = "mean", geom = "line", aes(linetype = Tecnologia, colour = Tecnologia)) +
  scale_x_continuous(breaks = unique(dados_soja$Ano))




# Modelagem ---------------------------------------------------------------

# Dummy coding for each categorical variable
Solo_dummy <- model.matrix(~ Solo - 1, data = dados_soja)
Ano_dummy <- model.matrix(~ factor(Ano) - 1, data = dados_soja)
GM_dummy <- model.matrix(~ GM - 1, data = dados_soja)
Textura_dummy <- model.matrix(~ Textura_do_solo - 1, data = dados_soja)

# modelo aditivo
fit <- lm(kgha ~ Solo_dummy + Ano_dummy + GM_dummy - 1,
          data = dados_soja)
summary(fit)

# modelo multiplicativo
fit2 <- lm(kgha ~ Solo_dummy*Ano_dummy*GM_dummy - 1,
          data = dados_soja)
summary(fit2)
