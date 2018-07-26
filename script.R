library(magrittr)
library(dplyr)
library(ggplot2)
library(stringr)

dados.reclamacoes <- read.csv('reclamacoes-avaliadas.csv')
dados.reclamacoes = dados.reclamacoes %>% mutate(reclamacao.tamanho = str_length(reclamacao))

# Médial do nível de insatisfação por órgão
orgaos.medias <- dados.reclamacoes %>% 
  group_by(orgao) %>% 
  summarize(media = sum(mediana)/n())

ggplot(data = orgaos.medias, aes(x = orgao, y = media)) +
  geom_bar(stat = "identity") +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Média do grau de insatisfação por órgão")

# Correlação entre tamanho da opinião e grau de insatisfação
tamanho_insatisfacao.cor <- cor(dados.reclamacoes$reclamacao.tamanho, dados.reclamacoes$mediana)
ggplot(data = dados.reclamacoes, aes(x = reclamacao.tamanho, y = mediana)) +
  geom_point() +
  geom_smooth(method=lm, se=F) +
  xlab("Comprimento da opinião") + ylab("Grau de insatisfação da opinião") +
  labs(title = "Grau de insatisfação da opinião x Comprimento da opinião")

# Grau de insatisfação maior ou igual a 4
orgaos.insatisfacao <- dados.reclamacoes %>% 
  group_by(orgao) %>% 
  summarize(total = sum(mediana >= 4))

ggplot(data = orgaos.insatisfacao, aes(x = orgao, y = total)) +
  geom_bar(stat = "identity") +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
  labs(title = "Quantidade de opiniões cujo grau de insatisfação é maior \n ou igual a 4")
