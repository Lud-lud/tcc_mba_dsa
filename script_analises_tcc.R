# Script para análise de dados para TCC do MBA em Data Science & Analytics USP/Esalq

# Objetivo: produzir tabela com produtos dependendentes de polinizadores nas linhas
# e dados referentes a área plantada, quantidade produzida associada à polinização
# e valor de produção associado à polinização para o ano de 2022 nas colunas.

# Pacotes necessários:
library(tidyverse)
library(readxl)
library(writexl)

# Abrir tabelas baixadas diretamente do SIDRA que estão no diretório do projeto:

dados_area_plant <- read_excel("dados_brasil.xlsx", sheet = 1) # área plantada
dados_quant_prod <- read_excel("dados_brasil.xlsx", sheet = 2) # quantidade produzida
dados_valor_prod <- read_excel("dados_brasil.xlsx", sheet = 3) # valor de produção

# Selecionar produtos dependentes de polinizadores:

taxa_dep <- read_excel("dados_TD_v1.xlsx") # abrir tabela com cultivos dependentes de polinizadores e respectivas taxas de dependência
cult_depend <- taxa_dep$Cultivo # obter nomes dos cultivos

lista_dados <- list(dados_area_plant, dados_quant_prod, dados_valor_prod) # input do loop
lista_dados_final <- list() # output do loop

for (tabela in lista_dados){
  dados_final <- as.data.frame(t(tabela)[c(723:794),]) %>% 
    filter(V4 %in% cult_depend) %>% 
    select(V5)
  lista_dados_final <- append(lista_dados_final, list(dados_final))
}

tabela <- cbind(taxa_dep, lista_dados_final[c(1:3)]) # montar tabela de resultados
glimpse(tabela)
colnames(tabela)[3:5] <- c("area", "quant_prod", "valor_prod")

# adicionar quantidade produzida e valor de produção associados à polinização (assumindo que há polinização adequada)
tabela$"quant_prod_dep" <- round(as.numeric(tabela$quant_prod) * as.numeric(tabela$TD), 0)
tabela$"valor_prod_dep" <- round(as.numeric(tabela$valor_prod) * as.numeric(tabela$TD), 0)

# retirar informação de coco-da-bahía, pois está em mil unidades e não toneladas

coco_da_baia <- as.data.frame(tabela[12,]) # salvar a informação
tabela <- tabela[-12,] # remover

#coco_da_baia <- as.data.frame(tabela_2022_dep[9,]) # salvar a informação
#names(coco_da_baia) <- names(tabela_2022_dep)
#tabela_2022_dep <- tabela_2022_dep[-9,] # remover

# adicionar total ao final da tabela

totais <- c("Totais", "-",
            sum(as.numeric(tabela$area), na.rm = T),
            sum(as.numeric(tabela$quant_prod), na.rm = T),
            sum(as.numeric(tabela$valor_prod), na.rm = T),
            sum(tabela$quant_prod_dep, na.rm = T),
            sum(tabela$valor_prod_dep, na.rm = T))

tabela <- rbind(tabela, totais)

write_xlsx(tabela, "prod_2022.xlsx")
write_xlsx(coco_da_baia, "prod_2022_coco.xlsx")



# Gráficos ----------------------------------------------------------------

# gráfico de linha para série temporal (11 anos) de área plantada total de
# produtos e dependentes e não-dependentes

### obter área plantada total

# filtrar tabela pelas linhas que contém os anos
anos <- seq(2012,2022, 1)
area_plant_total_anos <- tabela %>% filter(str_detect(V3, paste(anos, collapse = "|")))


### obter área plantada dependente

# retirar linhas que não interessam (linhas com valores totais e 2 primeiras linhas)
dados_sem_total <- anti_join(tabela, area_plant_total_anos, by = names(tabela))
dados_sem_total <- dados_sem_total[3:nrow(dados_sem_total),]

# filtrar para os produtos dependentes
dados_sem_total <- dados_sem_total %>% filter(str_detect(V4, paste(cult_depend, collapse = "|")))

# dividir dados por anos
tabela_ano <- list()

for (i in 1:11) {
  inicio <- (i - 1) * 33 + 1
  fim <- i * 33
  tabela_ano[[i]] <- dados_sem_total[inicio:fim, ]
}

names(tabela_ano) <- anos

# calcular a soma da área plantada para cada ano
soma_area_plantada_dep <- numeric(nrow(area_plant_total_anos))

for (i in 1:length(soma_area_plantada_dep)) {
  dados_ano <- tabela_ano[[i]]
  soma <- sum(as.numeric(dados_ano$"V5"), na.rm = T)
  soma_area_plantada_dep[i] <- soma
}

# adicionar somas à tabela com dados de produtos não-dependentes
area_plant_total_anos$"area_plant_dep" <- soma_area_plantada_dep

# organizar tabela
area_plant_total_anos <- area_plant_total_anos[,-c(1,2,4,6)]

names(area_plant_total_anos) <- c("ano", "area_plant_total", "area_plant_dep")

# calcular a área plantada não-dependente

area_plant_total_anos$area_plant_nao_dep <- as.numeric(area_plant_total_anos$area_plant_total) - as.numeric(area_plant_total_anos$area_plant_dep)
area_plant_total_anos$perc_dep <- round(as.numeric(area_plant_total_anos$area_plant_dep) / as.numeric(area_plant_total_anos$area_plant_total) * 100, 0)
area_plant_total_anos$perc_nao_dep <- round(as.numeric(area_plant_total_anos$area_plant_nao_dep) / as.numeric(area_plant_total_anos$area_plant_total) * 100, 0)

# a área plantada de cultivos dependentes de fato está aumentando no Brasil
# conforme observado por Aizen et al., 2009 (verificar) para o mundo todo
# entretanto eles se estagnam a partir de 2018
# talvez seja legal pegar dados de área plantada de todos os anos disponíveis
# os gráficos de área para produto mostrarão quais são os que mais influenciam
# nesta tendência

