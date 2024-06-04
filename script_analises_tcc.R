# Script para análise de dados para TCC do MBA em Data Science & Analytics USP/Esalq

# pacotes necessários
library(tidyverse)
library(readxl)
library(stringr)

# abrir tabelas baixadas diretamente do SIDRA que estão no diretório do projeto

dados_area_plantada <- read_excel("dados_brasil.xlsx", sheet = 1)
dados_quant_prod <- read_excel("dados_brasil.xlsx", sheet = 2)
dados_valor_prod <- read_excel("dados_brasil.xlsx", sheet = 3)

# produzir tabela com produtos dependendentes de polinizadores nas linhas
# com colunas referentes a área plantada, quantidade produzida associada à
# polinização e valor de produção associado à polinização para o ano de 2022

# transpor a matriz com os dados necessários para conseguir visualizar melhor
# aqui comecei utilizando dados de area plantada e depois adicionarei colunas
# para as demais variáveis
tabela <- as.data.frame(t(dados_area_plantada))


# Tabela ------------------------------------------

# 1. selecionar dados de produção para 2022
tabela_2022 <- tabela[c(723:794),]

# remover colunas vazias
tabela_2022 <- tabela_2022[,c(4,5)]
colnames(tabela_2022) <- c("Produtos", "Área plantada (ha)")

# renomear colunas

# 2. selecionar produtos dependentes de polinizadores
cult_depend <- c("Ervilha", "Fava em grão", "Goiaba", "Linho", "Uva",
                 "Algodão arbóreo", "Algodão herbáceo", "Amendoim", "Café arábica",
                 "Coco-da-baía", "Feijão", "Laranja", "Pêssego", "Soja", "Tomate",
                 "Abacate", "Açaí", "Café canephora", "Caqui", "Dendê",
                 "Girassol", "Guaraná", "Limão", "Maçã", "Mamona", "Manga", "Melancia",
                 "Pera", "Tangerina", "Cacau", "Caju", "Mamão", "Maracujá",
                 "Marmelo", "Melão", "Urucum")

tabela_2022_dep <- tabela_2022 %>%
  filter(str_detect(Produtos, paste(cult_depend, collapse = "|")))


# 3. adicionar coluna de quantidade produzida

# adicionar coluna com taxa de dependência de cada cultivo
taxa_dep <- read_excel("dados_TD_v1.xlsx")

tabela_2022_dep <- merge(tabela_2022_dep, taxa_dep, by.x = "Produtos", by.y = "Cultivo", all.x = TRUE)

# adicionar quantidade produzida bruta
quant_prod <- as.data.frame(t(dados_quant_prod)[c(723:794),]) %>% 
  filter(str_detect(V4, paste(cult_depend, collapse = "|"))) %>% 
  select(V5)

tabela_2022_dep <- cbind(tabela_2022_dep, quant_prod)
names(tabela_2022_dep)[names(tabela_2022_dep) == "V5"] <- "Quant. prod. (bruta)"

# adicionar quantidade produzida associada à polinização (assumindo que há polinização adequada)
tabela_2022_dep$"Quant. prod. (dependente)" <- round(as.numeric(tabela_2022_dep$`Quant. prod. (bruta)`) * as.numeric(tabela_2022_dep$TD), 0)

# 4. adicionar coluna de valor de produção bruto
valor_prod <- as.data.frame(t(dados_valor_prod)[c(723:794),]) %>% 
  filter(str_detect(V4, paste(cult_depend, collapse = "|"))) %>% 
  select(V5)

tabela_2022_dep <- cbind(tabela_2022_dep, valor_prod)
names(tabela_2022_dep)[names(tabela_2022_dep) == "V5"] <- "Valor. prod. (bruto)"

# calcular fração associada à polinização (assumindo que há polinização adequada)
tabela_2022_dep$"Valor prod. (dependente)" <- round(as.numeric(tabela_2022_dep$`Valor. prod. (bruto)`) * as.numeric(tabela_2022_dep$TD), 0)

# mudar posição da coluna TD para depois de Produtos
tabela_2022_dep <- tabela_2022_dep %>% relocate(TD, .after = Produtos)


# retirar informação de coco-da-bahía, pois está em mil unidades e não toneladas
coco_da_baia <- as.data.frame(tabela_2022_dep[9,]) # salvar a informação
names(coco_da_baia) <- names(tabela_2022_dep)
tabela_2022_dep <- tabela_2022_dep[-9,] # remover

# adicionar total ao final da tabela
totais <- c("Totais", "-",
            sum(as.numeric(tabela_2022_dep$`Área plantada (ha)`), na.rm = T),
            sum(as.numeric(tabela_2022_dep$`Quant. prod. (bruta)`), na.rm = T),
            sum(tabela_2022_dep$`Quant. prod. (dependente)`, na.rm = T),
            sum(as.numeric(tabela_2022_dep$`Valor. prod. (bruto)`), na.rm = T),
            sum(tabela_2022_dep$`Valor prod. (dependente)`, na.rm = T))
            

tabela_2022_dep <- rbind(tabela_2022_dep, totais)

