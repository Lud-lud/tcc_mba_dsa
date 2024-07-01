# Script para análise de dados para TCC do MBA em Data Science & Analytics USP/Esalq


# Tabela ------------------------------------------------------------------


# Objetivo: produzir tabela com produtos dependendentes de polinizadores nas linhas
# e dados referentes a área plantada, quantidade produzida associada à polinização
# e valor de produção associado à polinização para o ano de 2022 nas colunas.

# Pacotes necessários:
library(tidyverse)
library(readxl)
library(writexl)

# Abrir tabelas baixadas diretamente do SIDRA por meio de API:

cod_area_plant <- "/t/5457/n1/all/v/8331/p/last%2010/c782/all"
dados_area_plant <- sidrar::get_sidra(api = cod_area_plant)

cod_quant_prod <- "/t/5457/n1/all/v/214/p/last%2010/c782/all"
dados_quant_prod <- sidrar::get_sidra(api = cod_quant_prod)

cod_valor_prod <- "/t/5457/n1/all/v/215/p/last%2010/c782/all"
dados_valor_prod <- sidrar::get_sidra(api = cod_valor_prod)

taxa_dep <- read_excel("dados_TD_v1.xlsx") # abrir tabela com cultivos dependentes de polinizadores e respectivas taxas de dependência
cult_depend <- taxa_dep$Cultivo # obter nomes dos cultivos

lista_dados <- list(dados_area_plant, dados_quant_prod, dados_valor_prod) # input do loop
lista_dados_final <- list() # output do loop

for (tabela in lista_dados){
  dados_final <- filter(tabela, Ano == "2022" & `Produto das lavouras temporárias e permanentes` %in% cult_depend)
  lista_dados_final <- append(lista_dados_final, list(dados_final))
}

tabela <- lista_dados_final[[1]][,c("Produto das lavouras temporárias e permanentes", "Valor")] %>%
  cbind(lista_dados_final[[2]][,"Valor"], lista_dados_final[[3]][,"Valor"], taxa_dep$TD)

colnames(tabela) <- c("produto", "area", "quant_prod", "valor_prod", "TD")

tabela <- tabela %>% select(produto, TD, everything())


# adicionar quantidade produzida e valor de produção associados à polinização (assumindo que há polinização adequada)
tabela$"quant_prod_dep" <- round(as.numeric(tabela$quant_prod) * as.numeric(tabela$TD), 0)
tabela$"valor_prod_dep" <- round(as.numeric(tabela$valor_prod) * as.numeric(tabela$TD), 0)


# adicionar total ao final da tabela

totais <- c("Totais", "-",
            sum(as.numeric(tabela$area), na.rm = T),
            sum(as.numeric(tabela$quant_prod), na.rm = T),
            sum(as.numeric(tabela$valor_prod), na.rm = T),
            sum(tabela$quant_prod_dep, na.rm = T),
            sum(tabela$valor_prod_dep, na.rm = T))

tabela <- rbind(tabela, totais)

write_xlsx(tabela, "prod_2022.xlsx")


# Gráfico -----------------------------------------------------------------

library(ggplot2)

# obter uma tabela que contenha os cultivos nas linhas, uma coluna para cada ano
# e os valores correspondentes ao total multiplicado pela taxa de dependência preenchendo
# a tabela

# figuras: gráfico de linhas, com anos no eixo X e área no eixo Y


area_plantada_dep <- filter(dados_area_plant, `Produto das lavouras temporárias e permanentes` %in% cult_depend) %>% 
  merge(y = taxa_dep,
        by.x = "Produto das lavouras temporárias e permanentes",
        by.y = "Cultivo")

# Retirando dados ausentes (pois não serão plotados):
area_plantada_dep <- area_plantada_dep %>%
  filter(!is.na(Ano) & !is.na(Valor))

# Agrupar por taxa de dependência:
td_baixa <- filter(area_plantada_dep, TD < 0.1)
td_modesta <- filter(area_plantada_dep, TD >= 0.1 & TD < 0.4)
td_alta <- filter(area_plantada_dep, TD >= 0.4 & TD < 0.9)
td_essencial <- filter(area_plantada_dep, TD >= 0.9)


fazer_grafico <- function(tabela, titulo){
  p <- ggplot(
    tabela,
    mapping = aes(x = Ano, y = Valor, color = `Produto das lavouras temporárias e permanentes`,
                  group = `Produto das lavouras temporárias e permanentes`)
  ) +
    labs(title = titulo,
         y = "Área (ha)") +
    geom_line()
  print(p)
}

fazer_grafico(td_baixa, "Área plantada de cultivos com baixa dependência em polinizadores")
fazer_grafico(td_modesta, "Área plantada de cultivos com modesta dependência em polinizadores")
fazer_grafico(td_alta, "Área plantada de cultivos com alta dependência em polinizadores")
fazer_grafico(td_essencial, "Área plantada de cultivos com essencial dependência em polinizadores")

