# API para obtenção de dados do IBGE

library("sidrar")
library("readxl")
library("tidyverse")

td_cod_cultivos <- read_excel("td_cod_cultivos.xlsx")
codigos_cultivos <- td_cod_cultivos$Codigo

num_parts <- 6
part_length <- ceiling(length(codigos_cultivos) / num_parts)
codigo_cultivos_parts <- split(codigos_cultivos, ceiling(seq_along(codigos_cultivos) / part_length))


dados <- data.frame()

info_sidra(5457, wb = FALSE)

for (part in codigo_cultivos_parts){
  part_str <- paste(part, collapse = ',')
  result <- get_sidra(
    x = 5457,
    geo = "City",
    period = "2022",
    variable = 8331,
    classific = "c782",
    category = list(part_str)
  )
  dados <- rbind(dados, result)
}

glimpse(dados)
dados_ap <- select(dados, "Município (Código)", "Município",
                   "Produto das lavouras temporárias e permanentes (Código)",
                   "Produto das lavouras temporárias e permanentes", "Valor")

dados_ap_t <- dados_ap %>% 
  pivot_wider(
    names_from = "Produto das lavouras temporárias e permanentes",
    values_from = "Valor"
  )

