gc()
rm(list=ls())

library(haven)
library(modi)
library(acid)
library(tidyverse)
library(hutils)
library(tidyr)
library(scales)
library(this.path)
library(writexl)
library(readxl)
library(forcats)
setwd(this.dir())

cores_made <- c("#45ff66", "#eb52ff", "#3366ff", "#feff41")
load('../data/baseRendimentosIsentosPlrAdj.Rda')

inflaciona <- function(var) {
  (var * 1.16)
}

pnadc_receita_final <- pnadc_receita_final %>%
  mutate(across(.cols = c(rendimento_todasfontes, rendimento_todasfontes_calibrado, `Renda Total`:`Dívida`), .fns = inflaciona))
pnadc_receita_final <- pnadc_receita_final %>% mutate(renda_irpfepnad = coalesce(rendimento_todasfontes_calibrado, rendimento_todasfontes),)
# Preenche NAs antes de qualquer cálculo
pnadc_receita_final <- pnadc_receita_final %>%
  mutate(across(c(`13º salário`, `Rendimentos Recebidos Acumuladamente`,
                  `Ganhos de Capital na Alienação de Bens/Direitos`,
                  `Ganhos Líquidos em Renda Variável`, `Juros sobre Capital Próprio`,
                  `Outros`, `Rendimentos de Caderneta de Poupança etc`,
                  `Indenização por Rescisão do Contrato de Trabalho etc`,
                  `Lucros e Dividendos`), ~replace_na(., 0)))
pnadc_receita_final <- pnadc_receita_final %>%
  mutate(renda_base = renda_irpfepnad - `Rendimentos Recebidos Acumuladamente` -
           `Ganhos de Capital na Alienação de Bens/Direitos` -
           `Rendimentos de Caderneta de Poupança etc` -
           `Indenização por Rescisão do Contrato de Trabalho etc`)

pnadc_receita_final <- pnadc_receita_final %>% mutate(centis = weighted_ntile(renda_base, peso_comcalib, 100))

tabelaCentis <- pnadc_receita_final %>% group_by(centis) %>% summarise(
  'Min' = min(renda_base),
  'Max' = max(renda_base),
  'Média' = sum(renda_base*peso_comcalib)/sum(peso_comcalib)
)
write.csv(tabelaCentis, '../tables/tabelaCentis.csv', row.names = F)