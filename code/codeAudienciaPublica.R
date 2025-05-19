library(tidyverse)
library(this.path)
setwd(this.dir())

load('../data/baseRendimentosIsentosPlrAdj.Rda')

pnadc_receita_final$renda_irpfepnad <- coalesce(pnadc_receita_final$rendimento_todasfontes_calibrado, pnadc_receita_final$rendimento_todasfontes)
pnadc_receita_final <- pnadc_receita_final %>%
  arrange(renda_irpfepnad)
total_pobres_renda <- 0
total_pobres_numero <- 0
total_ricos_renda <- 0
total_ricos_numero <- 0
for(i in 1:nrow(pnadc_receita_final)) {
  row <- pnadc_receita_final[i,]
  total_pobres_renda <- total_pobres_renda + (row$renda_irpfepnad*row$peso_comcalib)
  total_pobres_numero <- total_pobres_numero + row$peso_comcalib
  if(total_pobres_numero>10e6){
    break
  }
}
pnadc_receita_final <- pnadc_receita_final %>%
  arrange(desc(renda_irpfepnad))
for(i in 1:nrow(pnadc_receita_final)) {
  row <- pnadc_receita_final[i,]
  total_ricos_renda <- total_ricos_renda + (row$renda_irpfepnad*row$peso_comcalib)
  total_ricos_numero <- total_ricos_numero + row$peso_comcalib
  if(total_ricos_renda>total_pobres_renda){
    break
  }
}

individuo_mais_rico_renda <- 1321919521.95
pobres_renda <- 0
pobres_numero <- 0
pnadc_receita_final <- pnadc_receita_final %>%
  arrange(renda_irpfepnad)
for(i in 1:nrow(pnadc_receita_final)) {
  row <- pnadc_receita_final[i,]
  pobres_renda <- pobres_renda + (row$renda_irpfepnad*row$peso_comcalib)
  pobres_numero <- pobres_numero + row$peso_comcalib
  if(pobres_renda>individuo_mais_rico_renda){
    break
  }
}
source('utils/IneqFunctions.R')
Bottom_Aprop(pnadc_receita_final$renda_irpfepnad, pnadc_receita_final$peso_comcalib, 10)
Top_Aprop(pnadc_receita_final$renda_irpfepnad, pnadc_receita_final$peso_comcalib, 91)
