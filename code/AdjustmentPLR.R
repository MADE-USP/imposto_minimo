options(scipen = 999)
gc()
library(tidyverse)
library(this.path)
setwd(this.dir())
load('../data/baseRendimentosIsentos.Rda')

rendaMaxima99 <- pnadc_receita_final %>% filter(Centésimo == 99) %>% pull(rendimento_todasfontes_calibrado) %>% max()
pnadc_receita_final <- pnadc_receita_final %>% mutate(centesimoReceita = if_else(rendimento_todasfontes_calibrado>rendaMaxima99, 100, Centésimo))
pnadc_receita_final <- pnadc_receita_final %>% mutate(`Participação nos Lucros ou Resultados` = replace_na(`Participação nos Lucros ou Resultados`,0))
pnadc_receita_final <- pnadc_receita_final %>% group_by(centesimoReceita) %>% mutate(TotalPlr = sum(`Participação nos Lucros ou Resultados`)) %>% ungroup()

metaGrupo <- 384168/8

pnad_ordenada <- pnadc_receita_final %>% filter(!is.na(centesimoReceita)) %>% group_by(centesimoReceita) %>%
  arrange(desc(rendimento_todasfontes_calibrado), .by_group = TRUE) %>%
  mutate(
    peso_acumulado = cumsum(peso_comcalib)
  ) %>% ungroup()

pnad_ricos_centesimo <- pnad_ordenada %>%
  group_by(centesimoReceita) %>%
  filter(peso_acumulado <= metaGrupo) %>%
  ungroup()
pnad_ricos_centesimo <- pnad_ricos_centesimo %>% group_by(centesimoReceita) %>% mutate(plr_distribuido = TotalPlr/n()) %>% ungroup()

pnad_ricos_centesimo <- pnad_ricos_centesimo %>% select(c(id_ind, plr_distribuido))
pnadc_receita_final <- pnadc_receita_final %>% left_join(pnad_ricos_centesimo, by="id_ind")
pnadc_receita_final <- pnadc_receita_final %>% mutate(plr_distribuido = replace_na(plr_distribuido,0))

rb8 <- readRDS('../data/colunasRB8.Rds')
rb8 <- rb8 %>% select(c("id_ind", "RB8"))
pnadc_receita_final <- pnadc_receita_final %>% left_join(rb8, by = "id_ind")
save(pnadc_receita_final, file = "../data/baseRendimentosIsentosPlrAdj.Rda")

