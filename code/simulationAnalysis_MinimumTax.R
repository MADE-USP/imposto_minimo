gc()
rm(list=ls())


# 0 - Imports e Carregamento da Base --------------------------------------

library(haven)
library(ggplot2)
library(modi)
library(acid)
library(tidyverse)
library(hutils)
library(dplyr)
library(tidyr)
library(scales)
library(this.path)
install.packages("writexl")
library(writexl)
setwd(this.dir())

cores_made <- c("#45ff66", "#eb52ff", "#3366ff","#feff41")
load('../data/baseRendimentosIsentos.Rda')


# 1 - Parâmetros de Faixas e Alíquotas (Regime Atual) -------------------------

faixa1 <- 2259.20/1.16
faixa2 <- 2826.65/1.16
faixa3 <- 3751.05/1.16
faixa4 <- 4664.68/1.16

aliquota1 <- 0.075
aliquota2 <- 0.15
aliquota3 <- 0.225   
aliquota4 <- 0.275  


# 2 - Função IR Mensal (Regime Atual) -------------------------------------

calcula_irpf_mensal_antigo <- function(renda) {
  if (renda <= faixa1) {
    0
  } else if (renda <= faixa2) {
    (renda - faixa1) * aliquota1
  } else if (renda <= faixa3) {
    (renda - faixa2) * aliquota2 + (faixa2 - faixa1) * aliquota1
  } else if (renda <= faixa4) {
    (renda - faixa3) * aliquota3 + (faixa3 - faixa2) * aliquota2 + (faixa2 - faixa1) * aliquota1
  } else {
    (renda - faixa4) * aliquota4 +
      (faixa4 - faixa3) * aliquota3 +
      (faixa3 - faixa2) * aliquota2 +
      (faixa2 - faixa1) * aliquota1
  }
}

tax_table_plr <- data.frame(
  lower = c(0, 6677.56/1.0462, 9922.29/1.0462, 13167.01/1.0462, 16380.38/1.0462),
  upper = c(6677.55/1.0462, 9922.28/1.0462, 13167/1.0462, 16380.38/1.0462, Inf),
  rate = c(0, 0.075, 0.15, 0.225, 0.275)
)
tax_table <- data.frame(
  lower = c(0, faixa1, faixa2, faixa3, faixa4),
  upper = c(faixa1, faixa2, faixa3, faixa4, Inf),
  rate = c(0, 0.075, 0.15, 0.225, 0.275)
)



# Função para calcular o imposto devido com base no valor bruto e na tabela progressiva
calculate_tax <- function(bruto, tax_table) {
  tax <- 0
  for (i in 1:nrow(tax_table)) {
    lower <- tax_table$lower[i]
    upper <- tax_table$upper[i]
    rate <- tax_table$rate[i]
    # Se o valor bruto ultrapassa o limite inferior da faixa,
    # calcula o rendimento tributável nessa faixa.
    if (bruto > lower) {
      taxable_income <- min(bruto, upper) - lower
      tax <- tax + taxable_income * rate
    }
  }
  return(tax)
}

# Função para encontrar o valor bruto dado o valor líquido
# Usamos a função uniroot para resolver a equação: bruto - calculate_tax(bruto) = liquido
find_bruto <- function(liquido, tax_table, lower_bound = 0, upper_bound = 1e9) {
  f <- function(bruto) {
    bruto - calculate_tax(bruto, tax_table) - liquido
  }
  result <- uniroot(f, lower = lower_bound, upper = upper_bound)
  return(result$root)
}

pnadc_receita_final <- pnadc_receita_final %>% mutate(`13º salário` =  replace_na(`13º salário`, 0))
pnadc_receita_final <- pnadc_receita_final %>% mutate(imposto_13 = mapply(find_bruto, `13º salário`, MoreArgs = list(tax_table = tax_table))-`13º salário`)

pnadc_receita_final <- pnadc_receita_final %>% mutate(`Rendimentos Recebidos Acumuladamente` =  replace_na(`Rendimentos Recebidos Acumuladamente`, 0))
pnadc_receita_final <- pnadc_receita_final %>% mutate(imposto_rra = mapply(find_bruto, `Rendimentos Recebidos Acumuladamente`, MoreArgs = list(tax_table = tax_table))-`Rendimentos Recebidos Acumuladamente`)

pnadc_receita_final <- pnadc_receita_final %>% mutate(`Participação nos Lucros ou Resultados` =  replace_na(`Rendimentos Recebidos Acumuladamente`, 0))
pnadc_receita_final <- pnadc_receita_final %>% mutate(`Participação nos Lucros ou Resultados` =  `Participação nos Lucros ou Resultados`*12)
pnadc_receita_final <- pnadc_receita_final %>% mutate(imposto_plr = mapply(find_bruto, `Participação nos Lucros ou Resultados`, MoreArgs = list(tax_table = tax_table_plr))-`Participação nos Lucros ou Resultados`)
pnadc_receita_final <- pnadc_receita_final %>% mutate(imposto_plr = imposto_plr/12)

pnadc_receita_final <- pnadc_receita_final %>% mutate(`Ganhos de Capital na Alienação de Bens/Direitos` =  replace_na(`Ganhos de Capital na Alienação de Bens/Direitos`, 0))
pnadc_receita_final <- pnadc_receita_final %>% mutate(`Ganhos Líquidos em Renda Variável` =  replace_na(`Ganhos Líquidos em Renda Variável`, 0))
pnadc_receita_final <- pnadc_receita_final %>% mutate(`Juros sobre Capital Próprio` =  replace_na(`Juros sobre Capital Próprio`, 0))
pnadc_receita_final <- pnadc_receita_final %>% mutate(`Outros` =  replace_na(`Outros`, 0))


pnadc_receita_final <- pnadc_receita_final %>% mutate(capital = `Rendimentos de Aplicações Financeiras` + `Ganhos Líquidos em Renda Variável`+`Juros sobre Capital Próprio` + Outros)
pnadc_receita_final <- pnadc_receita_final %>% mutate(imposto_capital = (capital/(1-0.15)-capital))
pnadc_receita_final$renda_irpfepnad <- coalesce(pnadc_receita_final$rendimento_todasfontes_calibrado, pnadc_receita_final$rendimento_todasfontes)

pnadc_receita_final <- pnadc_receita_final %>% mutate(`Rendimentos de Caderneta de Poupança etc` = replace_na(`Rendimentos de Caderneta de Poupança etc`,0))
pnadc_receita_final <- pnadc_receita_final %>% mutate(`Indenização por Rescisão do Contrato de Trabalho etc` = replace_na(`Indenização por Rescisão do Contrato de Trabalho etc`,0))
pnadc_receita_final <- pnadc_receita_final %>% mutate(renda_base = renda_irpfepnad - `Rendimentos Recebidos Acumuladamente`-`Ganhos de Capital na Alienação de Bens/Direitos`-`Rendimentos de Caderneta de Poupança etc`-`Indenização por Rescisão do Contrato de Trabalho etc`)
pnadc_receita_final <- pnadc_receita_final %>% mutate(imposto_withholding = imposto_capital+imposto_plr)
pnadc_receita_final <- pnadc_receita_final %>% mutate(imposto_withholding = replace_na(imposto_withholding,0))

# 3 - Função IR Mensal (Nova Proposta Até 7k) -----------------------------
#    - Até 5k isento
#    - 5k a 7k: redução linear
#    - Acima de 7k: sem abatimento adicional


calcula_irpf_mensal_novo <- function(renda) {
  # Primeiro, calcula o imposto base conforme a redução até 7.000
  if (renda <= 5000) {
    base_tax <- 0
  } else if (renda <= 7000) {
    reducao <- 1095.11 - 0.156445 * renda
    base_tax <- max(calcula_irpf_mensal_antigo(renda) - reducao, 0)
  } else {
    base_tax <- calcula_irpf_mensal_antigo(renda)
  }
}

pnadc_receita_final$base_c <- coalesce(pnadc_receita_final$`Base de Cálculo`, 0)

pnadc_receita_final <- pnadc_receita_final %>%
  mutate(
    # IR mensal no regime antigo
    irpf_mensal_antigo = map_dbl(base_c, calcula_irpf_mensal_antigo),
    # IR mensal com isenção/redução até 7k
    irpf_mensal_novo = map_dbl(base_c, calcula_irpf_mensal_novo)
  )

pnadc_receita_final <- pnadc_receita_final %>% mutate(aliquota_pre_ricos = (imposto_withholding+irpf_mensal_novo)/renda_base)
pnadc_receita_final <- pnadc_receita_final %>% mutate(base_tax = imposto_withholding+irpf_mensal_novo)
# Aplica complemento para altas rendas:
imposto_final <- function(base_tax,renda){
  if (renda <= 50000) {
    return(base_tax)
  } else if (renda < 100000) {
    desired_tax <- (((renda * 12 / 60000 - 10)))/100 * renda
    return(max(base_tax, desired_tax))
  } else {  # Caso renda >= 100
    desired_tax <- (0.10) * renda
    return(max(base_tax, desired_tax))
  }
}


# 4 - Aplica Imposto e Calcula Arrecadação --------------------------------

pnadc_receita_final <- pnadc_receita_final %>%
  mutate(imposto_calculado = pmap_dbl(list(base_tax, renda_base), imposto_final))


irpf_total_atual <- 1.16 * 12* sum(pnadc_receita_final$peso_comcalib * (pnadc_receita_final$irpf_mensal_antigo+pnadc_receita_final$imposto_withholding)) / 1e9
custo_isencao_mensal    <- 1.16 * 12* sum(pnadc_receita_final$peso_comcalib *
                                          (pnadc_receita_final$irpf_mensal_antigo -
                                             pnadc_receita_final$irpf_mensal_novo)) / 1e9
irpf_total_novo <- 1.16 * 12* sum(pnadc_receita_final$peso_comcalib * pnadc_receita_final$imposto_calculado) / 1e9

pnadc_receita_final <- pnadc_receita_final %>% mutate('renda_pos_novo'= renda_base - imposto_calculado)
pnadc_receita_final <- pnadc_receita_final %>% mutate('renda_pos_atual'= renda_base - (imposto_withholding+irpf_mensal_antigo))

# 5 - Estatísticas Distributivas ------------------------------------------
## GINI - NOVO ###
source('utils/IneqFunctions.R')
Bottom_Aprop(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib, 50)
Top_Aprop(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib, 100)
Top_Aprop(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib, 91)
StatsGini(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib)

## GINI - ANTIGO ###
Bottom_Aprop(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib, 50)
Top_Aprop(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib, 100)
Top_Aprop(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib, 91)
StatsGini(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib)


# 6 - Gráfico de Alíquotas Efetivas ---------------------------------------

# GRÁFICO DE ALÍQUOTAS EFETIVAS MENSAL
#    - Dividimos em quantis (para < 50k), "Ricos" (50k-100k), "Milionários" (>= 100k)
#    - Alíquota efetiva = [sum(IR * peso) / sum(Renda * peso)] * 100
#pnadc_receita_final <- pnadc_receita_final %>% mutate(aliq_efetiva_antigo = 
                                                        #sum(irpf_mensal_antigo*peso_comcalib) / sum(renda_irpfepnad*peso_comcalib),
                                                      #aliq_efetiva_novo = sum(irpf_mensal_novo*peso_comcalib)/sum(renda_irpfepnad*peso_comcalib)
#)

#df_taxpayers <- pnadc_receita_final %>%
  # Se quiser, filtra só quem paga IR em algum regime
  #filter(irpf_mensal_antigo > 0 | irpf_mensal_novo > 0)

# Calcula quantis mensais
pnadc_receita_final$quantis <- weighted_ntile(pnadc_receita_final$renda_base,
                                              pnadc_receita_final$peso_comcalib, 100)

# Cria a variável de "divisao_renda" em termos mensais
pnadc_receita_final <- pnadc_receita_final %>%
  mutate(
    divisao_renda = case_when(
      renda_base >= 100000 ~ "Milionários",
      renda_base >= 50000  ~ "Ricos",
      TRUE ~ as.character(quantis)
    )
  )

pnadc_receita_agg <- pnadc_receita_final %>% group_by(divisao_renda) %>%  
                                               summarise(Regime_Atual = sum((imposto_withholding+irpf_mensal_antigo)*peso_comcalib)/sum(renda_base*peso_comcalib)*100,
                                                         Nova_Proposta = sum(imposto_calculado*peso_comcalib)/sum(renda_base*peso_comcalib)*100)

# Converte para formato longo
df_long <- pnadc_receita_agg %>%
  pivot_longer(cols = c(Regime_Atual, Nova_Proposta),
               names_to = "Regime",
               values_to = "Aliquota_Efetiva")
df_long <- df_long %>% distinct()
df_long <- df_long %>% filter(Aliquota_Efetiva>=0)
# Ordena o eixo x
quantis_numeric <- suppressWarnings(as.numeric(df_long$divisao_renda))
ordem_quantis   <- sort(unique(quantis_numeric[!is.na(quantis_numeric)]))
ordem_x         <- c(as.character(ordem_quantis), "Ricos", "Milionários")
df_long$divisao_renda <- factor(df_long$divisao_renda, levels = ordem_x)
df_long <- df_long %>% filter(!divisao_renda %in% as.character(1:75))
# Plot
p <- ggplot(df_long, aes(x = divisao_renda, y = Aliquota_Efetiva,
                         color = Regime, group = Regime)) +
  geom_line(linewidth = 1.25) +
  theme_bw() +
  scale_color_manual(values = c("Regime_Atual" = "#3366ff", "Nova_Proposta" = "#eb52ff")) +
  xlab("Posição na Distribuição de Renda (Mensal)") +
  ylab("Alíquota Efetiva (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(p)


# 7 - Simulação - Alíquota Máxima -----------------------------------------

aliMax <- max(pnadc_receita_agg$Nova_Proposta)/100

pnadc_receita_final <- pnadc_receita_final %>% mutate(imposto_ali_max = if_else(divisao_renda %in% c("100", "Ricos", "Milionários"), 
                                                                                aliMax*renda_base, imposto_calculado))

graphAliMax <- pnadc_receita_final %>% group_by(divisao_renda) %>%  
  summarise(Regime_Atual = sum((imposto_withholding+irpf_mensal_antigo)*peso_comcalib)/sum(renda_base*peso_comcalib)*100,
            Nova_Proposta = sum(imposto_calculado*peso_comcalib)/sum(renda_base*peso_comcalib)*100,
            Proposta_Aliquota_Maxima = sum(imposto_ali_max*peso_comcalib)/sum(renda_base*peso_comcalib)*100)

# Converte para formato longo
df_long <- graphAliMax %>%
  pivot_longer(cols = c(Regime_Atual, Nova_Proposta,Proposta_Aliquota_Maxima),
               names_to = "Regime",
               values_to = "Aliquota_Efetiva")
df_long <- df_long %>% distinct()
df_long <- df_long %>% filter(Aliquota_Efetiva>=0)
# Ordena o eixo x
quantis_numeric <- suppressWarnings(as.numeric(df_long$divisao_renda))
ordem_quantis   <- sort(unique(quantis_numeric[!is.na(quantis_numeric)]))
ordem_x         <- c(as.character(ordem_quantis), "Ricos", "Milionários")
df_long$divisao_renda <- factor(df_long$divisao_renda, levels = ordem_x)
df_long <- df_long %>% filter(!divisao_renda %in% as.character(1:75))
# Plot
pAliMax <- ggplot(df_long, aes(x = divisao_renda, y = Aliquota_Efetiva,
                         color = Regime, group = Regime)) +
  geom_line(linewidth = 1.25) +
  theme_bw() +
  scale_color_manual(values = c("Regime_Atual" = "#3366ff", "Nova_Proposta" = "#eb52ff", "Proposta_Aliquota_Maxima"="#feff41")) +
  xlab("Posição na Distribuição de Renda (Mensal)") +
  ylab("Alíquota Efetiva (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(pAliMax)



###############################################################################################
                                  ######### GKM ###########

# Cria coluna auxiliar com alíquota efetiva apenas para cálculo do limite
pnadc_receita_final <- pnadc_receita_final %>%
  mutate(aliq_efetiva_atual = if_else(renda_base > 0, imposto_calculado / renda_base, NA_real_))

limite_renda <- pnadc_receita_final %>%
  filter(is.finite(aliq_efetiva_atual)) %>%
  slice(which.min(abs(aliq_efetiva_atual - aliMax))) %>%
  pull(renda_base)

# Aplica imposto com alíquota máxima acima do limite
pnadc_receita_final <- pnadc_receita_final %>%
  mutate(imposto_ali_max_novo = if_else(renda_base > limite_renda,
                                        aliMax * renda_base,
                                        imposto_calculado))

irpf_total_novo_aliMax <- 1.16 * 12 * sum(pnadc_receita_final$peso_comcalib * pnadc_receita_final$imposto_ali_max_novo, na.rm = TRUE) / 1e9

# Gini:

# Recalcula arrecadações com base nas suas fórmulas
irpf_total_atual <- 1.16 * 12 * sum(pnadc_receita_final$peso_comcalib *
                                      (pnadc_receita_final$irpf_mensal_antigo +
                                         pnadc_receita_final$imposto_withholding)) / 1e9

irpf_total_novo <- 1.16 * 12 * sum(pnadc_receita_final$peso_comcalib *
                                     pnadc_receita_final$imposto_calculado) / 1e9

irpf_total_aliMax <- 1.16 * 12 * sum(pnadc_receita_final$peso_comcalib *
                                       pnadc_receita_final$imposto_ali_max_novo) / 1e9

# Diferenças em relação ao atual
dif_arrec_novo <- irpf_total_novo - irpf_total_atual
dif_arrec_aliMax <- irpf_total_aliMax - irpf_total_atual

# Estatísticas distributivas
## Regime atual
gini_atual <- StatsGini(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib)
bottom50_atual <- Bottom_Aprop(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib, 50)
top9_atual <- Top_Aprop(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib, 91)
top1_atual <- Top_Aprop(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib, 99)

## Nova proposta original
gini_novo <- StatsGini(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib)
bottom50_novo <- Bottom_Aprop(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib, 50)
top9_novo <- Top_Aprop(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib, 91)
top1_novo <- Top_Aprop(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib, 99)

## Nova com alíquota máxima estendida
gini_aliMax <- StatsGini(pnadc_receita_final$renda_pos_aliMax, pnadc_receita_final$peso_comcalib)
bottom50_aliMax <- Bottom_Aprop(pnadc_receita_final$renda_pos_aliMax, pnadc_receita_final$peso_comcalib, 50)
top9_aliMax <- Top_Aprop(pnadc_receita_final$renda_pos_aliMax, pnadc_receita_final$peso_comcalib, 91)
top1_aliMax <- Top_Aprop(pnadc_receita_final$renda_pos_aliMax, pnadc_receita_final$peso_comcalib, 99)

# Tabela final
tabela_resultados <- data.frame(
  Cenário = c("Regime Atual", "Nova Proposta", "Nova c/ Aliq. Máxima"),
  Gini = c(gini_atual, gini_novo, gini_aliMax),
  Bottom_50 = c(bottom50_atual, bottom50_novo, bottom50_aliMax),
  Top_9 = c(top9_atual, top9_novo, top9_aliMax),
  Top_1 = c(top1_atual, top1_novo, top1_aliMax),
  Arrecadacao_BR = c(irpf_total_atual, irpf_total_novo, irpf_total_aliMax),
  Dif_Arrecadacao_BR = c(0, dif_arrec_novo, dif_arrec_aliMax)
)

# Exibe
print(tabela_resultados)

# Salva como CSV
write.csv(tabela_resultados, "resultados_distributivos_com_arrecadacao.csv", row.names = FALSE)
write_xlsx(tabela_resultados, "resultados_distributivos_com_arrecadacao.xlsx")

# Gerar outros indicadores de desigualdade:
# Função para calcular média ponderada no top 0.1%
media_top_01 <- function(renda, peso) {
  limite_999 <- wtd.quantile(renda, weights = peso, probs = 0.999, na.rm = TRUE)
  top_01 <- renda > limite_999
  sum(renda[top_01] * peso[top_01], na.rm = TRUE) / sum(peso[top_01], na.rm = TRUE)
}

# Função estendida com a nova razão
calcular_indicadores_ext <- function(renda, peso, nome_cenario) {
  p10 <- wtd.quantile(renda, weights = peso, probs = 0.10, na.rm = TRUE)
  p50 <- wtd.quantile(renda, weights = peso, probs = 0.50, na.rm = TRUE)
  p99 <- wtd.quantile(renda, weights = peso, probs = 0.99, na.rm = TRUE)
  media_top001 <- media_top_01(renda, peso)
  
  data.frame(
    Cenário = nome_cenario,
    P10 = p10,
    P50 = p50,
    P99 = p99,
    Dif_P50_P10 = p50 - p10,
    Dif_P99_P50 = p99 - p50,
    Razao_P50_P10 = p50 / p10,
    Razao_P99_P50 = p99 / p50,
    Media_Top_0.1 = media_top001,
    Razao_Top001_P50 = media_top001 / p50
  )
}

# Aplica para os 3 cenários
indicadores_atual_ext <- calcular_indicadores_ext(pnadc_receita_final$renda_pos_atual,
                                                  pnadc_receita_final$peso_comcalib,
                                                  "Regime Atual")

indicadores_novo_ext <- calcular_indicadores_ext(pnadc_receita_final$renda_pos_novo,
                                                 pnadc_receita_final$peso_comcalib,
                                                 "Nova Proposta")

indicadores_aliMax_ext <- calcular_indicadores_ext(pnadc_receita_final$renda_pos_aliMax,
                                                   pnadc_receita_final$peso_comcalib,
                                                   "Nova c/ Aliq. Máxima")

# Junta tudo
tabela_percentis_ext <- bind_rows(indicadores_atual_ext, indicadores_novo_ext, indicadores_aliMax_ext)

# Exibe
print(tabela_percentis_ext)

# Salva em Excel
write_xlsx(tabela_percentis_ext, "indicadores_percentis_renda_extendido.xlsx")

# Grafico com apropriacao:

library(dplyr)
library(ggplot2)

# Função para calcular apropriação por centil
apropriacao_por_centil <- function(renda, peso, nome_cenario) {
  df <- data.frame(renda = renda, peso = peso) %>%
    mutate(centil = weighted_ntile(renda, peso, 100)) %>%
    group_by(centil) %>%
    summarise(
      renda_total = sum(renda * peso, na.rm = TRUE)
    ) %>%
    mutate(
      prop_renda = renda_total / sum(renda_total),
      Cenário = nome_cenario
    )
  return(df)
}

# Aplica para os 3 cenários
df_atual <- apropriacao_por_centil(pnadc_receita_final$renda_pos_atual,
                                   pnadc_receita_final$peso_comcalib,
                                   "Regime Atual")

df_novo <- apropriacao_por_centil(pnadc_receita_final$renda_pos_novo,
                                  pnadc_receita_final$peso_comcalib,
                                  "Nova Proposta")

df_aliMax <- apropriacao_por_centil(pnadc_receita_final$renda_pos_aliMax,
                                    pnadc_receita_final$peso_comcalib,
                                    "Nova c/ Aliq. Máxima")

# Junta tudo
df_aprop <- bind_rows(df_atual, df_novo, df_aliMax)

ggplot(df_aprop, aes(x = centil, y = prop_renda, color = Cenário)) +
  geom_line(linewidth = 0.4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(values = c(
    "Regime Atual" = "#3366ff",
    "Nova Proposta" = "#eb52ff",
    "Nova c/ Aliq. Máxima" = "#feff41"
  )) +
  labs(
    title = "Apropriação da Renda por Centil",
    x = "Centil de Renda",
    y = "Proporção da Renda Total (%)",
    color = "Cenário"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("apropriacao_renda_por_centil.png", width = 8, height = 5, dpi = 300)

#Curva de apropriacao acumulada:

# Função para apropriação por centil
apropriacao_por_centil <- function(renda, peso, nome_cenario) {
  data.frame(renda = renda, peso = peso) %>%
    mutate(centil = weighted_ntile(renda, peso, 100)) %>%
    group_by(centil) %>%
    summarise(
      renda_total = sum(renda * peso, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      prop_renda = renda_total / sum(renda_total),
      Cenário = nome_cenario
    )
}

# Recria as 3 bases
df_atual   <- apropriacao_por_centil(pnadc_receita_final$renda_pos_atual,   pnadc_receita_final$peso_comcalib, "Regime Atual")
df_novo    <- apropriacao_por_centil(pnadc_receita_final$renda_pos_novo,    pnadc_receita_final$peso_comcalib, "Nova Proposta")
df_aliMax  <- apropriacao_por_centil(pnadc_receita_final$renda_pos_aliMax,  pnadc_receita_final$peso_comcalib, "Nova c/ Aliq. Máxima")


df_aprop_acumulada <- df_aprop %>%
  group_by(Cenário) %>%
  arrange(centil) %>%
  mutate(prop_renda_acumulada = cumsum(prop_renda)) %>%
  ungroup()

ggplot(df_aprop_acumulada, aes(x = centil, y = prop_renda_acumulada, color = Cenário)) +
  geom_line(linewidth = 0.4) +  # <- mais fino aqui
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_color_manual(values = c(
    "Regime Atual" = "#3366ff",
    "Nova Proposta" = "#eb52ff",
    "Nova c/ Aliq. Máxima" = "#feff41"
  )) +
  labs(
    title = "Apropriação Acumulada da Renda por Centil",
    x = "Centil de Renda (ordem crescente)",
    y = "Apropriação Acumulada (%)",
    color = "Cenário"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

