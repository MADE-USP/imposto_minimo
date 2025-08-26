gc()
rm(list=ls())

# 0 - Imports e Carregamento da Base --------------------------------------

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
source('utils/IneqFunctions.R')

cores_made <- c("#45ff66", "#eb52ff", "#3366ff", "#feff41")
load('../data/baseRendimentosIsentosPlrAdj.Rda')
# 0 - Ajustando os valores da base pela inflação ------------------------------

inflaciona <- function(var) {
  (var * 1.16)
}

pnadc_receita_final <- pnadc_receita_final %>%
  mutate(across(.cols = c(rendimento_todasfontes, rendimento_todasfontes_calibrado, `Renda Total`:`Dívida`), .fns = inflaciona))

# 1 - Parâmetros de Faixas, Alíquotas e Despesas Dedutíveis ---------------------

faixa1 <- 2428
faixa2 <- 2826.65
faixa3 <- 3751.05
faixa4 <- 4664.68

aliquota1 <- 0.075
aliquota2 <- 0.15
aliquota3 <- 0.225
aliquota4 <- 0.275

despesas_dedutiveis <- c(
  "Previdência Oficial", "Previdência RRA", "Previdência Privada",
  "Dependentes", "Instrução", "Despesas Médicas", "Pensão Alimentícia", "Livro-Caixa"
)

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
  lower = c(0, 6677.56, 9922.29, 13167.01, 16380.38),
  upper = c(6677.55, 9922.28, 13167, 16380.38, Inf),
  rate = c(0, 0.075, 0.15, 0.225, 0.275)
)

tax_table <- data.frame(
  lower = c(0, faixa1, faixa2, faixa3, faixa4),
  upper = c(faixa1, faixa2, faixa3, faixa4, Inf),
  rate = c(0, 0.075, 0.15, 0.225, 0.275)
)

calculate_tax <- function(bruto, tax_table) {
  tax <- 0
  for (i in 1:nrow(tax_table)) {
    lower <- tax_table$lower[i]
    upper <- tax_table$upper[i]
    rate <- tax_table$rate[i]
    if (bruto > lower) {
      taxable_income <- min(bruto, upper) - lower
      tax <- tax + taxable_income * rate
    }
  }
  return(tax)
}

find_bruto <- function(liquido, tax_table, lower_bound = 0, upper_bound = 1e9) {
  f <- function(bruto) {
    bruto - calculate_tax(bruto, tax_table) - liquido
  }
  result <- uniroot(f, lower = lower_bound, upper = upper_bound)
  return(result$root)
}

# Preenche NAs antes de qualquer cálculo
pnadc_receita_final <- pnadc_receita_final %>%
  mutate(across(c(`13º salário`, `Rendimentos Recebidos Acumuladamente`,
                  `Ganhos de Capital na Alienação de Bens/Direitos`,
                  `Ganhos Líquidos em Renda Variável`, `Juros sobre Capital Próprio`,
                  `Outros`, `Rendimentos de Caderneta de Poupança etc`,
                  `Indenização por Rescisão do Contrato de Trabalho etc`,
                  `Lucros e Dividendos`), ~replace_na(., 0)))

# Calcula impostos intermediários
pnadc_receita_final <- pnadc_receita_final %>%
  mutate(
    imposto_13 = mapply(find_bruto, `13º salário`, MoreArgs = list(tax_table = tax_table)) - `13º salário`,
    imposto_rra = mapply(find_bruto, `Rendimentos Recebidos Acumuladamente`, MoreArgs = list(tax_table = tax_table)) - `Rendimentos Recebidos Acumuladamente`,
    plr_distribuido = plr_distribuido * 12,
    imposto_plr = mapply(find_bruto, plr_distribuido, MoreArgs = list(tax_table = tax_table_plr)) - plr_distribuido,
    imposto_plr = imposto_plr / 12,
    capital = `Rendimentos de Aplicações Financeiras` + `Ganhos Líquidos em Renda Variável` + `Juros sobre Capital Próprio` + Outros,
    imposto_capital = (capital / (1 - 0.15) - capital),
    renda_irpfepnad = coalesce(rendimento_todasfontes_calibrado, rendimento_todasfontes),
    imposto_withholding = replace_na(imposto_capital + imposto_plr, 0)
  )

# 3 - Aplica IR Mensal  ----------------------------------

pnadc_receita_final <- pnadc_receita_final %>%
  mutate(across(all_of(despesas_dedutiveis), ~ replace_na(., 0)),
         despesas_dedutiveis_tot = rowSums(across(all_of(despesas_dedutiveis))),
         `Rendimento Tributável` = replace_na(`Rendimento Tributável`, 0),
         base_c_hip = pmax(pmin((`Rendimento Tributável` - despesas_dedutiveis_tot), 0.8 * `Rendimento Tributável`), 0),
         base_c_rb4 = coalesce(`Base de Cálculo`, 0),
         base_c_rb8 = coalesce(RB8, 0),
         base_c = if_else(base_c_rb8 <= 5000, base_c_rb8, base_c_hip))

calcula_irpf_mensal_novo <- function(renda, rendimento_tributavel) {
  if (renda <= 5000) {
    0
  } else if (renda <= 7000) {
    reducao <- 1095.11 - 0.156445 * rendimento_tributavel
    max(calcula_irpf_mensal_antigo(renda) - reducao, 0)
  } else {
    calcula_irpf_mensal_antigo(renda)
  }
}

calcula_irpf_mensal_lira <- function(renda, rendimento_tributavel) {
  if (renda <= 5000) {
    0
  } else if (renda <= 7350) {
    reducao_lira <- pmax(978.62 - 0.133145 * rendimento_tributavel, 0)
    max(calcula_irpf_mensal_antigo(renda) - reducao_lira, 0)
  } else {
    calcula_irpf_mensal_antigo(renda)
  }
}

pnadc_receita_final <- pnadc_receita_final %>%
  mutate(irpf_mensal_antigo = map_dbl(base_c, calcula_irpf_mensal_antigo),
         irpf_mensal_novo = map2_dbl(base_c, `Rendimento Tributável`, calcula_irpf_mensal_novo),
         base_tax = imposto_withholding + irpf_mensal_novo)


pnadc_receita_final <- pnadc_receita_final %>%
  mutate(irpf_mensal_antigo = map_dbl(base_c, calcula_irpf_mensal_antigo),
         irpf_mensal_lira = map2_dbl(base_c, `Rendimento Tributável`, calcula_irpf_mensal_lira),
         base_tax_lira = imposto_withholding + irpf_mensal_lira)



#imposto topo

imposto_final_p10 <- function(base_tax, renda) {
  if (is.na(base_tax) || is.na(renda)) {
    return(NA_real_)
  } else if (renda <= 50000) {
    return(base_tax)
  } else if (renda < 100000) {
    desired_tax <- ((((renda * 12 / 60000 - 10)))/100) * renda
    return(max(base_tax, desired_tax))
  } else {
    desired_tax <- (0.10) * renda
    return(max(base_tax, desired_tax))
  }
}

imposto_final_p10_lira <- function(base_tax_lira, renda) {
  if (is.na(base_tax_lira) || is.na(renda)) {
    return(NA_real_)
  } else if (renda <= 50000) {
    return(base_tax_lira)
  } else if (renda < 100000) {
    desired_tax <- ((((renda * 12 / 60000 - 10)))/100) * renda
    return(max(base_tax_lira, desired_tax))
  } else {
    desired_tax <- (0.10) * renda
    return(max(base_tax_lira, desired_tax))
  }
}

# Hipótese com redução de 50% sobre lucros e dividendos
pnadc_receita_final <- pnadc_receita_final %>%
  mutate(renda_base = renda_irpfepnad -
           `Rendimentos Recebidos Acumuladamente` -
           `Ganhos de Capital na Alienação de Bens/Direitos` -
           `Rendimentos de Caderneta de Poupança etc` -
           `Indenização por Rescisão do Contrato de Trabalho etc`)


pnadc_receita_final <- pnadc_receita_final %>%
  mutate(imposto_calculado = pmap_dbl(list(base_tax, renda_base), imposto_final_p10),
         renda_pos_novo = renda_base - imposto_calculado,
         renda_pos_atual = renda_base - (imposto_withholding + irpf_mensal_antigo))

pnadc_receita_final <- pnadc_receita_final %>%
  mutate(imposto_calculado_lira = pmap_dbl(list(base_tax_lira, renda_base), imposto_final_p10_lira),
         renda_pos_novo_lira = renda_base - imposto_calculado_lira,
         renda_pos_atual = renda_base - (imposto_withholding + irpf_mensal_antigo))

irpf_total_atual_B <- 12 * sum(pnadc_receita_final$peso_comcalib * (pnadc_receita_final$irpf_mensal_antigo + pnadc_receita_final$imposto_withholding), na.rm = TRUE) / 1e9
custo_isencao_mensal_B <- 12 * sum(pnadc_receita_final$peso_comcalib * (pnadc_receita_final$irpf_mensal_antigo - pnadc_receita_final$irpf_mensal_novo), na.rm = TRUE) / 1e9
irpf_total_novo_B <- 12 * sum(pnadc_receita_final$peso_comcalib * pnadc_receita_final$imposto_calculado, na.rm = TRUE) / 1e9
irpf_total_novo_B_lira <- 12 * sum(pnadc_receita_final$peso_comcalib * pnadc_receita_final$imposto_calculado_lira, na.rm = TRUE) / 1e9


# Gráfico - Aliquotas Efetivas --------------------------------------------

# Calcula centis
pnadc_receita_final$quantis <- weighted_ntile(pnadc_receita_final$renda_base,
                                              pnadc_receita_final$peso_comcalib, 100)

# Cria subdecis dentro do centil 100
pnadc_receita_final <- pnadc_receita_final %>%
  group_by(subgrupo = quantis == 100) %>%
  mutate(subdecis_ultimo = if_else(subgrupo,
                                   weighted_ntile(renda_base, peso_comcalib, 10),
                                   NA_integer_)) %>%
  ungroup()

# Cria a variável de divisão de renda com agrupamento desejado
pnadc_receita_final <- pnadc_receita_final %>%
  mutate(
    divisao_renda = case_when(
      quantis < 100 ~ as.character(quantis),
      quantis == 100 & !is.na(subdecis_ultimo) & subdecis_ultimo <= 7 ~ "100.1-100.7",
      quantis == 100 & subdecis_ultimo == 8 ~ "100.8",
      quantis == 100 & subdecis_ultimo == 9 ~ "100.9",
      quantis == 100 & subdecis_ultimo == 10 ~ "100.10",
      TRUE ~ NA_character_  # Isso evita que entre NA onde nada for definido
    )
  )

pnadc_receita_agg <- pnadc_receita_final %>%
  group_by(divisao_renda) %>%
  summarise(
    Regime_Atual = sum((imposto_withholding + irpf_mensal_antigo) * peso_comcalib) / sum(renda_base * peso_comcalib) * 100,
    Proposta_atual = sum(imposto_calculado * peso_comcalib) / sum(renda_base * peso_comcalib) * 100,
    Proposta_lira = sum(imposto_calculado_lira * peso_comcalib) / sum(renda_base * peso_comcalib) * 100,
  )


df_long <- pnadc_receita_agg %>%
  pivot_longer(cols = c(Regime_Atual, Proposta_atual, Proposta_lira),
               names_to = "Regime",
               values_to = "Aliquota_Efetiva") %>%
  mutate(Regime = recode(Regime,
                         "Regime_Atual" = "Regime Atual",
                         "Proposta_atual" = "PL 1.087",
                         "Proposta_lira" = "Substitutivo")) %>%
  distinct() %>%
  filter(Aliquota_Efetiva >= 0)


# Ordenar eixo x
ordem_x <- c(as.character(76:99), "100.1-100.7", "100.8", "100.9", "100.10")

df_long <- df_long %>%
  filter(divisao_renda %in% ordem_x)

df_long$divisao_renda <- factor(df_long$divisao_renda, levels = ordem_x)


# Adicionando marcacoes no grafico
# Define os limites
limite_100k <- 100000 / 1
limite_50k  <- 50000 / 1

# Identifica os grupos (divisao_renda) que contêm esses limites
marcadores <- pnadc_receita_final %>%
  filter(renda_base >= limite_50k) %>%
  group_by(divisao_renda) %>%
  summarise() %>%
  pull(divisao_renda)

ponto_50k <- pnadc_receita_final %>%
  filter(renda_base >= limite_50k) %>%
  arrange(renda_base) %>%
  slice(1) %>%
  pull(divisao_renda)

ponto_100k <- pnadc_receita_final %>%
  filter(renda_base >= limite_100k) %>%
  arrange(renda_base) %>%
  slice(1) %>%
  pull(divisao_renda)


p <- ggplot(df_long, aes(x = divisao_renda, y = Aliquota_Efetiva,
                         color = Regime, group = Regime)) +
  geom_line(linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = c("Regime Atual" = "#3366ff", "PL 1.087" = "#FF746C", "Substitutivo"="#45ff66")) +
  xlab("Posição na Distribuição de Renda") +
  ylab("Alíquota Efetiva (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  geom_vline(xintercept = which(levels(df_long$divisao_renda) == ponto_50k),
             linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = which(levels(df_long$divisao_renda) == ponto_100k),
             linetype = "dashed", color = "red") +
  annotate("text", x = which(levels(df_long$divisao_renda) == ponto_50k),
           y = 5, label = "R$ 50 mil", vjust = 0, color = "darkgreen",
           angle = 90, size = 4) +
  annotate("text", x = which(levels(df_long$divisao_renda) == ponto_100k),
           y = 5, label = "R$ 100 mil", vjust = 0, color = "red",
           angle = 90, size = 4)







# Gráfico - Aliquotas Efetivas --------------------------------------------

# Calcula centis
pnadc_receita_final$quantis <- weighted_ntile(pnadc_receita_final$renda_base,
                                              pnadc_receita_final$peso_comcalib, 100)

# Cria subdecis dentro do centil 100
pnadc_receita_final <- pnadc_receita_final %>%
  group_by(subgrupo = quantis == 100) %>%
  mutate(subdecis_ultimo = if_else(subgrupo,
                                   weighted_ntile(renda_base, peso_comcalib, 10),
                                   NA_integer_)) %>%
  ungroup()

# Cria a variável de divisão de renda com agrupamento desejado
pnadc_receita_final <- pnadc_receita_final %>%
  mutate(
    divisao_renda = case_when(
      quantis < 100 ~ as.character(quantis),
      quantis == 100 & !is.na(subdecis_ultimo) & subdecis_ultimo <= 7 ~ "100.1-100.7",
      quantis == 100 & subdecis_ultimo == 8 ~ "100.8",
      quantis == 100 & subdecis_ultimo == 9 ~ "100.9",
      quantis == 100 & subdecis_ultimo == 10 ~ "100.10",
      TRUE ~ NA_character_  # Isso evita que entre NA onde nada for definido
    )
  )

pnadc_receita_agg <- pnadc_receita_final %>%
  group_by(divisao_renda) %>%
  summarise(
    Regime_Atual = sum((imposto_withholding + irpf_mensal_antigo) * peso_comcalib) / sum(renda_base * peso_comcalib) * 100,
    Proposta_p10 = sum(imposto_calculado * peso_comcalib) / sum(renda_base * peso_comcalib) * 100,
    Proposta_lira = sum(imposto_calculado_lira * peso_comcalib) / sum(renda_base * peso_comcalib) * 100,
  )


df_long <- pnadc_receita_agg %>%
  pivot_longer(cols = c(Regime_Atual, Proposta_p10, Proposta_lira),
               names_to = "Regime",
               values_to = "Aliquota_Efetiva") %>%
  mutate(Regime = recode(Regime,
                         "Regime_Atual" = "Regime Atual",
                         "Proposta_p10" = "PL 1.087",
                         "Proposta_lira" = "Substitutivo")) %>%
  distinct() %>%
  filter(Aliquota_Efetiva >= 0)

# Ordenar eixo x
ordem_x <- c(as.character(76:99), "100.1-100.7", "100.8", "100.9", "100.10")
df_long <- df_long %>%
  filter(divisao_renda %in% ordem_x)

df_long$divisao_renda <- factor(df_long$divisao_renda, levels = ordem_x)


# Adicionando marcacoes no grafico
# Define os limites
limite_100k <- 100000 / 1
limite_50k  <- 50000 / 1

# Identifica os grupos (divisao_renda) que contêm esses limites
marcadores <- pnadc_receita_final %>%
  filter(renda_base >= limite_50k) %>%
  group_by(divisao_renda) %>%
  summarise() %>%
  pull(divisao_renda)

ponto_50k <- pnadc_receita_final %>%
  filter(renda_base >= limite_50k) %>%
  arrange(renda_base) %>%
  slice(1) %>%
  pull(divisao_renda)

ponto_100k <- pnadc_receita_final %>%
  filter(renda_base >= limite_100k) %>%
  arrange(renda_base) %>%
  slice(1) %>%
  pull(divisao_renda)


p <- ggplot(df_long, aes(x = divisao_renda, y = Aliquota_Efetiva,
                         color = Regime, group = Regime)) +
  geom_line(linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = c("Regime Atual" = "#3366ff", "PL 1.087" = "#FF746C", "Substitutivo"="#45ff66")) +
  xlab("Posição na Distribuição de Renda") +
  ylab("Alíquota Efetiva (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  geom_vline(xintercept = which(levels(df_long$divisao_renda) == ponto_50k),
             linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = which(levels(df_long$divisao_renda) == ponto_100k),
             linetype = "dashed", color = "red") +
  annotate("text", x = which(levels(df_long$divisao_renda) == ponto_50k),
           y = 5, label = "R$ 50 mil", vjust = 0, color = "darkgreen",
           angle = 90, size = 4) +
  annotate("text", x = which(levels(df_long$divisao_renda) == ponto_100k),
           y = 5, label = "R$ 100 mil", vjust = 0, color = "red",
           angle = 90, size = 4)

# Tabela - Estatísticas Distributivas -------------------------------------


# Estatísticas distributivas
## Regime atual
gini_atual <- StatsGini(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib)
bottom50_atual <- Bottom_Aprop(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib, 50)
top10_atual <- Top_Aprop(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib, 91)
top1_atual <- Top_Aprop(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib, 100)

## PL1087 de IR Mínimo
gini_p10 <- StatsGini(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib)
bottom50_p10 <- Bottom_Aprop(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib, 50)
top10_p10 <- Top_Aprop(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib, 91)
top1_p10 <- Top_Aprop(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib, 100)

## Substitutivo
gini_lucros <- StatsGini(pnadc_receita_final$renda_pos_novo_lira, pnadc_receita_final$peso_comcalib)
bottom50_lucros <- Bottom_Aprop(pnadc_receita_final$renda_pos_novo_lira, pnadc_receita_final$peso_comcalib, 50)
top10_lucros <- Top_Aprop(pnadc_receita_final$renda_pos_novo_lira, pnadc_receita_final$peso_comcalib, 91)
top1_lucros <- Top_Aprop(pnadc_receita_final$renda_pos_novo_lira, pnadc_receita_final$peso_comcalib, 100)


# Tabela com resultados
tabela_resultados <- data.frame(
  Cenário = c("Regime Atual", "PL 1.087","Substitutivo"),
  Gini = c(gini_atual, gini_p10, gini_lucros),
  Bottom_50 = c(bottom50_atual, bottom50_p10, bottom50_lucros),
  Top_10 = c(top10_atual, top10_p10, top10_lucros),
  Top_1 = c(top1_atual, top1_p10, top1_lucros),
  Arrecadacao_BR = c(irpf_total_atual_B, irpf_total_novo_B, irpf_total_novo_B_lira)
  )


# Salvando os Resultados --------------------------------------------------

write.csv(tabela_resultados, "/Users/klein/Documents/GitHub/imposto_minimo/tables/resultados_propostas_lira_srled.csv", row.names = FALSE)
write_xlsx(tabela_resultados, "/Users/klein/Documents/GitHub/imposto_minimo/tables/resultados_propostas_lira_srled.xlsx")
ggsave("/Users/klein/Documents/GitHub/imposto_minimo/figures/grafico_propostas_lira_srled.png",
       plot = p, width = 10, height = 6, dpi = 300)