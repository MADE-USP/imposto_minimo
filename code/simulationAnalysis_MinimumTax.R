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
library(writexl)
library(readxl)
library(forcats)
setwd(this.dir())

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

# 3 - Aplica IR Mensal (Nova Proposta) ----------------------------------

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

pnadc_receita_final <- pnadc_receita_final %>%
  mutate(irpf_mensal_antigo = map_dbl(base_c, calcula_irpf_mensal_antigo),
         irpf_mensal_novo = map2_dbl(base_c, `Rendimento Tributável`, calcula_irpf_mensal_novo),
         base_tax = imposto_withholding + irpf_mensal_novo)

# ---------------------- Hipótese A: sem dedução de 20% sobre lucros e dividendos ----------------------

pnadc_hipotese_A <- pnadc_receita_final %>%
  mutate(renda_base = renda_irpfepnad - `Rendimentos Recebidos Acumuladamente` -
           `Ganhos de Capital na Alienação de Bens/Direitos` -
           `Rendimentos de Caderneta de Poupança etc` -
           `Indenização por Rescisão do Contrato de Trabalho etc`)

# Define a função imposto_final no escopo global
imposto_final <- function(base_tax, renda) {
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

# Recalcula imposto final e renda pós-tributação
pnadc_hipotese_A <- pnadc_hipotese_A %>%
  mutate(imposto_calculado = pmap_dbl(list(base_tax, renda_base), imposto_final),
         renda_pos_novo = renda_base - imposto_calculado,
         renda_pos_atual = renda_base - (imposto_withholding + irpf_mensal_antigo))

# Calcula indicadores fiscais para ambas as hipóteses
irpf_total_atual_A <- 12 * sum(pnadc_hipotese_A$peso_comcalib * (pnadc_hipotese_A$irpf_mensal_antigo + pnadc_hipotese_A$imposto_withholding), na.rm = TRUE) / 1e9
custo_isencao_mensal_A <- 12 * sum(pnadc_hipotese_A$peso_comcalib * (pnadc_hipotese_A$irpf_mensal_antigo - pnadc_hipotese_A$irpf_mensal_novo), na.rm = TRUE) / 1e9
irpf_total_novo_A <- 12 * sum(pnadc_hipotese_A$peso_comcalib * pnadc_hipotese_A$imposto_calculado, na.rm = TRUE) / 1e9

# Hipótese B: com redução de 20% sobre lucros e dividendos
pnadc_hipotese_B <- pnadc_receita_final %>%
  mutate(renda_base = renda_irpfepnad - 0.2 * `Lucros e Dividendos` -
           `Rendimentos Recebidos Acumuladamente` -
           `Ganhos de Capital na Alienação de Bens/Direitos` -
           `Rendimentos de Caderneta de Poupança etc` -
           `Indenização por Rescisão do Contrato de Trabalho etc`)

pnadc_hipotese_B <- pnadc_hipotese_B %>%
  mutate(imposto_calculado = pmap_dbl(list(base_tax, renda_base), imposto_final),
         renda_pos_novo = renda_base - imposto_calculado,
         renda_pos_atual = renda_base - (imposto_withholding + irpf_mensal_antigo))

irpf_total_atual_B <- 12 * sum(pnadc_hipotese_B$peso_comcalib * (pnadc_hipotese_B$irpf_mensal_antigo + pnadc_hipotese_B$imposto_withholding), na.rm = TRUE) / 1e9
custo_isencao_mensal_B <- 12 * sum(pnadc_hipotese_B$peso_comcalib * (pnadc_hipotese_B$irpf_mensal_antigo - pnadc_hipotese_B$irpf_mensal_novo), na.rm = TRUE) / 1e9
irpf_total_novo_B <- 12 * sum(pnadc_hipotese_B$peso_comcalib * pnadc_hipotese_B$imposto_calculado, na.rm = TRUE) / 1e9

# Monta data frame comparativo
resultados_comparativos <- tibble::tibble(
  Indicador = c("IRPF Total Atual (bi R$)", "Custo Isenção Mensal (bi R$)", "IRPF Total Novo (bi R$)"),
  Hipotese_A = c(irpf_total_atual_A, custo_isencao_mensal_A, irpf_total_novo_A),
  Hipotese_B = c(irpf_total_atual_B, custo_isencao_mensal_B, irpf_total_novo_B)
)

# Exporta para Excel
writexl::write_xlsx(resultados_comparativos, "tabela_irpf_comparativo.xlsx")




# 5 - Estatísticas Distributivas ------------------------------------------
## GINI - NOVO ###
source('utils/IneqFunctions.R')
Bottom_Aprop(pnadc_hipotese_A$renda_pos_novo, pnadc_hipotese_A$peso_comcalib, 50)
Top_Aprop(pnadc_hipotese_A$renda_pos_novo, pnadc_hipotese_A$peso_comcalib, 100)
Top_Aprop(pnadc_hipotese_A$renda_pos_novo, pnadc_hipotese_A$peso_comcalib, 91)
StatsGini(pnadc_hipotese_A$renda_pos_novo, pnadc_hipotese_A$peso_comcalib)

## GINI - ANTIGO ###
Bottom_Aprop(pnadc_hipotese_A$renda_pos_atual, pnadc_hipotese_A$peso_comcalib, 50)
Top_Aprop(pnadc_hipotese_A$renda_pos_atual, pnadc_hipotese_A$peso_comcalib, 100)
Top_Aprop(pnadc_hipotese_A$renda_pos_atual, pnadc_hipotese_A$peso_comcalib, 91)
StatsGini(pnadc_hipotese_A$renda_pos_atual, pnadc_hipotese_A$peso_comcalib)

pnadc_receita_final <- pnadc_hipotese_A

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



##### Abrindo ultimo decil:

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
    Nova_Proposta = sum(imposto_calculado * peso_comcalib) / sum(renda_base * peso_comcalib) * 100
  )


df_long <- pnadc_receita_agg %>%
  pivot_longer(cols = c(Regime_Atual, Nova_Proposta),
               names_to = "Regime",
               values_to = "Aliquota_Efetiva") %>%
  mutate(Regime = recode(Regime,
                         "Regime_Atual" = "Regime Atual",
                         "Nova_Proposta" = "Nova Proposta")) %>%
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
  scale_color_manual(values = c("Regime Atual" = "#3366ff", "Nova Proposta" = "#eb52ff")) +
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
print(p)

ggsave("../figures/grafico_aliquota.png", plot = p, width = 10, height = 6, dpi = 300)



# 7 - Simulação - Alíquota Máxima -----------------------------------------

aliMax <- max(pnadc_receita_agg$Nova_Proposta)/100

pnadc_receita_final <- pnadc_receita_final %>% mutate(imposto_ali_max = if_else(divisao_renda %in% c("100.10", "100.9", "100.8", "100.1-100.7"), 
                                                                                aliMax*renda_base, imposto_calculado))

graphAliMax <- pnadc_receita_final %>% group_by(divisao_renda) %>%  
  summarise(Regime_Atual = sum((imposto_withholding+irpf_mensal_antigo)*peso_comcalib)/sum(renda_base*peso_comcalib)*100,
            Nova_Proposta = sum(imposto_calculado*peso_comcalib)/sum(renda_base*peso_comcalib)*100,
            Proposta_Aliquota_Maxima = sum(imposto_ali_max*peso_comcalib)/sum(renda_base*peso_comcalib)*100)
pnadc_receita_final <- pnadc_receita_final %>% mutate('renda_pos_aliMax' = renda_base - imposto_ali_max)

# Converte para formato longo e renomeia os regimes
df_long <- graphAliMax %>%
  pivot_longer(cols = c(Regime_Atual, Nova_Proposta, Proposta_Aliquota_Maxima),
               names_to = "Regime",
               values_to = "Aliquota_Efetiva") %>%
  mutate(Regime = case_when(
    Regime == "Regime_Atual" ~ "Regime Atual",
    Regime == "Nova_Proposta" ~ "Nova Proposta",
    Regime == "Proposta_Aliquota_Maxima" ~ "Proposta Progressiva",
    TRUE ~ Regime
  )) %>%
  distinct() %>%
  filter(Aliquota_Efetiva >= 0)

# Ordena o eixo x
quantis_numeric <- suppressWarnings(as.numeric(df_long$divisao_renda))
ordem_quantis   <- sort(unique(quantis_numeric[!is.na(quantis_numeric)]))
# Cria vetor com os quantis de 76 a 99
ordem_quantis <- 76:99

# Define os níveis finais manuais
extremos <- c("100.1-100.7", "100.8", "100.9", "100.10")

# Combina a ordem correta do eixo x
ordem_x <- c(as.character(ordem_quantis), extremos)

# Aplica a ordenação à variável
df_long$divisao_renda <- factor(df_long$divisao_renda, levels = ordem_x)
df_long <- df_long %>% filter(!divisao_renda %in% as.character(1:75))
df_long <- df_long %>% filter(!is.na(divisao_renda))
# Plot
pAliMax <- ggplot(df_long, aes(x = divisao_renda, y = Aliquota_Efetiva,
                               color = Regime, group = Regime)) +
  geom_line(linewidth = 1.25) +
  theme_bw() +
  scale_color_manual(values = c(
    "Regime Atual" = "#3366ff",
    "Nova Proposta" = "#eb52ff",
    "Proposta Progressiva" = "#FFC107"  # amarelo forte
  )) +
  xlab("Posição na Distribuição de Renda") +
  ylab("Alíquota Efetiva (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(pAliMax)


ggsave("../figures/grafico_proposta.png", plot = pAliMax, width = 10, height = 6, dpi = 300)


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

irpf_total_novo_aliMax <- 1 * 12 * sum(pnadc_receita_final$peso_comcalib * pnadc_receita_final$imposto_ali_max_novo, na.rm = TRUE) / 1e9

# Gini:

# Recalcula arrecadações com base nas suas fórmulas
irpf_total_atual <- 1 * 12 * sum(pnadc_receita_final$peso_comcalib *
                                   (pnadc_receita_final$irpf_mensal_antigo +
                                      pnadc_receita_final$imposto_withholding)) / 1e9

irpf_total_novo <- 1 * 12 * sum(pnadc_receita_final$peso_comcalib *
                                  pnadc_receita_final$imposto_calculado) / 1e9

irpf_total_aliMax <- 1 * 12 * sum(pnadc_receita_final$peso_comcalib *
                                    pnadc_receita_final$imposto_ali_max_novo) / 1e9

# Diferenças em relação ao atual
dif_arrec_novo <- irpf_total_novo - irpf_total_atual
dif_arrec_aliMax <- irpf_total_aliMax - irpf_total_atual

# Estatísticas distributivas
## Regime atual
gini_atual <- StatsGini(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib)
bottom50_atual <- Bottom_Aprop(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib, 50)
top10_atual <- Top_Aprop(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib, 91)
top1_atual <- Top_Aprop(pnadc_receita_final$renda_pos_atual, pnadc_receita_final$peso_comcalib, 100)

## Nova proposta original
gini_novo <- StatsGini(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib)
bottom50_novo <- Bottom_Aprop(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib, 50)
top10_novo <- Top_Aprop(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib, 91)
top1_novo <- Top_Aprop(pnadc_receita_final$renda_pos_novo, pnadc_receita_final$peso_comcalib, 100)

## Nova com alíquota máxima estendida
gini_aliMax <- StatsGini(pnadc_receita_final$renda_pos_aliMax, pnadc_receita_final$peso_comcalib)
bottom50_aliMax <- Bottom_Aprop(pnadc_receita_final$renda_pos_aliMax, pnadc_receita_final$peso_comcalib, 50)
top10_aliMax <- Top_Aprop(pnadc_receita_final$renda_pos_aliMax, pnadc_receita_final$peso_comcalib, 91)
top1_aliMax <- Top_Aprop(pnadc_receita_final$renda_pos_aliMax, pnadc_receita_final$peso_comcalib, 100)

# Tabela final
tabela_resultados <- data.frame(
  Cenário = c("Regime Atual", "Nova Proposta", "Nova c/ Aliq. Máxima"),
  Gini = c(gini_atual, gini_novo, gini_aliMax),
  Bottom_50 = c(bottom50_atual, bottom50_novo, bottom50_aliMax),
  Top_10 = c(top10_atual, top10_novo, top10_aliMax),
  Top_1 = c(top1_atual, top1_novo, top1_aliMax),
  Arrecadacao_BR = c(irpf_total_atual, irpf_total_novo, irpf_total_aliMax),
  Dif_Arrecadacao_BR = c(0, dif_arrec_novo, dif_arrec_aliMax)
)

# Exibe
print(tabela_resultados)

# Salva como CSV
write.csv(tabela_resultados, "../tables/resultados_distributivos_com_arrecadacao.csv", row.names = FALSE)
write_xlsx(tabela_resultados, "../tables/resultados_distributivos_com_arrecadacao.xlsx")

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
write_xlsx(tabela_percentis_ext, "../tables/indicadores_percentis_renda_extendido.xlsx")

# Grafico com apropriacao:

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
    "Nova c/ Aliq. Máxima" = "#FFC107" 
  )) +
  labs(
    x = "Centil de Renda",
    y = "Proporção da Renda Total (%)",
    color = "Cenário"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("../figures/apropriacao_renda_por_centil.png", width = 8, height = 5, dpi = 300)



# Filtra a partir do centil 85
df_aprop_filtrado <- df_aprop %>% filter(centil >= 85)

# Gráfico
ggplot(df_aprop_filtrado, aes(x = centil, y = prop_renda, color = Cenário)) +
  geom_line(linewidth = 0.4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_color_manual(values = c(
    "Regime Atual" = "#3366ff",
    "Nova Proposta" = "#eb52ff",
    "Nova c/ Aliq. Máxima" = "#FFC107"
  )) +
  labs(
    title = "Apropriação da Renda a partir do Centil 85",
    x = "Centil de Renda",
    y = "Proporção da Renda Total (%)",
    color = "Cenário"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# Salva a figura
ggsave("../figures/apropriacao_renda_top15.png", width = 8, height = 5, dpi = 300)




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

ggplot(df_aprop_acumulada %>% filter(Cenário != "Regime Atual"), aes(x = centil, y = prop_renda_acumulada, color = Cenário)) +
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



# WID dados:

# Lê a base
wil <- read_excel("../data/wil.xlsx")

# Renomeia coluna para Apropriação
df_filtrado <- wil %>%
  rename(País = 1, Apropriação = 2)

# Lista final de países a manter no gráfico
paises_selecionados <- c(
  "Brasil", "Brasil Pos-Reforma",
  "USA", "Germany", "France", "United Kingdom", "Italy", "Spain",
  "Netherlands", "Sweden", "Norway", "Denmark",
  "Japan", "South Korea", "New Zealand", "Portugal", "Poland",
  "Chile", "Colombia", "Uruguay", "Peru", "Bolivia", "Paraguay", "Guatemala",
  "South Africa", "China", "Indonesia", "Pakistan", "Bangladesh",
  "Sri Lanka", "Nepal", "Russia", "Kazakhstan", "Iran", "Egypt",
  "Kenya", "Nigeria", "Ghana", "Uganda", "Mozambique"
)

# Filtra países
df_filtrado <- df_filtrado %>%
  filter(País %in% paises_selecionados)

# Define países desenvolvidos
desenvolvidos <- c(
  "USA", "Germany", "France", "United Kingdom", "Italy", "Spain",
  "Netherlands", "Sweden", "Norway", "Denmark",
  "Japan", "South Korea", "New Zealand", "Portugal", "Poland"
)

# Classificação final
df_filtrado <- df_filtrado %>%
  mutate(
    grupo_renda = case_when(
      País == "Brasil" ~ "Brasil",
      País == "Brasil Pos-Reforma" ~ "Brasil Pós-Reforma",
      País %in% desenvolvidos ~ "Países desenvolvidos",
      TRUE ~ "Países em desenvolvimento"
    )
  )

# Ordena os países por Apropriação
df_filtrado <- df_filtrado %>%
  arrange(desc(Apropriação)) %>%
  mutate(País = factor(País, levels = unique(País)))

# Cores
cores <- c(
  "Brasil" = "#3366ff",
  "Brasil Pós-Reforma" = "#eb52ff",
  "Países desenvolvidos" = "grey80",
  "Países em desenvolvimento" = "grey50"
)

# Gráfico
grafico_apropriacao <- ggplot(df_filtrado, aes(x = Apropriação, y = País, fill = grupo_renda)) +
  geom_col() +
  scale_fill_manual(
    values = cores,
    name = "Grupo de País"
  ) +
  xlab("Apropriação pelo Topo 1% (%)") +
  ylab(NULL) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    legend.position = "bottom"
  )

# Salva como PNG
ggsave("../figures/grafico_apropriacao_WID.png", plot = grafico_apropriacao, width = 10, height = 8, dpi = 300)