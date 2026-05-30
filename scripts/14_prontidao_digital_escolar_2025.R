library(tidyverse)

# ------------------------------------------------
# Leitura da base
# ------------------------------------------------

escolas <- read_delim(
  "dados/microdados_censo_escolar_2025/dados/Tabela_Escola_2025.csv",
  delim = ";",
  show_col_types = FALSE
)

# ------------------------------------------------
# Índice de Prontidão Digital Escolar
# ------------------------------------------------

prontidao <- escolas %>%
  mutate(
    
    energia = replace_na(IN_ENERGIA_REDE_PUBLICA, 0),
    internet = replace_na(IN_INTERNET, 0),
    banda_larga = replace_na(IN_BANDA_LARGA, 0),
    internet_aprendizagem = replace_na(IN_INTERNET_APRENDIZAGEM, 0),
    laboratorio = replace_na(IN_LABORATORIO_INFORMATICA, 0),
    desktop = replace_na(IN_DESKTOP_ALUNO, 0),
    notebook = replace_na(IN_COMP_PORTATIL_ALUNO, 0),
    tablet = replace_na(IN_TABLET_ALUNO, 0),
    
    indice_prontidao =
      energia +
      internet +
      banda_larga +
      internet_aprendizagem +
      laboratorio +
      desktop +
      notebook +
      tablet,
    
    nivel_prontidao = case_when(
      indice_prontidao <= 2 ~ "Muito baixa",
      indice_prontidao <= 4 ~ "Baixa",
      indice_prontidao <= 6 ~ "Média",
      indice_prontidao <= 7 ~ "Alta",
      indice_prontidao == 8 ~ "Muito alta"
    )
  )

# ------------------------------------------------
# Distribuição nacional
# ------------------------------------------------

resumo_prontidao <- prontidao %>%
  count(nivel_prontidao) %>%
  mutate(
    percentual = round(
      n / sum(n) * 100,
      1
    )
  )

# ------------------------------------------------
# Distribuição por região
# ------------------------------------------------

prontidao_regiao <- prontidao %>%
  group_by(NO_REGIAO) %>%
  summarise(
    indice_medio = round(
      mean(indice_prontidao, na.rm = TRUE),
      2
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(indice_medio))

# ------------------------------------------------
# Exporta tabelas
# ------------------------------------------------

write_csv(
  resumo_prontidao,
  "tabelas/prontidao_digital_nacional_2025.csv"
)

write_csv(
  prontidao_regiao,
  "tabelas/prontidao_digital_regiao_2025.csv"
)

# ------------------------------------------------
# Gráfico regional
# ------------------------------------------------

grafico_prontidao <- ggplot(
  prontidao_regiao,
  aes(
    x = reorder(NO_REGIAO, indice_medio),
    y = indice_medio
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Índice médio de prontidão digital escolar por região (2025)",
    x = "",
    y = "Índice médio"
  )

ggsave(
  "graficos/prontidao_digital_regiao_2025.png",
  plot = grafico_prontidao,
  width = 8,
  height = 5
)

cat("Análise concluída com sucesso!\n")

cat("\nDistribuição nacional:\n")
print(resumo_prontidao)

cat("\nProntidão por região:\n")
print(prontidao_regiao)