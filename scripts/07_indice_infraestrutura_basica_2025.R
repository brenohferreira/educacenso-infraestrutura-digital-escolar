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
# Índice simples de infraestrutura digital
# ------------------------------------------------

indice_2025 <- escolas %>%
  mutate(
    IN_INTERNET = replace_na(IN_INTERNET, 0),
    IN_BANDA_LARGA = replace_na(IN_BANDA_LARGA, 0),
    IN_DESKTOP_ALUNO = replace_na(IN_DESKTOP_ALUNO, 0),
    IN_COMP_PORTATIL_ALUNO = replace_na(IN_COMP_PORTATIL_ALUNO, 0),
    IN_TABLET_ALUNO = replace_na(IN_TABLET_ALUNO, 0),
    
    indice_digital =
      IN_INTERNET +
      IN_BANDA_LARGA +
      IN_DESKTOP_ALUNO +
      IN_COMP_PORTATIL_ALUNO +
      IN_TABLET_ALUNO,
    
    nivel_infraestrutura = case_when(
      indice_digital <= 1 ~ "Muito baixa",
      indice_digital <= 2 ~ "Baixa",
      indice_digital <= 3 ~ "Média",
      indice_digital <= 4 ~ "Alta",
      indice_digital == 5 ~ "Muito alta"
    )
  )

# ------------------------------------------------
# Resumo nacional
# ------------------------------------------------

resumo_indice <- indice_2025 %>%
  count(nivel_infraestrutura) %>%
  mutate(
    percentual = round(
      n / sum(n) * 100,
      1
    )
  )

# ------------------------------------------------
# Exporta tabela
# ------------------------------------------------

write_csv(
  resumo_indice,
  "tabelas/indice_infraestrutura_2025.csv"
)

# ------------------------------------------------
# Gráfico índice
# ------------------------------------------------

grafico_indice <- ggplot(
  resumo_indice,
  aes(
    x = reorder(
      nivel_infraestrutura,
      percentual
    ),
    y = percentual
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Distribuição das escolas por nível de infraestrutura digital (2025)",
    x = "",
    y = "%"
  )

ggsave(
  "graficos/indice_infraestrutura_2025.png",
  plot = grafico_indice,
  width = 8,
  height = 5
)

cat("Análise concluída com sucesso!\n")

print(resumo_indice)