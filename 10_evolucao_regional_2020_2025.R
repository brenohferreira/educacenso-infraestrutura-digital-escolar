library(tidyverse)

# ------------------------------------------------
# Função para ler cada ano
# ------------------------------------------------

ler_escolas <- function(ano) {
  
  if (ano <= 2024) {
    caminho <- paste0(
      "dados/microdados_censo_escolar_",
      ano,
      "/dados/microdados_ed_basica_",
      ano,
      ".csv"
    )
  } else {
    caminho <- paste0(
      "dados/microdados_censo_escolar_",
      ano,
      "/dados/Tabela_Escola_",
      ano,
      ".csv"
    )
  }
  
  read_delim(
    caminho,
    delim = ";",
    show_col_types = FALSE,
    col_select = c(
      NO_REGIAO,
      IN_INTERNET,
      IN_BANDA_LARGA,
      IN_DESKTOP_ALUNO,
      IN_COMP_PORTATIL_ALUNO,
      IN_TABLET_ALUNO
    )
  ) %>%
    mutate(ano = ano)
}

# ------------------------------------------------
# Leitura e junção dos anos
# ------------------------------------------------

escolas_todos_anos <- bind_rows(
  ler_escolas(2020),
  ler_escolas(2021),
  ler_escolas(2022),
  ler_escolas(2023),
  ler_escolas(2024),
  ler_escolas(2025)
)

# ------------------------------------------------
# Evolução regional da infraestrutura digital
# ------------------------------------------------

evolucao_regional <- escolas_todos_anos %>%
  group_by(ano, NO_REGIAO) %>%
  summarise(
    internet = round(mean(IN_INTERNET, na.rm = TRUE) * 100, 1),
    banda_larga = round(mean(IN_BANDA_LARGA, na.rm = TRUE) * 100, 1),
    desktop = round(mean(IN_DESKTOP_ALUNO, na.rm = TRUE) * 100, 1),
    notebook = round(mean(IN_COMP_PORTATIL_ALUNO, na.rm = TRUE) * 100, 1),
    tablet = round(mean(IN_TABLET_ALUNO, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

# ------------------------------------------------
# Exporta tabela
# ------------------------------------------------

write_csv(
  evolucao_regional,
  "tabelas/evolucao_regional_2020_2025.csv"
)

# ------------------------------------------------
# Gráfico evolução da internet por região
# ------------------------------------------------

grafico_evolucao_regional <- ggplot(
  evolucao_regional,
  aes(
    x = ano,
    y = internet,
    group = NO_REGIAO
  )
) +
  geom_line() +
  geom_point() +
  facet_wrap(~ NO_REGIAO) +
  labs(
    title = "Evolução do acesso à internet por região (2020-2025)",
    x = "Ano",
    y = "%"
  )

ggsave(
  "graficos/evolucao_internet_regional_2020_2025.png",
  plot = grafico_evolucao_regional,
  width = 10,
  height = 6
)

cat("Análise concluída com sucesso!\n")

print(evolucao_regional)