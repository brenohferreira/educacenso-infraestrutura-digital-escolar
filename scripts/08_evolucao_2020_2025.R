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
    show_col_types = FALSE
  ) %>%
    transmute(
      ano = ano,
      IN_INTERNET,
      IN_BANDA_LARGA,
      IN_DESKTOP_ALUNO,
      IN_COMP_PORTATIL_ALUNO,
      IN_TABLET_ALUNO
    )
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
# Evolução nacional da infraestrutura digital
# ------------------------------------------------

evolucao <- escolas_todos_anos %>%
  group_by(ano) %>%
  summarise(
    internet = round(mean(IN_INTERNET, na.rm = TRUE) * 100, 1),
    banda_larga = round(mean(IN_BANDA_LARGA, na.rm = TRUE) * 100, 1),
    desktop = round(mean(IN_DESKTOP_ALUNO, na.rm = TRUE) * 100, 1),
    notebook = round(mean(IN_COMP_PORTATIL_ALUNO, na.rm = TRUE) * 100, 1),
    tablet = round(mean(IN_TABLET_ALUNO, na.rm = TRUE) * 100, 1)
  )

# ------------------------------------------------
# Exporta tabela
# ------------------------------------------------

write_csv(
  evolucao,
  "tabelas/evolucao_infraestrutura_2020_2025.csv"
)

# ------------------------------------------------
# Gráfico evolução da internet
# ------------------------------------------------

grafico_evolucao_internet <- ggplot(
  evolucao,
  aes(x = ano, y = internet)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolução do acesso à internet nas escolas brasileiras (2020-2025)",
    x = "Ano",
    y = "%"
  )

ggsave(
  "graficos/evolucao_internet_2020_2025.png",
  plot = grafico_evolucao_internet,
  width = 8,
  height = 5
)

cat("Análise concluída com sucesso!\n")

print(evolucao)