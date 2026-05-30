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
# Dispositivos digitais por região e localização
# ------------------------------------------------

dispositivos <- escolas %>%
  mutate(
    localizacao = case_when(
      TP_LOCALIZACAO == 1 ~ "Urbana",
      TP_LOCALIZACAO == 2 ~ "Rural"
    )
  ) %>%
  group_by(NO_REGIAO, localizacao) %>%
  summarise(
    desktop = round(mean(IN_DESKTOP_ALUNO, na.rm = TRUE) * 100, 1),
    notebook = round(mean(IN_COMP_PORTATIL_ALUNO, na.rm = TRUE) * 100, 1),
    tablet = round(mean(IN_TABLET_ALUNO, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

# ------------------------------------------------
# Exporta tabela
# ------------------------------------------------

write_csv(
  dispositivos,
  "tabelas/dispositivos_por_aluno_2025.csv"
)

# ------------------------------------------------
# Gráfico notebooks
# ------------------------------------------------

grafico_notebook <- ggplot(
  dispositivos,
  aes(
    x = reorder(NO_REGIAO, notebook),
    y = notebook
  )
) +
  geom_col() +
  facet_wrap(~ localizacao) +
  coord_flip() +
  labs(
    title = "Escolas com notebooks para alunos por região e localização (2025)",
    x = "",
    y = "%"
  )

ggsave(
  "graficos/notebooks_por_regiao_localizacao_2025.png",
  plot = grafico_notebook,
  width = 10,
  height = 6
)

cat("Análise concluída com sucesso!\n")

print(dispositivos)