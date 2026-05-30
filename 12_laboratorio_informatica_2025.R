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
# Laboratório de informática por região e localização
# ------------------------------------------------

laboratorio <- escolas %>%
  mutate(
    localizacao = case_when(
      TP_LOCALIZACAO == 1 ~ "Urbana",
      TP_LOCALIZACAO == 2 ~ "Rural"
    )
  ) %>%
  group_by(NO_REGIAO, localizacao) %>%
  summarise(
    laboratorio_informatica = round(mean(IN_LABORATORIO_INFORMATICA, na.rm = TRUE) * 100, 1),
    internet_aprendizagem = round(mean(IN_INTERNET_APRENDIZAGEM, na.rm = TRUE) * 100, 1),
    desktop = round(mean(IN_DESKTOP_ALUNO, na.rm = TRUE) * 100, 1),
    notebook = round(mean(IN_COMP_PORTATIL_ALUNO, na.rm = TRUE) * 100, 1),
    tablet = round(mean(IN_TABLET_ALUNO, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

# ------------------------------------------------
# Exporta tabela
# ------------------------------------------------

write_csv(
  laboratorio,
  "tabelas/laboratorio_informatica_regiao_localizacao_2025.csv"
)

# ------------------------------------------------
# Gráfico laboratório de informática
# ------------------------------------------------

grafico_laboratorio <- ggplot(
  laboratorio,
  aes(
    x = reorder(NO_REGIAO, laboratorio_informatica),
    y = laboratorio_informatica
  )
) +
  geom_col() +
  facet_wrap(~ localizacao) +
  coord_flip() +
  labs(
    title = "Escolas com laboratório de informática por região e localização (2025)",
    x = "",
    y = "%"
  )

ggsave(
  "graficos/laboratorio_informatica_regiao_localizacao_2025.png",
  plot = grafico_laboratorio,
  width = 10,
  height = 6
)

cat("Análise concluída com sucesso!\n")

print(laboratorio)