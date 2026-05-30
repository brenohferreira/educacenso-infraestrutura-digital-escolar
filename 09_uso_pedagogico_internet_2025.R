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
# Uso pedagógico da internet
# ------------------------------------------------

uso_internet <- escolas %>%
  group_by(NO_REGIAO) %>%
  summarise(
    internet = round(mean(IN_INTERNET, na.rm = TRUE) * 100, 1),
    internet_aprendizagem = round(mean(IN_INTERNET_APRENDIZAGEM, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

# ------------------------------------------------
# Exporta tabela
# ------------------------------------------------

write_csv(
  uso_internet,
  "tabelas/uso_pedagogico_internet_2025.csv"
)

# ------------------------------------------------
# Gráfico
# ------------------------------------------------

grafico_uso_internet <- ggplot(
  uso_internet,
  aes(
    x = reorder(NO_REGIAO, internet_aprendizagem),
    y = internet_aprendizagem
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Uso pedagógico da internet por região (2025)",
    x = "",
    y = "%"
  )

ggsave(
  "graficos/uso_pedagogico_internet_2025.png",
  plot = grafico_uso_internet,
  width = 8,
  height = 5
)

cat("Análise concluída com sucesso!\n")

print(uso_internet)