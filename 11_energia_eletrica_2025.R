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
# Energia elétrica por região e localização
# ------------------------------------------------

energia <- escolas %>%
  mutate(
    localizacao = case_when(
      TP_LOCALIZACAO == 1 ~ "Urbana",
      TP_LOCALIZACAO == 2 ~ "Rural"
    )
  ) %>%
  group_by(NO_REGIAO, localizacao) %>%
  summarise(
    energia_rede_publica = round(mean(IN_ENERGIA_REDE_PUBLICA, na.rm = TRUE) * 100, 1),
    energia_inexistente = round(mean(IN_ENERGIA_INEXISTENTE, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

# ------------------------------------------------
# Exporta tabela
# ------------------------------------------------

write_csv(
  energia,
  "tabelas/energia_eletrica_regiao_localizacao_2025.csv"
)

# ------------------------------------------------
# Gráfico ausência de energia
# ------------------------------------------------

grafico_energia_inexistente <- ggplot(
  energia,
  aes(
    x = reorder(NO_REGIAO, energia_inexistente),
    y = energia_inexistente
  )
) +
  geom_col() +
  facet_wrap(~ localizacao) +
  coord_flip() +
  labs(
    title = "Escolas sem energia elétrica por região e localização (2025)",
    x = "",
    y = "%"
  )

ggsave(
  "graficos/energia_inexistente_regiao_localizacao_2025.png",
  plot = grafico_energia_inexistente,
  width = 10,
  height = 6
)

cat("Análise concluída com sucesso!\n")

print(energia)