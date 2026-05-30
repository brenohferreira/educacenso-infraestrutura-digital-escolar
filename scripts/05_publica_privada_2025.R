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
# Infraestrutura digital por rede
# ------------------------------------------------

publica_privada <- escolas %>%
  mutate(
    rede = case_when(
      TP_DEPENDENCIA == 4 ~ "Privada",
      TRUE ~ "Pública"
    )
  ) %>%
  group_by(rede) %>%
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
  publica_privada,
  "tabelas/infraestrutura_publica_privada_2025.csv"
)

# ------------------------------------------------
# Gráfico Internet
# ------------------------------------------------

grafico_internet_publica_privada <- ggplot(
  publica_privada,
  aes(
    x = reorder(rede, internet),
    y = internet
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Escolas com acesso à internet por rede (2025)",
    x = "",
    y = "%"
  )

ggsave(
  "graficos/internet_publica_privada_2025.png",
  plot = grafico_internet_publica_privada,
  width = 8,
  height = 5
)

cat("Análise concluída com sucesso!\n")

print(publica_privada)