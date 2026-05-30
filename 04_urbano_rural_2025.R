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
# Infraestrutura digital por localização
# ------------------------------------------------

urbano_rural <- escolas %>%
  mutate(
    localizacao = case_when(
      TP_LOCALIZACAO == 1 ~ "Urbana",
      TP_LOCALIZACAO == 2 ~ "Rural"
    )
  ) %>%
  group_by(localizacao) %>%
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
  urbano_rural,
  "tabelas/infraestrutura_urbano_rural_2025.csv"
)

# ------------------------------------------------
# Gráfico Internet
# ------------------------------------------------

grafico_internet_urbano_rural <- ggplot(
  urbano_rural,
  aes(
    x = reorder(localizacao, internet),
    y = internet
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Escolas com acesso à internet por localização (2025)",
    x = "",
    y = "%"
  )

ggsave(
  "graficos/internet_urbano_rural_2025.png",
  plot = grafico_internet_urbano_rural,
  width = 8,
  height = 5
)

cat("Análise concluída com sucesso!\n")

print(urbano_rural)