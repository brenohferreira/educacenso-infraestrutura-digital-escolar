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
# Índice de Prontidão Digital Escolar por região e localização
# ------------------------------------------------

prontidao_urbano_rural <- escolas %>%
  mutate(
    localizacao = case_when(
      TP_LOCALIZACAO == 1 ~ "Urbana",
      TP_LOCALIZACAO == 2 ~ "Rural"
    ),
    
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
      tablet
  ) %>%
  group_by(NO_REGIAO, localizacao) %>%
  summarise(
    indice_medio = round(mean(indice_prontidao, na.rm = TRUE), 2),
    escolas = n(),
    .groups = "drop"
  ) %>%
  arrange(indice_medio)

# ------------------------------------------------
# Exporta tabela
# ------------------------------------------------

write_csv(
  prontidao_urbano_rural,
  "tabelas/prontidao_digital_urbano_rural_2025.csv"
)

# ------------------------------------------------
# Gráfico
# ------------------------------------------------

grafico_prontidao_urbano_rural <- ggplot(
  prontidao_urbano_rural,
  aes(
    x = reorder(NO_REGIAO, indice_medio),
    y = indice_medio
  )
) +
  geom_col() +
  facet_wrap(~ localizacao) +
  coord_flip() +
  labs(
    title = "Índice médio de prontidão digital escolar por região e localização (2025)",
    x = "",
    y = "Índice médio"
  )

ggsave(
  "graficos/prontidao_digital_urbano_rural_2025.png",
  plot = grafico_prontidao_urbano_rural,
  width = 10,
  height = 6
)

cat("Análise concluída com sucesso!\n")

print(prontidao_urbano_rural)