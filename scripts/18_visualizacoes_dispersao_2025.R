library(tidyverse)

cat("Gerando visualizações de dispersão...\n")

# ------------------------------------------------
# Leitura da base
# ------------------------------------------------

escolas <- read_delim(
  "dados/microdados_censo_escolar_2025/dados/Tabela_Escola_2025.csv",
  delim = ";",
  show_col_types = FALSE
)

# ------------------------------------------------
# Preparação dos dados
# ------------------------------------------------

dados_modelo <- escolas %>%
  transmute(
    NO_REGIAO,
    SG_UF,
    
    localizacao = case_when(
      TP_LOCALIZACAO == 1 ~ "Urbana",
      TP_LOCALIZACAO == 2 ~ "Rural",
      TRUE ~ "Ignorado"
    ),
    
    dependencia = case_when(
      TP_DEPENDENCIA == 1 ~ "Federal",
      TP_DEPENDENCIA == 2 ~ "Estadual",
      TP_DEPENDENCIA == 3 ~ "Municipal",
      TP_DEPENDENCIA == 4 ~ "Privada",
      TRUE ~ "Ignorado"
    ),
    
    energia = replace_na(IN_ENERGIA_REDE_PUBLICA, 0),
    internet = replace_na(IN_INTERNET, 0),
    banda_larga = replace_na(IN_BANDA_LARGA, 0),
    uso_pedagogico = replace_na(IN_INTERNET_APRENDIZAGEM, 0),
    laboratorio = replace_na(IN_LABORATORIO_INFORMATICA, 0),
    desktop = replace_na(IN_DESKTOP_ALUNO, 0),
    notebook = replace_na(IN_COMP_PORTATIL_ALUNO, 0),
    tablet = replace_na(IN_TABLET_ALUNO, 0)
  ) %>%
  mutate(
    indice_prontidao =
      energia +
      internet +
      banda_larga +
      uso_pedagogico +
      laboratorio +
      desktop +
      notebook +
      tablet,
    
    vulnerabilidade = 8 - indice_prontidao
  )

# ------------------------------------------------
# Amostra para gráficos com muitas bolinhas
# ------------------------------------------------

set.seed(123)

amostra <- dados_modelo %>%
  sample_n(15000)

# ------------------------------------------------
# 1. Dispersão com bolinhas por região
# Cada ponto representa uma escola
# ------------------------------------------------

grafico_jitter_regiao <- ggplot(
  amostra,
  aes(
    x = NO_REGIAO,
    y = indice_prontidao
  )
) +
  geom_jitter(
    width = 0.25,
    height = 0.08,
    alpha = 0.15,
    size = 1
  ) +
  labs(
    x = "Região",
    y = "Índice de prontidão digital"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/experimento_dispersao_escolas_por_regiao_2025.png",
  plot = grafico_jitter_regiao,
  width = 10,
  height = 6
)

# ------------------------------------------------
# 2. Dispersão com bolinhas por região e localização
# ------------------------------------------------

grafico_jitter_regiao_localizacao <- ggplot(
  amostra,
  aes(
    x = NO_REGIAO,
    y = indice_prontidao,
    color = localizacao
  )
) +
  geom_jitter(
    width = 0.25,
    height = 0.08,
    alpha = 0.2,
    size = 1
  ) +
  labs(
    x = "Região",
    y = "Índice de prontidão digital",
    color = "Localização"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/experimento_dispersao_regiao_localizacao_2025.png",
  plot = grafico_jitter_regiao_localizacao,
  width = 10,
  height = 6
)

# ------------------------------------------------
# 3. Dispersão por UF
# Cada ponto representa uma UF
# ------------------------------------------------

ranking_uf <- dados_modelo %>%
  group_by(SG_UF) %>%
  summarise(
    escolas = n(),
    indice_medio = round(mean(indice_prontidao), 2),
    vulnerabilidade_media = round(mean(vulnerabilidade), 2),
    .groups = "drop"
  )

grafico_dispersao_uf <- ggplot(
  ranking_uf,
  aes(
    x = escolas,
    y = indice_medio,
    label = SG_UF
  )
) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(
    nudge_y = 0.08,
    size = 3
  ) +
  labs(
    x = "Número de escolas",
    y = "Índice médio de prontidão digital"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/experimento_dispersao_uf_escolas_prontidao_2025.png",
  plot = grafico_dispersao_uf,
  width = 10,
  height = 6
)

write_csv(
  ranking_uf,
  "tabelas/experimento_dispersao_ranking_uf_2025.csv"
)

# ------------------------------------------------
# 4. Bubble chart por UF
# X = vulnerabilidade média
# Y = prontidão média
# Tamanho = número de escolas
# ------------------------------------------------

grafico_bolhas_uf <- ggplot(
  ranking_uf,
  aes(
    x = vulnerabilidade_media,
    y = indice_medio,
    size = escolas,
    label = SG_UF
  )
) +
  geom_point(alpha = 0.6) +
  geom_text(size = 3) +
  scale_size_continuous(
    range = c(3, 14)
  ) +
  labs(
    x = "Vulnerabilidade média",
    y = "Índice médio de prontidão digital",
    size = "Número de escolas"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/experimento_bolhas_uf_prontidao_vulnerabilidade_2025.png",
  plot = grafico_bolhas_uf,
  width = 10,
  height = 6
)

# ------------------------------------------------
# 5. Densidade do índice por região
# Mostra a distribuição completa, não apenas a média
# ------------------------------------------------

grafico_densidade_regiao <- ggplot(
  dados_modelo,
  aes(
    x = indice_prontidao,
    fill = NO_REGIAO
  )
) +
  geom_density(alpha = 0.35) +
  labs(
    x = "Índice de prontidão digital",
    y = "Densidade",
    fill = "Região"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/experimento_densidade_prontidao_regiao_2025.png",
  plot = grafico_densidade_regiao,
  width = 10,
  height = 6
)

# ------------------------------------------------
# 6. Histograma nacional da prontidão digital
# ------------------------------------------------

grafico_histograma_prontidao <- ggplot(
  dados_modelo,
  aes(x = indice_prontidao)
) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,
    closed = "left"
  ) +
  labs(
    x = "Índice de prontidão digital",
    y = "Número de escolas"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/experimento_histograma_prontidao_nacional_2025.png",
  plot = grafico_histograma_prontidao,
  width = 9,
  height = 6
)

# ------------------------------------------------
# 7. Dispersão região/localização agregada
# Cada ponto representa um grupo: Região + Urbana/Rural
# ------------------------------------------------

regiao_localizacao <- dados_modelo %>%
  group_by(NO_REGIAO, localizacao) %>%
  summarise(
    escolas = n(),
    indice_medio = round(mean(indice_prontidao), 2),
    vulnerabilidade_media = round(mean(vulnerabilidade), 2),
    .groups = "drop"
  ) %>%
  mutate(
    grupo = paste(NO_REGIAO, localizacao)
  )

write_csv(
  regiao_localizacao,
  "tabelas/experimento_dispersao_regiao_localizacao_2025.csv"
)

grafico_dispersao_grupos <- ggplot(
  regiao_localizacao,
  aes(
    x = escolas,
    y = indice_medio,
    label = grupo
  )
) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text(
    nudge_y = 0.08,
    size = 3
  ) +
  labs(
    x = "Número de escolas",
    y = "Índice médio de prontidão digital"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  "graficos/experimento_dispersao_grupos_regiao_localizacao_2025.png",
  plot = grafico_dispersao_grupos,
  width = 10,
  height = 6
)

cat("\nVisualizações de dispersão concluídas com sucesso!\n")

cat("\nArquivos gerados em graficos/:\n")
print(
  list.files(
    "graficos",
    pattern = "experimento_.*2025.png"
  )
)

cat("\nTabela UF:\n")
print(ranking_uf)

cat("\nTabela região/localização:\n")
print(regiao_localizacao)