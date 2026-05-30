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
# Preparação dos dados
# ------------------------------------------------

dados_modelo <- escolas %>%
  transmute(
    NO_REGIAO,
    SG_UF,
    localizacao = case_when(
      TP_LOCALIZACAO == 1 ~ "Urbana",
      TP_LOCALIZACAO == 2 ~ "Rural"
    ),
    dependencia = case_when(
      TP_DEPENDENCIA == 1 ~ "Federal",
      TP_DEPENDENCIA == 2 ~ "Estadual",
      TP_DEPENDENCIA == 3 ~ "Municipal",
      TP_DEPENDENCIA == 4 ~ "Privada"
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
    indice_prontidao = energia +
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
# 1. Ranking de prontidão digital por UF
# ------------------------------------------------

ranking_uf <- dados_modelo %>%
  group_by(SG_UF) %>%
  summarise(
    escolas = n(),
    indice_medio = round(mean(indice_prontidao, na.rm = TRUE), 2),
    vulnerabilidade_media = round(mean(vulnerabilidade, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(indice_medio)

write_csv(
  ranking_uf,
  "tabelas/experimento_ranking_prontidao_uf_2025.csv"
)

grafico_ranking_uf <- ggplot(
  ranking_uf,
  aes(x = reorder(SG_UF, indice_medio), y = indice_medio)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Ranking de prontidão digital escolar por UF (2025)",
    x = "",
    y = "Índice médio de prontidão digital"
  )

ggsave(
  "graficos/experimento_ranking_prontidao_uf_2025.png",
  plot = grafico_ranking_uf,
  width = 9,
  height = 7
)

# ------------------------------------------------
# 2. Boxplot de prontidão por região
# ------------------------------------------------

grafico_boxplot_regiao <- ggplot(
  dados_modelo,
  aes(x = NO_REGIAO, y = indice_prontidao)
) +
  geom_boxplot() +
  labs(
    title = "Distribuição da prontidão digital por região (2025)",
    x = "Região",
    y = "Índice de prontidão digital"
  )

ggsave(
  "graficos/experimento_boxplot_prontidao_regiao_2025.png",
  plot = grafico_boxplot_regiao,
  width = 9,
  height = 6
)

# ------------------------------------------------
# 3. Heatmap região x localização
# ------------------------------------------------

heatmap_regiao_localizacao <- dados_modelo %>%
  group_by(NO_REGIAO, localizacao) %>%
  summarise(
    indice_medio = round(mean(indice_prontidao, na.rm = TRUE), 2),
    escolas = n(),
    .groups = "drop"
  )

write_csv(
  heatmap_regiao_localizacao,
  "tabelas/experimento_heatmap_regiao_localizacao_2025.csv"
)

grafico_heatmap <- ggplot(
  heatmap_regiao_localizacao,
  aes(x = localizacao, y = NO_REGIAO, fill = indice_medio)
) +
  geom_tile() +
  geom_text(aes(label = indice_medio)) +
  labs(
    title = "Prontidão digital média por região e localização (2025)",
    x = "",
    y = "",
    fill = "Índice médio"
  )

ggsave(
  "graficos/experimento_heatmap_regiao_localizacao_2025.png",
  plot = grafico_heatmap,
  width = 8,
  height = 5
)

# ------------------------------------------------
# 4. Clusterização K-means
# ------------------------------------------------

variaveis_cluster <- dados_modelo %>%
  select(
    energia,
    internet,
    banda_larga,
    uso_pedagogico,
    laboratorio,
    desktop,
    notebook,
    tablet
  )

set.seed(123)

cluster_k4 <- kmeans(
  variaveis_cluster,
  centers = 4,
  nstart = 25
)

dados_cluster <- dados_modelo %>%
  mutate(
    cluster = as.factor(cluster_k4$cluster)
  )

perfil_cluster <- dados_cluster %>%
  group_by(cluster) %>%
  summarise(
    escolas = n(),
    indice_medio = round(mean(indice_prontidao), 2),
    energia = round(mean(energia) * 100, 1),
    internet = round(mean(internet) * 100, 1),
    banda_larga = round(mean(banda_larga) * 100, 1),
    uso_pedagogico = round(mean(uso_pedagogico) * 100, 1),
    laboratorio = round(mean(laboratorio) * 100, 1),
    desktop = round(mean(desktop) * 100, 1),
    notebook = round(mean(notebook) * 100, 1),
    tablet = round(mean(tablet) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(indice_medio)

write_csv(
  perfil_cluster,
  "tabelas/experimento_perfil_clusters_2025.csv"
)

grafico_cluster <- ggplot(
  perfil_cluster,
  aes(x = reorder(cluster, indice_medio), y = indice_medio)
) +
  geom_col() +
  labs(
    title = "Índice médio de prontidão digital por cluster (2025)",
    x = "Cluster",
    y = "Índice médio"
  )

ggsave(
  "graficos/experimento_cluster_prontidao_2025.png",
  plot = grafico_cluster,
  width = 8,
  height = 5
)

cat("Experimentos avançados concluídos com sucesso!\n")

cat("\nRanking UF:\n")
print(ranking_uf)

cat("\nPerfil dos clusters:\n")
print(perfil_cluster)

cat("\nHeatmap região/localização:\n")
print(heatmap_regiao_localizacao)