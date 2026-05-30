library(tidyverse)

cat("Analisando perfil dos clusters...\n")

# ------------------------------------------------
# Leitura da base
# ------------------------------------------------

escolas <- read_delim(
  "dados/microdados_censo_escolar_2025/dados/Tabela_Escola_2025.csv",
  delim = ";",
  show_col_types = FALSE
)

# ------------------------------------------------
# Preparação
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
      tablet
  )

# ------------------------------------------------
# Recria os clusters
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

# ------------------------------------------------
# Resumo geral dos clusters
# ------------------------------------------------

perfil_cluster <- dados_cluster %>%
  group_by(cluster) %>%
  summarise(
    escolas = n(),
    indice_medio = round(mean(indice_prontidao), 2),
    .groups = "drop"
  ) %>%
  arrange(indice_medio)

write_csv(
  perfil_cluster,
  "tabelas/cluster_resumo_2025.csv"
)

print(perfil_cluster)

# ------------------------------------------------
# Região
# ------------------------------------------------

cluster_regiao <- dados_cluster %>%
  count(cluster, NO_REGIAO) %>%
  group_by(cluster) %>%
  mutate(
    percentual = round(100 * n / sum(n), 1)
  ) %>%
  arrange(cluster, desc(percentual))

write_csv(
  cluster_regiao,
  "tabelas/cluster_por_regiao_2025.csv"
)

print(cluster_regiao)

# ------------------------------------------------
# Localização
# ------------------------------------------------

cluster_localizacao <- dados_cluster %>%
  count(cluster, localizacao) %>%
  group_by(cluster) %>%
  mutate(
    percentual = round(100 * n / sum(n), 1)
  )

write_csv(
  cluster_localizacao,
  "tabelas/cluster_por_localizacao_2025.csv"
)

print(cluster_localizacao)

# ------------------------------------------------
# Dependência administrativa
# ------------------------------------------------

cluster_dependencia <- dados_cluster %>%
  count(cluster, dependencia) %>%
  group_by(cluster) %>%
  mutate(
    percentual = round(100 * n / sum(n), 1)
  )

write_csv(
  cluster_dependencia,
  "tabelas/cluster_por_dependencia_2025.csv"
)

print(cluster_dependencia)

# ------------------------------------------------
# Top 10 UFs por cluster
# ------------------------------------------------

cluster_uf <- dados_cluster %>%
  count(cluster, SG_UF) %>%
  group_by(cluster) %>%
  mutate(
    percentual = round(100 * n / sum(n), 1)
  ) %>%
  arrange(cluster, desc(percentual))

write_csv(
  cluster_uf,
  "tabelas/cluster_por_uf_2025.csv"
)

# ------------------------------------------------
# Gráfico - Região
# ------------------------------------------------

grafico_regiao <- ggplot(
  cluster_regiao,
  aes(
    x = NO_REGIAO,
    y = percentual,
    fill = cluster
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribuição regional dos clusters",
    x = "Região",
    y = "% dentro do cluster"
  ) +
  theme_minimal()

ggsave(
  "graficos/cluster_por_regiao_2025.png",
  grafico_regiao,
  width = 9,
  height = 6
)

# ------------------------------------------------
# Gráfico - Localização
# ------------------------------------------------

grafico_localizacao <- ggplot(
  cluster_localizacao,
  aes(
    x = localizacao,
    y = percentual,
    fill = cluster
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribuição urbano/rural dos clusters",
    x = "",
    y = "% dentro do cluster"
  ) +
  theme_minimal()

ggsave(
  "graficos/cluster_por_localizacao_2025.png",
  grafico_localizacao,
  width = 8,
  height = 5
)

# ------------------------------------------------
# Gráfico - Dependência
# ------------------------------------------------

grafico_dependencia <- ggplot(
  cluster_dependencia,
  aes(
    x = dependencia,
    y = percentual,
    fill = cluster
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribuição por dependência administrativa",
    x = "",
    y = "% dentro do cluster"
  ) +
  theme_minimal()

ggsave(
  "graficos/cluster_por_dependencia_2025.png",
  grafico_dependencia,
  width = 10,
  height = 5
)

cat("\n")
cat("===================================\n")
cat("PERFIL DOS CLUSTERS CONCLUÍDO\n")
cat("===================================\n")