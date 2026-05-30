library(tidyverse)

# ------------------------------------------------
# Função para ler cada ano
# ------------------------------------------------

ler_escolas <- function(ano) {
  
  if (ano <= 2024) {
    caminho <- paste0(
      "dados/microdados_censo_escolar_",
      ano,
      "/dados/microdados_ed_basica_",
      ano,
      ".csv"
    )
  } else {
    caminho <- paste0(
      "dados/microdados_censo_escolar_",
      ano,
      "/dados/Tabela_Escola_",
      ano,
      ".csv"
    )
  }
  
  read_delim(
    caminho,
    delim = ";",
    show_col_types = FALSE,
    col_select = c(
      NU_ANO_CENSO,
      NO_REGIAO,
      TP_LOCALIZACAO,
      TP_DEPENDENCIA,
      IN_INTERNET,
      IN_BANDA_LARGA,
      IN_INTERNET_APRENDIZAGEM,
      IN_LABORATORIO_INFORMATICA,
      IN_DESKTOP_ALUNO,
      IN_COMP_PORTATIL_ALUNO,
      IN_TABLET_ALUNO,
      IN_ENERGIA_REDE_PUBLICA,
      IN_ENERGIA_INEXISTENTE
    )
  ) %>%
    mutate(ano = ano)
}

# ------------------------------------------------
# Leitura dos anos
# ------------------------------------------------

escolas_todos_anos <- bind_rows(
  ler_escolas(2020),
  ler_escolas(2021),
  ler_escolas(2022),
  ler_escolas(2023),
  ler_escolas(2024),
  ler_escolas(2025)
)

escolas_2025 <- escolas_todos_anos %>%
  filter(ano == 2025)

# ------------------------------------------------
# Caracterização da amostra por ano
# ------------------------------------------------

caracterizacao_anos <- escolas_todos_anos %>%
  group_by(ano) %>%
  summarise(
    escolas = n(),
    urbanas = sum(TP_LOCALIZACAO == 1, na.rm = TRUE),
    rurais = sum(TP_LOCALIZACAO == 2, na.rm = TRUE),
    publicas = sum(TP_DEPENDENCIA != 4, na.rm = TRUE),
    privadas = sum(TP_DEPENDENCIA == 4, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------
# Caracterização geral 2025
# ------------------------------------------------

caracterizacao_geral_2025 <- tibble(
  indicador = c(
    "Total de escolas",
    "Escolas urbanas",
    "Escolas rurais",
    "Escolas públicas",
    "Escolas privadas",
    "Escolas federais",
    "Escolas estaduais",
    "Escolas municipais"
  ),
  n = c(
    nrow(escolas_2025),
    sum(escolas_2025$TP_LOCALIZACAO == 1, na.rm = TRUE),
    sum(escolas_2025$TP_LOCALIZACAO == 2, na.rm = TRUE),
    sum(escolas_2025$TP_DEPENDENCIA != 4, na.rm = TRUE),
    sum(escolas_2025$TP_DEPENDENCIA == 4, na.rm = TRUE),
    sum(escolas_2025$TP_DEPENDENCIA == 1, na.rm = TRUE),
    sum(escolas_2025$TP_DEPENDENCIA == 2, na.rm = TRUE),
    sum(escolas_2025$TP_DEPENDENCIA == 3, na.rm = TRUE)
  )
) %>%
  mutate(
    percentual = round(n / nrow(escolas_2025) * 100, 1)
  )

# ------------------------------------------------
# Distribuição regional 2025
# ------------------------------------------------

caracterizacao_regiao_2025 <- escolas_2025 %>%
  count(NO_REGIAO, name = "n") %>%
  mutate(
    percentual = round(n / sum(n) * 100, 1)
  ) %>%
  arrange(desc(n))

# ------------------------------------------------
# Região e localização 2025
# ------------------------------------------------

caracterizacao_regiao_localizacao_2025 <- escolas_2025 %>%
  mutate(
    localizacao = case_when(
      TP_LOCALIZACAO == 1 ~ "Urbana",
      TP_LOCALIZACAO == 2 ~ "Rural"
    )
  ) %>%
  count(NO_REGIAO, localizacao, name = "n") %>%
  group_by(NO_REGIAO) %>%
  mutate(
    percentual_regiao = round(n / sum(n) * 100, 1)
  ) %>%
  ungroup()

# ------------------------------------------------
# Infraestrutura nacional 2025
# ------------------------------------------------

infraestrutura_nacional_2025 <- tibble(
  indicador = c(
    "Internet",
    "Banda larga",
    "Internet para aprendizagem",
    "Laboratório de informática",
    "Desktop para alunos",
    "Notebook para alunos",
    "Tablet para alunos",
    "Energia por rede pública",
    "Energia inexistente"
  ),
  n = c(
    sum(escolas_2025$IN_INTERNET == 1, na.rm = TRUE),
    sum(escolas_2025$IN_BANDA_LARGA == 1, na.rm = TRUE),
    sum(escolas_2025$IN_INTERNET_APRENDIZAGEM == 1, na.rm = TRUE),
    sum(escolas_2025$IN_LABORATORIO_INFORMATICA == 1, na.rm = TRUE),
    sum(escolas_2025$IN_DESKTOP_ALUNO == 1, na.rm = TRUE),
    sum(escolas_2025$IN_COMP_PORTATIL_ALUNO == 1, na.rm = TRUE),
    sum(escolas_2025$IN_TABLET_ALUNO == 1, na.rm = TRUE),
    sum(escolas_2025$IN_ENERGIA_REDE_PUBLICA == 1, na.rm = TRUE),
    sum(escolas_2025$IN_ENERGIA_INEXISTENTE == 1, na.rm = TRUE)
  )
) %>%
  mutate(
    percentual = round(n / nrow(escolas_2025) * 100, 1)
  )

# ------------------------------------------------
# Exporta tabelas
# ------------------------------------------------

write_csv(caracterizacao_anos, "tabelas/caracterizacao_anos.csv")
write_csv(caracterizacao_geral_2025, "tabelas/caracterizacao_geral_2025.csv")
write_csv(caracterizacao_regiao_2025, "tabelas/caracterizacao_regiao_2025.csv")
write_csv(caracterizacao_regiao_localizacao_2025, "tabelas/caracterizacao_regiao_localizacao_2025.csv")
write_csv(infraestrutura_nacional_2025, "tabelas/infraestrutura_nacional_2025.csv")

cat("Caracterização da amostra concluída com sucesso!\n")

print(caracterizacao_anos)
print(caracterizacao_geral_2025)
print(caracterizacao_regiao_2025)
print(caracterizacao_regiao_localizacao_2025)
print(infraestrutura_nacional_2025)