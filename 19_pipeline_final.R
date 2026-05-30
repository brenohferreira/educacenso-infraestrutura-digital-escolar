# ==================================================
# Script: 19_pipeline_final.R
# Projeto: Infraestrutura Digital e Prontidão Escolar
# Autor: Breno Heleno Ferreira
#
# Objetivo:
# Executar integralmente a reprodução das análises,
# tabelas, gráficos e experimentos do estudo.
# ==================================================

cat("Iniciando reprodução completa do estudo...\n")

rm(list = ls())
gc()

source("scripts/00_setup.R")

# Caracterização da amostra
source("scripts/01_caracterizacao_amostra.R")

# Diagnóstico 2025
source("scripts/02_analise_regional_2025.R")
source("scripts/03_dependencia_administrativa_2025.R")
source("scripts/04_urbano_rural_2025.R")
source("scripts/05_publica_privada_2025.R")
source("scripts/06_uf_2025.R")
source("scripts/07_indice_infraestrutura_basica_2025.R")

# Evolução nacional 2020-2025
rm(list = ls())
gc()
source("scripts/00_setup.R")
source("scripts/08_evolucao_2020_2025.R")

# Uso pedagógico da internet
rm(list = ls())
gc()
source("scripts/00_setup.R")
source("scripts/09_uso_pedagogico_internet_2025.R")

# Evolução regional 2020-2025
rm(list = ls())
gc()
source("scripts/00_setup.R")
source("scripts/10_evolucao_regional_2020_2025.R")

# Análises finais de infraestrutura e prontidão digital em 2025
rm(list = ls())
gc()
source("scripts/00_setup.R")
source("scripts/11_energia_eletrica_2025.R")
source("scripts/12_laboratorio_informatica_2025.R")
source("scripts/13_dispositivos_por_aluno_2025.R")
source("scripts/14_prontidao_digital_escolar_2025.R")
source("scripts/15_prontidao_digital_urbano_rural_2025.R")

# Experimentos complementares e visualizações finais
rm(list = ls())
gc()
source("scripts/00_setup.R")
source("scripts/16_experimentos_avancados_2025.R")
source("scripts/17_perfil_clusters_2025.R")
source("scripts/18_visualizacoes_dispersao_2025.R")

cat("Reprodução completa do estudo concluída com sucesso!\n")