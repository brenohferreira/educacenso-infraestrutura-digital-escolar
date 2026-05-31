Índice de Prontidão Digital Escolar no Brasil (2020–2025)

Este repositório reúne os dados processados, scripts, tabelas e visualizações desenvolvidos para o Trabalho de Conclusão de Curso (TCC) do MBA em Data Science e Analytics da Escola Superior de Agricultura Luiz de Queiroz da Universidade de São Paulo (ESALQ/USP).

O estudo utiliza os microdados do Censo Escolar da Educação Básica (Educacenso) para analisar a evolução da prontidão digital das escolas brasileiras entre 2020 e 2025, considerando dimensões relacionadas à conectividade, uso pedagógico das tecnologias e disponibilidade de recursos tecnológicos.

Objetivo

Construir e analisar o Índice de Prontidão Digital Escolar (IPDE), um indicador sintético desenvolvido para mensurar o grau de preparação das escolas brasileiras para utilização de tecnologias digitais em atividades educacionais, administrativas e de aprendizagem.

O estudo busca:

Analisar a evolução da prontidão digital das escolas brasileiras entre 2020 e 2025;
Identificar desigualdades entre regiões geográficas, redes administrativas e localização urbana e rural;
Produzir evidências que possam subsidiar políticas públicas e estratégias de transformação digital na educação básica.
Estrutura do Repositório
.
├── data/
│   ├── raw/
│   └── processed/
├── outputs/
│   ├── tabelas/
│   └── graficos/
├── scripts/
├── docs/
└── README.md
Diretórios principais
data/: bases de dados utilizadas no estudo.
scripts/: scripts em R responsáveis pelo processamento, construção dos indicadores, análises e geração dos resultados.
outputs/tabelas/: tabelas produzidas durante as análises.
outputs/graficos/: visualizações utilizadas no TCC.
docs/: documentos e materiais complementares.
Dados

Os dados utilizados são provenientes dos microdados públicos do Censo Escolar da Educação Básica, disponibilizados pelo Instituto Nacional de Estudos e Pesquisas Educacionais Anísio Teixeira (INEP).

Os arquivos originais podem ser obtidos em:

https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar

Devido ao volume dos arquivos e às etapas de processamento realizadas, os microdados originais não são redistribuídos neste repositório.

Metodologia

O Índice de Prontidão Digital Escolar (IPDE) foi construído a partir da combinação de três dimensões:

Conectividade: acesso à internet e disponibilidade de conexão banda larga;
Uso Pedagógico: utilização da internet em atividades de ensino e aprendizagem;
Recursos Tecnológicos: disponibilidade de laboratório de informática e dispositivos para estudantes.

O indicador final corresponde à média aritmética simples das três dimensões, produzindo um escore entre 0 e 1, em que valores mais elevados indicam maiores níveis de prontidão digital escolar.

Reprodutibilidade

A reprodução completa das análises pode ser realizada por meio da execução sequencial dos scripts disponíveis na pasta scripts/.

Os resultados finais utilizados no trabalho encontram-se disponíveis nas pastas:

outputs/tabelas/
outputs/graficos/
Autor

Breno Heleno Ferreira

Doutorando em Ciência da Informação pela Universidade Federal de Minas Gerais (UFMG)

Mestre em Educação e Docência pela Universidade Federal de Minas Gerais (UFMG)

MBA em Data Science e Analytics pela Escola Superior de Agricultura Luiz de Queiroz da Universidade de São Paulo (ESALQ/USP)

MBA em Gestão de Projetos pela Universidade de São Paulo (USP)

E-mail: brenohferreira@ufmg.br

GitHub: https://github.com/brenohferreira

Licença

Este projeto está licenciado sob os termos da Licença MIT.
