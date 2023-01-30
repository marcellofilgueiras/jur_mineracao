# Projeto: Jurimetria do Direito Minerário
# Cliente: Adv. Suzy
# Autor: Marcello Filgueiras
# Data Início: 23/11/2022

library(tidyverse)
library(stf)


### Descrição 

# Pesquisa será feita inicialmente pelas palavra Sugeridas: "Mineração" ou "Direito Minerário"
# Tribunal: stf
# Pacote Web Scrapper {stf} disponível em github.com/jjesusfilho/stj

#devtools::install_github("jjesusfilho/stf")


# # MONOCRÁTICAS ----------------------------------------------------------


# Baixando ----------------------------------------------------------------

# Termo de Busca: Mineração

#stf::stf_download_cjsg(search = "mineração",
#                      base = "decisoes",
#                      size = 10,
#                      dir = "stf/data_raw/mineracao/mono")
#

stf_minerac_mono_raw <- tibble(stf::stf_read_cjsg( dir = "stf/data_raw/mineracao/mono"),
                               termo_busca = "mineração")

# Termo de Busca: Direito Minerário


#stf::stf_download_cjsg(search = "direito minerário",
#                       base = "decisoes",
#                       size = 10,
#                       dir = "stf/data_raw/dir_minerario/mono")


stf_dir_minerario_mono_raw <- tibble(stf::stf_read_cjsg( dir = "stf/data_raw/dir_minerario/mono"),
                                     termo_busca = "direito minerário")



# DF interativo
#stf_dir_minerario_mono_raw %>%
  #dplyr::select(classe, numero,
   #             registro, relator, decisao) %>%
 # DT::datatable(extensions = "Responsive",
  #              filter = "top")

# Tibble para ver o contéudo das colunas

stf_dir_minerario_mono_raw %>%
  #select(processo, ministro_facet, legislacao_citada, objetivos_onu)%>%
  as_tibble() # %>% str()

# Tidying ? ---------------------------------------------------------------

# No Json original da API do STF, temos muitas colunas que julgo serem repetidas da mesma informação
# ou desnecessárias para o contexto de análise de dados.

##### Aqui está um exemplo de dataframe 
#com essas colunas ordenadas por função-----

# Colunas ordenadas

# stf_dir_minerario_mono_order <- stf_dir_minerario_mono_raw %>%
#  relocate ( #Número e Classe dos Processos
#    titulo, processo_codigo_completo,
#    processo_classe_processual_unificada_classe_sigla, processo_numero,
#    processo_classe_processual_unificada_incidente_sigla, #externo_seq_objeto_incidente,
#    processo_classe_processual_unificada_extenso,
#    # Geografia
#    procedencia_geografica_uf_sigla, procedencia_geografica_uf_extenso,
#    procedencia_geografica_completo,
#    procedencia_geografica_pais_sigla, procedencia_geografica_uf_sigla,
#    # Datas
#    publicacao_data, julgamento_data,
#   # Ministros
#   relator_decisao_nome, relator_processo_nome, is_decisao_presidencia,
#   presidente_nome, ministro_facet,
#   #Decisao
#   decisao_texto,
#   # Documentos Citados
#   documental_legislacao_citada_texto, documental_decisao_mesmo_sentido_lista_texto,
#   documental_decisao_mesmo_sentido_is_secundario,
#   documental_doutrina_texto,
#   documental_indexacao_texto, documental_observacao_texto,
#   partes_lista_texto, ods_onu,documental_publicacao_lista_texto,
#   #Url e API e Outros
#   inteiro_teor_url, acompanhamento_processual_url, dje_url, base,
#   id, dg_atualizado_em, dg_unique, 
# )

# ----

# Função para s diminuir número de colunas,
# Seleciona apenas as colunas "necessárias"
# "dg_unique" e "dg_atualizado_em" nao foram inseridos

stf_clean_cjsg_mono <- function(x) {
  x %>%
    select(processo = titulo, classe = processo_classe_processual_unificada_classe_sigla,
           incidente = processo_classe_processual_unificada_incidente_sigla,
           classe_extenso = processo_classe_processual_unificada_extenso,
           procedencia_geografica = procedencia_geografica_completo,
           #data
           data_publicacao = publicacao_data, data_julgamento = julgamento_data,
           #Decisao
           decisao_texto,
           #docs
           legislacao_citada = documental_legislacao_citada_texto,
           decisao_mesmo_sentido = documental_decisao_mesmo_sentido_lista_texto,
           decisao_mesmo_sentido_is_secundario = documental_decisao_mesmo_sentido_is_secundario,
           doutrina_citada = documental_doutrina_texto,
           indexacao_texto = documental_indexacao_texto, observacao_texto = documental_observacao_texto,
           # Partes e Ministros
           relator_decisao_nome, relator_processo_nome, is_decisao_presidencia,
           presidente_nome, ministro_facet,
           partes = partes_lista_texto, objetivos_onu = ods_onu, plataforma_publicacao =documental_publicacao_lista_texto,
           #Url e API e Outros
           inteiro_teor_url, acompanhamento_processual_url,  base,
           termo_busca
           ) #%>%
    #mutate(across(legislacao_citada:))
}

stf_dir_minerario_mono_select <- stf_dir_minerario_mono_raw %>%
  stf_clean_cjsg_mono() %>%
  mutate(across(.cols= c(legislacao_citada, ministro_facet, objetivos_onu),
                ~purrr::map_chr(.x,
                                ~str_c(.x, collapse = "; ")
                                )
                )
  )


stf_minerac_mono_select <- stf_minerac_mono_raw %>%
  stf_clean_cjsg_mono() %>%
  mutate(across(.cols= c(legislacao_citada, ministro_facet, objetivos_onu),
                ~purrr::map_chr(.x,
                                ~str_c(.x, collapse = "; ")
                                )
                )
         )



# Classificação -----------------------------------------------------------

amazonia_legal <- c("AC - ACRE","AM - AMAZONAS", "AP - AMAPÁ",
"MA - MARANHÃO", "MT - MATO GROSSO", "PA - PARÁ","RO - RONDÔNIA",
"RR - RORAIMA","TO - TOCANTINS")

stf_amazon <- stf_dir_minerario_mono_select %>%
full_join(stf_minerac_mono_select) %>%
  #Filtrando pela Amazônia
  #filter(procedencia_geografica %in% amazonia_legal) %>%
  mutate(is_amazonia_legal= if_else( procedencia_geografica %in% amazonia_legal,
                                     "sim",
                                     "nao"),
                                    .after = procedencia_geografica ) %>%
  as_tibble()

# Tipo de Julgados
stf_amazon %>%
  group_by(is_amazonia_legal) %>%
  dplyr::count(classe_extenso) %>%
  dplyr::arrange(desc(n)) #%>% view()

# Origem  
stf_amazon %>%
  dplyr::count(procedencia_geografica) %>%
  dplyr::arrange(desc(n))

stf_amazon %>%
  dplyr::count(is_amazonia_legal) %>%
  dplyr::arrange(desc(n))

stf_amazon %>%
  #dplyr::select(classe, numero,
  #             registro, relator, decisao) %>%
  DT::datatable(extensions = "Responsive",
                filter = "top")


# Não deu certo
readr::write_excel_csv2( stf_amazon,
                     "stf/exports/stf_amazon2.csv",
                     eol= "\r\n")

#deu cerro                     
rio::export(stf_amazon,
            format = "xlsx")

# Acórdãos ----------------------------------------------------------------

# Acórdãos

#stf::stf_download_cjsg(search = "direito minerário",
#                      base = "acordaos",
#                      size = 10,
#                      dir = "stf/data_raw/dir_minerario/acordaos")

stf_dir_minerario_ac_raw <- tibble(stf::stf_read_cjsg( dir = "stf/data_raw/dir_minerario/acordaos"),
                                   termo_busca = "direito minerário")

# Acórdãos

#stf::stf_download_cjsg(search = "mineração",
#                      base = "acordaos",
#                      size = 10,
#                      dir = "stf/data_raw/mineracao/acordaos")

stf_minerac_ac_raw <- tibble(stf::stf_read_cjsg( dir = "stf/data_raw/mineracao/acordaos"),
                             termo_busca = "mineração")

# diretorio = "stf/data_raw/mineracao/acordaos")


# Codigo Velho ------------------------------------------------------------

# Acórdãos

stf_dir_minerario_ac_tidy <- stf_dir_minerario_ac_raw %>%
  relocate ( #Número e Classe dos Processos
    titulo, processo_codigo_completo,
    processo_classe_processual_unificada_classe_sigla, processo_numero,
    processo_classe_processual_unificada_incidente_sigla, #externo_seq_objeto_incidente,
    processo_classe_processual_unificada_extenso,
    #geografia
    procedencia_geografica_uf_sigla, procedencia_geografica_uf_extenso,
    procedencia_geografica_completo,
    procedencia_geografica_pais_sigla, procedencia_geografica_uf_sigla,
    #Datas
    publicacao_data, julgamento_data
  )


# Tidying de Acórdãos
stf_minerac_ac_tidy <- stf_minerac_ac_raw %>%
  relocate ( #Número e Classe dos Processos
    titulo, processo_codigo_completo,
    processo_classe_processual_unificada_classe_sigla, processo_numero,
    processo_classe_processual_unificada_incidente_sigla, #externo_seq_objeto_incidente,
    processo_classe_processual_unificada_extenso,
    #geografia
    procedencia_geografica_uf_sigla, procedencia_geografica_uf_extenso,
    procedencia_geografica_completo,
    procedencia_geografica_pais_sigla, procedencia_geografica_uf_sigla,
    #Datas
    publicacao_data, julgamento_data,
    #Documentos citados
    
  )

stf_minerac_ac_tidy %>%
  dplyr::count(processo_classe_processual_unificada_extenso) %>%
  arrange(desc(n))


stf_minerac_ac_order <- stf_minerac_ac_raw %>%
 relocate ( #Número e Classe dos Processos
   titulo, processo_codigo_completo,
   processo_classe_processual_unificada_classe_sigla, processo_numero,
   processo_classe_processual_unificada_incidente_sigla, #externo_seq_objeto_incidente,
   processo_classe_processual_unificada_extenso,
   # Geografia
   procedencia_geografica_uf_sigla, procedencia_geografica_uf_extenso,
   procedencia_geografica_completo,
   procedencia_geografica_pais_sigla, procedencia_geografica_uf_sigla,
   # Datas
   publicacao_data, julgamento_data,
   # Ministros
    relator_processo_nome, orgao_julgador, ministro_facet,
   relator_acordao_nome, revisor_processo_nome, 
   #Decisao
   acordao_ata,
   # Documentos Citados
   documental_legislacao_citada_texto,
   documental_doutrina_texto,
   documental_assunto_texto, documental_jurisprudencia_citada_texto,   
   documental_indexacao_texto, documental_observacao_texto,
   partes_lista_texto, ods_onu, documental_publicacao_lista_texto,
   #Url e API e Outros
   inteiro_teor_url, acompanhamento_processual_url, dje_url, base,
   id, dg_atualizado_em, dg_unique, 
 )
