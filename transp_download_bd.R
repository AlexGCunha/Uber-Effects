#------------------------------------------------------------------------------------------------
#This code will download Data from Base dos Dados
#------------------------------------------------------------------------------------------------

library(basedosdados)
library(tidyverse)
library(arrow)
library(readxl)
library(beepr)
rm(list=ls())

#Define WD
data_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Data"

setwd(data_path)
#Define project
set_billing_id("ra-ubi-informality")

#############################
#RAIS
#############################
for (ano in 2013:2012){
  ano = as.character(ano)
  
  query_a <- "SELECT ano,
  trimestre,
  id_uf,
  capital,
  rm_ride,
  V1022 as situacao_domicilio,
  V1027 as peso_sem_extrat,
  V1028 as peso_com_extrat,
  V2005 as codicao_domicilio,
  V2007 as sexo,
  V20082 as ano_nasc,
  
  
            
            FROM basedosdados.br_ibge_pnadc.microdados
"
  
  query_b <- paste0('WHERE (ano = ',ano, ' AND sigla_uf = ',sQuote(estado,options(useFancyQuotes = F)),')')

  query_final = paste(query_a, query_b, sep=" ")
  
  write_parquet(read_sql(query_final),paste0("rais_",ano,estado,".parquet"))
  
  gc()
  
  
  print(ano) 
  beep()

}


#############################
#DataSUS
#############################
#Dictionary
query <- "SELECT *
  FROM basedosdados.br_ms_sim.dicionario
"
  
  
write_excel_csv(read_sql(query),"SIM_dict.csv")
gc()
beep()
  
#Mortality data

query <- "SELECT ano,
data_obito,
causa_basica,
circunstancia_obito,
idade,
sexo,
raca_cor,
id_municipio_ocorrencia,
FROM basedosdados.br_ms_sim.microdados
WHERE ano IN (2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

"


write_parquet(read_sql(query),"SIM.parquet")
gc()
beep()


#############################
#CAGED
#############################

query <- "SELECT ano, mes, id_municipio, grau_instrucao, saldo_movimentacao, count(saldo_movimentacao) as saldo, avg(salario_mensal) as wages
FROM `basedosdados.br_me_caged.microdados_antigos` 
WHERE ano IN (2010,2011,2012,2013,2014,2015,2016,2017,2018,2019) AND indicador_aprendiz = '0'

GROUP BY ano, mes, id_municipio, grau_instrucao,saldo_movimentacao


"


write_parquet(read_sql(query),"CAGED_mod.parquet")
gc()
beep()


#############################
#Frota de Veículos
#############################

query <- "SELECT sigla_uf,id_municipio, ano, mes,automovel, total
FROM `basedosdados.br_denatran_frota.municipio_tipo` 
WHERE ano IN (2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
"
write_parquet(read_sql(query),"FROTA.parquet")
gc()
beep()



#############################
#ESTBAN
#############################
for (ano in 2011:2019){
  ano = as.character(ano)
  print(Sys.time())
  
  query_a <- "SELECT ano,
  mes,
  id_municipio,
  cnpj_basico,
  instituicao,
  id_verbete,
  valor
  FROM basedosdados.br_bcb_estban.municipio
"
  
  query_b <- paste0('WHERE (ano = ',ano, ')')
  
  query_final = paste(query_a, query_b, sep=" ")
  
  write_parquet(read_sql(query_final),paste0("estban_",ano,".parquet"))
  
  gc()
  
  
  print(ano)
  beep()
  
}


#############################
#PIB Municipal
#############################

query <- "SELECT *
FROM basedosdados.br_ibge_pib.municipio 
WHERE ano IN (2005,2006,2007, 2008, 2009, 2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
"
write_parquet(read_sql(query),"pib_mun.parquet")
gc()
beep()


#############################
#CNPJ/MEI
#############################

query <- "SELECT id_municipio, data_opcao_mei, data_exclusao_mei, COUNT(id_municipio) FROM
	(SELECT cnpj_basico, data_opcao_mei, data_exclusao_mei 
	FROM `basedosdados.br_me_cnpj.simples`
	)  a
LEFT JOIN 
	(SELECT cnpj_basico, id_municipio 
	FROM `basedosdados.br_me_cnpj.estabelecimentos`
	)  b
ON a.cnpj_basico = b.cnpj_basico
WHERE a. data_opcao_mei IS NOT NULL
GROUP BY id_municipio, data_opcao_mei, data_exclusao_mei
"
write_parquet(read_sql(query),"cnpj_mei.parquet")
gc()
beep()

