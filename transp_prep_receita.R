#------------------------------------------------------------------------------------------------
#This code will:
# - Work on arrecadacao dataset
#------------------------------------------------------------------------------------------------

library(tidyverse)
library(haven)
library(readxl)
library(arrow)
library(stringi)

rm(list=ls())
data_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Data"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Output"

setwd(data_path)

#########################################
#DARF
#########################################
df_darf <- read_excel("arrecadacao.xlsx",sheet="Darf")

#Pivot Longer and get municipalities IDS
df_darf <-df_darf %>% 
  pivot_longer(3:ncol(df_darf), names_to ="year", values_to = "darf") %>% 
  mutate(state = case_when(UF =="AM"~ "Amazonas",
                           UF =="PA"~ "Para",
                           UF =="AC"~ "Acre",
                           UF =="TO"~ "Tocantins",
                           UF =="AP"~ "Amapa",
                           UF =="RO"~ "Rondonia",
                           UF =="RR"~ "Roraima",
                           UF =="MA"~ "Maranhao",
                           UF =="PI"~ "Piaui",
                           UF =="CE"~ "Ceara",
                           UF =="RN"~ "Rio Grande do Norte",
                           UF =="PB"~ "Paraiba",
                           UF =="PE"~ "Pernambuco",
                           UF =="BA"~ "Bahia",
                           UF =="SE"~ "Sergipe",
                           UF =="AL"~ "Alagoas",
                           UF =="MT"~ "Mato Grosso",
                           UF =="MS"~ "Mato Grosso do Sul",
                           UF =="GO"~ "Goias",
                           UF =="RJ"~ "Rio de Janeiro",
                           UF =="SP"~ "Sao Paulo",
                           UF =="ES"~ "Espirito Santo",
                           UF =="MG"~ "Minas Gerais",
                           UF =="PR"~ "Parana",
                           UF =="SC"~ "Santa Catarina",
                           UF =="RS"~ "Rio Grande do Sul",
                           UF =="DF"~ "Distrito Federal")) %>% 
  mutate(munic_aux = paste0(state,MUNICIPIOS)) %>% 
  #Remove Special Characters
  mutate(munic_aux = stri_trans_general(str=munic_aux, id="Latin-ASCII")) %>%
  mutate(munic_aux = gsub("[[:punct:]]", "",munic_aux)) %>% 
  mutate(munic_aux = str_replace_all(munic_aux,"[^a-zA-Z0-9]","")) %>% 
  #Upper case
  mutate(munic_aux = toupper(munic_aux)) %>% 
  filter(UF!="EX") %>% #excluding revenues from abroad (exterior)
  select(-c(MUNICIPIOS,UF, state))


#Read Munic codes data
df_munic <- read_excel("munics_2021.xlsx",sheet="munics")

#Create a variable of State-Munic and remove special characters
df_munic <- df_munic %>% 
  mutate(munic_aux = paste0(state,munic_name)) %>% 
  #Remove Special Characters
  mutate(munic_aux = stri_trans_general(str=munic_aux, id="Latin-ASCII")) %>% 
  mutate(munic_aux = gsub("[[:punct:]]", "",munic_aux)) %>% 
  mutate(munic_aux = str_replace_all(munic_aux,"[^a-zA-Z0-9]","")) %>% 
  #Upper case
  mutate(munic_aux = toupper(munic_aux)) %>% 
  select(munic_aux, munic)


#Join
df_darf <- df_darf %>% 
  left_join(df_munic, by="munic_aux",na_matches="never") %>% 
  filter(!is.na(munic)) %>% 
  select(!munic_aux)


#########################################
#GPS
#########################################
df_gps <- read_excel("arrecadacao.xlsx",sheet="GPS")

#Pivot Longer and get municipalities IDS
df_gps <-df_gps %>% 
  pivot_longer(3:ncol(df_gps), names_to ="year", values_to = "gps") %>% 
  mutate(state = case_when(UF =="AM"~ "Amazonas",
                           UF =="PA"~ "Para",
                           UF =="AC"~ "Acre",
                           UF =="TO"~ "Tocantins",
                           UF =="AP"~ "Amapa",
                           UF =="RO"~ "Rondonia",
                           UF =="RR"~ "Roraima",
                           UF =="MA"~ "Maranhao",
                           UF =="PI"~ "Piaui",
                           UF =="CE"~ "Ceara",
                           UF =="RN"~ "Rio Grande do Norte",
                           UF =="PB"~ "Paraiba",
                           UF =="PE"~ "Pernambuco",
                           UF =="BA"~ "Bahia",
                           UF =="SE"~ "Sergipe",
                           UF =="AL"~ "Alagoas",
                           UF =="MT"~ "Mato Grosso",
                           UF =="MS"~ "Mato Grosso do Sul",
                           UF =="GO"~ "Goias",
                           UF =="RJ"~ "Rio de Janeiro",
                           UF =="SP"~ "Sao Paulo",
                           UF =="ES"~ "Espirito Santo",
                           UF =="MG"~ "Minas Gerais",
                           UF =="PR"~ "Parana",
                           UF =="SC"~ "Santa Catarina",
                           UF =="RS"~ "Rio Grande do Sul",
                           UF =="DF"~ "Distrito Federal")) %>% 
  mutate(munic_aux = paste0(state,MUNICIPIOS)) %>% 
  #Remove Special Characters
  mutate(munic_aux = stri_trans_general(str=munic_aux, id="Latin-ASCII")) %>%
  mutate(munic_aux = gsub("[[:punct:]]", "",munic_aux)) %>% 
  mutate(munic_aux = str_replace_all(munic_aux,"[^a-zA-Z0-9]","")) %>% 
  #Upper case
  mutate(munic_aux = toupper(munic_aux)) %>% 
  filter(UF!="EX") %>% #excluding revenues from abroad (exterior)
  select(-c(MUNICIPIOS,UF, state))


#Read Munic codes data
df_munic <- read_excel("munics_2021.xlsx",sheet="munics")

#Create a variable of State-Munic and remove special characters
df_munic <- df_munic %>% 
  mutate(munic_aux = paste0(state,munic_name)) %>% 
  #Remove Special Characters
  mutate(munic_aux = stri_trans_general(str=munic_aux, id="Latin-ASCII")) %>% 
  mutate(munic_aux = gsub("[[:punct:]]", "",munic_aux)) %>% 
  mutate(munic_aux = str_replace_all(munic_aux,"[^a-zA-Z0-9]","")) %>% 
  #Upper case
  mutate(munic_aux = toupper(munic_aux)) %>% 
  select(munic_aux, munic)


#Join
df_gps <- df_gps %>% 
  left_join(df_munic, by="munic_aux",na_matches="never") %>% 
  filter(!is.na(munic)) %>% 
  select(!munic_aux)


#########################################
#Merge and save
#########################################
df <- left_join(df_darf, df_gps, by=c("year","munic"), na_matches="never")

df <- df %>% 
  mutate(total_collection = gps+darf)

#save
setwd(data_path)
write_parquet(df,"tax_collection.parquet")
rm(list=ls())
  
