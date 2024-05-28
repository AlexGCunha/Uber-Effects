#------------------------------------------------------------------------------------------------
#This code will:
#- Input municipalities codes on initial dates sheet 
#- Merge populational data on 2014
#------------------------------------------------------------------------------------------------


library(tidyverse)
library(arrow)
library(readxl)
library(haven)
rm(list=ls())

data_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Data"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Output"

setwd(data_path)


#Read Initial entry DF
df <- read_excel("initial_dates_base.xlsx",sheet="032023")

#Create a variable of State-Munic and remove special characters
df <- df %>% 
  mutate(munic_aux = paste0(state,munic_name)) %>% 
  #Remove Special Characters
  mutate(munic_aux = str_replace_all(munic_aux,"[^a-zA-Z0-9]","")) %>% 
  #Upper case
  mutate(munic_aux = toupper(munic_aux))



#Read Munic codes data
df_munic <- read_excel("munics_2021.xlsx",sheet="munics")

#Create a variable of State-Munic and remove special characters
df_munic <- df_munic %>% 
  mutate(munic_aux = paste0(state,munic_name)) %>% 
  #Remove Special Characters
  mutate(munic_aux = str_replace_all(munic_aux,"[^a-zA-Z0-9]","")) %>% 
  #Upper case
  mutate(munic_aux = toupper(munic_aux)) %>% 
  select(munic_aux, munic)


#Join
df <- df %>% 
  left_join(df_munic, by="munic_aux",na_matches="never") %>% 
  select(-c(munic_aux))

#save
write_excel_csv(df, "initial_dates.csv")

