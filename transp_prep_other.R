#------------------------------------------------------------------------------------------------
#This code will:
# - Work on alternative datasets and merge
# - run after transp_prep_rais
#-----------------------------------------------------s-------------------------------------------



library(tidyverse)
library(haven)
library(readxl)
library(arrow)
library(dtplyr)

rm(list=ls())
data_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Data"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Output"

setwd(data_path)
a = Sys.time()

df <- read_parquet("after_rais.parquet")

#########################################
#SIM
#########################################

df_sim <- read_parquet("SIM.parquet") %>% 
  rename(id_municipio = id_municipio_ocorrencia) %>% 
  mutate(id_municipio = as.numeric(id_municipio))

#Create monthly values of homicides per municipality
df_sim <- lazy_dt(df_sim) %>% 
  filter(circunstancia_obito=="3") %>% 
  mutate(year = format(data_obito,"%Y")) %>% 
  mutate(month = format(data_obito,"%m")) %>% 
  mutate(quarter = case_when(month %in% c("01","02","03")~1,
                             month %in% c("04","05","06")~2,
                             month %in% c("07","08","09")~3,
                             month %in% c("10","11","12")~4,
                             T~NA_real_)) %>%
  mutate(year_quarter = paste0(year,quarter)) %>% 
  mutate(semester = case_when(quarter %in% c(1,2)~1,
                              quarter %in% c(3,4)~2,
                              T~NA_real_)) %>% 
  mutate(aux=1) %>% 
  group_by(id_municipio,year_quarter) %>% 
  summarise(year = first(year),
            semester = first(semester),
            quarter = first(quarter),
            homicides = sum(aux),
            homicides_f = sum(aux[sexo=="2"]),
            homicides_m = sum(aux[sexo=="1"])) %>% 
  mutate_at(c("homicides_f","homicides_m"),~ifelse(is.na(.),0,.)) %>% 
  ungroup() %>% 
  as.data.frame()

#Create year statistics
df_sim <-df_sim %>% 
  group_by(year,id_municipio) %>% 
  mutate(homicides_y = sum(homicides),
         homicides_f_y = sum(homicides_f),
         homicides_m_y = sum(homicides_m)) %>% 
  ungroup()

#Create quarter statistics
df_sim <-df_sim %>% 
  mutate(year_sem = paste0(year, semester)) %>% 
  group_by(year_sem,id_municipio) %>% 
  mutate(homicides_s = sum(homicides),
         homicides_f_s = sum(homicides_f),
         homicides_m_s = sum(homicides_m)) %>% 
  ungroup()

#drop some columns
df_sim <- df_sim %>% 
  select(!c(year,semester, quarter, year_sem))


#Merge
df <- df %>% 
  left_join(df_sim,by=c("id_municipio","year_quarter"),na_matches="never")
rm(df_sim)

df<-df %>% 
  mutate_at(c("homicides","homicides_f","homicides_m",
              "homicides_y","homicides_f_y","homicides_m_y",
              "homicides_s","homicides_f_s","homicides_m_s"),~ifelse(is.na(.),0,.))

#Metrics over Population
df <- df %>% 
  mutate(homicides_p = homicides/populacao*100000) %>% 
  mutate(homicides_p_f = homicides_f/populacao*100000) %>% 
  mutate(homicides_p_m = homicides_m/populacao*100000) %>% 
  mutate(homicides_p_y = homicides_y/populacao*100000) %>% 
  mutate(homicides_p_f_y = homicides_f_y/populacao*100000) %>% 
  mutate(homicides_p_m_y = homicides_m_y/populacao*100000) %>% 
  mutate(homicides_p_s = homicides_s/populacao*100000) %>% 
  mutate(homicides_p_f_s = homicides_f_s/populacao*100000) %>% 
  mutate(homicides_p_m_s = homicides_m_s/populacao*100000) %>% 
  mutate(lhomicides = log(homicides)) %>% 
  mutate(lhomicides_s = log(homicides_s)) %>% 
  mutate(lhomicides_y = log(homicides_y))


#########################################
#Microrregions
#########################################
df_mmc <- read_excel("dtb_2015.xlsx")
df_mmc <- df_mmc %>% 
  select(municipio_completo, mmc) %>% 
  rename(id_municipio = municipio_completo) %>% 
  mutate(id_municipio = as.numeric(id_municipio))

df <- df %>% 
  left_join(df_mmc, by="id_municipio",na_matches="never")
rm(df_mmc)


#########################################
#Deflator
#########################################
df_def <- read_excel("ipeadata_inpc.xlsx") %>% 
  mutate(month = substr(date, nchar(date)-1,nchar(date))) %>% 
  mutate(year = substr(date,1,4)) %>%
  filter(month %in% c("03", "06", "09", "12")) %>% 
  mutate(quarter = case_when(month == "03"~1,
                             month=="06"~2,
                             month=="09"~3,
                             month=="12"~4,
                             T~NA_real_)) %>% 
  mutate(year_quarter = paste0(year,quarter)) %>% 
  mutate(deflator_201412 = inpc[year_quarter=="20144"]/inpc) %>% 
  select(year_quarter, inpc, deflator_201412)

df <- df %>% 
  left_join(df_def, by="year_quarter",na_matches="never")

#########################################
#CAGED
#########################################
df_caged <- read_parquet("CAGED_mod.parquet")

df_caged <- lazy_dt(df_caged)
#Group by instruction level and transform into quarterly data
df_caged <- df_caged %>% 
  mutate(grau_instrucao = as.integer(grau_instrucao)) %>% 
  mutate(instruct_level = case_when(grau_instrucao < 7 ~"lths",
                                   grau_instrucao == 7 ~ 'hs',
                                   grau_instrucao >7 ~"mths",
                                   T~ NA_character_)) %>%
  mutate(quarter = case_when(mes %in% c(1,2,3) ~ 1,
                             mes %in% c(4,5,6) ~ 2,
                             mes %in% c(7,8,9) ~ 3,
                             mes %in% c(10,11,12) ~ 4,
                             T ~ NA_real_)) %>% 
  mutate(year_quarter = paste0(ano, quarter)) %>% 
  mutate(wages = ifelse(wages<=0.01,NA_real_,wages)) %>% 
  group_by(year_quarter,id_municipio,instruct_level, saldo_movimentacao) %>% 
  summarise(wage= sum(wages*saldo, na.rm=T)/sum(saldo, na.rm=T),
            saldo = sum(saldo, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(movimentacao = case_when(saldo_movimentacao==1 ~ "hire",
                                  saldo_movimentacao==-1 ~ "fire",
                                  T ~NA_character_)) %>% 
  select(!saldo_movimentacao) %>% 
  mutate(wage = ifelse(wage<=0.01,NA_real_,wage)) %>% 
  as.data.frame()

#Create a balanced panel 
munics <- unique(df_caged$id_municipio)
dates <- unique(df_caged$year_quarter)
instruct  <- unique(df_caged$instruct_level)
mov  <- unique(df_caged$movimentacao)

df_aux <- expand.grid(id_municipio = munics, year_quarter = dates, instruct_level = instruct, movimentacao = mov) %>% 
  arrange(id_municipio, year_quarter , instruct_level , movimentacao)

df_aux <- df_aux %>% 
  left_join(df_caged, by=c("id_municipio", "year_quarter" , "instruct_level" , "movimentacao"),na_matches="never")

df_aux <- df_aux %>% 
  mutate(saldo = ifelse(is.na(saldo),0,saldo)) %>% 
  mutate(wage = ifelse(saldo==0,NA_real_,wage))

df_caged <- df_aux
rm(df_aux)
gc()

#Create totals per municipality and date
df_aux <- lazy_dt(df_caged) %>%
  group_by(year_quarter,id_municipio, movimentacao) %>%
  summarise(wage= sum(wage*saldo, na.rm=T)/sum(saldo, na.rm=T),
            saldo = sum(saldo, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(wage = ifelse(wage<=0.01,NA_real_,wage)) %>% 
  mutate(instruct_level = "all") %>% 
  select(colnames(df_caged)) %>% 
  as.data.frame()

df_caged <- rbind(df_caged, df_aux)  
rm(df_aux)
gc()

#Create Saldo(admissions - layoffs) per municipality, date and educational level
df_aux <- lazy_dt(df_caged) %>%
  group_by(year_quarter,id_municipio, instruct_level) %>%
  summarise(wage= NA_real_,
            saldo = sum(saldo[movimentacao=="hire"], na.rm=T)-sum(saldo[movimentacao=="fire"], na.rm=T)) %>% 
  ungroup() %>% 
  mutate(movimentacao = "net") %>% 
  select(colnames(df_caged)) %>% 
  as.data.frame()

df_caged <- rbind(df_caged, df_aux)  
rm(df_aux)
gc()

#Create semester and year variables
df_caged <- lazy_dt(df_caged) %>% 
  mutate(quarter = substr(year_quarter,5,5)) %>% 
  mutate(year = substr(year_quarter,1,4)) %>% 
  mutate(semester = case_when(quarter <= 2 ~1,
                              quarter >2 ~2,
                              T ~NA_real_)) %>% 
  #By semester
  group_by(year, semester,id_municipio,instruct_level, movimentacao) %>% 
  mutate(wage_s = sum(wage*saldo, na.rm=T)/sum(saldo, na.rm=T)) %>% 
  mutate(saldo_s = sum(saldo, na.rm=T)) %>% 
  mutate(wage_s = ifelse(wage_s<=0.01,NA_real_,wage_s)) %>% 
  ungroup() %>%
  #By year
  group_by(year,id_municipio,instruct_level, movimentacao) %>% 
  mutate(wage_y = sum(wage*saldo, na.rm=T)/sum(saldo, na.rm=T)) %>% 
  mutate(saldo_y = sum(saldo, na.rm=T)) %>% 
  mutate(wage_y = ifelse(wage_y<=0.01,NA_real_,wage_y)) %>% 
  ungroup() %>% 
  as.data.frame()


#Merge with deflator df
df_caged <- df_caged %>% 
  left_join(df_def, by="year_quarter",na_matches="never") %>% 
  mutate_at(c("wage","wage_s","wage_y"),~.*deflator_201412)
#rm(df_def)
#gc()

#Create variables in ratios and logs
#as an auxiliary, lets take the population of each municipality
df_aux <- df %>% 
  filter(year_quarter=="20154") %>% 
  select(id_municipio, populacao)

df_caged <- df_caged %>% 
  mutate(id_municipio = as.numeric(id_municipio)) %>% 
  left_join(df_aux, by="id_municipio", na_matches="never")
rm(df_aux)
gc()

#Metrics over population
df_caged <- df_caged %>% 
  mutate(saldo_p = saldo/populacao) %>% 
  mutate(saldo_p_s = saldo_s/populacao) %>% 
  mutate(saldo_p_y = saldo_y/populacao) %>% 
  select(!c(populacao, quarter, year, semester))

#Logs
df_caged <- lazy_dt(df_caged) %>% 
  mutate(lwage = ifelse(saldo<=0,NA_real_, log(wage))) %>% 
  mutate(lwage_s = ifelse(saldo_s<=0,NA_real_, log(wage_s))) %>% 
  mutate(lwage_y = ifelse(saldo_y<=0,NA_real_, log(wage_y))) %>% 
  mutate(lsaldo = ifelse(saldo<=0,NA_real_, log(saldo))) %>% 
  mutate(lsaldo_s = ifelse(saldo_s<=0,NA_real_, log(saldo_s))) %>%  
  mutate(lsaldo_y = ifelse(saldo_y<=0,NA_real_, log(saldo_y)))

df_caged <- df_caged %>% 
  pivot_wider(id_cols = c("year_quarter", "id_municipio"),
              names_from = c("instruct_level","movimentacao"),
              values_from = c(starts_with("saldo"), starts_with("lsaldo"), starts_with("lwage"))) %>% 
  select(!c(contains("_NA_"),
            starts_with("saldo_s"), 
            starts_with("saldo_y"), 
            (starts_with("wage_")& ends_with("net")),
            (starts_with("lsaldo_")& ends_with("net")),
            (starts_with("wage_")& ends_with("net")),
            (starts_with("lwage_")& ends_with("net"))
            )
         ) %>% 
  arrange(id_municipio, year_quarter) %>% 
  as.data.frame()


#Merge
df <- df %>% 
  left_join(df_caged,by=c("year_quarter","id_municipio"),na_matches="never")
rm(df_caged)
gc()



#########################################
#Number of vehicles
#########################################
df_ve <- read_parquet("FROTA.parquet")

df_ve <- df_ve %>% 
  rename(automobile = automovel, tot_vehicle = total) %>% 
  mutate(id_municipio = as.numeric(id_municipio)) %>% 
  mutate(quarter = case_when(mes %in% c(1,2,3)~1,
                             mes %in% c(4,5,6)~2,
                             mes %in% c(7,8,9)~3,
                             mes %in% c(10,11,12)~4,
                             T~NA_real_,)) %>% 
  filter(mes %in% c(3,6,9,12)) %>% 
  mutate(year_quarter = paste0(ano,quarter)) %>% 
  select(id_municipio, year_quarter, automobile, tot_vehicle) %>% 
  #For some reason, some municipalities have two values for total of vehicles
  group_by(id_municipio, year_quarter) %>% 
  summarise_at(c("automobile", "tot_vehicle"),~max(.)) %>% 
  ungroup()
  
df <- df %>% 
  left_join(df_ve, by=c("id_municipio","year_quarter"),na_matches="never")
rm(df_ve)

#Create ratios
df <- df %>% 
  mutate(automobile_p = automobile/populacao) %>% 
  mutate(tot_vehicle_p = tot_vehicle/populacao) %>% 
  mutate(lautomobile = log(automobile)) %>% 
  mutate(ltot_vehicle = log(tot_vehicle))



#########################################
#Tax collection
#########################################
df_rec <- read_parquet("tax_collection.parquet")
df_rec <- df_rec %>% 
  mutate(year_quarter = paste0(year,"4")) %>% 
  select(!c(year)) %>% 
  rename(id_municipio = munic) %>% 
  mutate(id_municipio = as.numeric(id_municipio)) %>% 
  #filter some problematic municipalities
  filter(!is.na(gps)&!is.na(darf)&gps>0&darf>0) %>% 
  group_by(id_municipio, year_quarter) %>% 
  summarise_all(max) %>% 
  ungroup()

#Merge
df <- df %>% 
  left_join(df_rec, by=c("id_municipio","year_quarter"),na_matches = "never") %>% 
  mutate_at(c("gps","darf","total_collection"),~.*deflator_201412) %>% 
  mutate(lgps = log(gps)) %>% 
  mutate(ldarf = log(darf)) %>% 
  mutate(ltotal_collection = log(total_collection)) %>% 
  mutate(gps_p = gps/populacao) %>% 
  mutate(darf_p = darf/populacao) %>% 
  mutate(total_collection_p = total_collection/populacao)

rm(df_rec)


#########################################
#GDP Municipality
#########################################
df_gdp<- read_parquet("pib_mun.parquet") %>% 
  mutate(id_municipio = as.numeric(id_municipio)) %>% 
  mutate(year_quarter = paste0(ano,"4")) %>% 
  select(!ano)

#Merge
df <- df %>% 
  left_join(df_gdp,by=c("year_quarter","id_municipio"),na_matches="never")
rm(df_gdp)

df <- df %>% 
  mutate_at(c("pib","impostos_liquidos","va","va_agropecuaria","va_industria","va_servicos","va_adespss"),~.*deflator_201412) %>% 
  mutate(pib_p = pib/populacao) %>% 
  mutate(imposto_p = impostos_liquidos/populacao) %>% 
  mutate(va_p = va/populacao) %>% 
  mutate(va_ind_p = va_industria/populacao) %>% 
  mutate(va_agro_p = va_agropecuaria/populacao) %>% 
  mutate(va_serv_p = va_servicos/populacao) %>% 
  mutate(va_otherserv_p = va_adespss/populacao)


#########################################
#Distances
#########################################
df_dist <- read_parquet("Distances_munic.parquet") %>% 
  select(1:2) %>% 
  rename(id_municipio = munic) %>% 
  mutate(id_municipio = as.numeric(id_municipio)) %>% 
  filter(!is.na(dist_min))

df <- df %>% 
  left_join(df_dist, by="id_municipio",na_matches="never")
rm(df_dist)


#########################################
#MEI
#########################################
df_mei <- read_parquet("cnpj_mei.parquet") %>% 
  mutate(id_municipio = as.numeric(id_municipio)) %>% 
  rename(count_mei = f0_) %>% 
  filter(!is.na(id_municipio))

df_mei <- df_mei %>% 
  mutate(month = format(data_opcao_mei,"%m")) %>% 
  mutate(year = format(data_opcao_mei,"%Y")) %>% 
  mutate(quarter = case_when(month %in% c("01","02","03")~1,
                             month %in% c("04","05","06")~2,
                             month %in% c("07","08","09")~3,
                             month %in% c("10","11","12")~4,
                             T~NA_real_)) %>% 
  mutate(year_quarter = paste0(year, quarter)) %>% 
  group_by(id_municipio, year_quarter) %>% 
  summarise(open_mei = sum(count_mei, na.rm=T)) %>% 
  ungroup()


#########################################
#Final adjustments and save
#########################################
df <- df %>% 
  mutate_at(c("wage_hired", "wage_avg", "wage_lths", "wage_hs", "wage_mths"), ~.*deflator_201412) %>% 
  mutate(lwage_hired = log(wage_hired)) %>% 
  mutate(lwage = log(wage_avg)) %>% 
  mutate(lwage_lths = log(wage_lths)) %>% 
  mutate(lwage_hs=log(wage_hs)) %>% 
  mutate(lwage_mths=log(wage_mths))

df <- df %>% 
  group_by(id_municipio) %>% 
  mutate(pib_p_14 = pib[ano==2014 & mes ==12]/populacao[ano==2014 & mes ==12]) %>% 
  mutate(populacao_14 =populacao[ano==2014 & mes ==12] ) %>% 
  ungroup()


write_parquet(df,"merged_month_munic.parquet")
b = Sys.time()
print(b-a)

rm(list=ls())
gc()

