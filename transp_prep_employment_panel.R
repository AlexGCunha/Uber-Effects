#------------------------------------------------------------------------------------------------
#This code will:
#- Read RAIS files and create a monthly panel of employment
#------------------------------------------------------------------------------------------------


library(tidyverse)
library(arrow)
library(readxl)
library(dtplyr)
library(data.table)
rm(list=ls())

rais_path = "/Users/xande/OneDrive/Documentos/Doutorado/Research/RAIS"
data_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Data"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Output"


###########################################
#Create panel of employment per municipality
###########################################
uf_vector <- c("SP","BA","MG","AC","AL","AP","AM","CE","ES","GO","MA","MT",
               "MS","PA","PB","PR","PE","PI","RJ","RN","RS",
               "RO","RR","SC","SE","TO","DF")


aux_count = 0
for (i in (2005:2009)){
  print(Sys.time())
  for(month in (1:12)){
    for(uf in uf_vector){
      setwd(rais_path)
      archive <- paste0("rais_",i,uf,".parquet")
      df_aux <- read_parquet(archive,
                             col_select = c("id_municipio",
                                            "mes_desligamento",
                                            "mes_admissao",
                                            "idade",
                                            "grau_instrucao_apos_2005",
                                            "sexo",
                                            "tempo_emprego",
                                            "raca_cor",
                                            "tamanho_estabelecimento",
                                            "tipo_vinculo",
                                            "valor_remuneracao_dezembro")) 
      gc()
      lazy_aux <- lazy_dt(df_aux)
      
      lazy_aux <- lazy_aux %>% 
        #Correct 2006
        mutate(mes_admissao_aux = case_when(is.na(mes_desligamento) & tempo_emprego > 12 ~NA_integer_,
                                            is.na(mes_desligamento) & tempo_emprego == 0 ~ mes_admissao,
                                            is.na(mes_desligamento) & tempo_emprego <=12 ~ mes_admissao,
                                            (!is.na(mes_desligamento)) &  tempo_emprego <=mes_desligamento ~ mes_admissao, 
                                            T ~NA_integer_))
      if (i==2006){lazy_aux <- lazy_aux %>% mutate(mes_admissao = mes_admissao_aux) }
      
      lazy_aux <- lazy_aux %>% 
        #Define if that match was active in the month
        mutate(worked = case_when(is.na(mes_admissao) & is.na(mes_desligamento) ~ 1, #not hired nor fired that year
                                  is.na(mes_admissao) & mes_desligamento > month ~ 1, #not hired that year but fired after the month
                                  mes_admissao <=month & mes_desligamento >month ~ 1, #hired that year before the month and fired after the month
                                  mes_admissao <=month & is.na(mes_desligamento) ~ 1, #hired before the month and not fired
                                  T ~0)) %>% 
        mutate(fired = case_when(mes_desligamento==month~1,T~0))
      #Define wages (note that wages are only available for december)
      if(month==12){
        lazy_aux <- lazy_aux %>% 
          mutate(wage = valor_remuneracao_dezembro)
      }else {
        lazy_aux <- lazy_aux %>% 
          mutate(wage = NA_real_)
      }
      lazy_aux <- lazy_aux %>% 
        #Correct time of employment for each month (only for current workers)
        mutate(employment_time = case_when(worked == 1 & is.na(mes_desligamento)~tempo_emprego - (12-month),
                                           worked ==1 & !is.na(mes_desligamento) ~ tempo_emprego - (mes_desligamento-month),
                                           T ~NA_real_)) %>% 
        #define temporary, public and private workers with a fixed contract
        mutate(temporary = ifelse(tipo_vinculo %in% c(50,60,65,70,75,90,95,96,97),1,0)) %>% 
        mutate(public = ifelse(tipo_vinculo %in% c(30,31,35),1,0)) %>% 
        mutate(fix = ifelse(!(tipo_vinculo %in% c(30,31,35,50,60,65,70,75,90,95,96,97)),1,0)) %>% 
        group_by(id_municipio) %>% 
        summarise(emprego = sum(worked[fix==1 & worked==1], na.rm=T),
                  emprego_public = sum(worked[public==1& worked==1], na.rm=T),
                  wage_hired = sum(wage[worked==1 & fix==1 & employment_time <=1], na.rm=T)/sum(worked[worked==1 & fix==1 & employment_time <=1], na.rm=T),
                  wage_lths = sum(wage[worked==1 & fix==1 & grau_instrucao_apos_2005 <7], na.rm = T)/sum(worked[worked==1 & fix==1 & grau_instrucao_apos_2005 <7], na.rm=T),
                  wage_hs = sum(wage[worked==1 & fix==1 & grau_instrucao_apos_2005 ==7], na.rm = T)/sum(worked[worked==1 & fix==1 & grau_instrucao_apos_2005 ==7], na.rm=T),
                  wage_mths = sum(wage[worked==1 & fix==1 & grau_instrucao_apos_2005 >7], na.rm = T)/sum(worked[worked==1 & fix==1 & grau_instrucao_apos_2005 >7], na.rm=T),
                  wage_avg = sum(wage[worked==1 & fix==1], na.rm = T)/sum(worked[worked==1 & fix==1], na.rm=T),
                  tenure_worked = sum(employment_time[fix==1& worked==1], na.rm=T)/sum(worked[fix==1& worked==1], na.rm=T),
                  tenure_fired = sum(tempo_emprego[fix==1& fired==1], na.rm=T)/sum(fired[fix==1 & fired==1], na.rm=T),
                  brancos = sum(worked[raca_cor==2&fix==1& worked==1], na.rm=T),
                  homens = sum(worked[sexo==1&fix==1& worked==1], na.rm=T),
                  fundamental_incompleto = sum(worked[grau_instrucao_apos_2005<5&fix==1& worked==1], na.rm=T),
                  fundamental_completo = sum(worked[grau_instrucao_apos_2005==5&fix==1& worked==1], na.rm=T),
                  medio_incompleto = sum(worked[grau_instrucao_apos_2005 ==6&fix==1& worked==1], na.rm=T),
                  medio_completo = sum(worked[grau_instrucao_apos_2005 ==7&fix==1& worked==1], na.rm=T),
                  superior_incompleto = sum(worked[grau_instrucao_apos_2005 ==8&fix==1& worked==1], na.rm=T),
                  superior_completo = sum(worked[grau_instrucao_apos_2005 >=9&fix==1& worked==1], na.rm=T),
                  mais40 = sum(worked[idade>=40&fix==1& worked==1], na.rm=T),
                  #Quebras de raça e sexo por educação
                  brancos_lths = sum(worked[raca_cor==2&fix==1& worked==1&grau_instrucao_apos_2005<7], na.rm=T),
                  brancos_hs = sum(worked[raca_cor==2&fix==1& worked==1&grau_instrucao_apos_2005==7], na.rm=T),
                  brancos_mths = sum(worked[raca_cor==2&fix==1& worked==1&grau_instrucao_apos_2005>7], na.rm=T),
                  homens_lths = sum(worked[sexo==1&fix==1& worked==1 & grau_instrucao_apos_2005<7], na.rm=T),
                  homens_hs = sum(worked[sexo==1&fix==1& worked==1 & grau_instrucao_apos_2005==7], na.rm=T),
                  homens_mths = sum(worked[sexo==1&fix==1& worked==1 & grau_instrucao_apos_2005>7], na.rm=T)
        ) %>% 
        ungroup() %>% 
        mutate(ano = i) %>% 
        mutate(mes = month) %>% 
        as.data.frame()
      gc() 
      df_aux <- lazy_aux
      
      if(aux_count ==0){
        df <- df_aux
        aux_count <- 1
      }
      else{
        df <- rbind(df,df_aux)
      }
      rm(df_aux)
      gc()
    }
    
  }
  print(i)
}

setwd(data_path)
write_parquet(df, "rais_treated.parquet")
rm(list=ls())
gc()


