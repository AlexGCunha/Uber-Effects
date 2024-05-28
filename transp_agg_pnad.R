#------------------------------------------------------------------------------------------------
#This code will:
#- Read PNAD files (from datazoom) and create statistics for capitals
#------------------------------------------------------------------------------------------------


library(tidyverse)
library(arrow)
library(readxl)
library(haven)
rm(list=ls())

pnad_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/PNADC/pnadcontinua"
data_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Data"
output_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Output"


###########################################
#Open yearly files and save in one df
###########################################
setwd(pnad_path)
aux_count=0
for(y in (2012:2020)){
  archive = paste0("PNADC_trimestral_",y,".dta")
  df_aux <- read_dta(archive,
                     col_select = c("Ano",
                                    "Trimestre",
                                    "UF",
                                    "Capital",
                                    "RM_RIDE",
                                    "V1027",
                                    "V1028",
                                    "V2007",
                                    "V20082",
                                    "V2010",
                                    "V3002",
                                    "VD4001",
                                    "VD4002",
                                    "V4003",
                                    "V4019",
                                    "V4039",
                                    "V4039C",
                                    "V4043",
                                    "V4048",
                                    "V405012",
                                    "V405112",
                                    "V4056",
                                    "VD3004",
                                    "VD4005",
                                    "VD4009",
                                    "VD4010",
                                    "VD4012",
                                    "VD4016",
                                    "VD4017",
                                    "VD4019")
                     )
  
  #Rename variables
  df_aux <- df_aux %>% 
    rename(
      year=Ano,
      quarter=Trimestre,
      uf = UF,
      capital=Capital,
      rm=RM_RIDE,
      weight_no_post=V1027,
      weight_post=V1028,
      sexo=V2007,
      birth_year=V20082,
      race=V2010,
      in_school=V3002,
      pea=VD4001,
      occupied=VD4002,
      bico=V4003,
      cnpj=V4019,
      wk_hours_main_hab=V4039,
      wk_hours_main_ef=V4039C,
      position_sec=V4043,
      wk_card_sec=V4048,
      wage_sec_hab=V405012,
      wage_sec_ef=V405112,
      wk_hours_sec_hab=V4056,
      instruct_level=VD3004,
      desalento=VD4005,
      position_main=VD4009,
      activity_main=VD4010,
      prev_all=VD4012,
      wage_main_hab=VD4016,
      wage_main_ef=VD4017,
      wage_all_hab=VD4019
    )
  
  if(aux_count==0){
    df <- df_aux
    aux_count <- 1
  } else{
    df <- rbind(df, df_aux)
  }
  rm(df_aux)
  gc()
  print(y)
}

#Save
setwd(data_path)
write_parquet(df,"pnad_agg.parquet")
rm(list=ls())
gc()


