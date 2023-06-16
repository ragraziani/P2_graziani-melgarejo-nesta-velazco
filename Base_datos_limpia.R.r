#Cargar las bibliotecas

library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx)

#Leer el archivo de Excel y leer hoja "Estadísitcas clave"

key_results <- read_excel("UEFA Champion's League 2022-2023 Estadistica por clubes.xlsx",
                         sheet = "Estadísticas clave")


#Leer el archivo de Excel y leer hoja "Goles"

goals <- read_excel("UEFA Champion's League 2022-2023 Estadistica por clubes.xlsx",
                          sheet = "Goles")

#Leer el archivo de Excel y leer hoja "Intentos de goles"

attempts <- read_excel("UEFA Champion's League 2022-2023 Estadistica por clubes.xlsx",
                       sheet = "Intentos de goles")


#Leer el archivo de Excel y leer hoja "Distribución"

distribution <- read_excel("UEFA Champion's League 2022-2023 Estadistica por clubes.xlsx",
                           sheet = "Distribución")


#Leer el archivo de Excel y leer hoja "Ataque"

attacking <- read_excel("UEFA Champion's League 2022-2023 Estadistica por clubes.xlsx",
                           sheet = "Ataque")

#Leer el archivo de Excel y leer hoja "Defensa"

defense <- read_excel("UEFA Champion's League 2022-2023 Estadistica por clubes.xlsx",
                        sheet = "Defensa")

#Leer el archivo de Excel y leer hoja "Portería"

goalkeeping <- read_excel("UEFA Champion's League 2022-2023 Estadistica por clubes.xlsx",
                          sheet = "Portería")

#Limpieza de las bases de datos

  #Key results

key_results <- na.omit(key_results)

key_results <- key_results %>% 
  rename(Club = "UEFA Champion's League 2022/2023") %>% 
  rename(Partidos_Jugados = "...2") %>% 
  rename(Ganados = "...3") %>% 
  rename(Empates = "...4") %>% 
  rename(Perdidos = "...5")

key_results <- key_results %>%
  filter(Club %in% c("Man City", "Inter", "Real Madrid", "Milan", "Bayern", "Napoli","Benfica","Chelsea",
                       "Liverpool", "Paris","Porto","Leipzig","Dortmund", "Tottenham","Club Brugge","Frankfurt"))

  #Goles

goals <- na.omit(goals)

goals <- goals %>% 
  rename(Club = "UEFA Champion's League 2022/2023") %>% 
  rename(Goles = "...2") %>% 
  select(Club, Goles)

goals <- goals %>%
  filter(Club %in% c("Man City", "Inter", "Real Madrid", "Milan", "Bayern", "Napoli","Benfica","Chelsea",
                       "Liverpool", "Paris","Porto","Leipzig","Dortmund", "Tottenham","Club Brugge","Frankfurt"))

  #Intentos de goles

attempts <- na.omit(attempts)

attempts <- attempts %>%
  rename(Club = "UEFA Champion's League 2022/2023") %>% 
  rename(Total_intentos = "...2") %>% 
  rename(Intentos_a_puerta = "...3") %>%
  rename(Intentos_fuera_de_puerta = "...4") %>% 
  select(Club, Total_intentos, Intentos_a_puerta, Intentos_fuera_de_puerta) %>% 
  filter(Club %in% c("Man City", "Inter", "Real Madrid", "Milan", "Bayern", "Napoli","Benfica","Chelsea",
                     "Liverpool", "Paris","Porto","Leipzig","Dortmund", "Tottenham","Club Brugge","Frankfurt"))

  #Distribución

distribution <- na.omit(distribution)

distribution <- distribution %>%
  rename(Club = "UEFA Champion's League 2022/2023") %>% 
  rename(`Precisión_pases(%)` = "...2") %>% 
  rename(`Pases_intentados` = "...3") %>%
  rename(Pases_completados = "...4") %>% 
  rename(`Posesión(%)` = "...5") %>%
  select(Club, `Precisión_pases(%)`, `Pases_intentados`, Pases_completados, `Posesión(%)`) %>% 
  filter(Club %in% c("Man City", "Inter", "Real Madrid", "Milan", "Bayern", "Napoli","Benfica","Chelsea",
                     "Liverpool", "Paris","Porto","Leipzig","Dortmund", "Tottenham","Club Brugge","Frankfurt"))

  #Ataque

attacking <- na.omit(attacking)

attacking <- attacking %>%
  rename(Club = "UEFA Champion's League 2022/2023") %>% 
  rename(Ataques = "...2") %>% 
  rename(Asistencias = "...3") %>%
  rename(Dribles = "...6") %>% 
  select(Club, Ataques, Asistencias, Dribles) %>% 
  filter(Club %in% c("Man City", "Inter", "Real Madrid", "Milan", "Bayern", "Napoli","Benfica","Chelsea",
                     "Liverpool", "Paris","Porto","Leipzig","Dortmund", "Tottenham","Club Brugge","Frankfurt"))


  #Defensa

defense <- na.omit(defense)

defense <- defense %>%
  rename(Club = "UEFA Champion's League 2022/2023") %>% 
  rename(Balones_recuperados = "...2") %>% 
  rename(Tacleadas = "...3") %>%
  rename(Tacleadas_ganadas = "...4") %>% 
  rename(Tacleadas_perdidas = "...5") %>%
  rename(Despejes_intentados = "...6") %>%
  select(Club, Balones_recuperados, Tacleadas, Tacleadas_ganadas, Tacleadas_perdidas, Despejes_intentados) %>% 
  filter(Club %in% c("Man City", "Inter", "Real Madrid", "Milan", "Bayern", "Napoli","Benfica","Chelsea",
                     "Liverpool", "Paris","Porto","Leipzig","Dortmund", "Tottenham","Club Brugge","Frankfurt"))


  #Portería

goalkeeping <- na.omit(goalkeeping)

goalkeeping <- goalkeeping %>%
  rename(Club = "UEFA Champion's League 2022/2023") %>% 
  rename(Salvadas = "...2") %>% 
  rename(Goles_concedidos = "...3") %>%
  rename(Salvadas_penalti = "...5") %>% 
  rename(Porterías_limpias = "...6") %>%
  select(Club, Salvadas, Goles_concedidos, Salvadas_penalti, Porterías_limpias) %>% 
  filter(Club %in% c("Man City", "Inter", "Real Madrid", "Milan", "Bayern", "Napoli","Benfica","Chelsea",
                     "Liverpool", "Paris","Porto","Leipzig","Dortmund", "Tottenham","Club Brugge","Frankfurt"))



#Unir todas las tablas

all <- key_results %>%
  left_join(goals, by = "Club") %>%
  left_join(attacking, by = "Club") %>%
  left_join(attempts, by = "Club") %>%
  left_join(distribution, by = "Club") %>%
  left_join(defense, by = "Club") %>%
  left_join(goalkeeping, by = "Club")


#Convertir las columnas que contengan números a tipo numérico

all[,2:25] <- lapply(all[,2:25], as.numeric)


# Guardar el data frame como un archivo Excel

write.xlsx(all, "Top 16 Championes League 22 y 23.xlsx")



