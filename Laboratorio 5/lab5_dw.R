# Laboratorio 5 Data Wrangling
# Gulio Valenzuela 20190336

# libreria
library(lubridate)
library(tidyverse)
library(readxl)
library(nycflights13)
library(ggplot2)

# Ejercicio 1

eclipse_usa <-  ymd_hms("2017 August 21th 18:26:40", tz ="UTC")
eclipse_usa
eclipse_nuevo <- eclipse_usa + days(21*223) + hours(12*223) + minutes(44*223) + seconds(3*223) 
eclipse_nuevo

# Ejercicio 2 

data <- read_excel("data.xlsx")
View(data)

data %>%
  data <- data %>% rename(fecha_creacion = `Fecha Creación`,
                          hora_creacion = `Hora Creación`,
                          caller_id = `Caller ID`,
                          fecha_final = `Fecha Final`,
                          hora_final = `Hora Final`)
fecha_buena <- grepl(pattern = '-', x = data$fecha_creacion)
data1 <- data[fecha_buena,]
data2 <- data[!fecha_buena,]
data2 <- data2 %>% 
  mutate(fecha_creacion = as.Date(as.numeric(fecha_creacion), origin="1899-12-30"),
         fecha_final = as.Date(as.numeric(fecha_final), origin="1899-12-30")) %>% 
  mutate(fecha_creacion = ydm(fecha_creacion),
         fecha_final = ydm(fecha_final))
data1 <- data1 %>% 
  mutate(fecha_creacion = dmy(fecha_creacion),
         fecha_final = dmy(fecha_final))
data <- rbind(data1,data2)
data$duracion <- difftime(time1 = data$hora_final, 
                          time2 = data$hora_creacion, 
                          units="mins")

# pregunta 1 
data$mes <- month(data$fecha_creacion)
por.mes <- data %>% group_by(mes, Cod) %>% 
  summarise(llamadas = n())
por.mes <- split(x = por.mes, f = por.mes$Cod)
por.mes <- lapply(por.mes, function(df) {df[order(df$llamadas, decreasing = T),]})
por.mes[[1]][1,]
por.mes[[2]][1,]
por.mes[[3]][1,]
por.mes[[4]][1,]
por.mes[[5]][1,]
por.mes[[6]][1,]
por.mes[[7]][1,]
#pregunta 2 
data$dia <- day(data$fecha_creacion)
por.dia <- data %>% group_by(dia) %>% summarise(llamadas=n())
por.dia[order(por.dia$llamadas, decreasing = T),][1,]
#pregunta 3
mensual <- data %>% group_by(mes) %>% summarise(llamadas=n())
mensual[order(mensual$llamadas, decreasing = T),][1,]
#pregunta 4 
g1 <- ggplot(data = mensual, mapping = aes(x=mes, y = llamadas))+
  geom_line()
g1
#pregunta 5
mean(data$duracion)

# Ejercicio 3

signo_zodiacal <- function(fecha_nac){
  
  fecha <- dmy(fecha_nac)
  mes <- month(fecha_nac)
  dia <- day(fecha_nac)
  
  if (mes==1){
    signo <- ifelse(test = dia<20,
                    yes = 'capricornio',
                    no = 'acuario')
  } else if(mes==2){
    signo <- ifelse(test = dia<19,
                    yes = 'acuario',
                    no = 'piscis')
  } else if(mes==3){
    signo <- ifelse(test = dia<21,
                    yes = 'piscis',
                    no = 'aries')
  } else if(mes==4){
    signo <- ifelse(test = dia<20,
                    yes = 'aries',
                    no = 'tauro')
  } else if(mes==5){
    signo <- ifelse(test = dia<21,
                    yes = 'tauro',
                    no = 'geminis')
  } else if(mes==6){
    signo <- ifelse(test = dia<21,
                    yes = 'geminis',
                    no = 'cancer')
  } else if(mes==7){
    signo <- ifelse(test = dia<23,
                    yes = 'cancer',
                    no = 'leo')
  } else if(mes==8){
    signo <- ifelse(test = dia<23,
                    yes = 'leo',
                    no = 'virgo')
  } else if(mes==9){
    signo <- ifelse(test = dia<23,
                    yes = 'virgo',
                    no = 'libra')
  } else if(mes==10){
    signo <- ifelse(test = dia<23,
                    yes = 'libra',
                    no = 'escorpio')
  } else if(mes==11){
    signo <- ifelse(test = dia<22,
                    yes = 'escorpio',
                    no = 'sagitario')
  } else if(mes==12){
    signo <- ifelse(test = dia<22,
                    yes = 'sagitario',
                    no = 'capricornio')
  }
  
  return(signo)
  
}
signo_zodiacal('12-10-1999')

# Ejercicio 5


flights$dep_time_new <- format(strptime(sprintf('%04d', 
                                                flights$dep_time), 
                                        format = '%H%M'),
                               '%H:%M')
flights$arr_time_new <- format(strptime(sprintf('%04d', 
                                                flights$arr_time), 
                                        format = '%H%M'),
                               '%H:%M')
flights$sched_dep_time_new <- format(strptime(sprintf('%04d', 
                                                      flights$sched_dep_time), 
                                              format = '%H%M'),
                                     '%H:%M')
flights$sched_arr_time_new <- format(strptime(sprintf('%04d', 
                                                      flights$sched_arr_time), 
                                              format = '%H%M'),
                                     '%H:%M')
head(flights)

flights$delay_total <- flights$dep_delay + flights$arr_delay