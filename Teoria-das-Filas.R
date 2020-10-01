###### Joziani Mota Vieira


# Pacotes
rm(list = ls())
if(!require(queuecomputer)){install.packages("queuecomputer");require(queuecomputer)}
if(!require(ggplot2)){install.packages("ggplot2");require(ggplot2)}


# Taxa de chegada
l = c(1/10, 1/3, 1/5, 1/8, 1/8, 1/5, 1/3)

# Taxa de Serviço
mu = 1/5

# Duração da simulação
duration = 60

time=0

# Chegadas antes do banco abrir
T1 = 0
while(time<duration){
  T1 = c(T1,ceiling(rexp(1, rate = l[1])))
  time <- sum(T1)
}
T1=T1[-length(T1)]

time <- sum(T1)

nEvents = length(T1) # número total de eventos ocorridos

antes=nEvents #número de pessoas que chegaram antes do banco abrir

#simulando as chegadas nas proximas 6 horas
for (i in 2:7) {
  while(time<duration*i){
    T1 = c(T1,(ceiling(rexp(1, rate = l[i]))))
    time <- sum(T1)
  }
  T1=T1[-length(T1)]
}
T1=T1[-length(T1)]
T1

time <- sum(T1)
time

nEvents = length(T1)
nEvents

#simulando os atendimentos
atend = ceiling(rexp(1, rate = mu))
for (i in 2:nEvents) {
  atend=c(atend,ceiling(rexp(1, rate = mu)))
}
atend

atend_antes=c(atend[1:antes])
atend_antes

#Simulação de Chegadas
print("Simulação de Chegadas")
cat("Chegada 1:" ,T1[1], "minutos", "\n")
for (i in 2:nEvents) {
  a=i-1
  cat("Chegada" , i,":" ,T1[i], "minutos","após chegada" ,a, "\n")
}

#Simulação de Atendimentos
print("Simulação de Atendimentos")
cat("Atendimento 1: " ,atend[1], "minutos", "\n")
for (i in 2:nEvents) cat("Atendimento" ,i,":" ,atend[i], "minutos","\n")

filas <- queue_step(arrivals=T1, service=atend, servers=2)

filas
summary(filas)


## Gráficos

## parcelas de densidade dos horários de chegada e partida
plot(filas, which = 1)

## histogramas dos horários de chegada e partida
plot(filas, which = 2)

## gráficos de densidade de tempos de espera e sistema
plot(filas, which = 3)

## função da etapa do comprimento da fila
plot(filas, which = 4)

## gráfico de intervalo de linhas do status do cliente e do servidor
plot(filas, which = 5)

## gráfico de distribuição empírica dos horários de chegada e partida
plot(filas, which = 6)


