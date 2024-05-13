# Program to simulate an inventory
# Based on the model of Kelton & Law
# Jorge de la Vega
# Date: May 2024

## List of variables and definitions
#
## input parameters:
# S = Lim sup of policy (s,S)
# s = Lim inf of policy (s,S)
# h = cost storage by unit
# incrmc = i, incremental cost per unit
# inv_inicial = initial inventory level 
# maxlag = max backlog
# mtd = mean time between demands
# minlag = min backlog 
# nmeses = length simulation in months
# num_politicas = number of policies to consider
# nvalue = max possible demand
# pi1  = backlog cost per unit
# DD = vector of demands
# K = order fix cost 
#
## Model variables
# aminus = área under function I^-(t)
# Z = order size in units 
# aplus = área under function I^+(t)
# inv_nivel = I(t) inventory level at t
# num_eventos = number of event types in the model
# sig_tipo_evento = type of next event to ocurr (1, 2, 3 o 4)
# reloj = simulation clock
# tue = time of last event
# tne[j] = time of next event of type j (j=1,2,3,4) 
# tordc = total cost orders
# tdue = time since last event
#
## Output variables
# acost = Total monthly Average Cost
# ahldc = Average monthly Cost of storage
# aordc = Average monthly Cost of ordering
# ashrc = Average monthly Cost of backlog


## Función principal
minv  <- function(prin = F){
  politica <<- list(c(20,40), 
                    c(20,60),
                    c(20,80), 
                    c(20,100), 
                    c(40,60),
                    c(40,80),
                    c(40,100),
                    c(60,80),
                    c(60,100))
   
  nmeses = 120
  inv_inicial = 60
  DD <<- c(1/6, 1/3, 1/3, 1/6) 
  ndemandas <<- length(DD) 
  mtd <<- 10
  K <<- 32
  incrmc <<- 3
  h <<- 1
  pi1 <<- 5 
  minlag <<- 0.5 
  maxlag <<- 1
  num_politicas <<-  length(politica)
  num_eventos <<- 4 # especifica el número de eventos para la función tiempo()
if(prin){  
    print("Inventory system of one product")
    print(paste("initial level of Inventory:", inv_inicial, sep = " "))
    print(paste("Demands sizes:", ndemandas, sep=" "))
    # print(paste("Probability function of demand:", DD[1:4], sep = " "))
    print(paste("Interdemand average time:", mtd, sep = " "))
    print(paste("Range of lagged delivery: uniform","(", minlag,", ", maxlag, ") months", sep = ""))
    print(paste("Length of simulation:", nmeses, "months", sep = " "))
    print(paste("K=",K,", i = ", incrmc,", h= ",h,", pi1 = ", pi1, sep = " "))
    print(paste("Number of policies:",num_politicas, sep = " "))
}
Resumen <- data.frame()
for(i in 1:num_politicas){
    # lee la política a considerar (s,S)
    s <<- politica[[i]][1] 
    S <<- politica[[i]][2]
    inicializa()
    repeat{
      tiempo()
      actualiza_estadisticas_tiempo_promedio()
      switch(as.character(sig_tipo_evento),
             "1" = {llegada_orden()},
             "2" = {demanda()},
             "4" = {evalua()},
             "3" = {b <- reporte()
                    Resumen <- rbind(Resumen,b)})
      
      if(sig_tipo_evento == 3) break else next
    }  
  }
 return(Resumen)
}


  
inicializa <- function(){
  reloj <<- 0  # inicializa el reloj de simulación
  # inicializa variables de estado
  inv_nivel <<- inv_inicial  
  tue <<- 0
  # inicializa los contadores estadísticos
  tordc <<- 0
  ahldc <<- 0
  ashrc <<- 0
  # inicializa la lista de eventos. No se considera llegada de orden porque no hay orden pendiente
  tse <<- c(1e30, reloj + rexp(1,mtd), nmeses,0)
}


llegada_orden <- function(){
  inv_nivel <<- inv_nivel + Z
  # como no hay orden pendiente, se elimina el evento de llegada de orden
  tse[1] <<- 1e30
}

demanda <- function(){
  # decrementa el inventario de acuerdo a la distribición de los tamaños de demanda
  inv_nivel <<- inv_nivel - sample(x = 1:4, 1, prob = DD)
  # programa el tiempo de la siguiente demanda
  tse[2] <<- reloj + rexp(1, mtd)
}

evalua <- function(){
  if(inv_nivel < s){
    Z <<- S - inv_nivel #pide orden
    tordc <<- tordc + K + incrmc*Z # incrementa el costo de ordenar
    tse[1] <<- reloj + runif(1,minlag,maxlag) # programa la llegada de la orden
  }
  #ahora programa la siguiente evaluación del inventario
 tse[4] <<- reloj + 1 
}

##La siguiente función calcula y reporta las medidas especificadas de desempeño del sistema
reporte <- function(){
  costo_prom_orden <<- tordc/nmeses
  costo_prom_almacen <<- h*ahldc/nmeses
  costo_prom_dns <<- pi1*ashrc/nmeses
#  print(paste0( "(",s," ",S,") ",
#                round(costo_prom_orden + costo_prom_almacen + costo_prom_dns,2)," ",
#                round(costo_prom_orden,2)," ",
#                round(costo_prom_almacen,2)," ",
#                round(costo_prom_dns,2)))
  results <- data.frame(
             policy_s = s,
             policy_S = S,
             total_cost = round(costo_prom_orden + costo_prom_almacen + costo_prom_dns,2),
             average_order_cost = round(costo_prom_orden,2),
             average_stock_cost = round(costo_prom_almacen,2),
             average_backlog_cost = round(costo_prom_dns,2))
  return(results)
}


actualiza_estadisticas_tiempo_promedio <- function(){
  tdue  <<- reloj - tue
  tue <<- reloj
  #determina el status del nivel del inventario durante el intervalo previo
  #Si fue negativo, actualiza el área de demanda no satisfecha. Si es positivo
  #actualiza el area de almacenamiento. Si es cero, no se hace nada
  if(inv_nivel < 0) 
    ashrc <<- ashrc - inv_nivel*tdue 
  else if(inv_nivel > 0) 
    ahldc <<- ahldc + inv_nivel*tdue
}


tiempo <- function(){
  assign("min_tiempo_sig_evento", 1e29, envir = .GlobalEnv)
  assign("sig_tipo_evento", 0, envir = .GlobalEnv)
  # Determina el tipo de evento del siguiente evento a ocurrir
  for(i in 1:num_eventos){
    if( tse[i] < min_tiempo_sig_evento ){
      assign("min_tiempo_sig_evento", tse[i], envir = .GlobalEnv)
      assign("sig_tipo_evento", i, envir = .GlobalEnv)
    }
  }
  #verifica si la lista de eventos está vacía
  if(sig_tipo_evento == 0) 
    stop(print(paste("La lista de eventos está vacía en el tiempo:", reloj,sep=" ")))
  
  #La lista de eventos no está vacía, avanza el reloj de simulación
  assign("reloj", min_tiempo_sig_evento, envir = .GlobalEnv)
}




