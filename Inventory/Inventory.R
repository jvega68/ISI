# Programa para simulación de inventario
# Basado en el modelo de Kelton & Law
# Jorge de la Vega
# Fecha: May 2024

## Lista de variables y definiciones
#
## parámetros de entrada:
# S = especificación de la política de inventario, S
# s = especificación de la política de inventario, s
# h = valordel costo del almacenamiento por unidad
# incrmc = i, costo incremental por unidad ordenada
# inv_inicial = nivel inicial del inventario
# maxlag = rezago de envío máximo
# mtd = media del tiempo entre demandas
# minlag = rezago de envío mínimo 
# nmeses = longitud de la simulación en meses
# num_politicas = número de políticas de inventario a considerarse
# nvalue = tamaño de la demanda máxima posible
# pi1  = valor del costo de atraso por unidad
# DD = vector de probabilidades de la demanda
# K = costo fijo de orden
#
# Variables del modelo
# aminus = área bajo la función I^-(t)
# Z = tamaño de orden en unidades  
# aplus = área bajo la función I^+(t)
# inv_nivel = I(t) nivel del inventario
# num_eventos = número de tipos de eventos para el modelo
# sig_tipo_evento = tipo de evento del siguiente evento a ocurrir (1, 2, 3 o 4)
# reloj = reloj de simulación
# tue = tiempo del último evento
# tne[j] = tiempo del siguiente evento de tipo j (j=1,2,3,4) 
# tordc = costo total de órdenes
# tdue = tiempo desde el último evento
#
# variables de salida
# acost = Costo promedio total mensual
# ahldc = Costo promedio de almacenamiento mensual
# aordc = Costo promedio de ordenar mensual
# ashrc = Costo promedio de demanda no satisfecha mensual


## Función principal
minv  <- function(){
  politica <<- list(c(20,40),c(20,60),c(20,80),c(20,100),c(40,60),c(40,80),c(40,100),c(60,80),c(60,100))
  inv_inicial <<- 60 
  nmeses <<- 120 
  
  DD <<- c(1/6,1/3,1/3,1/6) 
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
  
print("Sistema de inventario de un producto")
print(paste("Nivel inicial de inventario:",inv_inicial,sep=" "))
print(paste("tamaños de demanda:",ndemandas,sep=" "))
print(paste("Función de probabilidad de la demanda:",DD,sep=" "))
print(paste("Tiempo interdemanda promedio:",mtd,sep=" "))
print(paste("Rango de rezago en el envío: uniforme","(",minlag,",",maxlag,") meses",sep=""))
print(paste("Longitd de la simulación:",nmeses,"meses",sep=" "))
print(paste("K=",K,", i = ",incrmc,", h=",h,", pi1= ", pi1,sep=" "))
print(paste("Número de políticas:",num_politicas,sep=" "))

  for(i in 1:num_politicas){
    #lee la política a considerar (s,S)
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
             "3" = {reporte()})
      if(sig_tipo_evento ==3) break else next
    }  
  }  
}

  
inicializa <- function(){
  reloj <<- 0 #inicializa el reloj de simulación
  #inicializa variables de estado
  inv_nivel <<- inv_inicial  
  tue <<- 0
  #inicializa los contadores estadísticos
  tordc <<- 0
  ahldc <<- 0
  ashrc <<- 0
  #inicializa la lista de eventos. No se considera llegada de orden porque no hay orden pendiente
  tse <<- c(1e30,reloj+rexp(1,mtd),nmeses,0)
}


llegada_orden <- function(){
  inv_nivel <<- inv_nivel + Z
  #como no hay orden pendiente, se elimina el evento de llegada de orden
  tse[1] <<- 1e30
}

demanda <- function(){
  #decrementa el inventario de acuerdo a la distribición de los tamaños de demanda
  inv_nivel <<- inv_nivel - sample(x=1:4,1,prob=DD)
  #programa el tiempo de la siguiente demanda
  tse[2] <<- reloj + rexp(1,mtd)
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
  print(paste( "(",s,S,")",
                round(costo_prom_orden + costo_prom_almacen + costo_prom_dns,2),
                round(costo_prom_orden,2),
                round(costo_prom_almacen,2),
                round(costo_prom_dns,2),sep="  "))
}


actualiza_estadisticas_tiempo_promedio <- function(){
  tdue  <<- reloj - tue
  tue <<- reloj
  #determina el status del nivel del inventario durante el intervalo previo
  #Si fue negativo, actualiza el área de demanda no satisfecha. Si es positivo
  #actualiza el area de almacenamiento. Si es cero, no se hace nada
  if(inv_nivel <0) ashrc <<- ashrc - inv_nivel*tdue else if(inv_nivel > 0) ahldc <<- ahldc + inv_nivel*tdue
}


tiempo <- function(){
  assign("min_tiempo_sig_evento", 1e29, envir = .GlobalEnv)
  assign("sig_tipo_evento", 0, envir = .GlobalEnv)
  #Determina el tipo de evento del siguiente evento a ocurrir
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