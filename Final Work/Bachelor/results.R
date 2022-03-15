library(fda)
library(fda.usc)
library(roahd)
library(mrfDepth)
library(dplyr)

workspace = 'C:/Users/David.solano/Documents/David/Artículo/Detección de Outliers (Organizado)'
setwd(paste0(workspace,'/code/Bootstrap Model'))

source('Datos Simulados.R')
source('Funciones Cuerpo.R')
source('Profundidades.R')
source('Bootstraps-Originales.R')
source('Bootstraps-dirOut.R')
source('Bootstrap-Outlier-Detection.R')
source('Calculo-Tasas.R')
source('DirOut.R')

#* Magnitude 

K <- c(10,15,20,25)
depths <- c(MBD,MD)

set.seed(1234)
for(i in 1:2){

  for(k in K){
    
    rates = tasas(0.8,k=k,dfunc=depths[i],method = Outlier.DirOut,M = 100,boot = MBBo.DirOut)
    vector = c(rates$pf,rates$pc,rates$sd,rates$pdc)
    names(vector) <- paste0(c('pf','pc','sd','pdc'),'-',k)
    
    if(k == K[1]) tabla_depth = t(vector)
    if(k != K[1]) tabla_depth = cbind(tabla_depth,t(vector))
    
  }
  
  if(i == 1) {tabla = tabla_depth; print('Primera Iteración Completa')}
  if(i != 1) tabla = rbind(tabla,tabla_depth)
  
}

row.names(tabla) <- c('MBD','MD')
tabla = tabla %>% data.frame()

setwd(paste0(workspace,'/outputs'))

write.csv2(tabla,'tabla-magnitude.csv',row.names = F)
write.csv2(tabla %>% 
             select(pf.10,pc.10,pf.15,pc.15,pf.20,pc.20,pf.25,pc.25),
           'tabla-magnitude-paper.csv',row.names = F)

#* Shape

K <- c(4,5,6,7)
depths <- c(MBD,MD)

set.seed(1234)
for(i in 1:2){
  
  for(k in K){
    
    rates = tasas(0.8,model = shape,k=k,dfunc=depths[i],method = Outlier.DirOut,M = 100,boot = MBBo.DirOut)
    vector = c(rates$pf,rates$pc,rates$sd,rates$pdc)
    names(vector) <- paste0(c('pf','pc','sd','pdc'),'-',k)
    
    if(k == K[1]) tabla_depth = t(vector)
    if(k != K[1]) tabla_depth = cbind(tabla_depth,t(vector))
    
  }
  
  if(i == 1) {tabla = tabla_depth; print('Primera Iteración Completa')}
  if(i != 1) tabla = rbind(tabla,tabla_depth)
  
}

row.names(tabla) <- c('MBD','MD')
tabla = tabla %>% data.frame()

#setwd(paste0(workspace,'/outputs'))

write.csv2(tabla,'tabla-shape.csv',row.names = F)
write.csv2(tabla %>% 
             select(pf.4,pc.4,pf.5,pc.5,pf.6,pc.6,pf.7,pc.7),
           'tabla-shape-paper.csv',row.names = F)

#* Partial

K <- c(10,15,20,25)
depths <- c(MBD,MD)

set.seed(1234)
for(i in 1:2){
  
  for(k in K){
    
    rates = tasas(0.8,model = shape,k=k,dfunc=depths[i],method = Outlier.DirOut,M = 100,boot = MBBo.DirOut)
    vector = c(rates$pf,rates$pc,rates$sd,rates$pdc)
    names(vector) <- paste0(c('pf','pc','sd','pdc'),'-',k)
    
    if(k == K[1]) tabla_depth = t(vector)
    if(k != K[1]) tabla_depth = cbind(tabla_depth,t(vector))
    
  }
  
  if(i == 1) {tabla = tabla_depth; print('Primera Iteración Completa')}
  if(i != 1) tabla = rbind(tabla,tabla_depth)
  
}

row.names(tabla) <- c('MBD','MD')
tabla = tabla %>% data.frame()

#setwd(paste0(workspace,'/outputs'))

write.csv2(tabla,'tabla-partial.csv',row.names = F)
write.csv2(tabla %>% 
             select(pf.10,pc.10,pf.15,pc.15,pf.20,pc.20,pf.25,pc.25),
           'tabla-partial-paper.csv',row.names = F)

#* Mixed

K = list(c(10,4,10),c(15,5,15),c(20,6,20),c(25,7,25))
depths <- c(MBD,MD)

set.seed(1234)
for(i in 2){
  
  for(k in 2:4){
    
    rate = rates(0.8,k1=K[[i]][1],k2=K[[i]][2],k3=K[[i]][3],
                  dfunc=depths[i],method = Outlier.DirOut,M = 100,boot = MBBo.DirOut)
    vector = c(rate$pf,rate$pc,rate$sd,rate$pdc)
    names(vector) <- paste0(c('pf','pc','sd','pdc'),'-',k)
    
    if(k == 1) tabla_depth = t(vector)
    if(k != 1) tabla_depth = cbind(tabla_depth,t(vector))
    
  }
  
  if(i == 1) {tabla = tabla_depth; print('Primera Iteración Completa')}
  if(i != 1) tabla = rbind(tabla,tabla_depth)
  
}

row.names(tabla) <- c('MBD','MD')
tabla = tabla %>% data.frame()

#setwd(paste0(workspace,'/outputs'))

write.csv2(tabla,'tabla-mixed.csv',row.names = F)
write.csv2(tabla %>% 
             select(pf.10,pc.10,pf.15,pc.15,pf.20,pc.20,pf.25,pc.25),
           'tabla-mixed-paper.csv',row.names = F)
