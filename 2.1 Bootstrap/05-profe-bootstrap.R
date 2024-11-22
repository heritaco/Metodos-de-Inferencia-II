library(resampledata)
library(tidyverse)
data<-NCBirths2004
head(data)
# %>% pipe operator
# Sirve para aplicar una funcion a un dataframe
#Sin especificar el parametro de la base de datos

Y_smoker<-data %>% 
    filter(Smoker=="Yes") %>% 
    select(Weight) %>% pull()
X_non_smoker<-data %>% 
    filter(Smoker=="No") %>% 
    select(Weight) %>% pull()

#hAcer un resumen de los datos
resumen<-data %>% group_by(Smoker) %>% 
    summarise(mean=mean(Weight),sd=sd(Weight),n=n())

(th<-resumen$mean[1]-resumen$mean[2])
(g1<-resumen$sd[1]^2/resumen$n[1])
(g2<-resumen$sd[2]^2/resumen$n[2])
se<-sqrt(g1+g2)
df<-((g1+g2)^2)/((g1^2/(resumen$n[1]-1))+(g2^2/(resumen$n[2]-1)))
(t_obs<-th/se)
pt(t_obs,df=df,lower.tail = FALSE) #3.039807e-05


#Ejercicio de Clase
#a) Revisar normalidad de las muestras
ggplot(data,aes(sample=Weight))+
    geom_qq()+
    geom_qq_line()+
    facet_wrap(~Smoker)

ggplot(data,aes(x=Weight))+
    geom_histogram(aes(y=..density..))+
    facet_wrap(~Smoker)
#b)La prueba t es apropiada?
#Parece que si, porque las distribuciones parecen normales

#Lo que me dice el supuesto de normalidad es 
# la distribucion del estadiatico t es aproxamamente una t de student 
#Con los grados de libertad que calulamos

#c)Prueba de hipotesis bootstrap
#Eliminar la hipotesis de normalidad
#Cual es entonces la distribucion del estadistico t sin normalidad

#Es decir, voy a aproximar la distribucion del estadistico t
#Usando varios valores bootstrao de t

#Recordamos quien es T bajo la prueba clasica
#T=(theta_estimado-theta_0)/se ) #H_0: theta=theta_0
#Para T usamos el supuesto que la media es theta_0

#Para el bootstrap, suponemos que mi distribucion es la de los datos
#Es decir, el supuesto es que el theta_estimado es el real (el de los datos)
#Cuando calulamos T bootstrap
#T_bootstrap=(theta_estimado_bootstrap-theta_estimado)/se_bootstrap ) 

#theta=mu_x-mu_y
set.seed(123)
B<-10000
nx<-resumen$n[1]
ny<-resumen$n[2]

x_boot<-matrix(sample(X_non_smoker,nx*B,replace=TRUE),nx,B)
y_boot<-matrix(sample(Y_smoker,ny*B,replace=TRUE),ny,B)
theta_estimado_bootstrap<-colMeans(x_boot)-colMeans(y_boot)
#Calculamos se_bootstrap
g1_boot<-apply(x_boot,2,var)/nx
g2_boot<-apply(y_boot,2,var)/ny
se_boot<-sqrt(g1_boot+g2_boot)
T_bootstrap<-(theta_estimado_bootstrap-th)/se_boot

#Graficar la distribucion de T bootstrap y comparar con la distribucion teorica de T
T_bootstrap<-data.frame(t=T_bootstrap)

ggplot(T_bootstrap,aes(x=t))+
    geom_histogram(aes(y=..density..),bins=30)+
    geom_density(aes(y=..density..),col="red")+
    geom_vline(xintercept=t_obs,col="blue")+
    geom_function(fun=dt,args=list(df=df),
    aes(color="Dist Bajo Normalidad"),lwd=2)

#Calcular el p-valor
(p_val<-sum(T_bootstrap>=t_obs)/B)
