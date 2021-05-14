# Práctica Probabilidad y Estadística II
# Autores:
#   - Marcos Chamorro Casillas
#   - Arturo Peñas Mohedano
#   - Álvaro Adanez Huecas
#   - Ionel Constantin  Trifan

# Cargamos la seed que necesitamos para la práctica
set.seed(2021)

# Cargamos la tabla con los datos

datos <- read.csv(file = "PYE2DataSet47.csv")


# Mostramos algunos valores de ella
head(datos)


## Parte 1.1:  Identificación de Modelo y Muestreo


sleeptime <- sample(datos$sleeptime,200) # Muestras de tamaño 200

steps <- sample(datos$steps, 200) # Muestras de tamaño 200

# Antes que todo instalamos el paquete necesario para algunas representaciones
install.packages("e1071")

# Activar el paquete
library(e1071)

# Debemos hacer con las variables: sleeptime y steps



# Vamos a empezar con la variable SLEEPTIME
# -------------------------------------------------

# Calculamos las medias de sleeptime
summary(sleeptime)

# Histograma con los datos de sleeptime
hist(sleeptime, main = "sleeptime")

# Calculamos el skewness
skewness(sleeptime) # Del resultado podemos concluir que tiene un podo de desviación hacia la derecha

# Calculamos la kurtosis
kurtosis(sleeptime) # La kurtosis difiere bastante de 0 por lo cual la distribución de sleeptime no se parece a una normal

# Calculamos el boxplot de sleeptime
boxplot(sleeptime, main = "sleeptime")

# Para calcular la distribución fit debemos instalar el paquete MASS
install.packages("MASS")

# Lo activamos
library(MASS)

# Lo debemos usar para parecerse a las distribuciones: Normal, Exponencial y Gamma

# Normal:
fitdistr(sleeptime, c("normal"))
# La media y la desviación típica tendrían los valores:

#       Media: 9.3860940   
# Desv.típica: 4.8795351 
# CONCLUSIÓN: Valores lejanos de 0 --> No se parece a una normal

# Gamma:
fitdistr(sleeptime, c("gamma"))
# shape         rate   
# 3.48100367    0.37086832 
# (0.33276383) (0.03813724)

# Exponencial:
fitdistr(sleeptime, c("exponential"))
# El valor de lambda es: 0.106540591  


# Calculamos la densidad de sleeptime
density(sleeptime)
# RESULTADOS:
# x                y            
# Min.   :-3.552   Min.   :3.442e-05  
# 1st Qu.: 3.725   1st Qu.:6.696e-03  
# Median :11.001   Median :2.651e-02  
# Mean   :11.001   Mean   :3.432e-02  
# 3rd Qu.:18.278   3rd Qu.:5.960e-02  
# Max.   :25.554   Max.   :8.849e-02  

# Hacemos el test de Kolmogorov
ks.test(sleeptime, pnorm, mean(sleeptime), sd(sleeptime))
# Lo que nos dice
# es que si p-value es menor que 0.05 va a representar la hipótesis de que los valores
# vienen de una distribución normal

# RESULTADOS:
# data:  sleeptime
# D = 0.10616, p-value = 0.02204
# alternative hypothesis: two-sided
# ----------------------------------------


# Vamos a hacer lo mismo con la variable STEPS
#-------------------------------------------------

# Calculamos las medias de sleeptime
summary(steps)

# Histograma con los datos de sleeptime
hist(steps, main = "steps")

# Calculamos el skewness
skewness(steps) # Del resultado podemos concluir que tiene un podo de desviación hacia la izquierda

# Calculamos la kurtosis
kurtosis(steps) # La kurtosis difiere bastante de 0 por lo cual la distribución de steps no se parece a una normal

# Calculamos el boxplot de steps
boxplot(steps, main = "steps")


# Usamos la distribución fit para ver si se parece a las distribuciones: Normal, Exponencial y Gamma

# Normal:
fitdistr(steps, c("normal"))
# La media y la desviación típica tendrían los valores:

#       Media: 11403.70607           
# Desv.típica: 1381.44905   
# CONCLUSIÓN: Valores lejanos de 0 --> No se parece a una normal

# Gamma:
fitdistr(steps, c("gamma"))
# Shape:  6.780212e+01
# Rate:   5.945865e-03 

# Exponencial:
fitdistr(steps, c("exponential"))
# El valor de lambda es:   8.769079e-05
 


# Calculamos la densidad de steps
density(steps)
# RESULTADOS:
# x               y            
# Min.   : 7334   Min.   :1.007e-07  
# 1st Qu.: 9255   1st Qu.:3.485e-05  
# Median :11177   Median :1.028e-04  
# Mean   :11177   Mean   :1.300e-04  
# 3rd Qu.:13098   3rd Qu.:2.054e-04  
# Max.   :15019   Max.   :4.097e-04

# Hacemos el test de Kolmogorov
ks.test(steps, pnorm, mean(steps), sd(steps))
# Lo que nos dice
# es que si p-value es menor que 0.05 va a representar la hipótesis de que los valores
# vienen de una distribución normal

# RESULTADOS:
# data:  steps
# D = 0.14685, p-value = 0.0003587
# alternative hypothesis: two-sided
# ----------------------------------------

# ---------------------------------------


## Parte 1.2:  Identificación de Modelo y Muestreo

# Para esto debemos crear lista de vectores de tamaños 30, 50 y 100 para guardar los valores del atributo Age 
# de la tabra datos

EdadT30 <- vector(mode = "list", length = 30) # Para las 30 muestras

EdadT50 <- vector(mode = "list", length = 50) # Para las 50 muestras

EdadT100 <- vector(mode = "list", length = 100) # Para las 100 muestras

i <- 1 # Variable para recorrer los bucles while

# Ahora debemos escribir las listas de vectores con los valores de la tabla datos para Age

# Tamaño 30
while(i < 31)
{
  EdadT30[[i]] <- sample(datos$Age, size = 200)
  i <- i + 1
}

i <- 1

# Tamaño 50
while(i < 51)
{
  EdadT50[[i]] <- sample(datos$Age, size = 200)
  i <- i + 1
}

i <- 1

# Tamaño 100
while(i < 101)
{
  EdadT100[[i]] <- sample(datos$Age, size = 200)
  i <- i + 1
}

# Ya hemos hecho el muestreo de las edades con los tamaños de las listas

# Debemos calcular las medias muestrales de las 3 listas de vectores de Age

MediaAge30 <- vector(length = 30) # Vector de tamaño 30

MediaAge50 <- vector(length = 50) # Vector de tamaño 50

MediaAge100 <- vector(length = 100) # Vector de tamaño 100

i <- 1 # Para recorrer las listas y hacer las medias muestrales

# Lista de 30
while(i < 31)
{
  MediaAge30[i] <- mean(EdadT30[[i]]) 
  i <- i + 1 
}

i <- 1

# Lista de 50
while(i < 51)
{
  MediaAge50[i] <- mean(EdadT50[[i]])
  i <- i + 1
}

i <- 1

# Lista de 100
while(i < 101)
{
  MediaAge100[i] <- mean(EdadT100[[i]])
  i <- i + 1
}

# Ya tenemos las medias muestrales de las distintas listas: 30, 50 y 100


# Para las de tamaño 30

hist(MediaAge30, main = "Lista de 30")

boxplot(MediaAge30, main = "Lista de 30")

fitdistr((MediaAge30), c("normal"))

# mean           sd     
# 29.07437323    0.22682088 
# ( 0.04141164) ( 0.02928245)

#-------------------------------

# Para las de tamaño 50

hist(MediaAge50, main = "Lista de 50")

boxplot(MediaAge50, main = "Lista de 50")

fitdistr((MediaAge50), c("normal"))

# mean           sd     
# 29.04243771    0.24861621 
# ( 0.03515964) ( 0.02486162)

# ------------------------------

# Para las de tamaño 100

hist(MediaAge100, main = "Lista de 100")

boxplot(MediaAge100, main = "Lista de 100")

fitdistr((MediaAge100), c("normal"))

# mean           sd     
# 29.01645978    0.20293871 
# ( 0.02029387) ( 0.01434993)


# Ahora tenemos que hacer lo mismo pero para las varianza muestral
# Nos hacemos 3 listar para poder almacenar las varianzas

VarAge30 <- vector(length = 30) # Vector de tamaño 30

VarAge50 <- vector(length = 50) # Vector de tamaño 50

VarAge100 <- vector(length = 100) # Vector de tamaño 100

i <- 1 # Para recorrer las listas y hacer las varianzas muestrales


# Lista de 30
while(i < 31)
{
  VarAge30[i] <- var(EdadT30[[i]]) 
  i <- i + 1 
}

i <- 1

# Lista de 50
while(i < 51)
{
  VarAge50[i] <- var(EdadT50[[i]])
  i <- i + 1
}

i <- 1

# Lista de 100
while(i < 101)
{
  VarAge100[i] <- var(EdadT100[[i]])
  i <- i + 1
}

# Ya tenemos las distintas varianzas muestrales para las listas de 30, 50 y 100

# Lista de 30 varianzas muestrales

hist(VarAge30, main = "Lista de 30")

boxplot(VarAge30, main = "Lista de 30")

fitdistr((VarAge30), c("normal"))

# mean          sd    
# 10.7316176    0.9690967 
# ( 0.1769320) ( 0.1251098)


# Listas de 50 varianzas muestrales

hist(VarAge50, main = "Lista de 50")

boxplot(VarAge50, main = "Lista de 50")

fitdistr((VarAge50), c("normal"))

# mean           sd     
# 10.63896821    0.87183533 
# ( 0.12329614) ( 0.08718353)


# Listas de 100 varianzas muestrales

hist(VarAge100, main = "Lista de 100")

boxplot(VarAge100, main = "Lista de 100")

fitdistr((VarAge100), c("normal"))

# mean           sd     
# 10.63259757    0.95567296 
# ( 0.09556730) ( 0.06757628)


# A continuación vamos a hacer lo mismo pero para la proporción de Varones y
# Mujeres que tenemos en datos

# Para esto debemos crear lista de vectores de tamaños 30, 50 y 100 para guardar los valores del atributo Sex 
# de la tabra datos

SexT30 <- vector(mode = "list", length = 30) # Para las 30 muestras

SexT50 <- vector(mode = "list", length = 50) # Para las 50 muestras

SexT100 <- vector(mode = "list", length = 100) # Para las 100 muestras

i <- 1 # Variable para recorrer los bucles while

# Ahora debemos escribir las listas de vectores con los valores de la tabla datos para Age

# Tamaño 30
while(i < 31)
{
  SexT30[[i]] <- c(sample(datos$Sex, size = 200))
  i <- i + 1
}

i <- 1

# Tamaño 50
while(i < 51)
{
  SexT50[[i]] <- c(sample(datos$Sex, size = 200))
  i <- i + 1
}

i <- 1

# Tamaño 100
while(i < 101)
{
  SexT100[[i]] <- c(sample(datos$Sex, size = 200))
  i <- i + 1
}

# Es la proporción de Mujeres/Hombres esto hay que tenerlo en cuenta!!!

MediaSex30 <- vector(length = 30) # Vector de tamaño 30

MediaSex50 <- vector(length = 50) # Vector de tamaño 50

MediaSex100 <- vector(length = 100) # Vector de tamaño 100


i <- 1 # Para recorrer las listas y hacer las varianzas muestrales

# Lista de 30
while(i < 31)
{
  MediaSex30[i] <- sum(SexT30[[i]]=='M')/sum(SexT30[[i]]=='V') 
  i <- i + 1 
}

i <- 1

# Lista de 50
while(i < 51)
{
  MediaSex50[i] <- sum(SexT50[[i]]=='M')/sum(SexT50[[i]]=='V')
  i <- i + 1
}

i <- 1

# Lista de 100
while(i < 101)
{
  MediaSex100[i] <- sum(SexT100[[i]]=='M')/sum(SexT100[[i]]=='V')
  i <- i + 1
}

# Para las de tamaño 30

hist(MediaSex30, main = "Lista de 30")

boxplot(MediaSex30, main = "Lista de 30")

fitdistr((MediaSex30), c("normal"))

# mean          sd    
# 0.94638212   0.12578086 
# (0.02296434) (0.01623824)
#-------------------------------


# Para las de tamaño 30

hist(MediaSex50, main = "Lista de 50")

boxplot(MediaSex50, main = "Lista de 50")

fitdistr((MediaSex50), c("normal"))

# mean          sd    
# 0.99957220   0.14240297 
# (0.02013882) (0.01424030)

# Para las de tamaño 30

hist(MediaSex100, main = "Lista de 100")

boxplot(MediaSex100, main = "Lista de 100")

fitdistr((MediaSex100), c("normal"))

# mean           sd     
# 0.994629935   0.136969829 
# (0.013696983) (0.009685229)




#---------------------------------------------------------
## PARTE 2: Estimación clásica (puntual, intervalos)

# 2.1 Estimación puntual


# Tenemos que separar los hombres de las mujeres y entonces con eso ya podemos
# calcular los datos de este punto.

# Sacamos los hombres (cantidad)

datos_varones <- datos[datos$Sex == "V",]

# Sacamos las mujeres (cantidad)

datos_mujeres <- datos[datos$Sex == "M",]


# t.test y var.test

# Usamos sleeptime

# Con el T.test vamos a comprobar (H0) la igualdad de medias:

t.test(x = datos_varones$sleeptime, y = datos_mujeres$sleeptime)

# Welch Two Sample t-test

# data:  datos_varones$sleeptime and datos_mujeres$sleeptime
# t = -21.817, df = 9996.7, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# -2.150314 -1.795765
# sample estimates:
# mean of x mean of y 
# 8.120108 10.093147 

# CONCLUSIÓN: Descartamos la hipótesis nula (H0) --> p-value < 0.05


var.test(x = datos_varones$sleeptime, y = datos_mujeres$sleeptime)

# F test to compare two variances

# data:  datos_varones$sleeptime and datos_mujeres$sleeptime
# F = 1.0014, num df = 5029, denom df = 4969, p-value = 0.9606
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
# 0.9473796 1.0584892
# sample estimates:
# ratio of variances 
# 1.001399

# CONCLUSIÓN: 


# Ahora lo hacemos con muestras de tamaño 200

datos_varones200 <- datos_varones[sample(1: nrow(datos_varones), 200),]

datos_mujeres200 <- datos_mujeres[sample(1: nrow(datos_mujeres), 200),]


t.test(x = datos_varones200$sleeptime, y = datos_mujeres200$sleeptime)

# Welch Two Sample t-test

# data:  datos_varones200$sleeptime and datos_mujeres200$sleeptime
# t = -3.4079, df = 397.84, p-value = 0.0007216
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# -2.539209 -0.681325
# sample estimates:
# mean of x mean of y 
# 8.381405  9.991672 

# CONCLUSIÓN: Vamos a rechazar H0 --> p-value < 0.05


var.test(x = datos_varones200$sleeptime, y = datos_mujeres200$sleeptime)

# F test to compare two variances

# data:  datos_varones200$sleeptime and datos_mujeres200$sleeptime
# F = 0.88748, num df = 199, denom df = 199, p-value = 0.4006
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
# 0.6716366 1.1727015
# sample estimates:
# ratio of variances 
# 0.8874848


# Usamos steps

t.test(x = datos_varones$steps, y = datos_mujeres$steps)

# Welch Two Sample t-test

# data:  datos_varones$steps and datos_mujeres$steps
# t = -104.03, df = 9991.2, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# -2003.686 -1929.574
# sample estimates:
# mean of x mean of y 
# 10440.60  12407.23 

# CONCLUSIÓN: Vamos a rechazar H0 --> p-value < 0.05


var.test(x = datos_varones$steps, y = datos_mujeres$steps)

# F test to compare two variances

# data:  datos_varones$steps and datos_mujeres$steps
# F = 0.97234, num df = 5029, denom df = 4969, p-value = 0.3215
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
# 0.9198916 1.0277774
# sample estimates:
# ratio of variances 
# 0.9723439 


t.test(x = datos_varones200$steps, y = datos_mujeres200$steps)

# Welch Two Sample t-test

# data:  datos_varones200$steps and datos_mujeres200$steps
# t = -21.394, df = 397.99, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# -2138.265 -1778.359
# sample estimates:
# mean of x mean of y 
# 10490.81  12449.12 

# CONCLUSIÓN: Vamos a rechazas H0 --> p-value < 0.05


var.test(x = datos_varones200$steps, y = datos_mujeres200$steps)

# F test to compare two variances

# data:  datos_varones200$steps and datos_mujeres200$steps
# F = 0.98883, num df = 199, denom df = 199, p-value = 0.9369
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
# 0.7483301 1.3066111
# sample estimates:
# ratio of variances 
# 0.9888258 


# Ahora vamos a hacer uso de las funciones Tools y maxLog


# Función de máxima verosimilitud --> maxLogl()


# Lo primero es instalar los paquetes necessarios

install.packages("EstimationTools")

# Hacer uso del paquete

library(EstimationTools)

# Primero lo vamos a hacer para la variable SLEEPTIME

mujeres_MV_Sleep <- maxlogL(x = datos_mujeres$sleeptime, dist = "dnorm", link = list(over = "sd", fun = "log_link"))


# Mostrar resultados

summary(mujeres_MV_Sleep)

# _______________________________________________________________
# Optimization routine: nlminb 
# Standard Error calculation: Hessian from optim 
# _______________________________________________________________
# AIC     BIC
# 29102.28 29115.3
# _______________________________________________________________
# Estimate  Std. Error Z value Pr(>|z|)    
# mean  10.09315    0.06411   157.4   <2e-16 ***
# sd     4.51973    0.04533    99.7   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# _______________________________________________________________
# Note: p-values valid under asymptotic normality of estimators 
# ---



varones_MV_Sleep <- maxlogL(x = datos_varones$sleeptime, dist = "dnorm", link = list(over = "sd", fun = "log_link"))


# Mostrar resultados

summary(varones_MV_Sleep)

# _______________________________________________________________
# Optimization routine: nlminb 
# Standard Error calculation: Hessian from optim 
# _______________________________________________________________
# AIC      BIC
# 29460.61 29473.65
# _______________________________________________________________
# Estimate  Std. Error Z value Pr(>|z|)    
# mean   8.12011    0.06377   127.3   <2e-16 ***
#   sd     4.52290    0.04509   100.3   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# _______________________________________________________________
# Note: p-values valid under asymptotic normality of estimators 
# ---



# Ahora lo vamos a hacer para la variable STEPS

mujeres_MV_Steps <- maxlogL(x = datos_mujeres$steps, dist = "dnorm", link = list(over = "sd", fun = "log_link"))

summary(mujeres_MV_Steps)

# _______________________________________________________________
# Optimization routine: nlminb 
# Standard Error calculation: Hessian from optim 
# _______________________________________________________________
# AIC     BIC
# 82278.88 82291.9
# _______________________________________________________________
# Estimate  Std. Error Z value Pr(>|z|)    
# mean 12407.235     13.499  919.13   <2e-16 ***
# sd     951.664      9.546   99.69   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# _______________________________________________________________
# Note: p-values valid under asymptotic normality of estimators 
# ---


varones_MV_Steps <- maxlogL(x = datos_varones$steps, dist = "dnorm", link = list(over = "sd", fun = "log_link"))

summary(varones_MV_Steps)

# _______________________________________________________________
# Optimization routine: nlminb 
# Standard Error calculation: Hessian from optim 
# _______________________________________________________________
# AIC      BIC
# 83131.08 83144.13
# _______________________________________________________________
# Estimate  Std. Error Z value Pr(>|z|)    
# mean 10440.604     13.232   789.1   <2e-16 ***
# sd     938.414      9.356   100.3   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# _______________________________________________________________
# Note: p-values valid under asymptotic normality of estimators 
# ---


# Parte 2.2 Estimación por Intervalos, una población

# Tenemos que instalar los paquetes necesarios

install.packages("rcompanion")

# Hacemos uso del paquete

library(rcompanion)

# En primer lugar lo vamos a hacer para la proporción de mujeres y hombres en general

# VARONES

# Para SLEEPTIME

# Para una confianza del 90%
groupwiseMean(sleeptime ~ Sex, data = datos_varones, conf = 0.90, digits = 3)

# Sex    n      Mean       Conf.level      Trad.lower       Trad.upper
# 1   V 5030    8.12        0.9             8.02              8.23


# Para una confianza del 95%
groupwiseMean(sleeptime ~ Sex, data = datos_varones, conf = 0.95, digits = 3)

# Sex      n      Mean    Conf.level    Trad.lower  Trad.upper
# 1   V  5030    8.12       0.95          8           8.25



# Para una confianza del 99%
groupwiseMean(sleeptime ~ Sex, data = datos_varones, conf = 0.99, digits = 3)

# Sex      n        Mean       Conf.level        Trad.lower       Trad.upper
# 1   V  5030      8.12          0.99              7.96             8.28



# MUJERES

# Para SLEEPTIME

# Para una confianza del 90%
groupwiseMean(sleeptime ~ Sex, data = datos_mujeres, conf = 0.90, digits = 3)

# Sex       n        Mean         Conf.level        Trad.lower           Trad.upper
# 1   M    4970      10.1           0.9                9.99                  10.2


# Para una confianza del 95%
groupwiseMean(sleeptime ~ Sex, data = datos_mujeres, conf = 0.95, digits = 3)

# Sex       n      Mean         Conf.level          Trad.lower         Trad.upper
# 1   M   4970     10.1           0.95                   9.97             10.2


# Para una confianza del 99%
groupwiseMean(sleeptime ~ Sex, data = datos_mujeres, conf = 0.99, digits = 3)

# Sex       n        Mean         Conf.level        Trad.lower       Trad.upper
# 1   M   4970       10.1           0.99                9.93              10.3



# Ahora lo vamos a hacer para las muestras de tamaño 200

# HOMBRES

# Para SLEEPTIME

# Para una confianza del 90%
groupwiseMean(sleeptime ~ Sex, data = datos_varones200, conf = 0.90, digits = 3)

# Sex       n        Mean        Conf.level       Trad.lower        Trad.upper
# 1   V    200      8.08            0.9            7.56               8.6


# Para una confianza del 95%
groupwiseMean(sleeptime ~ Sex, data = datos_varones200, conf = 0.95, digits = 3)

# Sex      n       Mean        Conf.level       Trad.lower        Trad.upper
# 1    V  200       8.08         0.95            7.46                 8.7


# Para una confianza del 99%
groupwiseMean(sleeptime ~ Sex, data = datos_varones200, conf = 0.99, digits = 3)

# Sex       n         Mean        Conf.level         Trad.lower        Trad.upper
# 1   V    200        8.08          0.99               7.26                8.9



# MUJERES

# Para SLEEPTIME

# Para una confianza del 90%
groupwiseMean(sleeptime ~ Sex, data = datos_mujeres200, conf = 0.90, digits = 3)

# Sex      n       Mean        Conf.level       Trad.lower        Trad.upper
# 1   M   200       10.4           0.9            9.88               11


# Para una confianza del 95%
groupwiseMean(sleeptime ~ Sex, data = datos_mujeres200, conf = 0.95, digits = 3)

# Sex       n     Mean        Conf.level       Trad.lower        Trad.upper
# 1   M    200    10.4          0.95             9.77              11.1


# Para una confianza del 99%
groupwiseMean(sleeptime ~ Sex, data = datos_mujeres200, conf = 0.99, digits = 3)

# Sex       n       Mean        Conf.level       Trad.lower        Trad.upper
# 1   M    200     10.4           0.99             9.56              11.3



# Ahora lo vamos a hacer para la variable STEPS


# HOMBRES


# Para una confianza del 90%
groupwiseMean(steps ~ Sex, data = datos_varones, conf = 0.90, digits = 3)

# Sex       n      Mean        Conf.level     Trad.lower      Trad.upper
# 1   V   5030    10400          0.9            10400          10500


# Para una confianza del 95%
groupwiseMean(steps ~ Sex, data = datos_varones, conf = 0.95, digits = 3)

# Sex       n        Mean      Conf.level       Trad.lower      Trad.upper
# 1   V   5030      10400       0.95            10400             10500

# Para una confianza del 99%
groupwiseMean(steps ~ Sex, data = datos_varones, conf = 0.99, digits = 3)

# Sex       n        Mean        Conf.level     Trad.lower      Trad.upper
# 1   V   5030      10400       0.99              10400           10500


# Para MUJERES

# Para una confianza del 90%
groupwiseMean(steps ~ Sex, data = datos_mujeres, conf = 0.90, digits = 3)

# Sex       n        Mean        Conf.level       Trad.lower      Trad.upper
# 1   M     4970      12400        0.9             12400            12400



# Para una confianza del 95%
groupwiseMean(steps ~ Sex, data = datos_mujeres, conf = 0.95, digits = 3)

# Sex         n        Mean      Conf.level       Trad.lower      Trad.upper
# 1   M      4970      12400       0.95            12400             12400


# Para una confianza del 99%
groupwiseMean(steps ~ Sex, data = datos_mujeres, conf = 0.99, digits = 3)

# Sex       n        Mean        Conf.level       Trad.lower      Trad.upper
# 1   M    4970      12400        0.99             12400             12400


# Ahora vamos a usar la función bootstrap

# Cargamos el paquete

library(boot)


# Intervalos de confianza para los VARONES

VaronesBoot = boot(datos_varones$sleeptime, function(x, i) mean(x[i]), R = 100)

VaronesBootS = boot(datos_varones$steps, function(x, i) mean(x[i]), R = 100)


# Para la variable SLEEPTIME

mean(VaronesBoot$t[,1])

# [1] 8.122187


# IC --> 90%
boot.ci(VaronesBoot, conf = 0.90, type = c("norm"))

# RESULTADOS:

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 100 bootstrap replicates

# CALL : 
#  boot.ci(boot.out = VaronesBoot, conf = 0.9, type = c("norm"))

# Intervals : 
# Level      Normal        
# 90%   ( 8.025,  8.211 )  
# Calculations and Intervals on Original Scale


# IC --> 95%
boot.ci(VaronesBoot, conf = 0.95, type = c("norm"))

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 100 bootstrap replicates

# CALL : 
#  boot.ci(boot.out = VaronesBoot, conf = 0.95, type = c("norm"))

# Intervals : 
#  Level      Normal        
# 95%   ( 8.007,  8.229 )  
# Calculations and Intervals on Original Scale


# IC --> 99%
boot.ci(VaronesBoot, conf = 0.99, type = c("norm"))

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 100 bootstrap replicates

# CALL : 
#  boot.ci(boot.out = VaronesBoot, conf = 0.99, type = c("norm"))

# Intervals : 
# Level      Normal        
# 99%   ( 7.972,  8.264 )  
# Calculations and Intervals on Original Scale


# Para la variable STEPS

mean(VaronesBootS$t[,1])

# [1] 10439.89

# IC --> 90%
boot.ci(VaronesBootS, conf = 0.90, type = c("norm"))

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 100 bootstrap replicates

# CALL : 
#  boot.ci(boot.out = VaronesBootS, conf = 0.9, type = c("norm"))

# Intervals : 
#  Level      Normal        
#  90%   (10420, 10463 )  
# Calculations and Intervals on Original Scale


# IC --> 95%
boot.ci(VaronesBootS, conf = 0.95, type = c("norm"))

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 100 bootstrap replicates

# CALL : 
#  boot.ci(boot.out = VaronesBootS, conf = 0.95, type = c("norm"))

# Intervals : 
# Level      Normal        
# 95%   (10416, 10467 )  
# Calculations and Intervals on Original Scale


# IC --> 99%
boot.ci(VaronesBootS, conf = 0.99, type = c("norm"))

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 100 bootstrap replicates

# CALL : 
#   boot.ci(boot.out = VaronesBootS, conf = 0.99, type = c("norm"))

#Intervals : 
#  Level      Normal        
#  99%   (10408, 10475 )  
#  Calculations and Intervals on Original Scale




# Intervalos de confianza para las MUJERES

MujeresBoot = boot(datos_mujeres$sleeptime, function(x, i) mean(x[i]), R = 100)

MujeresBootS = boot(datos_mujeres$steps, function(x, i) mean(x[i]), R = 100)


mean(MujeresBoot$t[,1])

# [1] 10.10151


# IC --> 90%
boot.ci(MujeresBoot, conf = 0.90, type = c("norm"))

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 100 bootstrap replicates

# CALL : 
#  boot.ci(boot.out = MujeresBoot, conf = 0.9, type = c("norm"))

# Intervals : 
# Level      Normal        
# 90%   ( 9.98, 10.19 )  
# Calculations and Intervals on Original Scale

# IC --> 95%
boot.ci(MujeresBoot, conf = 0.95, type = c("norm"))

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 100 bootstrap replicates

# CALL : 
#  boot.ci(boot.out = MujeresBoot, conf = 0.95, type = c("norm"))

# Intervals : 
# Level      Normal        
# 95%   ( 9.96, 10.21 )  
# Calculations and Intervals on Original Scale

# IC --> 99%
boot.ci(MujeresBoot, conf = 0.99, type = c("norm"))

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 100 bootstrap replicates

# CALL : 
# boot.ci(boot.out = MujeresBoot, conf = 0.99, type = c("norm"))

# Intervals : 
# Level      Normal        
# 99%   ( 9.92, 10.25 )  
# Calculations and Intervals on Original Scale


# Para la variable STEPS

mean(MujeresBootS$t[,1])

# [1] 12408.29

# IC --> 90%
boot.ci(MujeresBootS, conf = 0.90, type = c("norm"))

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 100 bootstrap replicates

# CALL : 
#  boot.ci(boot.out = MujeresBootS, conf = 0.9, type = c("norm"))

#Intervals : 
#  Level      Normal        
#  90%   (12384, 12428 )  
#  Calculations and Intervals on Original Scale


# IC --> 95%
boot.ci(MujeresBootS, conf = 0.95, type = c("norm"))

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 100 bootstrap replicates

# CALL : 
#   boot.ci(boot.out = MujeresBootS, conf = 0.95, type = c("norm"))

# Intervals : 
#   Level      Normal        
#   95%   (12380, 12433 )  
# Calculations and Intervals on Original Scale


# IC --> 99%
boot.ci(MujeresBootS, conf = 0.99, type = c("norm"))

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 100 bootstrap replicates

# CALL : 
#  boot.ci(boot.out = MujeresBootS, conf = 0.99, type = c("norm"))

# Intervals : 
#   Level      Normal        
#   99%   (12371, 12441 )  
#   Calculations and Intervals on Original Scale


# IC para las VARIANZAS


# Para SLEEPTIME IC DEL 90%

longitud <- length(datos$sleeptime) - 1

varSleeptime <- var(datos$sleeptime)

lower90 = varSleeptime * longitud / qchisq(0.1/2, longitud, lower.tail = FALSE)

upper90 = varSleeptime * longitud / qchisq(1 - 0.1/2, longitud, lower.tail = FALSE)

c(lower90 = lower90, variance = varSleeptime, upper90 = upper90)


# lower90     variance      upper90 
# 20.92851    21.41773     21.92527 




# Para SLEEPTIME IC del 95%
longitud <- length(datos$sleeptime) - 1

varSleeptime <- var(datos$sleeptime)

lower95 = varSleeptime * longitud / qchisq(0.05/2, longitud, lower.tail = FALSE)

upper95 = varSleeptime * longitud / qchisq(1 - 0.05/2, longitud, lower.tail = FALSE)

c(lower95 = lower95, variance = varSleeptime, upper95 = upper95)

# lower     variance    upper95 
# 20.83623  21.41773   22.02405 




# Para SLEEPTIME IC del 99%
longitud <- length(datos$sleeptime) - 1

varSleeptime <- var(datos$sleeptime)

lower99 = varSleeptime * longitud / qchisq(0.01/2, longitud, lower.tail = FALSE)

upper99 = varSleeptime * longitud / qchisq(1 - 0.01/2, longitud, lower.tail = FALSE)

c(lower99 = lower99, variance = varSleeptime, upper99 = upper99)


#   lower99     variance    upper99 
#   20.65743     21.41773    22.21881 


# Para STEPS IC DEL 90%

longitudSt <- length(datos$steps) - 1

varSteps <- var(datos$steps)

lower90St = varSteps * longitudSt / qchisq(0.1/2, longitudSt, lower.tail = FALSE)

upper90St = varSteps * longitudSt / qchisq(1 - 0.1/2, longitudSt, lower.tail = FALSE)

c(lower90St = lower90St, variance = varSteps, upper90St = upper90St)

#     lower90St     variance     upper90St 
#     1817638        1860127        1904207


# Para STEPS IC DEL 95%

longitudSt <- length(datos$steps) - 1

varSteps <- var(datos$steps)

lower95St = varSteps * longitudSt / qchisq(0.05/2, longitudSt, lower.tail = FALSE)

upper95St = varSteps * longitudSt / qchisq(1 - 0.05/2, longitudSt, lower.tail = FALSE)

c(lower95St = lower95St, variance = varSteps, upper95St = upper95St)

# lower95St   variance     upper95St 
# 1809623     1860127       1912785

# Para STEPS IC DEL 99%

longitudSt <- length(datos$steps) - 1

varSteps <- var(datos$steps)

lower99St = varSteps * longitudSt / qchisq(0.01/2, longitudSt, lower.tail = FALSE)

upper99St = varSteps * longitudSt / qchisq(1 - 0.01/2, longitudSt, lower.tail = FALSE)

c(lower99St = lower99St, variance = varSteps, upper99St = upper99St)

# lower99St  variance upper99St 
# 1794095   1860127   1929700 

# Estimación de intervalos, dos poblaciones

# Lo que tenemos que hacer es crear intervalos de confianza para la diferencia de
# medias y también para la razón de varianzas, ya que pertenecen a distintas poblaciones

library("rcompanion")


# Para VARONES

# Para SLEEPTIME

# 90% confianza
groupwiseMean(sleeptime ~ 1, data = datos_varones, conf = 0.90, digits =  3)

# .id    n      Mean       Conf.level      Trad.lower    Trad.upper
#1 <NA> 5030    8.12        0.9             8.02          8.23


# 95% confianza
groupwiseMean(sleeptime ~ 1, data = datos_varones, conf = 0.95, digits =  3)

# .id     n      Mean       Conf.level        Trad.lower       Trad.upper
# 1 <NA> 5030    8.12       0.95               8             8.25


# 99% confianza
groupwiseMean(sleeptime ~ 1, data = datos_varones, conf = 0.99, digits =  3)

# .id    n       Mean       Conf.level        Trad.lower       Trad.upper
# 1 <NA> 5030   8.12          0.99              7.96              8.28


# Para STEPS

# 90% confianza
groupwiseMean(steps ~ 1, data = datos_varones, conf = 0.90, digits =  3)

# .id    n        Mean      Conf.level     Trad.lower      Trad.upper
# 1 <NA> 5030     10400        0.9         10400           10500


# 95% confianza
groupwiseMean(steps ~ 1, data = datos_varones, conf = 0.95, digits =  3)

# .id    n        Mean        Conf.level       Trad.lower      Trad.upper
# 1 <NA> 5030     10400       0.95              10400           10500


# 99% confianza
groupwiseMean(steps ~ 1, data = datos_varones, conf = 0.99, digits =  3)

# .id    n        Mean        Conf.level       Trad.lower        Trad.upper
# 1 <NA> 5030     10400        0.99             10400               10500


# Para MUJERES

# Para SLEEPTIME

# 90% confianza
groupwiseMean(sleeptime ~ 1, data = datos_mujeres, conf = 0.90, digits =  3)

# .id         n        Mean        Conf.level        Trad.lower       Trad.upper
# 1 <NA>    4970     10.1          0.9               9.99             10.2


# 95% confianza
groupwiseMean(sleeptime ~ 1, data = datos_mujeres, conf = 0.95, digits =  3)

#.id        n         Mean        Conf.level         Trad.lower          Trad.upper
# 1 <NA>  4970     10.1           0.95               9.97                10.2


# 99% confianza
groupwiseMean(sleeptime ~ 1, data = datos_mujeres, conf = 0.99, digits =  3)

# .id       n        Mean       Conf.level        Trad.lower         Trad.upper
# 1 <NA>    4970     10.1         0.99              9.93               10.3


# Para STEPS

# 90% confianza
groupwiseMean(steps ~ 1, data = datos_mujeres, conf = 0.90, digits =  3)

# .id         n        Mean        Conf.level       Trad.lower        Trad.upper
# 1 <NA>      4970     12400        0.9                12400              12400

# 95% confianza
groupwiseMean(steps ~ 1, data = datos_mujeres, conf = 0.95, digits =  3)

# .id         n          Mean        Conf.level       Trad.lower          Trad.upper
# 1 <NA>      4970       12400         0.95             12400               12400

# 99% confianza
groupwiseMean(steps ~ 1, data = datos_mujeres, conf = 0.99, digits =  3)

# .id       n        Mean        Conf.level       Trad.lower        Trad.upper
# 1 <NA>  4970       12400        0.99             12400             12400
