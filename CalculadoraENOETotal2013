
#   Calculadora productividad personal año 2013

memory.limit(4050)
options(digits=3)
library(foreign)
library(SDMTools)
library(Hmisc)
enoe2013<-read.dta("C:/Users/Luis Mauricio/Desktop/MCV Calculadora 2013/ENOESDEMCOE1_2013.dta")
enoe2013$unos<-1
View(enoe2013)

#   Variables de interes en el dta:
#   sector,edu,sexo,edad
#  	VARIABLES DE INTERES FILTRADAS POR SOCIODEM
#		YA NO NECESITAMOS FILTRAR
#gen sector=psector if ingocup>0 & (dur_est==4 | dur_est==5) & clase2==1
#gen edu=cat_esc if ingocup>0 & (dur_est==4 | dur_est==5) & clase2==1
#gen edad=cedad if ingocup>0 & (dur_est==4 | dur_est==5) & clase2==1
#gen sexo=sex if ingocup>0 & (dur_est==4 | dur_est==5) & clase2==1

#    CATEGORÍAS DE SECTOR, SEXO, EDAD Y EDUCACIÓN

csector<-c(1:28)
cedu<-c(1:5)
csexo<-c(1:2)
cedad<-c(1:5)

#   COMBINATORIA DE TODAS LAS CATEGORIAS POSIBLES

combinatoria<-expand.grid(csector,cedu,csexo,cedad) #1,400 combinaciones posibles
colnames(combinatoria) <- c("sector","edu","sexo","edad")
View(combinatoria)
length(combinatoria$sector)

#####################################################################    INICIO CHECKS1
#   tab sexo if sexo==1   236,480
nrow(enoe2013[complete.cases(enoe2013$sexo) &
                enoe2013$sexo==1,])

#   tab sexo if sexo==1 [fw=rfac]   17,327,710
prob<-data.frame(enoe2013[complete.cases(enoe2013$sexo) & enoe2013$sexo==1,"unos"],
                 enoe2013[complete.cases(enoe2013$sexo) & enoe2013$sexo==1,"rfac"],
                 enoe2013[complete.cases(enoe2013$sexo) & enoe2013$sexo==1,"unos"]*enoe2013[complete.cases(enoe2013$sexo) & enoe2013$sexo==1,"rfac"]
                 )
colnames(prob)<-c("unos","rfac","unosrfac")                 
View(prob)
sum(prob$unosrfac)

#   OTRA PARA RFAC

sum(enoe2013[complete.cases(enoe2013$sector) & 
               enoe2013$sector==combinatoria$sector[i] & 
               enoe2013$edu==combinatoria$edu[i] &
               enoe2013$sexo==combinatoria$sexo[i] & 
               enoe2013$edad==combinatoria$edad[i],"rfac"]
    )

obsfacprueba<-rep("ppp",10)
for(i in 1:10){
  obsfacprueba[[i]]<-c(sum(enoe2013[complete.cases(enoe2013$sector) & 
                                         enoe2013$sector==combinatoria$sector[i] & 
                                         enoe2013$edu==combinatoria$edu[i] &
                                         enoe2013$sexo==combinatoria$sexo[i] & 
                                         enoe2013$edad==combinatoria$edad[i],"rfac"]))
  print(i)
}
obsfacprueba


#   tab ingocup if sector==1 & edu==1 & sexo==1 & edad==1   32 OBS
nrow(enoe2013[complete.cases(enoe2013$sexo) & 
                enoe2013$sector==combinatoria$sector[1] & 
                enoe2013$edu==combinatoria$edu[1] &
                enoe2013$sexo==combinatoria$sexo[1] & 
                enoe2013$edad==combinatoria$edad[1],])

#length(combinatoria$sector)

#####################################################################    FIN CHECKS1

#   AGREGAR COLUMNA DE OBSERVACIONES SIN FACTOR DE EXPANSIÓN A COMBINATORIA

obsnofac<-rep("x",nrow(combinatoria))
for(i in 1:nrow(combinatoria)){
  obsnofac[[i]]<-c(nrow(enoe2013[complete.cases(enoe2013$sector) & 
  enoe2013$sector==combinatoria$sector[i] & 
  enoe2013$edu==combinatoria$edu[i] &
  enoe2013$sexo==combinatoria$sexo[i] & 
  enoe2013$edad==combinatoria$edad[i],]))
  print(i)
}
combinatoria$obsnofac<-obsnofac
View(combinatoria)

#   AGREGAR COLUMNA DE SALARIO PROMEDIO SIN FACTOR DE EXPANSIÓN A COMBINATORIA

ingnofac<-rep("y",nrow(combinatoria))
for(i in 1:nrow(combinatoria)){
  ingnofac[[i]]<-c(mean(enoe2013[complete.cases(enoe2013$sector) & 
                                   enoe2013$sector==combinatoria$sector[i] & 
                                   enoe2013$edu==combinatoria$edu[i] &
                                   enoe2013$sexo==combinatoria$sexo[i] & 
                                   enoe2013$edad==combinatoria$edad[i],"ingocup"]))
  print(i)
}
combinatoria$ingnofac<-ingnofac
View(combinatoria)

#   AGREGAR COLUMNA DE DESVIACION ESTANDAR DEL SALARIO SIN FACTOR DE EXPANSIÓN A COMBINATORIA

sdingnofac<-rep("z",nrow(combinatoria))
for(i in 1:nrow(combinatoria)){
  sdingnofac[[i]]<-c(sd(enoe2013[complete.cases(enoe2013$sector) & 
                                   enoe2013$sector==combinatoria$sector[i] & 
                                   enoe2013$edu==combinatoria$edu[i] &
                                   enoe2013$sexo==combinatoria$sexo[i] & 
                                   enoe2013$edad==combinatoria$edad[i],"ingocup"]))
  print(i)
}
combinatoria$sdingnofac<-sdingnofac
View(combinatoria)

#   AGREGAR COLUMNA DE OBSERVACIONES ***CON*** FACTOR DE EXPANSIÓN A COMBINATORIA

obsfac<-rep("xx",nrow(combinatoria))
for(i in 1:nrow(combinatoria)){
  obsfac[[i]]<-c(sum(enoe2013[complete.cases(enoe2013$sector) & 
                                enoe2013$sector==combinatoria$sector[i] & 
                                enoe2013$edu==combinatoria$edu[i] &
                                enoe2013$sexo==combinatoria$sexo[i] & 
                                enoe2013$edad==combinatoria$edad[i],"rfac"]))
  print(i)
}
combinatoria$obsfac<-obsfac
View(combinatoria)

#   AGREGAR COLUMNA DE SALARIO PROMEDIO **CON** FACTOR DE EXPANSIÓN A COMBINATORIA

ingfac<-rep("yy",nrow(combinatoria))
for(i in 1:nrow(combinatoria)){
  ingfac[[i]]<-c(weighted.mean(enoe2013[complete.cases(enoe2013$sector) & 
                                   enoe2013$sector==combinatoria$sector[i] & 
                                   enoe2013$edu==combinatoria$edu[i] &
                                   enoe2013$sexo==combinatoria$sexo[i] & 
                                   enoe2013$edad==combinatoria$edad[i],"ingocup"],
                               enoe2013[complete.cases(enoe2013$sector) & 
                                          enoe2013$sector==combinatoria$sector[i] & 
                                          enoe2013$edu==combinatoria$edu[i] &
                                          enoe2013$sexo==combinatoria$sexo[i] & 
                                          enoe2013$edad==combinatoria$edad[i],"rfac"],na.rm=TRUE))
  print(i)
}
combinatoria$ingfac<-ingfac
View(combinatoria)

#   AGREGAR COLUMNA DE DESVIACION ESTANDAR DEL SALARIO **CON** FACTOR DE EXPANSIÓN A COMBINATORIA

#   OPCIÓN UNO PARA OBTENER VARIANZA/DESVIACION ESTANDAR PONDERADA: FUNCION
weighted.var <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm =
                                       na.rm)
}

#   OPCIÓN DOS PARA OBTENER VARIANZA/DESVIACION ESTANDAR PONDERADA: wt.var() de library(SDMTools)
sqrt(wt.var(enoe2013[complete.cases(enoe2013$sector) & 
                       enoe2013$sector==combinatoria$sector[4] & 
                       enoe2013$edu==combinatoria$edu[4] &
                       enoe2013$sexo==combinatoria$sexo[4] & 
                       enoe2013$edad==combinatoria$edad[4],"ingocup"],
            enoe2013[complete.cases(enoe2013$sector) & 
                       enoe2013$sector==combinatoria$sector[4] & 
                       enoe2013$edu==combinatoria$edu[4] &
                       enoe2013$sexo==combinatoria$sexo[4] & 
                       enoe2013$edad==combinatoria$edad[4],"rfac"]))

#   USAMOS LA PRIMERA, AL PARECER DAN LOS MISMOS RESULTADOS (DISTINTOS A LOS DE STATA POR CIERTO)

sdingfac<-rep("zz",nrow(combinatoria))
for(i in 1:nrow(combinatoria)){
  sdingfac[[i]]<-c(sqrt(weighted.var(enoe2013[complete.cases(enoe2013$sector) & 
                                                enoe2013$sector==combinatoria$sector[i] & 
                                                enoe2013$edu==combinatoria$edu[i] &
                                                enoe2013$sexo==combinatoria$sexo[i] & 
                                                enoe2013$edad==combinatoria$edad[i],"ingocup"],
                                     enoe2013[complete.cases(enoe2013$sector) & 
                                                enoe2013$sector==combinatoria$sector[i] & 
                                                enoe2013$edu==combinatoria$edu[i] &
                                                enoe2013$sexo==combinatoria$sexo[i] & 
                                                enoe2013$edad==combinatoria$edad[i],"rfac"],na.rm=TRUE)))
  print(i)
}
combinatoria$sdingfac<-sdingfac
View(combinatoria)


#   EXPORTAR COMBINATORIA

write.table(combinatoria,file="C:/Users/Luis Mauricio/Desktop/MCV Calculadora 2013/tabulados2013.csv",col.names=T,sep="|")










