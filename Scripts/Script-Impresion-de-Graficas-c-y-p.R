plotqcc <- function(n) {
  id <- 1:n
  library(qcc)
  
  data <- vector(mode="character", length=length(id))# Se declara un vector de id longitud
                                                             
       # Imprimida de nombres de tablas
        for(i in seq_along(id)) {
                x <- id[i]
                id_string <- toString(x)
                if (x >= 1 && x <= 9) {  
                        monitor <- paste("00", id_string, ".csv", sep="")
                }
                else if (x >= 10 && x <= 99) {
                        id_string <- toString(x)
                        monitor <- paste("0", id_string, ".csv", sep="")
                }
                else {
                        id_string <- toString(x)
                        monitor <- paste(id_string, ".csv", sep="") 
                }
                data[i] <- monitor      
                #print(monitor)
        }
	library(qcc) # Se abre la libreria de Estadisticas de Control de Procesos

  for(i in data) { # Ciclo for para las Tablas de Datos
   
   
    tabla <- read.csv(i)  # Lectura de Tablas de Datos
    
    good <- complete.cases(tabla)   # Se eliminan los NA de las tablas
    
    tabla <- tabla[good, ] # La tabla se actualiza para graficar las tablas
    
  	attach(tabla) # Declara variables de base de datos
  
############### Analisis de Shapiro ##########################
	library(ggplot2)
	hist(tabla$x,nclass=10, prob=, xlab="Data", main="Hostograma")
	lines(density(tabla$x))
	boxplot(tabla$x, main="Grafico de Caja")       ###### Grafico de Cajas
	qqnorm(tabla$x, main="Grafico Q-Q Normal")     ######## qq Grafica
	qqline(tabla$x)        ##### Linea de Regresion Lineal
	shapiro.test(tabla$x) ####### Prueba de Shapiro
########################################  	
  	
  	
  	plot_np<-qcc(x[trial], sizes=size[trial], type="np") # Imprime grafica de p variables en Muestras
  	plot_m<- qcc(x[trial], sizes=size[trial], type="u")# Imprime grafica de medias 
	plot_p<- qcc(x[trial], sizes=size[trial], type="p") # Imprime grafica p de base de datos
	plot_q<- qcc(x[trial], sizes=size[trial], type="c", labels=inc) # imprime grafica c de base de datos

	}

}

















