# A continuación se presenta la función de masa de probabilidad de una variable aleatoria x.
x <- c(20,25,30,35)
y <- c(0.20, 0.15,0.25,0.40)

table01 <-  data.frame(cbind(x,y))

sum(table01[,2])


# a. ¿Es válida esta distribución de probabilidad?
sum(table01[,"y"]) # es valida
#b. ¿Cuál es la probabilidad de que x = 30?
filter(table01, x == 30) # la probabilidad es de 25%
#c. ¿Cuál es la probabilidad de que x sea menor o igual que 25?
sum(filter(table01, x <= 25)[,2]) # la probabilidad es de 35%
#d. ¿Cuál es la probabilidad de que x sea mayor que 30?
sum(filter(table01, x > 30)[,2]) # la probabilidad es de 40%
