Inv <-c(23, 26,30, 34,43, 48,52,
        57, 58)
Ventas <-c(651, 762, 856, 1063,1190,
           1298, 1421, 1440, 1518)
datos_ventas<-data.frame(Inv,Ventas)

require(tidyverse)


ggplot(datos_ventas,aes(Inv,Ventas))+
  geom_point(cex=2,col="blue")

require(stats)
require(gamlss)

modelo_1 <- lm(Ventas~Inv, data=datos_ventas)
modelo_1a <- glm(Ventas~Inv, data=datos_ventas)
modelo_1b <- gamlss(Ventas~Inv, data=datos_ventas)


resum_1 <- summary(modelo_1)
resum_1a <- summary(modelo_1a)
resum_1b <- summary(modelo_1b)

resum_1
resum_1a
resum_1b


confint(modelo_1)
confint(modelo_1a)
confint(modelo_1b)

new_data<-data.frame(Inv=c(31))


predict(modelo_1a, new_data,interval='confidence')
predict(modelo_1b, new_data,interval='confidence')




require(tidyverse)
Ventas <-read.csv("Ventas.txt")
ggplot(Ventas,aes(Promocion,Ventas))+
  geom_point(cex=2,col="blue")




ggplot(Ventas,aes(Promocion,Ventas,fill=Sucursal,
                  color=Sucursal))+
  geom_point(cex=2)+
  geom_smooth(method="lm")





modelo_2 <- lm(Ventas~Promocion+Sucursal,data=Ventas)
modelo_2a <- glm(Ventas~Promocion+Sucursal,data=Ventas)
modelo_2b <- gamlss(Ventas~Promocion+Sucursal,data=Ventas)

resum_2 <- summary(modelo_2)
resum_2a <- summary(modelo_2a)
resum_2b <- summary(modelo_2b)


resum_2
resum_2a
resum_2b

#confidence intervalo de confianza de la media
#confidence intervalo de confianza de la observacion


new_data<-data.frame(Promocion=c(20),Sucursal=c("S3"))
predict(modelo_2, new_data, interval = 'confidence')
predict(modelo_2a, new_data, interval = 'confidence')

predict(modelo_2, new_data, interval = 'confidence')



require(catdata)
data(heart)
heart<-data.frame(heart)

# parametro logit logaritmo natural d la propabilidad exito dividido probabilidad de fracaso

mod.glm<-glm(y~age+tobacco,data=heart,
             family=binomial(link="logit"))


glm.probs<-predict(mod.glm,type ="response")
#Mostrandolas primeras4probabilidadespredichas
glm.probs[1:4]
summary (mod.glm)


glm.pred<-rep(0,nrow(heart))
glm.pred[glm.probs>0.5]<-1
#Tabla deprediccionesversusyreales
table(factor(glm.pred), factor(heart$y))



require(gamlss)
mod.gamlss<-gamlss(y~age+tobacco,data=heart,
                   family=BI(mu.link="logit"))
summary (mod.gamlss)



gamlss.probs<-predict(mod.gamlss,type ="response")
#Mostrandolas primeras4probabilidadespredichas
gamlss.probs[1:4]


#Obteniendolasypredichas
gamlss.pred<-rep(0,nrow(heart))
gamlss.pred[gamlss.probs>0.5]<-1
#Tabla deprediccionesversusyreales
table(factor(gamlss.pred), factor(heart$y))
