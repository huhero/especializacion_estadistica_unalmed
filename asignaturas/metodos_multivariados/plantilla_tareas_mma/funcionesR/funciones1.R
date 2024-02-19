
#######################################################
#######      Resumenes descriptivos varios ############
#######################################################

######## coeficiente de asimetría muestral

asimetria=function(x) {
  m3=mean((x-mean(x))^3)
  skew=m3/(sd(x)^3)
  skew}

#### coeficiente de kurtosis muestral

kurtosis=function(x) {
  m4=mean((x-mean(x))^4)
  kurt=m4/(sd(x)^4)
  kurt}

#######################################
# Scatterplot con Histogramas paralelos
#######################################

scatterhist = function(x, y, xlab="", ylab=""){
  zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
  xhist = hist(x, plot=FALSE)
  yhist = hist(y, plot=FALSE)
  top = max(c(xhist$counts, yhist$counts))
  par(mar=c(3,3,1,1))
  plot(x,y)
  par(mar=c(0,3,1,1))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
  par(mar=c(3,0,1,1))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
  par(oma=c(3,3,0,0))
  mtext(xlab, side=1, line=1, outer=TRUE, adj=0,
        at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  mtext(ylab, side=2, line=1, outer=TRUE, adj=0,
        at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
}

############################################################
## Función para Gráfico de perfiles para cada variable
############################################################

makeProfilePlot <- function(mylist,names)
{
  require(RColorBrewer)
  # find out how many variables we want to include
  numvariables <- length(mylist)
  # choose 'numvariables' random colours
  colours <- brewer.pal(numvariables,"Set1")
  # find out the minimum and maximum values of the variables:
  mymin <- 1e+20
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  # plot the variables
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
    else         { points(vectori, col=colouri,type="l")                                     }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
}

##############################################################
# Setup of a Correlation Lower Panel in Scatterplot Matrix
##############################################################

myPanel.hist <- function(x, ...){
  usr <- par("usr")
  on.exit(par(usr))
  # Para definir región de graficiación
  par(usr = c(usr[1:2], 0, 1.5) )
  # Para obtener una lista que guarde las marcas de clase y conteos en cada una:
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks;
  nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  # Para dibujar los histogramas
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

###########################################################
# Setup of a Boxplot Diagonal Panel in Scatterplot Matrix
###########################################################

myPanel.box <- function(x, ...){
  usr <- par("usr", bty = 'n')
  on.exit(par(usr))
  par(usr = c(-1, 1, min(x) - 0.5, max(x) + 0.5))
  b <- boxplot(x, plot = F)
  whisker.i <- b$stats[1,]
  whisker.s <- b$stats[5,]
  hinge.i <- b$stats[2,]
  mediana <- b$stats[3,]
  hinge.s <- b$stats[4,]
  rect(-0.5, hinge.i, 0.5, mediana, col = 'gray')
  segments(0, hinge.i, 0, whisker.i, lty = 2)
  segments(-0.1, whisker.i, 0.1, whisker.i)
  rect(-0.5, mediana, 0.5, hinge.s, col = 'gray')
  segments(0, hinge.s, 0, whisker.s, lty = 2)
  segments(-0.1, whisker.s, 0.1, whisker.s)
}

################################################################
# Setup of a Correlation Lower Panel in Scatterplot Matrix
################################################################

myPanel.cor <- function(x, y, digits = 2, prefix = "", cex.cor){
  usr <- par("usr")
  on.exit(par(usr = usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if(missing(cex.cor))
    cex = 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1 + 1.5*abs(r))
}

##################################################
# QQ-plot with Shapiro-Wilk normal test
##################################################

QQnorm <- function(datos){
    lab.plot <- "Normal Q-Q Plot of Datos Crudos"
  shapiro <- shapiro.test(datos)
  shapvalue <- ifelse(shapiro$p.value < 0.001,
      "P value < 0.001", paste("P value = ",
        round(shapiro$p.value, 4), sep = ""))
  shapstat <- paste("W = ", round(shapiro$statistic, 4),
                    sep = "")
  q <- qqnorm(datos, plot.it = FALSE)
  qqnorm(datos, main = lab.plot)
  qqline(datos, lty = 1, col = 2)
  text(min(q$x, na.rm = TRUE), max(q$y,
      na.rm = TRUE)*0.95, pos = 4,
      'Shapiro-Wilk Test', col = "blue", font = 2)
  text(min(q$x, na.rm = TRUE), max(q$y,
      na.rm = TRUE)*0.80, pos = 4, shapstat,
      col = "blue", font = 3)
  text(min(q$x, na.rm = TRUE), max(q$y, na.rm = TRUE)*0.65,
       pos = 4, shapvalue, col = "blue", font = 3)
}

#################################################################
# QQ-plot with Shapiro-Wilk normal test (datos transformados)
#################################################################

QQnorm_transf <- function(datos){
  lab.plot <- "Normal Q-Q Plot of Datos Transformados"
  shapiro <- shapiro.test(datos)

  shapvalue <- ifelse(shapiro$p.value < 0.001,
          "P value < 0.001", paste("P value = ",
          round(shapiro$p.value, 4), sep = ""))

  shapstat <- paste("W = ", round(shapiro$statistic, 4),
                    sep = "")

  q <- qqnorm(datos, plot.it = FALSE)
  qqnorm(datos, main = lab.plot)
  qqline(datos, lty = 2, col = 2)

  text(min(q$x, na.rm = TRUE),
       max(q$y-0.2, na.rm = TRUE)*0.95, pos = 4,
       'Shapiro-Wilk Test', col = "blue", font = 2)

  text(min(q$x, na.rm = TRUE),
       max(q$y-0.7, na.rm = TRUE)*0.80, pos = 4,
       shapstat, col = "blue", font = 3)

  text(min(q$x, na.rm = TRUE),
       max(q$y-1.5, na.rm = TRUE)*0.65, pos = 4,
       shapvalue, col = "blue", font = 3)
}

##########################################################
## Función para Resumen descriptivo por grupos
##########################################################

resumen_xgrupos <- function(misdatos,grupos)
{
  # se hallan los nombres de las variables
  nombres_misdatos <- c(names(grupos),names(as.data.frame(misdatos)))
  # se halla la media dentro de cada grupo
  grupos <- grupos[,1] # nos aseguramos de que la var grupos no sea una lista
  medias <- aggregate(as.matrix(misdatos) ~ grupos, FUN = mean)
  names(medias) <- nombres_misdatos
  # se hallan las desv-estandar dentro de cada grupos:
  sds <- aggregate(as.matrix(misdatos) ~ grupos, FUN = sd)
  names(sds) <- nombres_misdatos
  # se hallan las varianzas dentro de cada grupos:
  varianzas <- aggregate(as.matrix(misdatos) ~ grupos, FUN = var)
  names(varianzas) <- nombres_misdatos
  # se hallan las medianas dentro de cada grupos:
  medianas <- aggregate(as.matrix(misdatos) ~ grupos, FUN = median)
  names(medianas) <- nombres_misdatos
  # se hallan los tama?os muestrales de cada grupo:
  tamanos_n <- aggregate(as.matrix(misdatos) ~ grupos, FUN = length)
  names(tamanos_n) <- nombres_misdatos
  list(Medias=medias,Desviaciones_Estandar=sds,
       Varianzas=varianzas, Medianas=medianas,
       Tamanos_Muestrales=tamanos_n)
}





