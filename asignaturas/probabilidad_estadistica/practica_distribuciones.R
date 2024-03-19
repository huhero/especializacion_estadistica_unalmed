#######################################################
# distribuciion binomial
#######################################################

# d: funcion de masa de probabilidad
# p: funcion de probabilidad acumulada
# q: funcion quantil
# r: random

dbinom(10:15, 15, 0.4)
sum(dbinom(10:15, 15, 0.4))


pbinom(5,15,0.4)-pbinom(4,15,0.4)

sum(dbinom(4:5, 15, 0.4))


# P(a<= X <= b) = F(b) - F(a-1)
# P(a<= X < b) = F(b-1) - F(a-1)
# P(a< X <= b) = F(b) - F(a)
# P(a< X < b) = F(b-1) - F(a)

dbinom(3,10,0.46)

#######################################################
# distribuciion hypergeometrica
#######################################################

dhyper(1,3,37,5)

#######################################################
# distribuciion binomial negativa 
#######################################################


gauss.medellin.unal.edu.co:3838/estadistica-shiny-apps/distribuciones_discretas