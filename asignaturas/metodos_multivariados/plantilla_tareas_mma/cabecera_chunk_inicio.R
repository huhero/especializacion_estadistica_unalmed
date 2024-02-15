
if(!require(pacman)){install.packages("pacman"); library(pacman)}
pacman::p_load("tidyverse", "knitr", "leaps","tidyr","vctrs",
               "exams","leaps","MASS","rsm","car","magrittr")

options(kableExtra.latex.load_packages = F)
knitr::opts_chunk$set(fig.path = 'figurasR/',
                      echo = TRUE, warning = FALSE, message = FALSE,
                      comment = NA,
                      fig.pos="H",fig.align="center",out.width="95%",
                      cache=FALSE) #

knitr::write_bib(c(.packages(),"knitr","rmarkdown"),
                 file="bib/paquetes.bib", width = 60)

#######################################
#exams::include_supplement("myQQnorm.R")
#source("myQQnorm.R")
