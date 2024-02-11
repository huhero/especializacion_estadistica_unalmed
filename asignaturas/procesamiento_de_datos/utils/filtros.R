filtro_por_genero <- function(df,p_genero) { # create a function with the name my_function
  result = df[df$genero==p_genero,]
  return(result)
}

filtro_por_edad <- function(df,p_edad) { # create a function with the name my_function
  result = df[df$edad>=p_edad,]
  return(result)
}