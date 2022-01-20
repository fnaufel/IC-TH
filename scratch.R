pchapeu <- .4767442 
erro_padrao <- .05385804 
margem_erro <- .1055598

plot_shaded(
  minimo = 0,
  maximo = 1,
  distr = dnorm,
  args = c(mean = pchapeu, sd = erro_padrao),
  cauda = pchapeu + margem_erro,
  tipo = 'ic',
  digitos = 3,
  preenchimento = 'blue'
)


plot_shaded(
  minimo = 0,
  maximo = 1,
  distr = dnorm,
  args = c(mean = .5, sd = erro_padrao),
  cauda = pchapeu,
  tipo = 'bi',
  digitos = 3
)


gl <- 26
valor_critico <- 2.055529
erro_padrao <- 44.41377
xbarra <- 653.2593

plot_shaded(
  minimo = -5,
  maximo = 5,
  distr = dt,
  args = c(df = gl),
  cauda = valor_critico,
  tipo = 'bi',
  digitos = 3
)
  
