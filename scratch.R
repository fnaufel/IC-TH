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


library(LaplacesDemon)

gl <- 26
erro_padrao <- 44.41377
valor_critico <- 2.055529 * erro_padrao
xbarra <- 653.2593

plot_shaded(
  minimo = 200,
  maximo = 1000,
  distr = dst,
  args = c(mu = xbarra, nu = gl, sigma = erro_padrao * sqrt((gl-2)/gl)),
  cauda = valor_critico,
  tipo = 'bi',
  digitos = 3
)


# Experimenting with dst() ------------------------------------------------

myt <- function(x) {
  
  dst(
    x, 
    mu = 653.2593,
    nu = 26,
    sigma = 44.41377 * sqrt((gl-2)/gl)
  )
  
}

ggplot() +
  stat_function(
    fun = myt,
    xlim = c(600, 700)
  )




# Multidao ----------------------------------------------------------------

setores <- c(
  200, 400, 800, 1000, 400, 800, 700, 1000, 400, 200, 700, 900, 400, 400, 700, 900,  
  600, 700, 900, 650, 672, 700, 900, 450, 566, 700, 900
)

    n = length(setores)
    cat('n =', n)
    
    xbarra <- mean(setores)
    cat('Média amostral =', xbarra)
    
    s <- sd(setores)
    cat('\nDesvio padrão amostral =', s)
    
    erro_padrao <- s / sqrt(n)
    cat('\nEP =', erro_padrao)
    
    gl <- n - 1
    cat('\nGraus de liberdade = ', gl)
    
    nivel_confianca <- .95
    valor_critico <- -qt((1 - nivel_confianca)/2, df = gl)
    cat('\nValor crítico =', valor_critico)
    
    margem_erro <- erro_padrao * valor_critico
    cat('\nME =', margem_erro)
    
    intervalo <- xbarra + c(-1, 1) * margem_erro
    cat('\nIC = [', intervalo[1], ';', intervalo[2], ']')

pst(
  xbarra + margem_erro, 
  mu = xbarra,
  nu = gl,
  sigma = erro_padrao / sqrt((gl-2)/gl),
  lower.tail = FALSE
)  


sigma <- erro_padrao / sqrt((gl-2)/gl)

pt(
  xbarra + margem_erro, 
  df = gl,
  ncp = xbarra * sqrt(n) / sigma
)  



# Checking EP -------------------------------------------------------------

dp <- (intervalo[2] - xbarra) /valor_critico



# New plot_shaded ---------------------------------------------------------


plot_shaded(
  args = c(mean = .5, sd = .2),
  extremos = c(.3, .7)
)

plot_shaded(
  args = c(mean = .5, sd = .2),
  extremos = c(.3, .7),
  tipo = 'bi'
)

plot_shaded(
  args = c(mean = .5, sd = .2),
  extremos = c(.3),
  tipo = 'dir'
)

