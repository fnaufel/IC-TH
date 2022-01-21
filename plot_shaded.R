
plot_shaded <- function(
  minimo = 0,
  maximo = 1,
  distr = dnorm,
  args = numeric(),
  extremos = numeric(),
  tipo = 'ic',
  digitos = 3,
  preenchimento = 'red'
) {
  
  minimo <- minimo %>% round(digitos)
  maximo <- maximo %>% round(digitos)
  extremos <- extremos %>% round(digitos)
  
  if (identical(distr, dnorm))
    media <- unname(args['mean']) %>% round(digitos)
  
  if (identical(distr, dt))
    media <- 0
  
  camadas <- list()
  
  if (tipo %in% c('bi', 'ic')) {
    esq <- extremos[1]
    dir <- extremos[2]
  }
  
  if (tipo == 'esq')
    esq <- extremos[1]
  
  if (tipo == 'dir')
    dir <- extremos[1]

  if (tipo %in% c('esq', 'bi')) {
    camadas <- camadas %>% 
      append(
        stat_function(
          fun = distr,
          args = args,
          xlim = c(minimo, esq),
          fill = preenchimento,
          alpha = .5,
          geom = 'area'
        )
      )
  }

  if (tipo %in% c('dir', 'bi')) {
    camadas <- camadas %>% 
      append(
        stat_function(
          fun = distr,
          args = args,
          xlim = c(dir, maximo),
          fill = preenchimento,
          alpha = .5,
          geom = 'area'
        )
      )
  }

  if (tipo == 'ic') {
    camadas <- camadas %>% 
      append(
        stat_function(
          fun = distr,
          args = args,
          xlim = c(esq, dir),
          fill = preenchimento,
          alpha = .5,
          geom = 'area'
        )
      )
  }
  
  brks <- c(minimo, media, maximo, extremos) %>% sort()
  
  ggplot() +
    stat_function(
      fun = distr,
      args = args,
      xlim = c(minimo, maximo)
    ) +
    camadas +
    scale_x_continuous(
      breaks = brks
    ) +
    labs(
      y = NULL
    ) +
    theme(axis.text.x = element_text(angle = 90))
  
}

