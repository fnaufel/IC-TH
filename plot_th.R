
plot_th <- function(
  minimo = 0,
  maximo = 1,
  distr = dnorm,
  distr_args = numeric(),
  media = .5,
  cauda,
  erro,
  tipo = 'ic'
) {
  
  p_cauda_esq <- media - abs(media - cauda)
  p_cauda_dir <- media + abs(media - cauda)

  if (identical(distr, dnorm))
    args_to_use = c(mean = media, sd = erro)
  
  if (identical(distr, dt))
    args_to_use = distr_args
    
  cauda_esq <- stat_function(
    fun = distr,
    args = args_to_use,
    xlim = c(minimo, p_cauda_esq),
    fill = 'red',
    alpha = .5,
    geom = 'area'
  )

  cauda_dir <- stat_function(
    fun = distr,
    args = args_to_use,
    xlim = c(p_cauda_dir, maximo),
    fill = 'red',
    alpha = .5,
    geom = 'area'
  )
  
  meio <- stat_function(
    fun = distr,
    args = args_to_use,
    xlim = c(p_cauda_esq, p_cauda_dir),
    fill = 'red',
    alpha = .5,
    geom = 'area'
  )

  preencher <- switch (
    tipo,
    'ic'  = meio,
    'bi'  = list(cauda_esq, cauda_dir),
    'esq' = cauda_esq,
    'dir' = cauda_dir
  )
  
  brks <- c(minimo, p_cauda_esq, media, p_cauda_dir, maximo)
  
  if (tipo == 'esq')
    brks <- brks[-4]

  if (tipo == 'dir')
    brks <- brks[-2]
  
  ggplot() +
    stat_function(
      fun = distr,
      args = args_to_use,
      xlim = c(minimo, maximo)
    ) +
    preencher +
    scale_x_continuous(
      breaks = brks
    ) +
    labs(
      y = NULL
    ) +
    theme(axis.text.x = element_text(angle = 90))
  
}
