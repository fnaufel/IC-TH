
plot_shaded <- function(
  minimo = 0,
  maximo = 1,
  distr = dnorm,
  args = numeric(),
  cauda,
  tipo = 'ic',
  digitos = 3,
  preenchimento = 'red'
) {
  
  minimo <- minimo %>% round(digitos)
  maximo <- maximo %>% round(digitos)
  
  if (identical(distr, dnorm))
    media <- unname(args['mean']) %>% round(digitos)
  
  if (identical(distr, dt))
    media <- 0
  
  p_cauda_esq <- (media - abs(media - cauda)) %>% round(digitos)
  p_cauda_dir <- (media + abs(media - cauda)) %>% round(digitos)

  cauda_esq <- stat_function(
    fun = distr,
    args = args,
    xlim = c(minimo, p_cauda_esq),
    fill = preenchimento,
    alpha = .5,
    geom = 'area'
  )

  cauda_dir <- stat_function(
    fun = distr,
    args = args,
    xlim = c(p_cauda_dir, maximo),
    fill = preenchimento,
    alpha = .5,
    geom = 'area'
  )
  
  meio <- stat_function(
    fun = distr,
    args = args,
    xlim = c(p_cauda_esq, p_cauda_dir),
    fill = preenchimento,
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
      args = args,
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
