df <- read_tsv('data/03 - Body Temperatures.txt') %>% 
  na.omit()

df <- df %>% 
  mutate(
    manhã = (5 * (`DAY 1 - 8AM` - 32)/9) %>% round(1),
    tarde = (5 * (`DAY 1 - 12AM` - 32)/9) %>% round(1)
  ) %>% 
  select(manhã, tarde)

library(datapasta)

tribble_construct(df)

df %>% View()

df %>% pull(tarde) %>% paste(collapse = ', ') %>% cat()
