# Problem 2-------------------------------------------------------------
# Valentina Andrade

# 1. Instalar paquetes ----------------------------------------------------
library(tidyverse)
library(magrittr)
#install.packages("showtext")
if(require(showtext)){
 
  sysfonts::font_add_google("IBM Plex Sans", "plex")
  showtext::showtext_auto()
   
}

# Set theme
theme_set(theme_minimal())

# Primer Loop -------------------------------------------------------------
primer_loop <- function(m = 50, distribucion_e = "norm01",
                        b0 = 1, b1 = 0.3, b2 = 0.7){
  
  # a) Cree m datos aleatorios provenientes de una distribucion normal 
  # x1 ~ N(0, sigma2)
  sigma2 <-  1
  x1 <- rnorm(m, 0, sigma2)
  
  # b) Cree m datos aleatorios provenientes de una distribucion chi-sa 
  # x2 ~ chi2(1)
  x2 <- rchisq(m, df = 1)
  
  # c) Cree m datos  aleatorios provenientes de una distribicón arbitraria
  rdist_e <- switch(
    distribucion_e,
    norm01 = partial(rnorm, mean = 0, sd = 1),
    unif01 = partial(runif, min = 0, max = 1),
    "poiss1/10" = partial(rpois, lambda = 1/10)
  )
  
  e <- rdist_e(m)

  # d) Construya la variable del lado izquierdo como y = b0 + x1b1 + x2b2 + e
  y <- b0 + x1*b1 + x2*b2 + e
  
  # e) Haga una regresion del modelo y guarde los estimadores
  df <- tibble::tibble(y, x1, x2)
  
  mod <- lm(y ~ x1 + x2, data = df)
  
  # moddf <- suppressWarnings(broom::tidy(mod))
  
  moddf <- tibble(
    parametro = c("b0", "b1", "b2"),
    estimacion = mod$coefficients,
    )
  
  moddf <- moddf %>%  
    add_row(
      parametro = "b1+b2",
      estimacion = sum(mod$coefficients[c(2, 3)])
    )
  
  # para que salgan ordenado en ggplot/facet
  moddf <- moddf %>% 
    mutate(parametro = fct_inorder(parametro))
  
  moddf
  
}

# ejemplo primer loop
b0 <- 1
b1 <- round(runif(1), 3)
b2 <- 1 - b1

primer_loop(m = 50, distribucion_e = "norm01", b0 = b0, b1 = b1, b2 = b2)

primer_loop(m = 50, distribucion_e = "poiss1/10", b0 = b0, b1 = b1, b2 = b2)


# Segundo Loop ------------------------------------------------------------
segundo_loop <- function(n = 50, m = 50, distribucion_e = "norm01", b0, b1, b2){
  
  message(str_glue("2do loop: n = {n}, m = {m}, b0 = {b0}, b1 = {b1}, b2 = {b2}, e ~ {distribucion_e}"))
  
  tablas <- list()
  
  for(i in 1:n){
    
    resultado_sim <- primer_loop(
      m = m, 
      distribucion_e = distribucion_e, 
      b0 = b0, b1 = b1, b2 = b2
      )
    
    resultado_sim <- resultado_sim %>%  
      mutate(
        n = n,
        replica = i,
        m = m, 
        distribucion_e = distribucion_e,
        .before = 1)
    
    tablas <- rbind(tablas, resultado_sim)
    
  }
  
  tablas
  
}

# ejemplo 2do loop
segundo_loop(n = 50, m = 50, distribucion_e = "norm01", b0 = b0, b1 = b1, b2 = b2)


# Resultados --------------------------------------------------------------
# por ejemplo para una combinacion de
m <- 100
n <- 500
dist_e <- "norm01"

set.seed(123)

b0 <- 1
b1 <- round(runif(1), 3)
b2 <- 1 - b1

resultado <- segundo_loop(
  n = n, 
  m = m, 
  distribucion_e = dist_e,
  b0 = b0, 
  b1 = b1, 
  b2 = b2
  )

valores_reales <- tibble(
  parametro = c("b0", "b1", "b2", "b1+b2"),
  estimacion =  c(b0, b1, b2, b1 + b2)
) %>% 
  mutate(parametro = fct_inorder(parametro))

ggplot(resultado) + 
  geom_histogram(aes(estimacion), fill = "gray60") +
  geom_vline(aes(xintercept = estimacion), data = valores_reales, color = "darkred") +
  facet_wrap(vars(parametro)) +
  labs(
    title = str_glue("Resultados:  n = {n}, m = {m}, e ~ {dist_e}"),
    subtitle = str_glue("b0 = {b0}, b1 = {b1}, b2 = {b2}")
  )


# si quieres todo junto ---------------------------------------------------
combinaciones <- crossing(
  # numero de loops2
  n = c(50, 100, 500),
  # numero de loop 1
  m = c(50, 100, 500),
  distribucion_e = c("norm01", "unif01", "poiss1/10")
) %>% 
  arrange(distribucion_e, n, m)

combinaciones <- combinaciones  %>%  
  mutate(b0 = b0, b1 = b1, b2 = b2)

graficar_segundo_loop <- function(resultado) {
  
  # resultado <- todos_los_resultados[[18]]
  ggplot(resultado) + 
    geom_histogram(aes(estimacion), fill = "gray60") +
    geom_vline(aes(xintercept = estimacion), data = valores_reales, color = "darkred") +
    facet_wrap(vars(parametro), scales = "free_x") +
    labs(
      title = str_glue("Resultados: e ~ {resultado$distribucion_e}, n = {resultado$n}, m = {resultado$m}"),
      subtitle = str_glue("b0 = {b0}, b1 = {b1}, b2 = {b2}")
    )
  
}

todos_los_resultados <- combinaciones %>%  
  pmap(segundo_loop)

todos_los_graficos <- todos_los_resultados %>%
  map(graficar_segundo_loop)


# todo bien con normal 01, al aumentar el n y m se obtienen ditribuciones 
# con menos varianza, más concentradas
todos_los_resultados[[9]]
todos_los_graficos[[9]]


# con uniforme MENOS varianza todavia dado que var(Unif(0, 1))= 1/2 >> menos error
# sin embargo dado que Unif(0, 1) no posee media 0, sino media 0.5
# la distribcion de estimaciones de b0 se traslada en 0.5
todos_los_resultados[[27]]
todos_los_graficos[[27]]

# con poisson lo mismo que unifiromes MENOS varianza var(poiss(1/10)) = 1/10
# de la misma forma que Unif(0, 1), pois no posee media 0, sino media 1/10
# por tanto la distribcion de estimaciones de b0 se traslada en 0.1
todos_los_resultados[[18]]
todos_los_graficos[[18]]


# Lo anterior explica que el sesgo del error (la media) se traslada al
# componente de posición b0.


pdf("resultados_problem2.pdf", width = 16, height = 9)

map(todos_los_graficos, print)

dev.off()






