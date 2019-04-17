library(tidyverse)
library(rio)
library(ggthemes)
library(MASS)
library(stats4)


df <- import("Hypermarkets.dta")
df <- df %>% as_tibble

profit = function(a1, a2, a3, a4, a5, g1, g2, g3, g4, g5, gL, gL2,
                  beta1, beta2, beta3, beta4){
  
  df <- df %>% mutate(
    FC = gL*hprice + gL2*wage,
    V = beta1*s_young + beta2*s_pens+ beta3*s_women + beta4*consumption + a1,
    S = pop,
    P1 = pnorm(S * V - g1 - FC), 
    P2 = pnorm(S * (V  - a2) - FC - g1 - g2), 
    P3 = pnorm(S * (V  - a2 - a3) - FC - g1 - g2 - g3), 
    P4 = pnorm(S * (V  - a2 - a3 - a4) - FC - g1 - g2 - g3 - g4),
    P5 = pnorm(S * (V  - a2 - a3 - a4 - a5) - FC - g1 - g2 - g3 - g4 - g5)
  )

    df <- df %>% mutate(lnf = case_when(
    stores == 0 ~ 1 - P1,
    stores == 1 ~ P1 -P2,
    stores == 2 ~ P2 -P3,
    stores == 3 ~ P3 -P4,
    stores == 4 ~ P4 -P5,
    stores == 5 ~P5))
  
  # return the negative to maximize rather than minimize
  -sum(log(df$lnf))
}

mle(profit, start = list(a1 = 0.7, a2 = 0.1, a3 = 0.1, a4 = 0.1, a5 = 0.1,
                         g1 = 0.1, g2 = 0.1, g3 = 0.1, g4 = 0.1, g5 = 0.1, 
                         gL = 0.1, gL2 = 0.1, 
                         beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0), 
    method = "L-BFGS-B", lower = c(0,0,0,0,0,0,0,0,0,0,-1, -1, 0, 0, 0, 0),
    upper = c(10,10,10,10,10,10,10,10,10,10,10,10,5, 5, 5, 5))


