library(tidyverse)
library(rio)
library(ggthemes)
library(MASS)
library(stats4)


df <- import("Hypermarkets.dta")
df <- df %>% as_tibble
df <- df %>% drop_na()

profit = function(a1, a2, a3, a4, a5, g1, g2, g3, g4, g5, gL, gL2,
                  beta1, beta2, beta3, beta4, lambda1, lambda2){
  
  
  df <- df %>% mutate(
    FC = g1 + gL*hprice + gL2*wage, # Monopolist's fixed cost
    V = a1+ beta1*s_young + beta2*s_pens+ beta3*s_women + beta4*consumption ,
    S = pop + pos_gpop * lambda1 + neg_gpop * lambda2,
    P1 = pnorm(S * V - FC), # Monopolist's profit
    P2 = pnorm(S * (V  - a2) - FC  - g2), # duopolists' profit
    P3 = pnorm(S * (V  - a2 - a3) - FC  - g2 - g3), # etc.
    P4 = pnorm(S * (V  - a2 - a3 - a4) - FC - g2 - g3 - g4),
    P5 = pnorm(S * (V  - a2 - a3 - a4 - a5) - FC - g2 - g3 - g4 - g5)
  )

    df <- df %>% mutate(lnf = case_when(
    stores == 0 ~ log(1 - P1),
    stores == 1 ~ log(P1 -P2),
    stores == 2 ~ log(P2 -P3),
    stores == 3 ~ log(P3 -P4),
    stores == 4 ~ log(P4 -P5),
    stores == 5 ~ log(P5)))
  
  # return the negative to maximize rather than minimize
  -sum(df$lnf)
}


mle(profit, start = list(a1 = -3.7, a2 = 0.72, a3 = .127, a4 = 0.424, a5 = 0.128,
                         g1 = 9.1, g2 = 1.04, g3 = 0.979, g4 = 0.047, g5 = 0.29, 
                         gL = -0.43, gL2 = -3.9, 
                         beta1 = -1.02, beta2 = 3.66, beta3 = 8.9, beta4 = 0.01,
                         lambda1 = -1.8, lambda2 = 3.96))


profit(a1 = -3.7392, a2 = 0.7245, a3 = .12683, a4 = 0.4240428, a5 = 0.1283891,
       g1 = 9.108482, g2 = 1.043629, g3 = 0.9790289, g4 = 0.0467959, g5 = 0.2889239, 
       gL = 0.4303386, gL2 = -3.948511, 
       beta1 = -1.024845, beta2 = 3.659195, beta3 = 8.902495, beta4 = 0.0104314,
       lambda1 = -1.8, lambda2 = 3.96)

