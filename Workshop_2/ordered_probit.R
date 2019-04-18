library(tidyverse)
library(rio)
library(ggthemes)
library(MASS)
library(stats4)


df <- import("Hypermarkets.dta")
df <- df %>% as_tibble

# Must drop nas before this will work! (Stata does this silently)
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


ML_full <- mle(profit, start = list(a1 = -3.73, a2 = 0.72, a3 = .127, a4 = 0.424, a5 = 0.128,
                         g1 = 9.1, g2 = 1.04, g3 = 0.979, g4 = 0.047, g5 = 0.29, 
                         gL = 0.43, gL2 = -3.9, 
                         beta1 = -1.02, beta2 = 3.66, beta3 = 8.9, beta4 = 0.01,
                         lambda1 = -1.8, lambda2 = 3.96))


### DROP some variables:
profit2 = function(a1, a2, a3, a4, a5, g1, g2, g3, g4, g5, gL, gL2,
                  beta4, lambda1){
  
  
  df <- df %>% mutate(
    FC = g1 + gL*hprice + gL2*wage, # Monopolist's fixed cost
    V = a1+ beta4*consumption ,
    S = pop + gpop * lambda1,
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



ML2 <- mle(profit2, start = list(a1 = 1.3, a2 = 0.01, a3 = 0.05, a4 = 0.5, a5 = 0.1,
                               g1 = 5.9, g2 = 1, g3 = 1, g4 = 0, g5 = 0.2, 
                               gL = 0.3, gL2 = -2.5, 
                               beta4 = 0.01,
                               lambda1 = -1))

