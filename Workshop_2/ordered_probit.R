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


#### Change specification:
#Y: pop, pos_gpop, neg_gpop, close_pop
# X s_kids, s_pens, s_young, s_women, consumption, cons_pc
# W (fixed costs): hprice wage

profit = function(a1, a2, a3, a4, a5, g1, g2, g3, g4, g5, gL, gL2,
                  beta1, beta2, beta3, beta4, beta5, beta6, 
                  lambda1, lambda2, lambda3){
  
  df <- df %>% mutate(
    FC = g1 + gL*hprice + gL2*wage, # Monopolist's fixed cost
    V = a1+ beta1*s_young + beta2*s_pens+ beta3*s_women + beta4*consumption +
      beta5*s_kids + beta6*cons_pc,
    S = pop + pos_gpop * lambda1 + neg_gpop * lambda2 + close_pop * lambda3,
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


ML_2 <- mle(profit, start = list(a1 = -3.73, a2 = 0.72, a3 = .127, a4 = 0.424, a5 = 0.128,
                                    g1 = 9.1, g2 = 1.04, g3 = 0.979, g4 = 0.047, g5 = 0.29, 
                                    gL = 0.43, gL2 = -3.9, 
                                    beta1 = -1.02, beta2 = 3.66, 
                                    beta3 = 8.9, beta4 = 0.01, 
                                    beta5 = 0.1, beta6 = 0.1,
                                    lambda1 = -1.8, lambda2 = 3.96, lambda3 = 1))

summary(ML_2)





## THIS IS THE ONE:
# Y: pop, pos_gpop, neg_gpop 
# X (local market characteristics): s_kids, s_pens, s_young, s_women, consumption per capita,
# W (fixed costs): hprice, wage

df <- df %>% drop_na()

profit = function(a1, a2, a3, a4, a5, g1, g2, g3, g4, g5, gL, gL2,
                  beta1, beta2, beta3, beta4, beta5,
                  lambda1, lambda2){
  
  df <- df %>% mutate(
    FC = g1 + gL*hprice + gL2*wage, # Monopolist's fixed cost
    V = a1+ beta1*s_kids + beta2*s_young + beta3*s_pens+ beta4*s_women + beta5*cons_pc ,
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


ML_23 <- mle(profit, start = list(a1 = -3.73, a2 = 0.72, a3 = .127, a4 = 0.424, a5 = 0.128,
                                    g1 = 9.1, g2 = 1.04, g3 = 0.979, g4 = 0.047, g5 = 0.29, 
                                    gL = 0.43, gL2 = -3.9, 
                                    beta1 = 0, beta2 = -1.02, beta3 = -3.6, beta4 = 8.9,
                                    beta5 = 0, lambda1 = -1.8, lambda2 = 3.96))

summary(ML_23)
table <- cbind(as.matrix(ML_23@coef), as.matrix(sqrt(diag(ML_23@vcov))))
table <- table %>% as_tibble(rownames = "variable") %>% 
  mutate(z = V1 / V2, p = (1-pnorm(z)),
         variable = c("a1", "a2", "a3", "a4", "a5", 
                      "g1", "g2", "g3", "g4", "g5", 
                      "House prices", "Wages","Fraction kids", "Fraction young", 
                      "Fraction pensioner","Fraction Women", "Consumption/capita", 
                      "Positive growth", "Negative growth")) %>%
  rename(Coef. = V1, s.e. = V2)

xtable::xtable(table, caption = "ML estimates")


