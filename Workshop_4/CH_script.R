library(tidyverse)
library(rio)
library(readxl)
library(magrittr)
library(glue)
library(ggplot2)
library(AER)
library(ggthemes)
library(stargazer)
library(reshape2)
library(xtable)
#library(plm)
library(zoo)
setwd("C:/Users/chris/Dropbox/02 SSE/5321 IO/Empirical workshop 4")
df <- import("jec.dta")
br1 = 166
br2 = br1 + 14
br3 = 328 - 5
df <- df %>% rename(week = WEEK,
                    month = MONTH,
                    lakes = LAKES,
                    tqg = TQG,
                    po = PO,
                    gr = GR,
                    pn = PN)
df <- df %>% mutate(gtr   = between(week,28,br1),
                    nyc   = between(week,br1+1,br2),
                    cain  = between(week,br2+1,br3),
                    caout = between(week,br3,328))
df <- df %>% mutate(era = case_when(
  between(week,28,br1)    ~ 1,
  between(week,br1+1,br2) ~ 2,
  between(week,br2+1,br3) ~ 3,
  between(week,br3,328)   ~ 4))

qd <- lm(log(tqg) ~ log(gr) + lakes, data = df)                         # qty   = f(price, lakes)
qs <- lm(log(gr) ~ log(tqg) + po + gtr + nyc + cain + caout, data = df) # price = f(qty, lakes, era)

ggplot(data=df, aes(x=week, y=po)) + geom_line(stat="identity")
ggplot(data=df, aes(x=week, y=gr)) + geom_line(stat="identity")
ggplot(data=df, aes(x=week, y=pn)) + geom_line(stat="identity")
# To find out info about theta, specific the supply equation. Summarise MC by weights of market share, 
# 