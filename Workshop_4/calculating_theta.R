# This file calculates two estimates of the market conduct paramter theta.
stargazer(qd, ps.po, ps.pn, iv.qd.po, iv.qd.pn, iv.ps.po, iv.ps.pn, type = "text")

# From IV using PO
beta2 <- 0.365 # Coefficient before PO/PN. beta3 in Porter
alpha1 <- -0.751 # Coefficient before log(gr)
theta <- alpha1/(exp(beta3)) -alpha1

# From IV using PN
beta2 <-  0.416 # Coefficient before PO/PN. beta3 in Porter
alpha1 <- -0.922 # Coefficient before log(gr)
theta <- alpha1/(exp(beta3)) -alpha1



