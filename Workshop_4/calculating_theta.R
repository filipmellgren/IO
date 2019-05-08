stargazer(qs, qd,iv_qs, iv_qd, type = "text")

# From IV using PO
beta2 <- 0.365 # Coefficient before PO. beta3 in Porter
alpha1 <- -0.751 # Coefficient before log(gr)
theta <- alpha1/(exp(beta3)) -alpha1

