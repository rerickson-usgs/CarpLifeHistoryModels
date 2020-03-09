load("vonBfitNot0_silver.RData")

## stanOutO_SVCP
stanOutOsummary_SVCP <-
    summary(stanOutO_SVCP, probs = c(0.025, 0.1, 0.50, 0.9, 0.975))
stanOutOsummary_SVCP[[1]][grepl("M", rownames(summary(stanOutO_SVCP)[[1]])), ]

stanOutOsummary_SVCP[[1]][grepl("bar", rownames(summary(stanOutO_SVCP)[[1]])), ]
stanOutOsummary_SVCP[[1]][grepl("K", rownames(summary(stanOutO_SVCP)[[1]])), ]
stanOutOsummary_SVCP[[1]][grepl("Linf", rownames(summary(stanOutO_SVCP)[[1]])),]


plot(stanOutO_SVCP, pars =c("Linf_bar", "K_bar", "Linf"))
