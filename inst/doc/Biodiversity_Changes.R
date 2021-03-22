## ---- fig.width = 5, fig.height = 5-------------------------------------------
library(mobsim)

S0 <- 100
N0 <- 1000
cv0 <- 0.5

sim_ref <- sim_poisson_community(s_pool = S0, n_sim = N0, sad_type = "lnorm",
                                 sad_coef = list("cv_abund" = cv0), fix_s_sim = T)
plot(sim_ref)

## -----------------------------------------------------------------------------
sim_lower_N <-  sim_poisson_community(s_pool = S0, n_sim = N0/2, sad_type = "lnorm",
                                      sad_coef = list("cv_abund" = cv0))

## ----  fig.width = 7, fig.height = 3.5----------------------------------------
curve_ref    <- spec_sample_curve(sim_ref)
curve_lower_N <- spec_sample_curve(sim_lower_N)

par(mfrow = c(1,2))
plot(spec_rarefied ~ n, data = curve_ref, type = "l", main = "Rarefaction curve",
     xlab = "No. of individuals", ylab = "No. of species" )
lines(spec_rarefied ~ n, data = curve_lower_N, col = "red")

plot(spec_accum ~ n, data = curve_ref, type = "l", main = "Species accumulation curve",
     xlab = "No. of individuals", ylab = "No. of species" )
lines(spec_accum ~ n, data = curve_lower_N, col = "red")

## ---- fig.width = 7, fig.height = 3.5-----------------------------------------
sim_uneven <-  sim_poisson_community(s_pool = S0, n_sim = N0, sad_type = "lnorm",
                                     sad_coef = list("cv_abund" = cv0*4),
                                     fix_s_sim = T)
curve_uneven <- spec_sample_curve(sim_uneven)

par(mfrow = c(1,2))
plot(spec_rarefied ~ n, data = curve_ref, type = "l", main = "Rarefaction curve",
     xlab = "No. of individuals", ylab = "No. of species" )
lines(spec_rarefied ~ n, data = curve_uneven, col = "red")

plot(spec_accum ~ n, data = curve_ref, type = "l", main = "Species accumulation curve",
     xlab = "No. of individuals", ylab = "No. of species" )
lines(spec_accum ~ n, data = curve_uneven, col = "red")

## ---- fig.width = 7, fig.height = 3.5-----------------------------------------
sim_clumped <-  sim_thomas_community(s_pool = S0, n_sim = N0, sad_type = "lnorm",
                                     sad_coef = list("cv_abund" = cv0),
                                     fix_s_sim = T,
                                     sigma = 0.05)

par(mfrow = c(1,2))
plot(sim_ref)
plot(sim_clumped)

## ---- fig.width = 7, fig.height = 3.5-----------------------------------------
curve_clumped<- spec_sample_curve(sim_clumped)

par(mfrow = c(1,2))
plot(spec_rarefied ~ n, data = curve_ref, type = "l", main = "Rarefaction curve",
     xlab = "No. of individuals", ylab = "No. of species" )
lines(spec_rarefied ~ n, data = curve_clumped, col = "red")

plot(spec_accum ~ n, data = curve_ref, type = "l", main = "Species accumulation curve",
     xlab = "No. of individuals", ylab = "No. of species" )
lines(spec_accum ~ n, data = curve_clumped, col = "red")


