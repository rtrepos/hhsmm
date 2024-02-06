library(hhsmm)

J <- 3
initial <- c(1, 0, 0)
semi <- c(FALSE, TRUE, FALSE)
P <- matrix(c(0.8, 0.1, 0.1, 0.5, 0, 0.5, 0.1, 0.2, 0.7), nrow = J,
            byrow = TRUE)
par <- list(mu = list(list(7, 8), list(10, 9, 11), list(12, 14)),
            sigma = list(list(3.8, 4.9), list(4.3, 4.2, 5.4), list(4.5, 6.1)),
            mix.p = list(c(0.3, 0.7), c(0.2, 0.3, 0.5), c(0.5, 0.5)))
sojourn <- list(shape = c(0, 3, 0), scale = c(0, 10, 0), type = "gamma")
model <- hhsmmspec(init = initial, transition = P, parms.emis = par,
                   dens.emis = dmixmvnorm, sojourn = sojourn, semi = semi)
train <- simulate(model, nsim = c(10, 8, 8, 18), seed = 1234,
                  remission = rmixmvnorm)
clus = initial_cluster(train, nstate = 3, nmix = c(2 ,2, 2),ltr = FALSE,
                       final.absorb = FALSE, verbose = TRUE)
initmodel1 = initialize_model(clus = clus, sojourn = "gamma",
                              M = max(train$N), semi = semi)
fit1 = hhsmmfit(x = train, model = initmodel1, M = max(train$N))
