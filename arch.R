set.seed(123)

T <- 1000                    # idősor hossza
alpha0 <- 0.1
alpha1 <- 0.8

eps  <- numeric(T)           # hibatag
sig2 <- numeric(T)           # feltételes variancia

# kezdeti érték
sig2[1] <- alpha0 / (1 - alpha1)  # hosszú távú variancia
eps[1]  <- rnorm(1, mean = 0, sd = sqrt(sig2[1]))

for (t in 2:T) {
  sig2[t] <- alpha0 + alpha1 * eps[t - 1]^2
  eps[t]  <- rnorm(1, mean = 0, sd = sqrt(sig2[t]))
}

arch_ts <- ts(eps)

# grafikon
par(mfrow = c(2, 2))
plot(arch_ts, main = "ARCH(1) hibatag", ylab = "eps_t")
plot(ts(eps^2), main = "ARCH(1) négyzetes hibatag", ylab = "eps_t^2")
acf(arch_ts, main = "ACF eps_t")
acf(eps^2, main = "ACF eps_t^2")
par(mfrow = c(1, 1))

####
set.seed(456)

T <- 1000

phi    <- 0.6
theta  <- 0.3
sigma2 <- 1

y     <- numeric(T)
eps   <- rnorm(T, mean = 0, sd = sqrt(sigma2))

# ARMA(1,1) rekurzió
for (t in 2:T) {
  y[t] <- phi * y[t - 1] + theta * eps[t - 1] + eps[t]
}

arma_ts <- ts(y)

par(mfrow = c(2, 2))
plot(arma_ts, main = "ARMA(1,1) idősor", ylab = "y_t")
acf(arma_ts, main = "ACF az idősor tagjaira")
acf(y^2, main = "ACF az idősor tagjainak négyzeteire ")
pacf(y, main = "PACF az idősor tagjaira")
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
plot(arch_ts, main = "ARCH(1) hibatag", ylab = "eps_t")
plot(ts(eps^2), main = "ARCH(1) négyzetes hibatag", ylab = "eps_t^2")
acf(arch_ts, main = "ACF a hibatagokra")
acf(eps^2, main = "ACF a hibatagok négyzetére")
par(mfrow = c(1, 1))

