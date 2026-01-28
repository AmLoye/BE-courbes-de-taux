
#==============================================================================#
# Méthode de Smith-Wilson à partir d’une courbe de taux spot calculée par la BCE
#==============================================================================#


# spot curve au 2 janvier 2026 d’obligation AAA : https://www.ecb.europa.eu/stats/financial_markets_and_interest_rates/euro_area_yield_curves/html/index.en.html
data = data.frame(maturite = 1:20, 
                  spot = c(2.014339,
                           2.109712,
                           2.221379,
                           2.339985,
                           2.459426,
                           2.575791,
                           2.686651,
                           2.790579,
                           2.886813,
                           2.975042,
                           3.055244,
                           3.127590,
                           3.192369,
                           3.249942,
                           3.300711,
                           3.345092,
                           3.383507,
                           3.416372,
                           3.444087,
                           3.467039)*10^(-2))

# paramètres du modèle de Smith-Wilson
llp = 20 # last liquid point
ufr = 0.033 # ufr = 3.3% https://www.eiopa.europa.eu/eiopa-publishes-ultimate-forward-rate-ufr-2026-2025-03-31_en
Tcv = 60 # point de convergence
tol = 0.0001 # tolerance de convergence = 1bp, notée tau par l’eiopa
# A lower bound for alpha is set at 0.05.
# The convergence criterion is assessed by EIOPA with a scanning procedure with six decimals precision for alpha.

# ultimate forward intensity
w = log(1 + ufr)

n = 20

# cash payment dates
u = data$maturite

# market price
p = 1/((1 + data$spot)^u)

# cash flow matrix C
C = diag(nrow = n, ncol = n)

d = as.vector(exp(-w %*% u))
# paramètre alpha
a = 0.1

fH = function(u,v){
  return (a*min(u,v) + (exp(-a*(u+v)) - exp(-a*abs(u-v)))/2)
}

# Wilson function
fW = function(u,v){
  return (exp(-w*(u+v))*fH(u,v))
}

# Q = diag(d, nrow = n, ncol = n) car C est l’identité dans notre cas
Q = diag(d, nrow = n, ncol = n) %*% C

q = t(Q) %*% rep(1, n)

v = seq(1, 150, by = 1)

W = matrix(NA, nrow = n, ncol = n)
for (i in 1:n){
  for (j in 1:n){
    W[i,j] = fW(u[i], u[j])
  }
}

b = p - q
zeta = solve(W, b)
t_grid = seq(1, 150, by = 1)

P_t = sapply(t_grid, function(t){
  exp(-w * t) + sum(zeta * sapply(u, function(ui) fW(t, ui)))
})

curve = (1/P_t)^(1/t_grid) - 1

k = (1 + a*t(u)%*%Q%*%b) / (sinh(a*t(u))%*%Q%*%b)
f = w + (a/(1 - k*exp(a*v)))

plot(t_grid, curve, col = "pink", main = "taux spot (zero coupon)")
abline(a = ufr, b = 0, col = "red")
abline(v = 60, col = "red")
points(u, data$spot, col = "blue", pch = 3)

plot(t_grid, f, col = "pink", ylim = c(min(f), 0.034), main = "taux forward (zero coupon)")
abline(a = w - tol, b = 0, col = "red")
abline(a = w + tol, b = 0, col = "red")
abline(v = 60, col = "red")


#==============================================================================#
# Méthode de Smith-Wilson à partir de taux swap euribor 6 mois
#==============================================================================#

# données issues de https://app.bluegamma.io/eur/6m_euribor le 22/01
# taux swaps euribor 6 mois au 22 janvier 2026
S = c(2.14, 2.22, 2.33, 2.43, 2.51, 2.59, 2.66, 2.73, 2.79, 2.84,
      2.89, 2.94, 2.98, 3.02, 3.05, 3.07, 3.09, 3.11, 3.12, 3.13) / 100

names(S) = 1:20

# ETAPE 1 : CREDIT RISK ADJUSTMENT
cra = 0.001 # 10 bps


# ETAPE 1 : BOOTSTRAPPING

# calcul des facteurs d’actualisation
DF = rep(0, n)
DF[1] = 1/(1 + S[1])

for (i in 2:n){
  DF[i] = (1 - S[i]*sum(DF[1:(i-1)])) / (1 + S[i])
}

# à partir des facteurs d’actualisation on en déduit les taux "sans risques" (hyp : pas de credit risk adjustment) 
r = (1/DF)^(1/(1:n)) - 1
names(r) = 1:20
points(1:20, r, pch = 3, col = "pink")



# ETAPE 2 : SMITH-WILSON
a = 0.3538707
# market price
p = 1/((1 + r)^u)

b = p - q
zeta = solve(W, b)
t_grid = seq(1, 150, by = 1)

P_t = sapply(t_grid, function(t){
  exp(-w * t) + sum(zeta * sapply(u, function(ui) fW(t, ui)))
})

curve = (1/P_t)^(1/t_grid) - 1

k = (1 + a*t(u)%*%Q%*%b) / (sinh(a*t(u))%*%Q%*%b)
f = w + (a/(1 - k*exp(a*v)))



plot(t_grid[1:20], curve[1:20], col = "pink", main = "taux spot (swaps)")
abline(a = ufr, b = 0, col = "red")
abline(v = 60, col = "red")
points(u, r, col = "blue", pch = 3)

plot(t_grid, f, col = "pink", ylim = c(min(f), 0.034), main = "taux forward (spot)")
abline(a = ufr - tol, b = 0, col = "red")
abline(a = ufr + tol, b = 0, col = "red")

abline(v = 60, col = "red")
