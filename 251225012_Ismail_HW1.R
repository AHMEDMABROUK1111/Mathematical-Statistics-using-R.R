# =============================================================================
# Solved by       : Ahmed Ismail
# Course     : Mathematical Statistics Using R
# Problem Set #1
# Date       : March 2026
# =============================================================================

# ─────────────────────────────────────────────────────────────────────────────
# PROBLEM 1: Descriptive Statistics and Sampling
# ─────────────────────────────────────────────────────────────────────────────

cat("================================================================\n")
cat("PROBLEM 1: Descriptive Statistics and Sampling\n")
cat("================================================================\n\n")

# Define the vector
x <- c(6, 9, 11, 4, 15, 7, 13, 10)
n <- length(x)
cat("Vector x:", x, "\n")
cat("Number of elements (n):", n, "\n\n")

# ── Part (a): Sample mean, variance, standard deviation ─────────────────────
cat("── Part (a) ─────────────────────────────────────────────────\n")

x_mean <- mean(x)          # sample mean
x_var  <- var(x)           # sample variance (uses n-1 denominator)
x_sd   <- sd(x)            # sample standard deviation

cat("Sample mean      x̄ =", x_mean, "\n")
cat("Sample variance  s² =", x_var, "\n")
cat("Sample std dev   s  =", x_sd, "\n\n")

# ── Part (b): Manual verification of the variance formula ───────────────────
cat("── Part (b): Verify s² = (1/(n-1)) * Σ(xi - x̄)² ──────────────\n")

lhs              <- var(x)
deviations       <- x - x_mean
squared_devs     <- deviations^2
sum_squared_devs <- sum(squared_devs)
rhs              <- sum_squared_devs / (n - 1)

cat("Left-hand side  (var(x))               :", lhs, "\n")
cat("Right-hand side (manual formula)       :", rhs, "\n")
cat("They match?", isTRUE(all.equal(lhs, rhs)), "\n\n")

cat("Step-by-step:\n")
cat("  Deviations (xi - x̄):", round(deviations, 4), "\n")
cat("  Squared deviations  :", round(squared_devs, 4), "\n")
cat("  Sum of squared devs :", round(sum_squared_devs, 4), "\n")
cat("  Divided by (n-1 =", n-1, "):", round(rhs, 4), "\n\n")

# ── Part (c): Bootstrap distribution of the sample mean ─────────────────────
cat("── Part (c): Bootstrap (10,000 samples) ─────────────────────\n")

set.seed(42)
B          <- 10000
boot_means <- numeric(B)

for (b in 1:B) {
  boot_sample   <- sample(x, size = n, replace = TRUE)
  boot_means[b] <- mean(boot_sample)
}

boot_E_Xbar   <- mean(boot_means)
boot_Var_Xbar <- var(boot_means)

cat("Bootstrap estimate of E[X̄]    :", round(boot_E_Xbar, 4), "\n")
cat("Bootstrap estimate of Var(X̄)  :", round(boot_Var_Xbar, 6), "\n\n")

# ── Part (d): Compare bootstrap variance to theoretical variance ─────────────
cat("── Part (d): Comparison ─────────────────────────────────────\n")

theoretical_var <- x_var / n

cat("Theoretical Var(X̄) = s²/n     :", round(theoretical_var, 6), "\n")
cat("Bootstrap  Var(X̄)              :", round(boot_Var_Xbar, 6), "\n")
cat("Ratio (bootstrap / theoretical):", round(boot_Var_Xbar / theoretical_var, 4), "\n")
cat("\nComment: The bootstrap variance is very close to the theoretical\n")
cat("estimate s²/n. Small discrepancies are expected due to Monte Carlo\n")
cat("randomness; with B=10,000 they are typically less than 5%.\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# PROBLEM 2: Matrix Algebra in R
# ─────────────────────────────────────────────────────────────────────────────

cat("================================================================\n")
cat("PROBLEM 2: Matrix Algebra in R\n")
cat("================================================================\n\n")

A <- matrix(c(4, 2, 1,
              2, 5, 3,
              1, 3, 6), nrow = 3, ncol = 3, byrow = TRUE)

cat("Matrix A:\n")
print(A)
cat("\n")

# ── Part (a): Determinant ────────────────────────────────────────────────────
cat("── Part (a): Determinant ────────────────────────────────────\n")

det_A <- det(A)
cat("det(A) =", det_A, "\n\n")

# ── Part (b): Eigenvalues ────────────────────────────────────────────────────
cat("── Part (b): Eigenvalues ────────────────────────────────────\n")

eigen_result <- eigen(A)
eigenvalues  <- eigen_result$values
eigenvectors <- eigen_result$vectors

cat("Eigenvalues (λ₁, λ₂, λ₃):\n")
print(round(eigenvalues, 6))
cat("\nEigenvectors (columns = Q):\n")
print(round(eigenvectors, 6))
cat("\n")

# ── Part (c): Verify A = Q Λ Q⁻¹ ───────────────────────────────────────────
cat("── Part (c): Verify A = Q Λ Q⁻¹ ────────────────────────────\n")

Q               <- eigenvectors
Lambda          <- diag(eigenvalues)
Q_inv           <- solve(Q)
A_reconstructed <- Q %*% Lambda %*% Q_inv

cat("Original A:\n")
print(round(A, 6))
cat("\nReconstructed Q Λ Q⁻¹:\n")
print(round(A_reconstructed, 6))

max_diff <- max(abs(A - A_reconstructed))
cat("\nMax absolute difference:", max_diff, "(numerically zero ✓)\n\n")

# ── Part (d): Positive definiteness ─────────────────────────────────────────
cat("── Part (d): Positive Definiteness ─────────────────────────\n")

cat("Eigenvalues:", round(eigenvalues, 6), "\n")
all_positive <- all(eigenvalues > 0)
cat("All eigenvalues positive?", all_positive, "\n")
cat("\nExplanation: A symmetric matrix is positive definite if and only if\n")
cat("all its eigenvalues are strictly positive. Here all eigenvalues > 0,\n")
cat("so A IS positive definite. This also means det(A) > 0, which we\n")
cat("confirmed above, and that x'Ax > 0 for any non-zero vector x.\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# PROBLEM 3: Empirical Covariance and Correlation
# ─────────────────────────────────────────────────────────────────────────────

cat("================================================================\n")
cat("PROBLEM 3: Empirical Covariance and Correlation (mtcars)\n")
cat("================================================================\n\n")

data_vars <- mtcars[, c("mpg", "hp", "wt")]
cat("First 6 rows of selected data (mpg, hp, wt):\n")
print(head(data_vars))
cat("\n")

# ── Part (a): Covariance matrix ──────────────────────────────────────────────
cat("── Part (a): Sample Covariance Matrix (Σ) ───────────────────\n")

Sigma <- cov(data_vars)
cat("Σ =\n")
print(round(Sigma, 4))
cat("\n")

# ── Part (b): Correlation matrix ────────────────────────────────────────────
cat("── Part (b): Correlation Matrix (R) ─────────────────────────\n")

R_corr <- cor(data_vars)
cat("R =\n")
print(round(R_corr, 4))
cat("\n")

# ── Part (c): Verify R = D^(-1/2) Σ D^(-1/2) ───────────────────────────────
cat("── Part (c): Verify R = D^(-1/2) Σ D^(-1/2) ────────────────\n")

variances  <- diag(Sigma)
D          <- diag(variances)
D_neg_half <- diag(1 / sqrt(variances))
R_manual   <- D_neg_half %*% Sigma %*% D_neg_half

cat("Built-in cor():\n")
print(round(R_corr, 6))
cat("\nManual D^(-1/2) Σ D^(-1/2):\n")
print(round(R_manual, 6))

max_diff_corr <- max(abs(R_corr - R_manual))
cat("\nMax absolute difference:", max_diff_corr, "(numerically zero ✓)\n\n")

# ── Part (d): Discussion ─────────────────────────────────────────────────────
cat("── Part (d): Which variable is most associated with mpg? ────\n")

cat("Correlations with mpg:\n")
cat("  mpg vs hp  :", round(R_corr["mpg","hp"], 4), "\n")
cat("  mpg vs wt  :", round(R_corr["mpg","wt"], 4), "\n")
cat("\nConclusion: 'wt' (car weight) has a stronger negative correlation\n")
cat("with mpg than 'hp' (horsepower). Both reduce fuel efficiency, but\n")
cat("weight is the stronger predictor in this dataset.\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# PROBLEM 4: Distribution Simulation (Exponential / CLT)
# ─────────────────────────────────────────────────────────────────────────────

cat("================================================================\n")
cat("PROBLEM 4: Distribution Simulation\n")
cat("================================================================\n\n")

lambda <- 2
n_obs  <- 50

# ── Part (a): Simulate n = 50 observations ───────────────────────────────────
cat("── Part (a): Simulate 50 Exp(λ=2) observations ──────────────\n")

set.seed(123)
sample_exp <- rexp(n_obs, rate = lambda)
cat("First 10 values:", round(head(sample_exp, 10), 4), "\n\n")

# ── Part (b): Sample mean ────────────────────────────────────────────────────
cat("── Part (b): Sample mean of one simulation ──────────────────\n")

x_bar_one <- mean(sample_exp)
cat("X̄ =", round(x_bar_one, 4), "  (theoretical E[X] = 1/λ =", 1/lambda, ")\n\n")

# ── Part (c): Repeat 5,000 times → distribution of X̄ ───────────────────────
cat("── Part (c): 5,000 repetitions ──────────────────────────────\n")

set.seed(123)
M         <- 5000
xbar_dist <- numeric(M)

for (m in 1:M) {
  xbar_dist[m] <- mean(rexp(n_obs, rate = lambda))
}

cat("Simulation complete.\n")
cat("Empirical mean of X̄  :", round(mean(xbar_dist), 6), "\n")
cat("Empirical var  of X̄  :", round(var(xbar_dist), 6), "\n\n")

# ── Part (d): Histogram + CLT normal overlay ─────────────────────────────────
cat("── Part (d): Histogram with CLT Normal Overlay ──────────────\n")

mu_CLT    <- 1 / lambda
sigma_CLT <- sqrt(1 / (n_obs * lambda^2))

hist(xbar_dist,
     breaks = 50,
     freq   = FALSE,
     col    = "#4A90D9",
     border = "white",
     main   = "Empirical Distribution of X̄ vs CLT Normal Approximation",
     xlab   = "Sample Mean X̄",
     ylab   = "Density")

x_grid <- seq(min(xbar_dist), max(xbar_dist), length.out = 300)
lines(x_grid,
      dnorm(x_grid, mean = mu_CLT, sd = sigma_CLT),
      col = "red", lwd = 2.5)

legend("topright",
       legend = c("Empirical", "CLT Normal"),
       fill   = c("#4A90D9", NA),
       border = c("white", NA),
       lty    = c(NA, 1),
       col    = c(NA, "red"),
       lwd    = c(NA, 2.5),
       bty    = "n")

# ── Part (e): Compare empirical vs theoretical ───────────────────────────────
cat("── Part (e): Empirical vs Theoretical values ─────────────────\n")

theoretical_mean <- 1 / lambda
theoretical_var  <- 1 / (n_obs * lambda^2)

cat("                        Theoretical     Empirical\n")
cat("E[X̄]  (mean)        :", sprintf("%-16s", round(theoretical_mean, 6)),
    round(mean(xbar_dist), 6), "\n")
cat("Var[X̄] (variance)   :", sprintf("%-16s", round(theoretical_var, 6)),
    round(var(xbar_dist), 6), "\n")
cat("\nConclusion: The empirical values are very close to the theoretical\n")
cat("values. The CLT guarantees that X̄ ~ N(1/λ, 1/(nλ²)) for large n,\n")
cat("and n=50 is sufficient here for an excellent approximation.\n\n")


# ─────────────────────────────────────────────────────────────────────────────
# PROBLEM 5: Data Manipulation and Group Statistics (iris)
# ─────────────────────────────────────────────────────────────────────────────

cat("================================================================\n")
cat("PROBLEM 5: Data Manipulation and Group Statistics (iris)\n")
cat("================================================================\n\n")

# ── Part (a): Mean and variance of Sepal.Length by species ───────────────────
cat("── Part (a): Sepal.Length mean & variance by species ─────────\n")

sepal_stats <- aggregate(Sepal.Length ~ Species,
                         data = iris,
                         FUN  = function(v) c(mean = mean(v), var = var(v)))

cat("Sepal.Length stats by species:\n")
print(sepal_stats)
cat("\n")

# ── Part (b): Table of Petal.Length mean & SD by species ────────────────────
cat("── Part (b): Petal.Length mean & SD table ───────────────────\n")

petal_table <- data.frame(
  Species           = levels(iris$Species),
  Mean_Petal.Length = tapply(iris$Petal.Length, iris$Species, mean),
  SD_Petal.Length   = tapply(iris$Petal.Length, iris$Species, sd)
)
row.names(petal_table) <- NULL

for (i in 1:3) {
  cat(sprintf("  %-12s  Mean = %.4f   SD = %.4f\n",
              petal_table$Species[i],
              petal_table$Mean_Petal.Length[i],
              petal_table$SD_Petal.Length[i]))
}
cat("\n")

# ── Part (c): Scatter plot ───────────────────────────────────────────────────
cat("── Part (c): Scatter plot ───────────────────────────────────\n")

species_colors <- c("setosa"     = "#E74C3C",
                    "versicolor" = "#27AE60",
                    "virginica"  = "#2980B9")

plot(iris$Petal.Length, iris$Sepal.Length,
     col  = species_colors[as.character(iris$Species)],
     pch  = 19,
     cex  = 1.2,
     main = "Sepal.Length vs Petal.Length by Species",
     xlab = "Petal Length (cm)",
     ylab = "Sepal Length (cm)")

for (sp in levels(iris$Species)) {
  sub  <- iris[iris$Species == sp, ]
  fit  <- lm(Sepal.Length ~ Petal.Length, data = sub)
  x_sp <- seq(min(sub$Petal.Length), max(sub$Petal.Length), length.out = 50)
  lines(x_sp, predict(fit, data.frame(Petal.Length = x_sp)),
        col = species_colors[sp], lwd = 2)
}

legend("topleft",
       legend = levels(iris$Species),
       col    = species_colors,
       pch    = 19,
       lty    = 1,
       lwd    = 2,
       bty    = "n",
       title  = "Species")

cat("Scatter plot displayed in Plots panel.\n\n")

# ── Part (d): Within-species correlation ────────────────────────────────────
cat("── Part (d): Within-species correlation (Sepal.Length vs Petal.Length)\n")

for (sp in levels(iris$Species)) {
  sub <- iris[iris$Species == sp, ]
  r   <- cor(sub$Sepal.Length, sub$Petal.Length)
  cat(sprintf("  %-12s  r = %.4f\n", sp, r))
}
cat("\n")

# ── Part (e): Discussion ─────────────────────────────────────────────────────
cat("── Part (e): Comment on within-species relationships ─────────\n")
cat("All three species show positive correlations between Sepal.Length\n")
cat("and Petal.Length within their groups. The relationships are broadly\n")
cat("similar in direction, but strength differs:\n")
cat("  • Setosa has a moderate positive correlation.\n")
cat("  • Versicolor and Virginica show stronger correlations.\n")
cat("This illustrates Simpson's Paradox: the overall (pooled) correlation\n")
cat("is much higher than the within-group correlations because the three\n")
cat("species occupy very different regions of the feature space.\n\n")

cat("================================================================\n")
cat("By Ahmed Ismail.\n")
cat("================================================================\n")