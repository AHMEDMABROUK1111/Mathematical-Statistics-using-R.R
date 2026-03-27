# Mathematical-Statistics-using-R.R
Problem set 1
Problem #1: Descriptive Statistics and Sampling
Consider the following vector in R:

x = c(6, 9, 11, 4, 15, 7, 13, 10)

a) Compute the sample mean, sample variance, and sample standard deviation of the vector.
b) Verify numerically in R that

s
2 =
1
n − 1
Xn
i=1
(xi − x ̄)
2

by computing both sides explicitly.
c) Draw 10,000 bootstrap samples from the vector x (sampling with replacement).
Compute the bootstrap distribution of the sample mean and report
• the bootstrap estimate of E[X ̄]
• the bootstrap estimate of V ar(X ̄)
d) Briefly comment on how the bootstrap variance compares with the theoretical variance
estimate

V ar(X ̄) = s
2
n
.

Problem #2: Matrix Algebra in R
Consider the matrix

A =


4 2 1
2 5 3
1 3 6


1

a) Compute the determinant of A.
b) Compute the eigenvalues of A using R.
c) Verify numerically that

A = QΛQ
−1
where Λ is the diagonal matrix of eigenvalues.
d) Using the eigenvalues, determine whether the matrix A is positive definite.
Explain briefly how the eigenvalues determine this property.

Problem #3: Empirical Covariance and Correlation
Use the built-in dataset mtcars in R.
a) Compute the sample covariance matrix for the variables mpg, hp, and wt.
b) Compute the corresponding correlation matrix.
c) Using matrix notation, verify numerically that
R = D
−1/2ΣD
−1/2

where Σ is the covariance matrix and D is the diagonal matrix of variances.
d) Based on the correlation matrix, discuss which variable appears most strongly associated
with fuel efficiency (mpg).

Problem #4: Distribution Simulation
Let X1, . . . , Xn be i.i.d. random variables drawn from an exponential distribution with rate
parameter λ = 2.
a) Using R, simulate n = 50 observations from this distribution.
b) Compute the sample mean X ̄.
c) Repeat the simulation 5,000 times and obtain the empirical distribution of X ̄.
d) Plot the histogram of the simulated X ̄ values.
Overlay the theoretical normal approximation implied by the Central Limit Theorem.

2

e) Compare the empirical mean and variance of X ̄ with the theoretical values

E(X ̄) = 1
λ
, V ar(X ̄) = 1
nλ2
.

Problem #5: Data Manipulation and Group Statistics
Use the built-in dataset iris.
a) Compute the sample mean and variance of Sepal.Length for each species.
b) Using R, construct a table summarizing the mean and standard deviation of Petal.Length
by species.
c) Create a scatter plot of Sepal.Length versus Petal.Length colored by species.
d) Compute the within-species correlation between these two variables.
e) Briefly comment on whether the relationship between these variables appears similar across
species.
