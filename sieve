##Sieve of Eratosthenes to find primes
## Takes in the xth prime number you want, sieves all the non-prime numbers,
## and returns a list of 1:xth prime.
sieve <- function(x) {
  t <- as.integer(x * (log(x) + log(log(x)))) ##https://en.wikipedia.org/wiki/Prime_number_theorem#Approximations_for_the_nth_prime_number
  if (x < 6) { ## Approximation only valid for x>=6
    t <- x ^ 2 ## Make the for loop run linear if so...
  }
  if (x <= 1) {
    y<-2
  }
  if (x > 1) {
    y <- 2:t
    for (i in 2:as.integer(sqrt(t))) {
      if (i %in% y) {
        y <- y[!y %% i == 0 | y == i]
      }
    }
  }
  y
}
