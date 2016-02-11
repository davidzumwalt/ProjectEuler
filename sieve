##Sieve of Eratosthenes to find primes
## Takes in the xth prime number you want, sieves all the non-prime numbers,
## and returns the xth prime.
sieve <- function(x,z=0) {
  t <- as.integer(x * (log(x) + log(log(x)))) ##https://en.wikipedia.org/wiki/Prime_number_theorem#Approximations_for_the_nth_prime_number
  if (x < 6) { ## Approximation only valid for x>=6
    t <- x ^ 2 ## Make the for loop run linear if so...
  }
  if (x <= 1) {
    y<-2
  }
  if (x > 1) {
    y <- 2:t
    if(z>0){ ## The argument z determines if you only want primes below a certain value, like 100.
      y<-2:z ## In this case, we can override the x argument telling us to guarantee we will find the xth prime.
    }
    s<-max(y)
    for (i in 2:as.integer(sqrt(s))) {
      if (i %in% y) {
       y <- y[!y %% i == 0 | y == i]
      }
    }
  }
  y
}
