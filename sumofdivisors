## This function returns the sum of all divisors of a number, including the number itself.
## Here I am using the method of primefactorization as well as the divisor function to generate the sum.

sumofdivisors<-function(x,y=0,z=0){
  if(z==0){ ## Flag indicating whether or not another function has already sourced primefactors.R so we don't source it again.
  source('primefactors.R')
  }
  if(y[1]==0){ ## Flag indicating whether or not the list of primes has already been generated so don't regenerate one.
    source('sieve.R')
    y<-sieve(x,x) ## If the list of primes from 1:x has not been generated, generate it.
  }
  t<-primefactors(x,y) ## Find all the prime factors of x, passing the list of primes as y.
  
  ## Example:
  
  ## > primefactors(42,y)
  ## num
  ## 2 3 7   
  ## 1 1 1   Prime factors are 2^1, 3^1, and 7^1
  
  vec1<-as.numeric(rownames(t)) ## t is a table. rownames(t) are the prime factors
  vec2<-as.numeric(t) ## the values of t are the powers of each prime factor
  vec3<-vector(mode = 'numeric',length=length(vec1)) ## Initialize a vector which will be the sum of the divisors.
  for(i in 1:length(vec1)){
    vec3[i]<-(vec1[i]^(vec2[i]+1)-1)/(vec1[i]-1) ## Here I am using the divisor function sigma_1(n) Eq: (18) from http://mathworld.wolfram.com/DivisorFunction.html
  }
prod(vec3) ## Return the sum of all divisors
}
