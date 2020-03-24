input = seq(0,100, 10)

#Factorial_loop: a version that computes the factorial of an integer using looping (such as a for loop)
Factorial_loop <- function(input) {
  sol <- 1
  if (input == 0){
    return(1)
  } else {
    for (i in 1:input){
      sol <- sol * i
    }
    return(sol)
  }
}

#Factorial_reduce: a version that computes the factorial using the reduce() 
#function in the purrr package. 
#Alternatively, you can use the Reduce() function in the base package.
Factorial_reduce <- function(input){
  if (input == 0){
    return(1)
  } else {
    return(purrr::reduce(c(as.numeric(1:input)), `*`))
  }
}

#Factorial_func: a version that uses recursion to compute the factorial.
Factorial_func <- function(input){
  if (input == 0){return(1)}
  else {
    return(input * Factorial_func(input - 1))
  }
  return(sol)
}

fac_tbl <- c(rep(NA, input))
#Factorial_mem: a version that uses memoization to compute the factorial.
Factorial_mem <- function(input){
  if(input == 0){
    return(1)
    } else{
      fac_tbl[input] <<- input * Factorial_mem(input - 1)
    }
  return(fac_tbl[input])
}

factor_test <- function(input){
  microbenchmark::microbenchmark(
    Factorial_loop(input),
    Factorial_reduce(input),
    Factorial_func(input),
    Factorial_mem(input)
  )
}


factor_test(10)
factor_test(50)
factor_test(100)
factor_test(1000)
