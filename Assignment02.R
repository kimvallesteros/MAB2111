# 1. Define an R function that removes NA values from a vector.

RemoveNA <- function(x)
{
  return(x[!is.na(x)])
}

MyVector <- c(10, 20, NA, 30, 40, NA, 50, NA, 60, 70)
RemoveNA(MyVector)

#another solution

RemoveNA <- function(x) #input length from the default vector
{
  MyVector <- c(10, 20, NA, 30, 40, NA, 50, NA, 60, 70)
  L <- x
  print(L)
  print(MyVector)
  
  for (i in 1:L)
  {
    print(i)
    print(MyVector[i])
    if (is.na(MyVector[i])==TRUE) 
    {
      j<-i
      while (j<L)
      {
        MyVector[j]<-MyVector[j+1]
        j<-j+1
      }
      L<-L-1
      print(MyVector, width=L)
    }
  }
  length(MyVector)<-L
  print(MyVector)
}

# 2. Define an R function that computes the factorial of a given integer argument. The output should be a vector of length 1.

NFactorial <- function(x)
{ #This function considers all integer inputs
  if(x<0) {print("Factorial is not applicable to negative integers!")}
  else if (x==0){
    NFactorial <- 1
    print(NFactorial)
  } else {
    NFactorial <- 1
    for (i in 1:x)
      NFactorial <- NFactorial *i
    print(NFactorial)
  }
}
  
  
#Define an R function that computes the determinant of a given matrix. The output should be a vector of length 1.
#Define an R function that sorts a given vector in decreasing order. The output should be a vector of the same length. It should accept both numeric or character vectors.
#Define an R function that accepts a Date (POSIXct) as argument and outputs the day of the week as characters. Use modulo operator.


# 6. Create a function to compute for your net pay at work.

NetPay <- function(MonthlyGrossPay, Deductions=0)
{
  GrossPay = MonthlyGrossPay*12
  if (GrossPay<= 250000){tax<-0}
  else if (GrossPay<=400000) {tax <- (GrossPay - 250000)*0.2}
  else if (GrossPay<=800000) {tax <- (GrossPay - 400000)*0.25 + 30000}
  else if (GrossPay<=2000000) {tax <- (GrossPay - 800000)*0.3 + 130000}
  else if (GrossPay<=8000000) {tax <- (GrossPay - 2000000)*0.32 + 490000}
  else {tax <- (GrossPay - 8000000)*0.35 + 2410000}

  NetPay = MonthlyGrossPay - (tax/12) - Deductions
  print(NetPay)
}

#Create a function that accepts a vector and an integer n and returns nth highest number


# 8. Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal.

CompInterest <- function(Principal, Rate=0.05, Compoundings=1)
{
  CompInterest <- (Principal * (1+Rate)^Compoundings) - Principal
  print(CompInterest)
}


# 9. Create a function isPrime(n) that accepts an integer and outputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime number or not.

isPrime <- function(n)
{
  if (n<2) {print("Prime or not Prime are for integers greater than 2 only!")}
  else if(n==2){print(TRUE)}
  else {
  CheckUntil <- round(sqrt(n)) #Check divisibility up to sqrt(n)
  i <- 2
  while (i<=CheckUntil)
  {
    if (n %% i == 0) {
      isPrime = FALSE
      break
    }
    else {
      i <- i + 1
      isPrime = TRUE
      }
  } #end while
  print(isPrime)
  }
}

