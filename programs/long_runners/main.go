package main

func main() {
  for x := 0; x < 300000; x++ {
    if (isPrime(x)) {
      println(x,"is prime")
    } 
  }
}

func isPrime(x int) bool {
  for f := 2; f <= x / 2; f++ { // use x /2 rather than sqrt(2) since there is no sqrt func
    if x % f == 0 {
      return false;
    }
  }
  return true;
}