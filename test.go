package main

func t(x []int) []int{
  var y []int = append(x, 4)
  println(y[0])
  return y
}

func main() int {
  var x []int
  x = t(x)
  return x[0]
}