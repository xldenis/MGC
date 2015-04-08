package main

func main() {
  var x = make([]int, 0, 10)
  var y = x
  x = append(x, 1)
  println(x)
  println(y)
}