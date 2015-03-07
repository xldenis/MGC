package main

func x() {
  type a int
  var x a = a(1)
  {
    type b float64
    var y b = b(1.0)
    x += a(1)
    y = y + y
  }
}