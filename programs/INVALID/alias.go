package alias

type a int

func x() {
  type a float64
  var x a = 1 // should be a type error
}