package alias

type a int

func() {
  type a float
  var x a = 1 // should be a type error
}