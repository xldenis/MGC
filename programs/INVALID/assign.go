package assignable

type a int
type b int

func x() {
  var x a
  var y b
  x == y // Mismatch a and b
}