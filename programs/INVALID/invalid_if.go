package invalid_if
type a bool
func x() {
  type a int
  var x a
  
  if x {

  }
}