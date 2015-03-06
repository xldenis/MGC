package valid_switch
func x() {
  type a struct{ x int }
  var y a
  switch x := 1; x /* int */ < 1 /* bool */ {
    case true:
      println("yay")
    case false:
      println("boo")
    default:
      println("this cant happen")
  }
  1 + 1 /* int */
  y /* a */.x /* int */
}