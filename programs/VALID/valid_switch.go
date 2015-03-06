package valid_switch
  type a struct{ x int }

func x() {
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