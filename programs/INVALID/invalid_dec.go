package invalid_dec

func x() {
  x := 1
  type a int
  var y a = x // error cant use int as a
}