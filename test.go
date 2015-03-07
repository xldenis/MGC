package scanner_literal_rune_invalid_escape

type Test int

func x() {
  type Test float64
  var x Test = Test(1)
  var y Test = x
}
