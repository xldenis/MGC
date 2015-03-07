package scanner_literal_rune_invalid_escape

type Test int

func x() {
  type Test int
  var x Test = Test(1)
  var y Test = x
}