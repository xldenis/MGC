package variable_scopes

func x() {
  var x int
  {
    var x float64
    {
      var x string
      x = "sss"
      {
        var x bool = true

        if x {
          var x = 1
        }
      }
    }
    x = 1.0
  }
  x = 1
}