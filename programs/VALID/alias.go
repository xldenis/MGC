package alias

type a int

type b a

type c b

func x() {
  var a c = 1
}