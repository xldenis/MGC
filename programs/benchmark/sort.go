package main

func main() {
  var l = 5 0000
  var list []int
  for i := 0; i < l; i++ {
    list = append(list, l - i)
  }  
  list = Sort(list, l)
  for i := 0; i < l; i++ { print(list[i], " ")}
}

func Sort(x []int, leng int) []int {
  var out []int
  for i := 0; i < leng; i++ {
    var cur = x[i]
    for j := 0; j <= i; j++ {
      if j == i {
        out = append(out, cur)
        break;
      } else if cur < out[j] {
        var old = out[j]
        for k := j+1; k < i; k++ {
          out[k], old = old, out[k]
        }
        out = append(out, old)
        out[j] = cur
        break
      }
    }
  }
  return out
}