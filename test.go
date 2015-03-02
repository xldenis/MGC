package main

var array [15]int;

func bubble(){
  var done bool = false;
  var swapped bool;
  for ; !done; {
    printarray()
    swapped = false;
    var x int;
    for x=1; x < 15; x++ {
      if array[x-1] > array[x] {
        var temp = array[x];
        array[x] = array[x-1]
        array[x-1] = temp
        swapped = true;
      }
    }
    if !swapped {
      done = true;
    }
  }
  return;
}
func printarray(){
  var iter int = 15;
  for ; iter > 0; iter-- {
    print("[", array[15-iter], "]");
  }
  println();
  return;
}

func main(){
  array[0] = 15;
  array[1] = 14;
  array[2] = 13;
  array[3] = 12
  array[4] = 11
  array[5] = 10;
  array[6] = 9
  array[7] = 8
  array[8] = 7;
  array[9] = 6
  array[10] = 5
  array[11] = 4;
  array[12] = 3
  array[13] = 2
  array[14] = 1
  
  bubble();
  println ("sorted.")
  printarray();
  return;
}
