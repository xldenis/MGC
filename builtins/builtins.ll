
;;------------------------------------------------
;; SLICE FUNCTIONS
;;------------------------------------------------


%slice = type {
  i32, ;; cur len
  i32, ;; max cap
  i32, ;; elm len
  i8*  ;; data
}

declare i8* @malloc(i32)
declare void @free(i8*)
declare i8* @memcpy(i8*, i8*, i32)

define fastcc %slice* @new_slice(i32 %length, i32 %capacity, i32 %tylen) nounwind {
  %base = getelementptr %slice* null, i32 1, i32 0
  %hsize = ptrtoint i32* %base to i32
  
  %1     = call i8* @malloc(i32 %hsize)
  %res   = bitcast i8* %1 to %slice*

  %elsize = mul i32 %capacity, %tylen
  %ptr = call i8* @malloc(i32 %elsize)

  %sizeptr = getelementptr %slice* %res, i32 0, i32 1
  store i32 %capacity, i32* %sizeptr

  %curszptr = getelementptr %slice* %res, i32 0, i32 1
  store i32 %length, i32* %curszptr

  %elszptr  = getelementptr %slice* %res, i32 0, i32 2
  store i32 %tylen, i32* %elszptr

  ret %slice* %res
}

define fastcc void @del_slice(%slice* %this) nounwind {
  ;; TODO

  ret void
}

define fastcc void @resize(%slice* %this, i32 %len) {
  %1 = getelementptr %slice* %this, i32 0, i32 1
  %cap = load i32* %1
  %rsz = icmp sgt i32 %cap, %len

  br i1 %rsz, label %end, label %realloc

  realloc: 
  %2 = getelementptr %slice* %this, i32 0, i32 3
  %curbuf = load i8** %2

  %3 = getelementptr %slice* %this, i32 0, i32 0
  %curlen = load i32* %3

  %4 = getelementptr %slice* %this, i32 0, i32 2
  %elsz = load i32* %4

  %newsz = mul i32 %len, %elsz
  %cursz = mul i32 %curlen, %elsz

  %buf = call i8* @malloc(i32 %newsz)

  %5 = call i8* @memcpy(i8* %buf, i8* %curbuf, i32 %cursz)

  call void @free(i8* %curbuf)

  store i8* %buf, i8** %2
  store i32 %len, i32* %1

  br label %end

  end:
  ret void
}

define fastcc i32 @copy(%slice* %dst, %slice* %src) {
  %1 = getelementptr %slice* %dst, i32 0, i32 0
  %2 = getelementptr %slice* %src, i32 0, i32 0

  %dlen = load i32* %1
  %slen = load i32* %2

  %3 = icmp sgt i32 %dlen, %slen
  %mlen = select i1 %3, i32 %slen, i32 %dlen ;; Find min length

  %4 = getelementptr %slice* %dst, i32 0, i32 3
  %5 = getelementptr %slice* %src, i32 0, i32 3

  %dbuf = load i8** %4
  %sbuf = load i8** %5

  call i8* @memcpy(i8* %dbuf, i8* %sbuf, i32 %mlen)

  ret i32 %mlen
}
 
define fastcc %slice* @append(%slice* %this, i8* %el) {
  %1 = getelementptr %slice* %this, i32 0, i32 1
  %cap = load i32* %1

  %2 = getelementptr %slice* %this, i32 0, i32 0
  %len = load i32* %2

  %3 = sub i32 %cap, %len
  %4 = icmp ne i32 %3, 0
  %ncap = mul i32 %cap, 2
  %ocap = select i1 %4, i32 %cap, i32 %ncap

  %5 = getelementptr %slice* %this, i32 0, i32 2
  %elsz = load i32* %5
  %ret = call %slice* @new_slice(i32 %len, i32 %ncap, i32 %elsz)

  call i32 @copy(%slice* %ret, %slice* %this)

  %7 = getelementptr %slice* %ret, i32 0, i32 3
  %8 = load i8** %7

  %9 = mul i32 %len, %elsz
  %10 = getelementptr i8* %8, i32 %9

  call i8* @memcpy(i8* %10, i8* %el, i32 %elsz)

  ret %slice* %ret
}

;;------------------------------------------------
;; STRING FUNCTIONS
;;------------------------------------------------

define %slice* @add_string(%slice* %a, %slice* %b) {
  %1 = getelementptr %slice* %a, i32 0, i32 0
  %2 = getelementptr %slice* %b, i32 0, i32 0

  %alen = load i32* %1
  %blen = load i32* %2

  %nlen = add i32 %alen, %blen

  %ret = call %slice* @new_slice(i32 %nlen,i32 %nlen, i32 1)

  %3 = getelementptr %slice* %a, i32 0, i32 3
  %4 = getelementptr %slice* %b, i32 0, i32 3

  %5 = load i8** %3
  %6 = load i8** %4

  %7 = getelementptr i8* %6, i32 %alen

  %8 = getelementptr %slice* %ret, i32 0, i32 3
  %9 = load i8** %8
  call i8* @memcpy(i8* %9, i8* %5, i32 %alen)
  call i8* @memcpy(i8* %9, i8* %7, i32 %blen)

  ret %slice* %ret
}