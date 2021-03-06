
;;------------------------------------------------
;; SLICE FUNCTIONS
;;------------------------------------------------


%slice = type {
  i32, ;; cur len
  i32, ;; max cap
  i32, ;; typ len
  i8*  ;; data
}

declare i8* @malloc(i32)
declare void @free(i8*)
declare i8* @memcpy(i8*, i8*, i32)

define %slice* @new_slice(i32 %length, i32 %capacity, i32 %tylen) nounwind {
  %base = getelementptr %slice* null, i32 1, i32 0
  %hsize = ptrtoint i32* %base to i32
  
  %1     = call i8* @malloc(i32 %hsize)
  %res   = bitcast i8* %1 to %slice*

  %elsize = mul i32 %capacity, %tylen
  %ptr = call i8* @malloc(i32 %elsize)

  %curszptr = getelementptr %slice* %res, i32 0, i32 0
  store i32 %length, i32* %curszptr

  %sizeptr = getelementptr %slice* %res, i32 0, i32 1
  store i32 %capacity, i32* %sizeptr

  %elszptr  = getelementptr %slice* %res, i32 0, i32 2
  store i32 %tylen, i32* %elszptr

  %bufptr   = getelementptr %slice* %res, i32 0, i32 3
  store i8* %ptr, i8** %bufptr

  ret %slice* %res
}

define void @del_slice(%slice* %this) nounwind {

  ;; Deallocation is for people who can't scale

  ret void
}

define void @resize(%slice* %this, i32 %len) {
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

define i32 @copy(%slice %dst, %slice %src) {
  %dlen = extractvalue %slice %dst, 0
  %slen = extractvalue %slice %src, 0

  %1 = icmp sgt i32 %dlen, %slen
  %mlen = select i1 %1, i32 %slen, i32 %dlen ;; Find min length

  %dbuf = extractvalue %slice %dst, 3
  %sbuf = extractvalue %slice %src, 3

  %tylen = extractvalue %slice %dst, 2

  %2 = mul i32 %mlen, %tylen

  call i8* @memcpy(i8* %dbuf, i8* %sbuf, i32 %2)

  ret i32 %mlen
}
 
define %slice @append(%slice %this, i8* %el) {
  %len   = extractvalue %slice %this, 0

  %cap   = extractvalue %slice %this, 1

  %1 = sub i32 %cap, %len ; space left in slice
  %2 = icmp ne i32 %1, 0
  %ncap = mul i32 %cap, 2 ; new cap
  %ocap = select i1 %2, i32 %cap, i32 %ncap ; if we need to, grow

  %elsz = extractvalue %slice %this, 2
  %newlen = add i32 %len, 1
  %ret = call %slice* @new_slice(i32 %newlen, i32 %ncap, i32 %elsz)
  %retval = load %slice* %ret

  call i32 @copy(%slice %retval, %slice %this)

  %4 = getelementptr %slice* %ret, i32 0, i32 3
  %5 = load i8** %4

  %6 = mul i32 %len, %elsz
  %7 = getelementptr i8* %5, i32 %6

  call i8* @memcpy(i8* %7, i8* %el, i32 %elsz)

  ret %slice %retval
}

;;------------------------------------------------
;; STRING FUNCTIONS
;;------------------------------------------------

define %slice @add_string(%slice %a, %slice %b) {
  %alen = extractvalue %slice %a, 0
  %blen = extractvalue %slice %b, 0

  %nlen = add i32 %alen, %blen

  %ret = call %slice* @new_slice(i32 %nlen,i32 %nlen, i32 1)

  %1 = extractvalue %slice %a, 3
  %2 = extractvalue %slice %b, 3

  %3 = getelementptr %slice* %ret, i32 0, i32 3
  %4 = load i8** %3

  %5 = getelementptr i8* %4, i32 %alen

  call i8* @memcpy(i8* %4, i8* %1, i32 %alen)
  call i8* @memcpy(i8* %5, i8* %2, i32 %blen)

  %8 = load %slice* %ret

  ret %slice %8
}

define %slice* @string_constant(i8* %cons, i32 %len) {
  %str = call %slice* @new_slice(i32 %len, i32 %len, i32 1)
  %bufptr = getelementptr %slice* %str, i32 0, i32 3
  store i8* %cons, i8** %bufptr

  ret %slice* %str
}

;;-----------------------------------------------
;; PRINT FUNCTIONS
;;-----------------------------------------------


declare void @putchar(i8)

define void @print.tstring(%slice %this) {
  %len = extractvalue %slice %this, 0

  %elsz = extractvalue %slice %this, 2

  %ptr = extractvalue %slice %this, 3

  br label %loop
  
  loop:
    %i = phi i32 [%len, %0], [%new_i, %loop]
    %p = phi i8* [%ptr, %0], [%new_p, %loop]

    %c = load i8* %p
    call void @putchar(i8 %c)

    %new_i  = sub i32 %i, 1
    %new_p  = getelementptr i8* %p, i32 %elsz

    %test = icmp ne i32 %new_i, 0
    br i1 %test, label %loop, label %loopdone

  loopdone:  

  ret void
}

define void @print.trune (i8 %this) {
  call void @putchar(i8 %this)
  ret void
}

@.fmtf = private unnamed_addr constant [3 x i8] c"%f\00"
@.fmti = private unnamed_addr constant [3 x i8] c "%d\00"
@.fmtslice = private unnamed_addr constant [14 x i8] c"[%d/%d](%d)%p\00"
@.fmtstruct = private unnamed_addr constant [3 x i8] c"%p\00"

declare i32 @printf(i8* noalias nocapture, ...)

; Function Attrs: nounwind readnone
define void @print.float(double %this) #1 {
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.fmtf, i32 0, i32 0), double %this)
  ret void
}

define void @print.slice(%slice*   %this) {

  ret void
}

define void @print.tinteger(i64 %this) {
  %call = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.fmti, i32 0, i32 0), i64  %this)
  ret void
}
