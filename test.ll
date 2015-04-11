; ModuleID = 'main'

%slice.0 = type { i32, i32, i32, i8* }

@.fmtf = private unnamed_addr constant [3 x i8] c"%f\00"
@.fmti = private unnamed_addr constant [3 x i8] c"%d\00"

; Function Attrs: nounwind
define internal fastcc { i32, i32, i32, i8* } @t(%slice.0 %x) #0 {
entry:
  %0 = alloca %slice.0
  %1 = alloca i64
  store i64 4, i64* %1
  %2 = bitcast i64* %1 to i8*
  %3 = call %slice.0 @append(%slice.0 %x, i8* %2)
  store %slice.0 %3, %slice.0* %0
  %4 = getelementptr %slice.0* %0, i64 0, i32 3
  %5 = load i8** %4
  %6 = bitcast i8* %5 to i64*
  %7 = load i64* %6
  tail call void @print.tinteger(i64 %7)
  tail call void @print.trune(i8 10)
  %oldret = extractvalue %slice.0 %3, 0
  %newret = insertvalue { i32, i32, i32, i8* } undef, i32 %oldret, 0
  %oldret1 = extractvalue %slice.0 %3, 1
  %newret2 = insertvalue { i32, i32, i32, i8* } %newret, i32 %oldret1, 1
  %oldret3 = extractvalue %slice.0 %3, 2
  %newret4 = insertvalue { i32, i32, i32, i8* } %newret2, i32 %oldret3, 2
  %oldret5 = extractvalue %slice.0 %3, 3
  %newret6 = insertvalue { i32, i32, i32, i8* } %newret4, i8* %oldret5, 3
  ret { i32, i32, i32, i8* } %newret6
}

; Function Attrs: nounwind
define i64 @main() #0 {
entry:
  %0 = tail call %slice.0* @new_slice(i32 0, i32 10, i32 1)
  %1 = load %slice.0* %0
  %2 = tail call fastcc { i32, i32, i32, i8* } @t(%slice.0 %1)
  %newret = extractvalue { i32, i32, i32, i8* } %2, 0
  %oldret = insertvalue %slice.0 undef, i32 %newret, 0
  %newret1 = extractvalue { i32, i32, i32, i8* } %2, 1
  %oldret2 = insertvalue %slice.0 %oldret, i32 %newret1, 1
  %newret3 = extractvalue { i32, i32, i32, i8* } %2, 2
  %oldret4 = insertvalue %slice.0 %oldret2, i32 %newret3, 2
  %newret5 = extractvalue { i32, i32, i32, i8* } %2, 3
  %oldret6 = insertvalue %slice.0 %oldret4, i8* %newret5, 3
  store %slice.0 %oldret6, %slice.0* %0
  %3 = getelementptr %slice.0* %0, i64 0, i32 3
  %4 = load i8** %3
  %5 = bitcast i8* %4 to i64*
  %6 = load i64* %5
  ret i64 %6
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i32) #0

; Function Attrs: nounwind
declare void @free(i8* nocapture) #0

; Function Attrs: nounwind
declare i8* @memcpy(i8*, i8* nocapture readonly, i32) #0

; Function Attrs: nounwind
define noalias %slice.0* @new_slice(i32 %length, i32 %capacity, i32 %tylen) #0 {
  %1 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (%slice.0* null, i32 1, i32 0) to i32)) #0
  %res = bitcast i8* %1 to %slice.0*
  %elsize = mul i32 %tylen, %capacity
  %ptr = tail call i8* @malloc(i32 %elsize) #0
  %curszptr = getelementptr %slice.0* %res, i32 0, i32 0
  store i32 %length, i32* %curszptr
  %sizeptr = getelementptr %slice.0* %res, i32 0, i32 1
  store i32 %capacity, i32* %sizeptr
  %elszptr = getelementptr %slice.0* %res, i32 0, i32 2
  store i32 %tylen, i32* %elszptr
  %bufptr = getelementptr %slice.0* %res, i32 0, i32 3
  store i8* %ptr, i8** %bufptr
  ret %slice.0* %res
}

; Function Attrs: nounwind readnone
define void @del_slice(%slice.0* nocapture %this) #1 {
  ret void
}

; Function Attrs: nounwind
define void @resize(%slice.0* nocapture %this, i32 %len) #0 {
  %1 = getelementptr %slice.0* %this, i32 0, i32 1
  %cap = load i32* %1
  %rsz = icmp sgt i32 %cap, %len
  br i1 %rsz, label %end, label %realloc

realloc:                                          ; preds = %0
  %2 = getelementptr %slice.0* %this, i32 0, i32 3
  %curbuf = load i8** %2
  %3 = getelementptr %slice.0* %this, i32 0, i32 0
  %curlen = load i32* %3
  %4 = getelementptr %slice.0* %this, i32 0, i32 2
  %elsz = load i32* %4
  %newsz = mul i32 %elsz, %len
  %cursz = mul i32 %elsz, %curlen
  %buf = tail call i8* @malloc(i32 %newsz)
  %5 = tail call i8* @memcpy(i8* %buf, i8* %curbuf, i32 %cursz)
  tail call void @free(i8* %curbuf)
  store i8* %buf, i8** %2
  store i32 %len, i32* %1
  br label %end

end:                                              ; preds = %realloc, %0
  ret void
}

; Function Attrs: nounwind
define i32 @copy(%slice.0 %dst, %slice.0 %src) #0 {
  %dlen = extractvalue %slice.0 %dst, 0
  %slen = extractvalue %slice.0 %src, 0
  %1 = icmp sgt i32 %dlen, %slen
  %mlen = select i1 %1, i32 %slen, i32 %dlen
  %dbuf = extractvalue %slice.0 %dst, 3
  %sbuf = extractvalue %slice.0 %src, 3
  %tylen = extractvalue %slice.0 %dst, 2
  %2 = mul i32 %mlen, %tylen
  %3 = tail call i8* @memcpy(i8* %dbuf, i8* %sbuf, i32 %2)
  ret i32 %mlen
}

; Function Attrs: nounwind
define %slice.0 @append(%slice.0 %this, i8* nocapture readonly %el) #0 {
  %len = extractvalue %slice.0 %this, 0
  %cap = extractvalue %slice.0 %this, 1
  %ncap = shl i32 %cap, 1
  %elsz = extractvalue %slice.0 %this, 2
  %newlen = add i32 %len, 1
  %ret = tail call %slice.0* @new_slice(i32 %newlen, i32 %ncap, i32 %elsz)
  %retval = load %slice.0* %ret
  %1 = tail call i32 @copy(%slice.0 %retval, %slice.0 %this)
  %2 = getelementptr %slice.0* %ret, i32 0, i32 3
  %3 = load i8** %2
  %4 = mul i32 %len, %elsz
  %5 = getelementptr i8* %3, i32 %4
  %6 = tail call i8* @memcpy(i8* %5, i8* %el, i32 %elsz)
  ret %slice.0 %retval
}

; Function Attrs: nounwind
define %slice.0 @add_string(%slice.0 %a, %slice.0 %b) #0 {
  %alen = extractvalue %slice.0 %a, 0
  %blen = extractvalue %slice.0 %b, 0
  %nlen = add i32 %blen, %alen
  %ret = tail call %slice.0* @new_slice(i32 %nlen, i32 %nlen, i32 1)
  %1 = extractvalue %slice.0 %a, 3
  %2 = extractvalue %slice.0 %b, 3
  %3 = getelementptr %slice.0* %ret, i32 0, i32 3
  %4 = load i8** %3
  %5 = getelementptr i8* %4, i32 %alen
  %6 = tail call i8* @memcpy(i8* %4, i8* %1, i32 %alen)
  %7 = tail call i8* @memcpy(i8* %5, i8* %2, i32 %blen)
  %8 = load %slice.0* %ret
  ret %slice.0 %8
}

; Function Attrs: nounwind
define noalias %slice.0* @string_constant(i8* %cons, i32 %len) #0 {
  %str = tail call %slice.0* @new_slice(i32 %len, i32 %len, i32 1)
  %bufptr = getelementptr %slice.0* %str, i32 0, i32 3
  store i8* %cons, i8** %bufptr
  ret %slice.0* %str
}

; Function Attrs: nounwind
declare void @putchar(i8) #0

; Function Attrs: nounwind
define void @print.tstring(%slice.0 %this) #0 {
  %len = extractvalue %slice.0 %this, 0
  %elsz = extractvalue %slice.0 %this, 2
  %ptr = extractvalue %slice.0 %this, 3
  br label %loop

loop:                                             ; preds = %loop, %0
  %i = phi i32 [ %len, %0 ], [ %new_i, %loop ]
  %p = phi i8* [ %ptr, %0 ], [ %new_p, %loop ]
  %c = load i8* %p
  tail call void @putchar(i8 %c)
  %new_i = add i32 %i, -1
  %new_p = getelementptr i8* %p, i32 %elsz
  %test = icmp eq i32 %new_i, 0
  br i1 %test, label %loopdone, label %loop

loopdone:                                         ; preds = %loop
  ret void
}

; Function Attrs: nounwind
define void @print.trune(i8 %this) #0 {
  tail call void @putchar(i8 %this)
  ret void
}

; Function Attrs: nounwind
declare i32 @printf(i8* noalias nocapture readonly, ...) #0

; Function Attrs: nounwind
define void @print.float(double %this) #0 {
  %call = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.fmtf, i32 0, i32 0), double %this)
  ret void
}

; Function Attrs: nounwind readnone
define void @print.slice(%slice.0* nocapture %this) #1 {
  ret void
}

; Function Attrs: nounwind
define void @print.tinteger(i64 %this) #0 {
  %call = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.fmti, i32 0, i32 0), i64 %this)
  ret void
}

attributes #0 = { nounwind }
attributes #1 = { nounwind readnone }
