; ModuleID = 'main'

%slice.0 = type { i32, i32, i32, i8* }

@.fmtf = private unnamed_addr constant [3 x i8] c"%f\00"
@.fmti = private unnamed_addr constant [3 x i8] c"%d\00"

; Function Attrs: nounwind readnone
define internal fastcc i1 @isPrime(i64 %x) #0 {
entry:
  %0 = icmp slt i64 %x, 4
  br i1 %0, label %for.end, label %for.body.lr.ph

for.body.lr.ph:                                   ; preds = %entry
  %1 = sdiv i64 %x, 2
  br label %for.body

for.start:                                        ; preds = %for.body
  %2 = icmp sgt i64 %5, %1
  br i1 %2, label %for.end, label %for.body

for.body:                                         ; preds = %for.body.lr.ph, %for.start
  %storemerge1 = phi i64 [ 2, %for.body.lr.ph ], [ %5, %for.start ]
  %3 = srem i64 %x, %storemerge1
  %4 = icmp eq i64 %3, 0
  %5 = add i64 %storemerge1, 1
  br i1 %4, label %for.end, label %for.start

for.end:                                          ; preds = %for.start, %for.body, %entry
  %merge = phi i1 [ true, %entry ], [ false, %for.body ], [ true, %for.start ]
  ret i1 %merge
}

; Function Attrs: nounwind
define void @main() #1 {
entry:
  br label %for.body

for.body:                                         ; preds = %if.exit, %entry
  %0 = phi i64 [ 0, %entry ], [ %2, %if.exit ]
  %1 = tail call fastcc i1 @isPrime(i64 %0)
  br i1 %1, label %if.then, label %if.exit

for.end:                                          ; preds = %if.exit
  ret void

if.then:                                          ; preds = %for.body
  tail call void @print.tinteger(i64 %0)
  br label %if.exit

if.exit:                                          ; preds = %for.body, %if.then
  %2 = add i64 %0, 1
  %exitcond = icmp eq i64 %2, 300000
  br i1 %exitcond, label %for.end, label %for.body
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i32) #1

; Function Attrs: nounwind
declare void @free(i8* nocapture) #1

; Function Attrs: nounwind
declare i8* @memcpy(i8*, i8* nocapture readonly, i32) #1

; Function Attrs: nounwind
define noalias %slice.0* @new_slice(i32 %length, i32 %capacity, i32 %tylen) #1 {
  %1 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (%slice.0* null, i32 1, i32 0) to i32)) #1
  %res = bitcast i8* %1 to %slice.0*
  %elsize = mul i32 %tylen, %capacity
  %ptr = tail call i8* @malloc(i32 %elsize) #1
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
define void @del_slice(%slice.0* nocapture %this) #0 {
  ret void
}

; Function Attrs: nounwind
define void @resize(%slice.0* nocapture %this, i32 %len) #1 {
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
define i32 @copy(%slice.0* nocapture readonly %dst, %slice.0* nocapture readonly %src) #1 {
  %1 = getelementptr %slice.0* %dst, i32 0, i32 0
  %2 = getelementptr %slice.0* %src, i32 0, i32 0
  %dlen = load i32* %1
  %slen = load i32* %2
  %3 = icmp sgt i32 %dlen, %slen
  %mlen = select i1 %3, i32 %slen, i32 %dlen
  %4 = getelementptr %slice.0* %dst, i32 0, i32 3
  %5 = getelementptr %slice.0* %src, i32 0, i32 3
  %dbuf = load i8** %4
  %sbuf = load i8** %5
  %6 = getelementptr %slice.0* %dst, i32 0, i32 2
  %tylen = load i32* %6
  %7 = mul i32 %tylen, %mlen
  %8 = tail call i8* @memcpy(i8* %dbuf, i8* %sbuf, i32 %7)
  ret i32 %mlen
}

; Function Attrs: nounwind
define noalias %slice.0* @append(%slice.0* nocapture readonly %this, i8* nocapture readonly %el) #1 {
  %1 = getelementptr %slice.0* %this, i32 0, i32 0
  %len = load i32* %1
  %2 = getelementptr %slice.0* %this, i32 0, i32 1
  %cap = load i32* %2
  %ncap = shl i32 %cap, 1
  %3 = getelementptr %slice.0* %this, i32 0, i32 2
  %elsz = load i32* %3
  %newlen = add i32 %len, 1
  %ret = tail call %slice.0* @new_slice(i32 %newlen, i32 %ncap, i32 %elsz)
  %4 = tail call i32 @copy(%slice.0* %ret, %slice.0* %this)
  %5 = getelementptr %slice.0* %ret, i32 0, i32 3
  %6 = load i8** %5
  %7 = mul i32 %elsz, %len
  %8 = getelementptr i8* %6, i32 %7
  %9 = tail call i8* @memcpy(i8* %8, i8* %el, i32 %elsz)
  ret %slice.0* %ret
}

; Function Attrs: nounwind
define noalias %slice.0* @add_string(%slice.0* nocapture readonly %a, %slice.0* nocapture readonly %b) #1 {
  %1 = getelementptr %slice.0* %a, i32 0, i32 0
  %2 = getelementptr %slice.0* %b, i32 0, i32 0
  %alen = load i32* %1
  %blen = load i32* %2
  %nlen = add i32 %blen, %alen
  %ret = tail call %slice.0* @new_slice(i32 %nlen, i32 %nlen, i32 1)
  %3 = getelementptr %slice.0* %a, i32 0, i32 3
  %4 = getelementptr %slice.0* %b, i32 0, i32 3
  %5 = load i8** %3
  %6 = load i8** %4
  %7 = getelementptr i8* %6, i32 %alen
  %8 = getelementptr %slice.0* %ret, i32 0, i32 3
  %9 = load i8** %8
  %10 = tail call i8* @memcpy(i8* %9, i8* %5, i32 %alen)
  %11 = tail call i8* @memcpy(i8* %9, i8* %7, i32 %blen)
  ret %slice.0* %ret
}

; Function Attrs: nounwind
declare void @putchar(i8) #1

; Function Attrs: nounwind
define void @print.string(%slice.0* nocapture readonly %this) #1 {
  %1 = getelementptr %slice.0* %this, i32 0, i32 0
  %len = load i32* %1
  %2 = getelementptr %slice.0* %this, i32 0, i32 2
  %elsz = load i32* %2
  %3 = getelementptr %slice.0* %this, i32 0, i32 3
  %ptr = load i8** %3
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
define void @print.trune(i8 %this) #1 {
  tail call void @putchar(i8 %this)
  ret void
}

; Function Attrs: nounwind
declare i32 @printf(i8* noalias nocapture readonly, ...) #1

; Function Attrs: nounwind
define void @print.float(double %this) #1 {
  %call = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.fmtf, i32 0, i32 0), double %this)
  ret void
}

; Function Attrs: nounwind readnone
define void @print.slice(%slice.0* nocapture %this) #0 {
  ret void
}

; Function Attrs: nounwind
define void @print.tinteger(i64 %this) #1 {
  %call = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.fmti, i32 0, i32 0), i64 %this)
  ret void
}

attributes #0 = { nounwind readnone }
attributes #1 = { nounwind }
