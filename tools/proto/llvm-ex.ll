
declare void @printf (i8 *);

define i32 @fact (i32 %n) {
entry:
	i1 %t1 = icmp i32 %n 0
	br i1 %t1, label %branch1, label %branch2
branch1:
	ret i32 1
branch2:
	i1 %t2 = icmp i32 %n 1;
	br i1 %t2, label %branch3, label %branch4
branch3:
	ret i32 1
branch4:
	i32 %m = 1
	i32 %f = 1
	br label %loop
loop:
	
}