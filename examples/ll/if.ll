
declare i32 @printf(i8*, ...) #1
@mod_d = private constant [2 x i8] c"%d" 

define i32 @something(i32 %a, i32 %b) {
    main_block:
    %c = icmp sgt i32 %a, %b
    br i1 %c, label %if_1, label %else_1

    if_1:
    ret i32 %a

    else_1:
    ret i32 %b
}

define i32 @main() {
    %large = call i32 @something(i32 10, i32 20)
    call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @mod_d , i32 0, i32 0), i32 %large)
    ret i32 0
}