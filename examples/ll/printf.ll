declare i32 @printf(i8*, ...) #1
@formatString = private constant [2 x i8] c"%d" 


define i32 @main() {
    %v = alloca i32
    store i32 34, i32* %v
    %p = load i32, i32* %v
    call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @formatString , i32 0, i32 0), i32 %p)

    ret i32 0
}