; simple while loops in ir
; # Tiny lang program
; let a: usize = 0;
; while(a < 10) {
;   a += 1;
; }
; 
declare i32 @printf(i8*, ...) #1
declare i32 @puts(i8* nocapture) nounwind
@d = private constant [4 x i8] c"%d\n" 
@n = private constant [2 x i8] c"\n"

; define i64 @main() {
;     main_block:
;     %a = alloca i64
;     store i64 0, ptr %a
;     br label %loop_cond

;     loop_cond:
;     %cond_a = load i64, ptr %a
;     %cond = icmp slt i64 %cond_a, 10
;     br i1 %cond, label %loop_body, label %end_main_block

;     loop_body:
;     %temp_a = load i64, ptr %a
;     call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @d, i64 0, i64 0), i64 %temp_a)
;     call i32 @puts(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @n, i64 0, i64 0))
;     %new_a = add i64 %temp_a, 1
;     store i64 %new_a, ptr %a
;     br label %loop_cond

;     end_main_block:
;     ret i64 0;
; }


define i64 @main() {
    main_block:
    %a = alloca i64
    store i64 0, ptr %a
    br label %loop_cond

    loop_cond:
    %cond_a = load i64, ptr %a
    %cond = icmp slt i64 %cond_a, 10
    br i1 %cond, label %loop_body, label %end_main_block

    loop_body:
    %temp_a = load i64, ptr %a
    call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @d, i64 0, i64 0), i64 %temp_a)
    call i32 @puts(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @n, i64 0, i64 0))
    %new_a = add i64 %temp_a, 1
    store i64 %new_a, ptr %a
    br label %loop_cond

    end_main_block:
    ret i64 0;
}