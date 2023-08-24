; ModuleID = './examples/simple_tlet.tiny'
source_filename = "./examples/simple_tlet.tiny"

define i64 @return_something(i64 %0) {
main_return_something_block:
  %1 = alloca i1, align 1
  store i1 false, ptr %1, align 1
  %2 = alloca i64, align 8
  store i64 3570, ptr %2, align 4
  %3 = alloca i64, align 8
  %4 = load i64, ptr %2, align 4
  %5 = add i64 %4, 353
  store i64 %5, ptr %3, align 4
}
