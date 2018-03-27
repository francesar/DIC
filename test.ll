; ModuleID = 'DIC'

@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@tmp = private unnamed_addr constant [12 x i8] c"hello world\00"

declare i8 @printf(i8*, ...)

define i32 @main() {
entry:
  %printf = call i8 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([12 x i8]* @tmp, i32 0, i32 0))
  ret i32 0
}
