./toplevel.native $1 > tmp.ll
llc tmp.ll > tmp.s 
cc -o tmp.exe tmp.s len.o
./tmp.exe
rm tmp.ll tmp.s tmp.exe
