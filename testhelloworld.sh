./toplevel.native test.dic > test.ll;
llc test.ll > test.s
cc -o test.exe test.s 
./test.exe
