RunTest() {
    CMP=$(echo $1 | cut -d'.' -f 1)
    ./toplevel.native $1 > $CMP.ll;
    llc $CMP.ll > $CMP.s
    cc -o $CMP.exe $CMP.s 
    ./$CMP.exe > test 
    rm $CMP.ll $CMP.s $CMP.exe
    RES="-res"
    FILE=$CMP$RES
    cmp --silent test $FILE && echo "$1 PASSED" || echo "$1 FAILED"
}

for file in test_cg/pos/*.dic
do 
   RunTest $file
done 

echo "Done"