RunTestPos() {
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

RunTestNeg() {
    CMP1=$(echo $1 | cut -d'.' -f 1)
    RES1="-res"
    FILE1=$CMP1$RES1
    ./toplevel.native $1 &> test
    cmp --silent test $FILE1 && echo "$1 PASSED" || echo "$1 FAILED"
    rm test
}

echo "Running positive tests"
for file in tests/pos/*.dic
do 
   RunTestPos $file
done 

echo "Running negative tests"
for file in tests/neg/*.dic
do
    RunTestNeg $file
done



echo "Done"