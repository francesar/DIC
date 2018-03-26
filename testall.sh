LI="lli"

# Path to the LLVM compiler
LLC="llc"

DIC="./toplevel.native"

ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.dic//'`
    reffile=`echo $1 | sed 's/.dic$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.ll ${basename}.s ${basename}.exe ${basename}.out" &&
    Run "$DIC" "$1" ">" "${basename}.ll" &&
    Run "$LLC" "${basename}.ll" ">" "${basename}.s" &&
    Run "./${basename}.exe" > "${basename}.out" &&
    Compare ${basename}.out ${reffile}.out ${basename}.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

if [ $# -ge 1 ]
then
    files=$@
else
    files="*.dic"
fi

Check test.dic 2>> $globallog