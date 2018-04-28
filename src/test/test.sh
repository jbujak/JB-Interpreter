#! /bin/bash
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

echo "Good programs tests"
for file in $(ls good/*.jb)
do
    ../interpreter $file >out 2>err
    code=$?
    expected_code=0
    if diff $file.out out 2>&1 >/dev/null && [ $code = $expected_code ]
    then
        echo -e $file ${GREEN}OK${NC}
    else
        echo -e $file ${RED}ERROR${NC}
        cat out
        cat err
    fi
    rm out
    rm err
done

echo "Bad programs tests"
for file in $(ls bad/*.jb)
do
    ../interpreter $file >out 2>err
    code=$?
    expected_code=1
    if [ $code = $expected_code ]
    then
        echo -e $file ${GREEN}OK${NC}
	echo -n -e '\t'
        cat err
    else
        echo -e $file ${RED}ERROR${NC}
    fi
    rm out
    rm err
done
