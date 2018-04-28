#! /bin/bash
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

for file in $(ls *.jb)
do
    ../interpreter $file >out 2>err
    code=$?
    if [ -f $file.err ]
    then expected_code=1
    else expected_code=0
    fi
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
