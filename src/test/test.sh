#! /bin/bash
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;36m'
BOLD='\e[1m'
RESET='\033[0m'

echo ""
echo -e ${BLUE}${BOLD}Good programs tests${RESET}
for file in $(ls good/*.jb)
do
    ../interpreter $file >out 2>err
    code=$?
    expected_code=0
    if diff $file.out out 2>&1 >/dev/null && [ $code = $expected_code ]
    then
        echo -e $file ${GREEN}OK${RESET}
    else
        echo -e $file ${RED}ERROR${RESET}
        cat out
        cat err
    fi
    rm out
    rm err
done

echo ""
echo -e ${BLUE}${BOLD}Bad programs tests${RESET}
for file in $(ls bad/*.jb)
do
    ../interpreter $file >out 2>err
    code=$?
    expected_code=1
    if [ $code = $expected_code ]
    then
        echo -e $file ${GREEN}OK${RESET}
	echo -n -e '\t'
        cat err
    else
        echo -e $file ${RED}ERROR${RESET}
    fi
    rm out
    rm err
done
