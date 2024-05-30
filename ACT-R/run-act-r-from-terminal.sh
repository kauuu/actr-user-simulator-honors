
## Change the terminal's title
echo -n -e "\033]0;ACT-R\007"

cd environment

processor=`uname -m`

if [ "$processor" = "arm64" ] 
then
  ./start-environment-osx-arm64 > /dev/null 2> /dev/null &
else
  ./start-environment-osx > /dev/null 2> /dev/null &
fi

cd ..
cd apps

## Run the included Lisp image

if [ "$processor" = "arm64" ] 
then
  ./act-r-arm64 --end-runtime-options --no-sysinit --no-userinit --load ../set-logical.lisp --eval "(load-patch-files)" --eval "(init-des)" --eval "(echo-act-r-output)" --eval "(load-user-files)" --eval "(mp-print-versions)" --eval "(declaim (sb-ext:muffle-conditions SB-KERNEL:REDEFINITION-WITH-DEFUN))"
else
  ./act-r-64 -n -l ../set-logical.lisp -e "(load-patch-files)" -e "(init-des)" -e "(echo-act-r-output)" -e "(load-user-files)" -e "(mp-print-versions)" 
fi