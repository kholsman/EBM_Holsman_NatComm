@echo off
echo 20000 >atf_catch.dat
:: step 1
:: Argument is endyr
:: read the new atf_msm.dat file and append to atfbsai.dat file
atfrw -ind atfbsai.dat
:: now run the assessment model (which will write atf_catch.dat)
cp atfnew.dat atfbsai.dat
:: atfbsai -nox -iprint 100 -nohess >NUL
:: call cleanad
call arcit.bat %1
