#!/bin/bash
# @echo off
echo 20000 >atf_catch.dat
# :: step 1
# :: Argument is endyr
# :: read the new atf_msm.dat file and append to atfbsai.dat file
# :: now run the assessment model (which will write atf_catch.dat)
./atfrw -ind atfbsai.dat

cp atfnew.dat atfbsai.dat
# :: atfbsai -nox -iprint 100 -nohess >NUL
 # call cleanad
call arcit.bat %1
