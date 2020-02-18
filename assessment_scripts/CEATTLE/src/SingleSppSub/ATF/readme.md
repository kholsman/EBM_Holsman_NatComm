# ATF single species notes
atfbsai_orig.dat is the starting file

Sequence for loop is to run   
  1. ` restart.bat ` once (sets up correct datafile)    

    :: write the original file to start model
    cp atfbsai_orig.dat atfbsai.dat  

Then within each loop run   
  2. ` atf_ass.bat `  This step takes output from atf_msm.dat and adds it to atfbsai.dat
  (increments by one year) and 
  then runs the assessment on that year and writes ` atf_catch.dat `
  for subsequent year (and reading by MSM)   

    :: step 1
    :: read the new atf_msm.dat file and append to atfbsai.dat file
    atfrw -ind atfbsai.dat
    :: now run the assessment model (which will write atf_catch.dat)
    cp atfnew.dat atfbsai.dat
    atfbsai -nox -iprint 100
    cat atf_catch.dat
