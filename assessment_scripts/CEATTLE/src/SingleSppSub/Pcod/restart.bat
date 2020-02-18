:: write the original file to start model
cp pcod_orig.dat pcod.dat
cp pcod_orig.ctl pcod.ctl
:: amakl -nox -ind pcod.ctl -iprint 100 -nohess >NUL
call arcit.bat %1
