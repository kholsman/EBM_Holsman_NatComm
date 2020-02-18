:: reset_all_ss.bat
cd ../poll/
call restart.bat %1
cd ../pcod/
call restart.bat %1
cd ../atf/
call restart.bat %1
cd ../main/
