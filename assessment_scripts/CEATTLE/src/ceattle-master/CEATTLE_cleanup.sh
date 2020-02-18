########################################################
# CEATTLE_cleanup.sh
# Main script for running the estimation mode of CEATTLE
# Kirstin Holsman
# kirstin.holsman@noaa.gov
# Last Updated: May 2016
# ./CEATTLE_cleanup.sh
#  Other notes
#  chmod +x CEATTLE_cleanup.sh   will create the executable
#  !/bin/bash
#   cd /Users/kkari/Dropbox/AFSC_models/CEATTLE/CEATTLE-master/ceattle-master
########################################################
default_execut='ceattle'
execut_name=$default_execut

while [ "${1+defined}" ]; do
    while :
    do
        case "$1" in
          -e | --executable)
             execut_name="$2"   #
             # echo reading executable
             shift 2
            ;;
          --) # End of all options
              shift
              break
              ;;
          -*)
            echo "Error: Unknown option: $1" >&2
            exit 1
            ;;
          *)  # No more options
            break
            ;;
        esac
        
    done
    shift
done

cd main
find . -maxdepth 1 -name "*$execut_name.l*" -exec rm {} \;
find . -maxdepth 1 -name "*$execut_name.r*" -exec rm {} \;
find . -maxdepth 1 -name "*$execut_name.p*" -exec rm {} \;
find . -maxdepth 1 -name "*$execut_name.b*" -exec rm {} \;
find . -maxdepth 1 -name "*eigv.rpt*" -exec rm {} \;
find . -maxdepth 1 -name "*input_ceattle.log*" -exec rm {} \;
find . -maxdepth 1 -name "*fmin.log*" -exec rm {} \;
find . -maxdepth 1 -name "*variance*" -exec rm {} \;
find . -maxdepth 1 -name "*$execut_name.log*" -exec rm {} \; 
find . -maxdepth 1 -name "*$execut_name.std*" -exec rm {} \; 
find . -maxdepth 1 -name "*$execut_name.eva*" -exec rm {} \; 
find . -maxdepth 1 -name "*$execut_name.cor*" -exec rm {} \; 
find . -maxdepth 1 -name "*Future_report.rep*" -exec rm {} \; 
find . -maxdepth 1 -name "*admodel.*" -exec rm {} \; 

rm -rf results
rm -rf dat_input_files/*
rm -rf fits_4_CEATTLE/*
echo "$execut_name cleaned"
cd ..

