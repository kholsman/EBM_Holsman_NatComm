#######!/bin/bash
########################################################
# CEATTLE_run.sh
# Main script for running the estimation mode of CEATTLE
# Kirstin Holsman
# kirstin.holsman@noaa.gov
# Last Updated: May 2016
# 
# ./CEATTLE_run.sh 0 or ./CEATTLE_run.sh 0 0    --> will run the ceattle model for single-species (0) and estimate RS functions
# ./CEATTLE_run.sh 2 or ./CEATTLE_run.sh 2 0    --> will run the ceattle model for multi-species (2) and estimate RS functions
# ./CEATTLE_run.sh 0 1                          --> will run the RS function fits for single species model (0) but skip the estimation mode of CEATTLE
# ./CEATTLE_run.sh 2 -1                         --> will run the estimation mode of CEATTLE for multi-species (2) and but skip RS functions
#
########################################################

########################################################
## JIM this is for you!
########################################################
# default_nm='ceattle'                           
# default file name if -f filename is not entered
default_execut='ceattle'

# read in the CEATTLE/.config folder configurations 

while IFS='' read -r line || [[ -n "$line" ]]; do
    tmp=${line[@]}   # assign the tmp obj to the line
    if [[ ${tmp[@]:0:1}  == "#" ]];then 
        # echo read in the name
        tmp_nm=${tmp[@]:2} 
        tmp_val="" # resest tmp_val
    else
        # echo assign the value to $tmp_nm
        tmp_val=${tmp[@]} 
         # echo $tmp_nm = ${tmp_val[@]}
        eval $tmp_nm=${tmp_val[@]}
        tmp_nm=""  # resest tmp_nm
    fi

 done < "../../config"

 if [[ "$testread" != "12345" ]];then
    echo Problem with .config file
    echo $testread
    exit
fi

# output_dir=CEATTLE_outputs/CEATTLE_newest       # root folder for model runs
# model_path=/CEATTLE-master/ceattle-master       # where the model and run.sh scripts live
# rec_path=CEATTLE-master/ceattle_recruit-master  # where the recruiment model lives
# data_path=CEATTLE-master/Data                   # where the recruiment model lives
# SingleSpp_path=CEATTLE-master/ceattle_SingleSppSub  # where Jim's single species models live
# cntl_path=CEATTLE-master/Control_files          # where the control files live
# rec_cntl=ceattle_recruit/RS_models.ctl          # recruitment control file path 


########################################################
# Now run the script
########################################################
function CLEAN_MAIN {
    # first clean up files
    ./CEATTLE_cleanup.sh -e $execut_name
}



# read in the positional parmaeters from the command line 
cd ../..
DIR_main="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $model_path

declare -a input
declare -a plot=-1                              # default but it is over read below

ntmp=$#
skip=0  # 0 will run rec, +1 will skip est and run rec, -1 will skip rec and only run est (-1 defunct presently)
mode=$1
filename=$default_nm                                  # default but it is over read below
ctl_filename=$default_nm".ctl"                       # default but it is over read below
execut_name=$default_execut
overwrt=-1
while [ "${1+defined}" ]; do
    while :
    do
        case "$1" in
          -f | --file)
             filename="$2"   # You may want to check validity of $2
             # echo reading filename
             shift 2
             ;;
          -e | --executable)
             execut_name="$2"   #
             # echo reading executable
             shift 2
            ;;
          -ctl | --control)
              ctl_filename=$2".ctl"  # Call your function
              # no shifting needed here, we're done.
              shift 2
             ;;
           -skip | --skip)
              skip=1 # skip est mode
              shift 1
              echo "skip the recruitment skip = "$skip
             ;;   
          -o | --overwrite)
              overwrt=1  # Call your function
              shift 1
             ;;
        -plot | --figs)
              plot=1  # Call your function
              shift 1
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

CLEAN_MAIN  # clean up the main folder
echo ""

filename=$filename"_"$mode                         # default but it is over read below
output_path=$output_dir/$filename
if [ "$skip" == "0" ]; then
    if [ $overwrt -lt 0 ]; then
        cd $DIR_main/$output_dir
         if [ -f $FILENAME ]; then
            echo -n $filename" file exists. Overwrite? (y/n) > "
            read response
            if [ "$response" != "y" ]; then
                echo ""
                echo "Exiting CEATTLE."
                echo ""

                exit 1
            else
                rm -rf $filename/*
                echo  '          --> removing contents of '$filename' and running the model'
                echo ""
                #statements
            fi
        fi
        cd $DIR_main
    elif [ $overwrt -gt 0 ]; then

        echo '            --> overwrite the files ! '$DIR_main/$output_path
        rm -rf $DIR_main/$output_path/*

    fi
fi
echo ""

today=`date +%Y-%m-%d` # for creating archive


########################################################
# Functions 
########################################################



function mkdir_kir {
    if [ ! -f "$1" ]; then
        mkdir -p "$1" 2>/dev/null
    fi
}

function rm_kir {
    #if [ -f "$mode" ]; then
    rm -rf "$1" 2>/dev/null
    #fi
}

function runMSM_RS {

    oldval=${array[1]}
    unset tmp; unset RSfilename
    declare -a tmp
    IFSold2=$IFS
    IFS=""
    echo "# "${cofilenames[@]}" #"> "$inputfile"
    echo ${array[@]}>> "$inputfile"
    IFS=$IFSold2
    nocovars="TRUE"
    T2="TRUE"
    IFS=""
    for (( c=0; c<=2; c++ ))
    do 
      if [[ "${array[$c]}" -gt "$yourValue" ]]; then
        tmp[0]=${tmp[0]}_${cofilenames[$c]}
        nocovars="FALSE"
      fi
    done
    IFS=$IFSold

    if [ "$nocovars" = "$T2" ]; then
        echo nocovars = $nocovars
        RSfilename="_0_0_0"
        RSfilename_main="_0_0_0"
        echo $RSfilename >> RS_log.csv
        ./msm_rs -nox
        make_MSM_RS_files
    else
        RSfilename=(${tmp[@]})
        echo nocovars = $nocovars
        echo $RSfilename >> RS_log.csv
        ./msm_rs -nox
        make_MSM_RS_files
            
        RSfilename=${tmp[@]}_NR
        echo $RSfilename >> RS_log.csv
        ./msm_rs -noRation -nox
        make_MSM_RS_files
        RSfilename=${tmp[@]}
        RSfilename_main=${tmp[@]}
    fi

    for (( i=3; i<=${ncov}; i++ ))
        do 
        echo $i
        # turn on each covariate one by one
        unset tmp
        #unset RSfilename
        declare -a tmp
        array[i-2]=$oldval
        oldval=${array[$i-1]}
        array[$i-1]=2
        IFSold2=$IFS
        IFS=""
        echo ${array[@]}
        echo "# "${cofilenames[@]}" #"> "$inputfile"
        echo ${array[@]}>> "$inputfile"
        IFS=$IFSold2
        yourValue=0
        #IFS=" " 
        tmp=${RSfilename_main[@]}
        for (( c=2; c<${ncov}; c++ ))
        do 
          if [[ "${array[$c]}" -gt "$yourValue" ]]; then
            tmp[0]=${tmp[0]}_${cofilenames[$c]} 
          fi
        done
        IFS=$IFSold
        RSfilename=(${tmp[0]})
        echo $RSfilename >> RS_log.csv
        #echo $RSfilename
        ./msm_rs -nox
        make_MSM_RS_files
            
        #copy_MSM_RS_files
        RSfilename=${tmp[0]}_NR
        echo $RSfilename >> RS_log.csv
        ./msm_rs -noRation -nox
        make_MSM_RS_files
        #copy_MSM_RS_files

    done
}

function remove_MSM_RS_files {
 rm -rf RS_fits
 mkdir_kir RS_fits
}
function make_MSM_RS_files {
    #remove_MSM_RS_files
    cp  msm_rs_output.rep RS_fits/msm_rs_output$RSfilename.rep
    cp  rs_data4MSM.dat RS_fits/rs_data4MSM$RSfilename.dat
    cp  msm_rs.std RS_fits/msm_rs$RSfilename.std
    cp  msm_rs.par RS_fits/msm_rs$RSfilename.par
    cp  msm_rs.ctl RS_fits/msm_rs$RSfilename.ctl
}
function copy_MSM_RS_files {
    mkdir_kir ../$filename/results/RS_fits/
    cd ../$filename/results/
    remove_MSM_RS_files
    cd ../../MSM_RS
    cp -r RS_fits/* ../$filename/results/RS_fits
}


# ###########################################################
# Get things set up
# ###########################################################


rd=$DIR"/main"
tmpEst="run"
tmpRunRec="run"


if [ $mode -eq 0 ]; then
model_type_text="Single-species mode"
fi  
if [ $mode -eq 2 ]; then
model_type_text="Multi-species mode"
fi   
echo " *************************************"
echo " *************************************"
echo "                       "
echo "             CEATTLE Model            "
echo "                       "
echo "          "$model_type_text"          "
echo "          executable   = "$execut_name   
echo "          model run    = "$filename      
echo "          control file = "$ctl_filename
echo "                       "
# if [ $ntmp -gt 1 ]; then
    if [ $skip -gt 0 ]; then
        tmpEst="skip"

        echo "  Estimate CEATTLE parameters? = No (skip)"  # estimation for CEATTLE"
        echo "  Estimate rec par using CEATTLE_"$mode" values? = Yes" 
        rm -rf $DIR_main/$rec_path/RS_fits
        rm -rf $DIR_main/$rec_path/Rec_figs
        rm -rf $DIR_main/$rec_path/fits_4_CEATTLE
        rm -rf $DIR_main/$rec_path/RS1_log.csv
        rm -rf $DIR_main/$rec_path/RS2_log.csv
        rm -rf $DIR_main/$rec_path/RS3_log.csv
        rm -rf $DIR_main/$rec_path/convergeFail_log.csv
    fi
    if [ $skip -lt 0 ]; then
        tmpRunRec="skip"

        echo "  Estimate CEATTLE parameters? = Yes"  # estimation for CEATTLE"
        echo "  Estimate recruimtent parameters using $filename values? = No (skip)" 
    fi
# else

    echo "  Estimate CEATTLE parameters? = Yes"  # estimation for CEATTLE"
    echo "  Estimate recruimtent parameters using $filename values? = Yes" 

    rm -rf $DIR_main/$rec_path/RS_fits
    rm -rf $DIR_main/$rec_path/Rec_figs
    rm -rf $DIR_main/$rec_path/fits_4_CEATTLE
    rm -rf $DIR_main/$rec_path/RS1_log.csv
    rm -rf $DIR_main/$rec_path/RS2_log.csv
    rm -rf $DIR_main/$rec_path/RS3_log.csv
    rm -rf $DIR_main/$rec_path/convergeFail_log.csv

# fi
echo "                       "
echo " *************************************"
echo " *************************************"
RS_filenameAll=$filename"_RS" # ceattle_RS_0

# cd $outputPath

echo DIR_main/$output_path

if [ "$tmpEst" = "run" ]; then
  rm -rf $DIR_main/$output_path
  mkdir -p $DIR_main/$output_path
  mkdir -p $DIR_main/$output_path/results
  # mkdir -p $DIR_main/$output_path/results/$RS_filenameAll
  mkdir -p $DIR_main/$model_path/results
  # mkdir -p $DIR_main/$model_path/results/$RS_filenameAll
  echo $pwd
fi
# else
#   #clear the model recruitment files in the output
#    rm -rf $DIR_main/$output_path/results/$RS_filenameAll
#   # make a new model recruitment folder in the output
#    mkdir -p $DIR_main/$output_path/results/$RS_filenameAll
# fi

 # copy the relevant model files over to the working main directory
cd $DIR_main/$model_path/main

rm -rf results
mkdir -p results
# mkdir -p results/$RS_filenameAll
rm -rf dat_input_files
mkdir -p dat_input_files
cp      $DIR_main/$cntl_path/$ctl_filename $ctl_filename
cp -r   $DIR_main/$data_path/dat_input_files/* dat_input_files
echo ""
echo ""

# echo "#######################################"
# echo "#######################################"

# ###########################################################
# RUN THE MODEL
# ###########################################################


if [ "$tmpEst" = "run" ]; then
    echo running estimation for $filename
    echo ""
    ./$execut_name -msmMode $mode -ind $ctl_filename -iprint 100 -nox -maxph 13 
    #./ceattle -msmMode $mode -ind $ctl_filename -iprint 100 -nox -maxph 13 
    #./ceattle -msmMode 2 -ind asmnt2018_2A.ctl -iprint 100 -nox -maxph 13 
    # create Rdata file in the corresponding folder and archive older file 
    echo " "
    echo creating Rdata file of results
    echo ""

    # ______________________________________________ 
    # Copy files and create Rdata file of results
    # ______________________________________________ 
    # your are here:  # ~CEATTLE/CEATTLE-master/ceattle-master/main
    echo copy files over
    cp dat_input.log $DIR_main/$output_path/results/dat_input.log
    cp results/ceattle_R2.rep $DIR_main/$output_path/results/ceattle_R2_est.rep
    cp $execut_name.std $DIR_main/$output_path/results/ceattle_est.std   #"../../CEATTLE_outputs/CEATTLE_newest/ceattle_0/results/ceattle_est.std"
    cp $execut_name.rep $DIR_main/$output_path/results/ceattle.rep
    cp $execut_name.par $DIR_main/$output_path/results/ceattle.par
    cp -rf $execut_name.bar $DIR_main/$output_path/$filename.bar
    cp $ctl_filename  $DIR_main/$output_path/$ctl_filename

    # cd ../$outputPath/$filename
    cd $DIR_main/$output_path
    pwd
    Rscript  $DIR_main/$model_path/Scripts/R_code/MAKE_resultsRdata.R
    cd $DIR_main/$model_path"/main"
    
    cp -rf results/ceattle_rs.dat $DIR_main/$output_path/results/$filename"_rs.dat"
   
    if [ -f $DIR_main/$output_path/results/$filename"_rs.dat" ];then
        echo copied $filename"_rs.dat" file to  $DIR_main/$output_path/results
    else
        echo ERROR : copy $filename"_rs.dat" file to  $DIR_main/$output_path/results
        exit
    fi
    # ls  $DIR_main/$output_path/results
    rm -rf $ctl_filename
    echo " "
    echo completed: creating Rdata file of results
    echo ""
else
    echo skipping ceattle fit
fi

# ###########################################################
# RUN THE RECRUITMENT MODEL
# ###########################################################


cd $DIR_main/$output_path
# echo $DIR_main/$output_path
echo model_path = $model_path
rm -rf "tmpfile.txt"
echo "#######################################"
echo "#######################################"

if [ "$tmpRunRec" = "run" ]; then
    echo running recruitment analysis
    pwd
   
     echo "# output_path">"tmpfile.txt"
        echo  "$output_path">>"tmpfile.txt"
     echo "# model_path">>"tmpfile.txt"
        echo  "$model_path">>"tmpfile.txt"
     echo "# rec_path">>"tmpfile.txt"
        echo  "$rec_path">>"tmpfile.txt"   
     echo "# data_path">>"tmpfile.txt"
        echo  "$data_path">>"tmpfile.txt"   
     echo "# SingleSpp_path">>"tmpfile.txt"
        echo  "$SingleSpp_path">>"tmpfile.txt"   
     echo "# cntl_path">>"tmpfile.txt"
        echo  "$cntl_path">>"tmpfile.txt"  
     echo "# rec_cntl">>"tmpfile.txt"
        echo  "$rec_cntl">>"tmpfile.txt"                       
     echo "# DIR_main">>"tmpfile.txt"
        echo  "$DIR_main">>"tmpfile.txt"
     echo "# filename">>"tmpfile.txt"
        echo  "$filename">>"tmpfile.txt"
     echo "# mode">>"tmpfile.txt"
        echo  "$mode">>"tmpfile.txt" 
     echo "# ctl_filename">>"tmpfile.txt"
        echo  "$ctl_filename">>"tmpfile.txt"             
     echo "# testread">>"tmpfile.txt"
        echo  "12345">>"tmpfile.txt"          
 
   Rscript  $DIR_main/$model_path/Scripts/R_code/FIT_recruitment.R
   # Rscript ../ceattle-master/Scripts/R_code/FIT_recruitment.R
    # this runs the recruitment script from inside R

    # current directory is 1: ~CEATTLE/CEATTLE_outputs/CEATTLE_newest/ceattle_0

    # cp -rf $DIR_main/$rec_path/RS_fits          results/$RS_filenameAll/RS_fits
    # cp -rf $DIR_main/$rec_path/Rec_figs         results/$RS_filenameAll/Rec_figs 
    # cp -rf $DIR_main/$rec_path/fits_4_CEATTLE   results/$RS_filenameAll/fits_4_CEATTLE

    # rm -rf ceattle_recruit-master/RS_fits
    # rm -rf ceattle_recruit-master/Rec_figs
    # rm -rf ceattle_recruit-master/fits_4_CEATTLE
else
    echo skipping Recruitment fits. Note projections require these files
fi

echo "#######################################"
echo "#######################################"

if [ $plot -gt 0 ]; then
    echo plot estimation results using PLOT_CEATTLE_EST.R
    Rscript $DIR_main/$model_path/Scripts/R_code/PLOT_CEATTLE_EST.R
fi


cd $DIR_main/$model_path
# echo now the directory is:
# pwd

CLEAN_MAIN

