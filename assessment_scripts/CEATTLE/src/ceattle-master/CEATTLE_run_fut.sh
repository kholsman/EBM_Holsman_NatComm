######!/bin/bash
########################################################
# CEATTLE_run_futNew.sh
# script for running the projections of the model
# Kirstin Holsman
# kirstin.holsman@noaa.gov
# Last Updated: May 23 2016
# 
# cd Dropbox/Manuscripts/recruitment_ROMS/ceattle_recruit 
#
#recMode  : recruitment mode                                            
# 0 = project under mean  rec   (no RS)
# 1 = mean RS function (mean RS no covariates, ricker; rs_data4CEATTLE_0_0_0)
# 2 = RS function plus SST (rs_data4CEATTLE_SST
# 3 = RS function based on top AIC selected environm. parms for each spp (rs_data4CEATTLE_TOP)
# 4 = RS function based on model with top R2 value (rs_data4CEATTLE_TopR2)
# 5 = RS function based on Recfile_name (above)   
#
#harvestMode  : used  for the calcmort  function  in  the projection  / projulation  models  
#0= hindcast
#1= project under no  fishing,  
#2= project under mean  F  rate  from  hindcast, 
#3= project to  PF (F rate) that  matches Btarget,  
#4= fit to NewCatch.dat
#5= project under F+Fdev radnom seeded fishing rate  from  hindcast
#6= project using max F from hindcast
#7= project using mean catch from hindcast
#8= project using max catch from hindcast
#9= project using f profiles
# cd  /Users/kkari/GitHub/CEATTLE/src/ceattle-master
########################################################
########################################################

# default_nm='ceattle'                           # default file name if -f filename is not entered
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
    echo Problem with config file
    echo $testread
    exit
fi


########################################################
# read in the positional parmaeters from the command line 
cd ../..
DIR_main="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $model_path

########################################################
#  default_nm ='ceattle'
#  output_dir = runs
# model_path = src/ceattle-master
# rec_path = src/ceattle_recruit-master
# data_path =  src/Data
# SingleSpp_path =  src/ceattle_SingleSppSub
# cntl_path =  src/Control_files
# rec_cntl = ceattle_recruit/RS_models.ctl
# testread =  12345

# output_dir=CEATTLE_outputs/CEATTLE_newest       # root folder for model runs
# model_path=/CEATTLE-master/ceattle-master       # where the model and run.sh scripts live
# rec_path=CEATTLE-master/ceattle_recruit-master  # where the recruiment model lives
# data_path=CEATTLE-master/Data                   # where the recruiment model lives
# SingleSpp_path=CEATTLE-master/ceattle_SingleSppSub  # where Jim's single species models live
# cntl_path=CEATTLE-master/Control_files          # where the control files live
# rec_cntl=ceattle_recruit/RS_models.ctl          # recruitment control file path 

# while IFS='' read -r line || [[ -n "$line" ]]; do
#     echo "Text read from file: $line"
# done < "$1"
##############################################################
## Set up file names
##############################################################

# if [ -z "$2" ]; then
#   filename=ceattle_$1  # #filename=ceattle_0    
# else
#     filename=$2$1
# fi


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
declare -a ntmp=$#
declare -a skip=-1
declare -a mode=$1
declare -a modelname=$default_nm                # default but it is over read below
declare -a filename=$default_nm                # default but it is over read below
declare -a ctl_filename=$default_nm".ctl"      # default but it is over read below
declare -a recMode=0                           # default but it is over read below
declare -a harvestMode=1                       # default but it is over read below
declare -a overwrt=-1                          # default but it is over read below
declare -a randn=0                             # default but it is over read below
declare -a itr=1                               # default but it is over read below
declare -a mse=0                               # default but it is over read below
declare -a plot=-1                              # default but it is over read below


while [ "${1+defined}" ]; do
    while :
    do
        case "$1" in
          -f | --file)
             futfilename="$2"   # You may want to check validity of $2
             filename="$2"   # You may want to check validity of $2
             # echo reading filename
             shift 2
            ;;
          -e | --executable)
             execut_name="$2"   #
             # echo reading executable
             shift 2
            ;;
          -m | --model)
             filename="$2"  
             # echo reading filename
             shift 2
            ;;
          -ctl | --control)
              ctl_filename=$2".ctl"  
              shift 2
             ;;
           -r | --recMode)
              recMode=$2  # Set recMode
              shift 2
             ;;  
          -h | --harvestMode)
              harvestMode=$2  # Set harvest mode
              shift 2
             ;;   
          -mse | -opr)
              mse=$2  # Set harvest mode
              harvestMode=4
              shift 2
             ;; 
          -o | --overwrite)
              overwrt=1  # Call your function
              # no shifting needed here, we're done.
              shift 1
             ;;
          -rand | --mcmc)
              randn=1  # turn on the randnom rec etc (0 == off)
              itr=$2  # iteration number (e.g, 10 will run random realization # 10 NOT 10 iterations, see run_fut_blended fro that)
              shift 2
             ;; 
          -plot | --figs)
              plot=1  # Call your function
              shift 1
             ;; 
          # -skip | --skip)
          #     skip=1 # skip est mode
          #     # no shifting needed here, we're done.
          #     shift 1     
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

if [[ "$harvestMode" == "0" ]];then
  echo "ERROR harvestMode must be > 0 in projections!"
  exit
fi
# echo model name = $filename
filename_fut=$futfilename"_"$mode 
filename_fut=$filename_fut"_"$recMode"_"$harvestMode  #filename_fut=ceattle_0_2_2
if [ $randn -gt 0 ]; then
  filename_fut=$filename_fut"_mc"$itr  #filename_fut=ceattle_0_2_2  
fi

# if [ $plot -lt 0 ]; then
#   echo "plot is less than 0"
# if


today=`date +%Y-%m-%d` # for creating archive
filename=$filename"_"$mode  

output_path=$output_dir/$filename
input_path=$output_dir/$filename 


# echo $skip
# if [ $skip -let 0 ]; then
#  echo this is working
#  exit
    if [ $overwrt -lt 0 ]; then
        cd $DIR_main/$output_dir
        if [ -f "$filename_fut" ]; then

            echo -n $filename_fut" file exists. Overwrite? (y/n) > "
            read response
            if [ "$response" != "y" ]; then
                echo ""
                echo "Exiting CEATTLE."
                echo ""

                exit 1
            else
                rm -rf $filename_fut/*
                echo  '          --> removing contents of '$filename_fut' and running the model'
                echo ""
                #statements
            fi
        fi
        cd $DIR_main
    elif [ $overwrt -gt 0 ]; then

        echo '            --> overwrite the files ! '$DIR_main/$output_path
        rm -rf $DIR_main/$output_path/*

    fi
# fi
echo ""

echo ""
echo ""

# echo recMode = $recMode
# echo harvestMode= $harvestMode
# echo Mode = $mode

##############################################################
## Functions
##############################################################

function mkdir_kir {
 if [ ! -f "$1" ]; then
  mkdir -p "$1" 2>/dev/null
 fi
}
function RM_KIR {
 if [ -f "$1" ]; then
  rm "$1" 2>/dev/null
 fi
}

##############################################################
## Now set up projection folder in outputs
##############################################################
mkdir_kir $DIR_main/$output_path
mkdir_kir $DIR_main/$output_path/projections
mkdir_kir $DIR_main/$output_path/projections/$filename_fut
mkdir_kir $DIR_main/$output_path/projections/$filename_fut/results

# now navigate back to model folder and copy recruitment results into the main folder
cd $DIR_main/$model_path
cd main

rm -rf fits_4_CEATTLE
rm -rf results
mkdir_kir fits_4_CEATTLE 
mkdir_kir results
cp -rf $DIR_main/$input_path/Recruitment_files/fits_4_CEATTLE/* fits_4_CEATTLE/
cp -rf $DIR_main/$input_path/results/$filename"_rs.dat" fits_4_CEATTLE/ceattle_rs.dat

# clear the dat_input_files folder
rm -rf dat_input_files
mkdir_kir dat_input_files

# copy the control file from the working control file directory (not the output) into the main folder
cp -rf $DIR_main/$cntl_path/$ctl_filename $ctl_filename
cp -rf $DIR_main/$data_path/dat_input_files/* dat_input_files

# clear the projections folder
rm -rf $DIR_main/$output_path/projections/$filename_fut/results/*

# copy bar file into main:
cp -rf $DIR_main/$input_path/$filename.bar $DIR_main/$model_path/main/$filename.bar

if [ $mode -eq 0 ]; then
model_type_text="Single-species mode"
fi  
if [ $mode -eq 2 ]; then
model_type_text="Multi-species mode"
fi   
# Now run models:
echo " *************************************"
echo " *************************************"
echo "                       "
echo "          CEATTLE Model Projections           "
echo "                       "
echo "          "$model_type_text"          "
echo "          input path    = "$input_path    
echo "          output path    = "$output_path   
echo "          model run    = "$filename       
echo "          future run    = "$filename_fut    
echo "          control file = "$ctl_filename
echo "          recMode      = "$recMode
echo "          harvestMode  = "$harvestMode
echo "          folder  = "$filename_fut
echo "          "

# if MCMC
# echo $DIR_main/"src/Data/randomSeeds.csv"

inputfile=$DIR_main"/src/Data/randomSeeds.csv"  
# echo $inputfile

while IFS=" " read -r -a seeds
    do
      c1=$(($itr*6+1-6))
      c2=$(($itr*6+2-6))
      c3=$(($itr*6+3-6))
      c4=$(($itr*6+4-6))
      c5=$(($itr*6+5-6))
      c6=$(($itr*6+6-6))
        var0=${seeds[0]}
        tmpseed1=${seeds[$c1]} #itr*6+1-6
        tmpseed2=${seeds[$c2]} #itr+2-1
        tmpseed3=${seeds[$c3]} #itr+3-1
        tmpseed4=${seeds[$c4]} #itr+1-1
        tmpseed5=${seeds[$c5]} #itr+2-1
        tmpseed6=${seeds[$c6]} #itr+3-1

        # echo "$var0 $tmpseed1 $tmpseed2 $tmpseed3"
        #echo "$(($itr*6+1-6)) $(($itr*6+2-6)) $(($itr*6+3-6)) $(($itr*6+4-6)) $(($itr*6+5-6)) $(($itr*6+6-6))"
    done < "$inputfile"
# echo completed seeds
# if harvest mode is not 9 (F profiles) then... otherwise rund F profiles
if [ $harvestMode != "9" ]; then
  ./$execut_name -fut -msmMode $mode -opr $mse -ind $ctl_filename -recMode $recMode -rand_rec $randn -harvestMode $harvestMode -seed $tmpseed1 $tmpseed2 $tmpseed3 $tmpseed4 $tmpseed5 $tmpseed6 -iprint 100 -nox -binp $filename.bar -phase 22 -nohess
else
	# clear the recruitment model outputs folder of F_profiles
	rm -rf $DIR_main/$output_path/projections/$filename_fut/F_profiles/*
	# read in Frates
	declare -a FP_in
	declare -a Frates
	declare -a nFrates
	declare -a nProfiles
	#nf: number of frates
	# for each Frate in the profile
	# run CEATTLE
		# copy F_profile report	
	# start again
	k=1


    inputfile=$DIR_main/$cntl_path/$ctl_filename
     tmp_switch="skip"
      while IFS='' read -r line || [[ -n "$line" ]]; do
        tmp=${line[@]}   # assign the tmp obj to the line
        if [[ ${tmp_switch} == "rec" ]];then
            tmp_val=${tmp[@]}
          eval $tmp_nm=${tmp_val[@]}
           tmp_switch="skip"
           echo $tmp_nm" = "$tmp_val
        fi
        if [[ ${tmp[@]}  == "#Fprofile_datafile" ]];then 

          tmp_nm=${tmp[@]:2} 
          tmp_switch="rec"

        fi

        
      done < "$inputfile"




# RR_FUN $DIR_main/$cntl_path/$ctl_filename.ctl


	inputfile=$profile_datafile
	while IFS=" ,#,  ," read -r -e -a line;do
	        if [ "$k" -lt  7 ]; then
	        	echo "Line # $k: $line"
	        fi
	        if [ "$k" ==  2 ];then
	                echo reading first line
	               #mod_names=$line
	                unset nFrates
	            nFrates=${line[@]}
	             
	        elif [ "$k" == 4 ];then
	            # echo ${line[@]}
	             unset ntmp
	             ntmp=${#line[@]}
	             echo $ntmp
	             unset Frates
	                for (( i=2; i<${ntmp}; i++ )); 
	                    do
	                    Frates[i]=${line[$i]} #itr*6+1-6
	                done
	                echo ${Frates[@]}
	        elif [ "$k" == 6 ];then
	            #covars=$line 
	            unset nProfiles
	            nProfiles=${line[@]}
	            unset nn
	             echo $nProfiles
	              nn=$((8+$nProfiles))
	        elif [ "$k" -gt  7  ];then	
	        	#if [ "$k" -lt  15 ];then
	        	if [ "$k" -lt  $nn ];then
                	echo "read FP values"
               
					unset ntmp
					ntmp=${#line[@]}
					echo $ntmp
					unset FP_in
					for (( i=0; i<${ntmp}; i++ )); 
						do
							fp_num=$(($k-7))
							FP_in[i]=${line[$i]} #itr*6+1-6
					done
							# run CEATTLE
							echo "FP_in = ${FP_in[@]}"
					echo "****************************************************************"	
					echo "************** RUNNING $fp_num of $nProfiles PROFILES ***************"		
					echo "****************************************************************"		
					
					# run the model  
					 ./$execut_name -fut -msmMode $mode -ind $ctl_filename -recMode $recMode -rand_rec $randn -harvestMode $harvestMode -Fprofiles ${FP_in[@]} -seed $tmpseed1 $tmpseed2 $tmpseed3 $tmpseed4 $tmpseed5 $tmpseed6 -iprint 100 -nox -binp $filename.bar -phase 22 -nohess
					
					# copy F_profile report into temporary projections folder
					 mkdir_kir $DIR_main/$output_path/projections/$filename_fut/F_profiles
					 # problem here:
           cp -rf results/Future_Fprofile_report.rep $DIR_main/$output_path/projections/$filename_fut/F_profiles/Future_Fprofile_report_$fp_num.rep
					 cp -rf results/ceattle_R_projection.rep $DIR_main/$output_path/projections/$filename_fut/F_profiles/ceattle_R_projection_$fp_num.rep
				fi
	        	
	        else
	            #skip
	             echo skip
	        fi
	        ((k++))
	done < "$inputfile"

	echo ${Frates[@]}
	echo ${FP_in[@]}
	
fi


cp -rf $DIR_main/$model_path/main/$filename.bar $DIR_main/$output_path/projections/$filename_fut/$filename.bar
rm $DIR_main/$model_path/main/$filename.bar

cp -rf results/* $DIR_main/$output_path/projections/$filename_fut/results
cp -rf $ctl_filename $DIR_main/$output_path/projections/$filename_fut/$filename_fut.ctl
rm $ctl_filename

cd ..
echo "########################################################"
echo $filename_fut Completed
echo "########################################################"

if [ "$plot" -gt 0 ]; then
    # filename_fut=$filename_fut"_mc"$itr  #filename_fut=ceattle_0_2_2  
    echo "########################################################"
    echo now running Scripts/R_code/PLOT_CEATTLE_FUT.R
    echo "########################################################"
    cd $DIR_main/$output_path/projections/$filename_fut/results
    Rscript $DIR_main/$model_path/Scripts/R_code/PLOT_CEATTLE_FUT.R
    # Rscript  $DIR_main/$model_path/Scripts/R_code/FIT_recruitment.R

    cd $DIR_main/$model_path
    echo "########################################################"
    echo plots complete
    echo "########################################################"
fi

# /Users/kholsman/Documents/GitHub/CEATTLE/src/ceattle-master/Scripts/R_code/PLOT_CEATTLE_FUT.R


