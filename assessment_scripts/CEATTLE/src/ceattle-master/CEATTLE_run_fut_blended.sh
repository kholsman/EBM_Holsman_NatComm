
# Run blended forcast under mean recruitment with random dev (recMode = 1), mean fishing mort from hindcast proj_mode=2
##chmod +x CEATTLE_run_fut_blended.sh 
## sudo ln -sf /Users/kkari/Dropbox/msm-master/run_futNew.sh /usr/local/bin/run_futNew
# cd /Users/kkari/Dropbox/CEATTLE-master/ceattle-master
## ./run_fut_blended.sh 'mode' 'rec' 'fishing' 'rand itr' 'starting iteration'
## ./CEATTLE_run_fut_blended.sh 0 1 2 10 
## ./run_fut_blended.sh 0 0 3
# will run single species model under x random rec devs and mean F rate (2)
#!/bin/bash

function mkdir_kir {
 if [ ! -f "$1" ]; then
  mkdir -p "$1" 2>/dev/null
 fi
}
# Set the root directory
  rd=$(pwd)/main
nitr=1000
startnum=1
# Set the working directory
if [ $# -lt 3 ]; then
  echo 1>&2 "$0: not enough arguments : #1=ss vs msm (0 or 2, msm_mode), 
  #2= rec mode, #3=fishing (proj_mode) mode, #4=number of iterations, #5= iteration to start from (i.e., 1)"
  exit 2
elif [ $# = 4 ]; then  
  echo "run iterations 1 trough $4"
  nitr=$(($4))
elif [ $# = 5 ]; then  
  echo  "run iterations $5 through $4"
  nitr=$(($4))
  startnum=$(($5)) 
elif [ $# -gt 5 ]; then  
  echo 1>&2 "$0: too many arguments :#1=ss vs msm (0 or 2, msm_mode), #2= rec mode, #3=fishing (proj_mode) mode, #4=number of iterations, #5= iteration to start from (i.e., 1)"
  exit 2
fi

now=$(date +"%m_%d_%Y_%H%M")
var_name=ceattle_$1


OUT=$var_name/projections/MCMC_ceattle_$1_$2_$3
echo $OUT
# create the MCMC directory
if [ ! -f "$OUT" ]; then
   mkdir -p "$OUT" 2>/dev/null
   mkdir -p "$OUT/newest" 2>/dev/null
fi
if [ ! -f "$OUT/$now" ]; then
   mkdir -p "$OUT/$now" 2>/dev/null
   mkdir -p "$OUT/$now/results" 2>/dev/null
   mkdir -p "$OUT/$now/tmp_results" 2>/dev/null
fi

cd main

if [ ! -f "results" ]; then
   mkdir -p "results" 2>/dev/null
fi
#cp ../$var_name/results/msm_rs_$1.dat msm_rs.dat 
#cp ../$var_name/results/rs_data4MSM_BH.dat rs_data4MSM_BH.dat
#cp ../$var_name/results/rs_data4MSM_0_0_1.dat rs_data4MSM_0_0_1.dat
#cp ../$var_name/results/rs_data4MSM_1_1_1.dat rs_data4MSM_1_1_1.dat
#cp ../$var_name/results/rs_data4MSM_0_0_0.dat rs_data4MSM_0_0_0.dat
#cp ../$var_name/results/rs_data4MSM_1_1_0.dat rs_data4MSM_1_1_0.dat
#cp ../$var_name/results/rs_data4MSM_AICmn.dat rs_data4MSM_AICmn.dat
#cp ../$var_name/results/rs_data4MSM_TOP.dat rs_data4MSM_TOP.dat
#cp ../$var_name/results/rs_data4msm.dat rs_data4msm.dat
# rm -rf fits_4_MSM
# mkdir_kir fits_4_MSM 
# cp -r ../$var_name/results/fits_4_MSM/* fits_4_MSM/
rm -rf fits_4_CEATTLE
mkdir_kir fits_4_CEATTLE 
mkdir_kir results
cp -r ../$var_name/Recruitment_files/fits_4_CEATTLE/* fits_4_CEATTLE/
cp -r ../$var_name/results/ceattle_rs_$1.dat fits_4_CEATTLE/ceattle_rs.dat
cp -r ../../Control_files/$var_name.ctl $var_name.ctl

# cp results/ceattle_recruitment.rep ../$OUT/$now/results/ceattle_recruitment.rep
# run the random MCMC 
VAR=-9999

#for i in {1..1000};
for (( i=$startnum; i<=$nitr; i++ ))
do
  #echo "=============================================================="
  #echo "*********************************"
  #echo "*********************************"
  #echo "*********************************"
  #echo "*********************************"
  echo "running $OUT ; iteration $i "
  #echo "*********************************"
  #echo "*********************************"
  #echo "*********************************"
  #echo "*********************************"
  #echo "*********************************"
  #echo "=============================================================="
  #inputfile="/Users/kkari/Dropbox/CEATTLE-master/ceattle-master/randomSeeds.csv"
  inputfile= "../Data/randomSeeds.csv"

  itr=$i
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

        echo "$var0 $tmpseed1 $tmpseed2 $tmpseed3"
        #echo "$(($itr*6+1-6)) $(($itr*6+2-6)) $(($itr*6+3-6)) $(($itr*6+4-6)) $(($itr*6+5-6)) $(($itr*6+6-6))"
    done < "$inputfile"
	#tmpseed1=$RANDOM   
  #tmpseed2=$RANDOM 
  #tmpseed3=$RANDOM 
  #tmpseed4=$RANDOM   
  #tmpseed5=$RANDOM 
  #tmpseed6=$RANDOM
	VAR=$VAR$'\t'$tmpseed1$'\t'$tmpseed2$'\t'$tmpseed3$'\t'$tmpseed4$'\t'$tmpseed5$'\t'$tmpseed6
   
	./ceattle -fut -msmMode $1 -ind $var_name.ctl -recMode $2 -rand_rec 1 -harvestMode $3 -seed $tmpseed1 $tmpseed2 $tmpseed3 $tmpseed4 $tmpseed5 $tmpseed6 -iprint 100 -nox -binp ../$var_name/$var_name.bar -phase 22 -nohess
	
  cp results/Future_report.rep ../$OUT/$now/tmp_results/Future_report.rep
  cp -r $var_name.ctl ../$OUT/$now/tmp_results/$var_name_sub/ceattle_$1_$2_$3.ctl
  #cp results/Future_Fprofile_report.rep ../$OUT/$now/tmp_results/Future_Fprofile_report.rep
	cp results/ceattle_fut_recruitment.rep ../$OUT/$now/tmp_results/ceattle_fut_recruitment.rep
	cp results/ceattle_R2.rep ../$OUT/$now/tmp_results/ceattle_R2.rep
  cp ../randomSeeds.csv ../$OUT/$now/results/randomSeeds.csv

	for file in ../$OUT/$now/tmp_results/*.rep
	do
    	mv "$file" "${file%/*}/i$i${file##*/}"
	done
	cp -r ../$OUT/$now/tmp_results/* ../$OUT/$now/results
	rm -r ../$OUT/$now/tmp_results/*
done
 echo "*********************************"
  echo "*********************************"
  echo "*********************************"
  echo "*********************************"
  echo "*********************************"
  echo "=============================================================="
#rm msm_rs.dat 
## copy results 
#cp $var_name.ctl ../$var_name/projections/$var_name_sub/ceattle_$1_$2_$3.ctl
cd ..

echo $VAR > $OUT/$now/randSeedLog_$now.csv
rm -r $OUT/newest/*
cp -r $OUT/$now/* $OUT/newest
echo "########################################################"
echo Completed MCMC projections , now summarizing results
echo "########################################################"

#cd $OUT/newest/results

#Rscript /plot_MCMC.R





