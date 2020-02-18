//////////////////////////////////////////////////////////// 
//  Conventions:                                         // 
//     fsh   - refers to fshery                         //
//     bts   - refers to trawl survey                  //
//     eit   - refers to hydroacoustic survey         //
//     eac    - refers to expected age composition   //
//     oac    - refers to observed age composition  //
//     et     - refers to expected total numbers   //
//     ot     - refers to observed total numbers  //
/////////////////////////////////////////////////// 
//   ngears   - number of gear types (including srv \\
//   n_       - refers to the number of observations \\
//   styr     - first calendar year of model          \\
//   endyr    - last calendar year of model            \\
//   styr_    - first year of data (suffix)             \\
//   endyr_r  - last year for retrospective run          \\
//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
// Aug 25 2009 add projection to next year to get survey biomass estimates
//
DATA_SECTION
  int count_Ffail;
  int count_mcmc;
  int count_mcsave;
  !! count_mcmc=0;
  !! count_mcsave=0;
  int pflag;
  int do_EIT1;
  int q_amin
  int q_amax
  !! q_amin = 3; q_amax= 15; // age range overwhich q applies (for prior specifications)
  vector selages(1,15)
  !! selages=1.0;selages(1)=0;selages(2)=0;
  vector avo_sel(1,15)
  !! avo_sel(1)=0.0;  avo_sel(2)=1;  avo_sel(3)=1;  avo_sel(4)=0.85;  avo_sel(5)=0.7;  avo_sel(6)=0.55;  avo_sel(7)=0.3;  avo_sel(8)=0.15;  avo_sel(9)=0.05;  avo_sel(10)=0.01;  avo_sel(11)=0.01;  avo_sel(12)=0.01;  avo_sel(13)=0.01;  avo_sel(14)=0.01;  avo_sel(15)=0.01;
  vector Cat_Fut(1,10);
  !! do_EIT1=1; // flag to carry EIT out in future year (for simulations only)
  !! pflag=0;
  !!CLASS ofstream srecout("srec_Ass_out.dat")
  !!CLASS ofstream projout("pm.prj")
  !!CLASS ofstream nofish("nofish.out")
  !!CLASS ofstream projout2("pmsr.prj")
  // writes when -mceval invoked (which uses pm.psv file from -mcmc 10000000 -mcsave 200)
  !!CLASS ofstream eval("pm_eval.dat")     
  // Control file read from here--------------------------------------
 // !! ad_comm::change_datafile_name("pm.ctl");
 !!  *(ad_comm::global_datafile) >>  model_name; 
 !!  *(ad_comm::global_datafile) >>  datafile_name; 
 !!  *(ad_comm::global_datafile) >>  selchng_filename; 
 !! write_log(datafile_name);
 !! write_log(model_name);
  init_int SrType
  init_int Do_Combined        // Flag to try experimental combined-surveys method (not used yet)
  init_int use_age_err        // Flag to use ageing error matrix
  init_int use_age1_eit       // This flag (2007) converts EIT age data to 2+ and uses 1yr olds as an index
  init_number age1_sigma_eit  // Input sigma for EIT age 1 data
  int mina_eit
  int mina_bts
  !! mina_bts=2;
  !! if (use_age1_eit) mina_eit=2; else mina_eit=1;
 !! write_log(mina_eit);
  !! cout<<" Minimum age for EIT is: "<<mina_eit<<endl;// exit(1);
  init_int use_endyr_len   // Flag (2008 assmnt) added to use fishery length frequency in terminal year
  init_int use_popwts_ssb  // Flag (2007 assmnt) to switch spawning wt-age
  init_number natmortprior
  init_number cvnatmortprior
  init_number q_all_prior  // This is the combined eit and bts catchability for ages 3-6
  init_number q_all_sigma
  init_number q_bts_prior
  init_number q_bts_sigma
  init_number sigrprior
  init_number cvsigrprior
  init_int    phase_sigr
  init_number steepnessprior
  init_number cvsteepnessprior
  init_int    phase_steepness 
  init_int use_spr_msy_pen  // Option to use (old) spr penalty instead of just solving for it
  init_number sigma_spr_msy // Research option for using SPR as a prior for S-R instead of steepness
  number lambda_spr_msy
  !! lambda_spr_msy = .5/(sigma_spr_msy*sigma_spr_msy);

  init_int use_last_eit_ac  // Flag to use most recent EIT age data (typically derived from BTS ages and EIT lengths)
  init_int nyrs_sel_avg     // Added 10/22/98 to adjust the avg fsh selectivity used in future harvests
  number Steepness_UB
  init_number cv_bts_tmp    // Defunct
  init_number cv_eit_tmp    // Defunct
  init_number srprior_a     // Beta prior alpha parameter
  init_number srprior_b     // Beta prior beta parameter
  init_int    nyrs_future;
  int nscen;
  !! nscen=20;
  init_number next_yrs_catch; // Used to grid / interpolate...when below target.
  init_number fixed_catch_fut1;
  init_number fixed_catch_fut2;
  init_number fixed_catch_fut3;
 !! write_log(nyrs_future);
 !! write_log(next_yrs_catch);
 !! write_log(fixed_catch_fut1);
 !! write_log(fixed_catch_fut2);
 !! write_log(fixed_catch_fut3);
 !! write_log(nyrs_sel_avg);
  init_int phase_F40;
  init_int robust_phase
  init_int eit_robust_phase 
  init_int eit_like_type  // 0=standard, 1=log-normal each age 

  init_int phase_logist_fsh  // Asymptotic or not...
  init_int phase_logist_bts  // Asymptotic or not...
 !! write_log(phase_F40);
 !! write_log(robust_phase);
 !! write_log(phase_logist_fsh);
 !! write_log(phase_logist_bts);

  int phase_selcoffs_fsh
  int phase_selcoffs_bts

  int phase_selcoffs_fsh_dev
  int phase_selcoffs_bts_dev

  int phase_logist_fsh_dev
  int phase_logist_bts_dev
  !! phase_selcoffs_bts   = 2;
  !! phase_selcoffs_fsh   = 2;

  init_int phase_seldevs_fsh   // To determine generically if selectivity to be time varying...
  init_int phase_seldevs_bts   // To determine generically if selectivity to be time varying...
  init_int phase_age1devs_bts  // To determine generically if selectivity to be time varying...
 !! write_log(phase_seldevs_bts);

  // Set correct deviations to selectivity forms---
 LOCAL_CALCS
  // Fishery selectivity................
  if (phase_logist_fsh>0) // Use logistic selectivity
  {
    phase_selcoffs_fsh = -2; 
    if(phase_seldevs_fsh>0) 
      phase_selcoffs_fsh_dev = -phase_seldevs_fsh; 
    else
      phase_selcoffs_fsh_dev =  phase_seldevs_fsh; 

    phase_logist_fsh_dev   =  phase_seldevs_fsh; 
  }
  else                    // Use coefficient selectivities...
  {
    if(phase_seldevs_fsh>0) 
      phase_logist_fsh_dev   = -phase_seldevs_fsh; 

    phase_selcoffs_fsh_dev =  phase_seldevs_fsh; 
  }
  // Trawl Survey selectivity................
  if (phase_logist_bts>0) // Use logistic selectivites...
  {
    phase_selcoffs_bts = -2; 
    if(phase_seldevs_bts>0) 
      phase_selcoffs_bts_dev = -phase_seldevs_bts; 
    else
      phase_selcoffs_bts_dev = phase_seldevs_bts; 

    phase_logist_bts_dev   =  phase_seldevs_bts; 
  }
  else                     // Use coefficient selectivites...
  {
    if(phase_seldevs_bts>0) 
      phase_logist_bts_dev   = -phase_seldevs_bts; 

    phase_selcoffs_bts_dev =  phase_seldevs_bts; 
  }

 END_CALCS
  init_int phase_selcoffs_eit
  init_int phase_selcoffs_eit_dev
  !! cout <<"Phase fsh coef: "<<phase_selcoffs_fsh<<" "<<phase_selcoffs_fsh_dev<<endl;
  !! cout <<"Phase bts coef: "<<phase_selcoffs_bts<<" "<<phase_selcoffs_bts_dev<<endl;
  !! cout <<"Phase eit coef: "<<phase_selcoffs_eit<<" "<<phase_selcoffs_eit_dev<<endl;

  !! cout <<"Phase fsh logist: "<<phase_logist_fsh<<" "<<phase_logist_fsh_dev<<endl;
  !! cout <<"Phase bts logist: "<<phase_logist_bts<<" "<<phase_logist_bts_dev<<endl;

  init_int phase_natmort
  init_int phase_q_bts    // phase for estimating survey q for bottom trawl survey
  init_int phase_q_std_area
  init_int phase_q_eit    // phase for estimating survey q for echo-integration trawl survey
  init_int phase_bt       // Phase for bottom temperature parameter
  init_int phase_rec_devs // Phase for estimating recruits (not from SR curve, but as N age 1)
  init_int phase_larv     // Phase to use advective larval dispersal (as a predictive aid) Eq. 8
  init_int phase_sr       // Phase for estimating sr curve
  init_int wt_fut_phase   // Phase for estimating future mean weight-at-age (Eq. 21)
  init_int last_age_sel_group_fsh
  init_int last_age_sel_group_bts
  init_int last_age_sel_group_eit
  init_vector ctrl_flag(1,29)
  !! cout<<ctrl_flag<<endl;
  // Following was used for getting selectivity blocks to end with the correct pattern (i.e., last block ends in last yr of data)
  init_int sel_dev_shift // Sets the year for selectivity changes, 0 = first change in 1966, -1 = first change in 65...

  init_int Sim_status   //Simulation flag 0=none, 1=1-yr ahead, 2=full simulation (all data)
  init_int iseed_junk   //rng seed in
  // modify endyr numbers at age to test alternative future "truths" in a simulation
  //   used for specifying "what-ifs" directly for simulations
  init_vector YC_mult(1,15) 
  !! cout << "Simulation status = "<<Sim_status<<endl;
  !! cout << "iseed             = "<<iseed_junk<<endl;
  !! cout << "YC                = "<<YC_mult<<endl;

  // Read in datafile now...
  !! global_datafile= new cifstream(datafile_name);
  init_int styr
  init_int styr_bts
  init_int styr_eit
  init_int endyr
  !! cout <<datafile_name<<" "<<styr<<" "<<endyr<<endl;;
  init_int recage
  init_int nages
  !! write_log(nages);write_log(recage);write_log(endyr);

  init_vector p_mature(1,nages)
  !!p_mature *= 0.5;
  init_ivector ewindex(styr,endyr)
  init_ivector nsindex(styr,endyr)
  init_matrix wt_fsh(styr,endyr,1,nages)
  init_matrix wt_ssb(styr,endyr,1,nages)     // Added in 2007, for constant or alternate time-varying SSB calculation
  !! if(use_popwts_ssb==0)  wt_ssb = wt_fsh; // this is the historical default (and continued use) Eq. 9
       matrix wt_tmp(1,nages,1991,endyr-1)
       vector wt_mn(1,nages)
       vector wt_sigma(1,nages)
  init_vector obs_catch(styr,endyr)
  !! write_log(p_mature);write_log(obs_catch);write_log(wt_fsh);
  // Effort vector input (but never used...placeholder)
  init_vector obs_effort(styr,endyr)
  // Historical CPUE (foreign) for early trend information 
  init_int n_cpue
  init_ivector yrs_cpue(1,n_cpue)
  init_vector obs_cpue(1,n_cpue) // pred_cpue(i)  = natage(iyr)*sel_fsh(iyr) * q_cpue;  Eq. 18
  init_vector obs_cpue_std(1,n_cpue)
  vector obs_cpue_var(1,n_cpue)
  !! obs_cpue_var = square(obs_cpue_std);
// Acoustic index from BTS data 
  init_int n_avo
  init_ivector yrs_avo(1,n_avo)
  init_vector obs_avo(1,n_avo) // pred_cpue(i)  = natage(iyr)*sel_fsh(iyr) * q_cpue;  Eq. 18
  init_vector obs_avo_std(1,n_avo)
  init_matrix wt_avo(1,n_avo,1,nages)
  vector obs_avo_var(1,n_avo)
  !! obs_avo_var = square(obs_avo_std); 
  !! write_log(yrs_avo);write_log(obs_avo);write_log(wt_avo);

  // Non-elegant way of dealing with observations from multiple sources...
  init_number ngears
  init_vector minind(1,ngears)
  !! write_log(minind);
  init_int n_fsh
  !! write_log(n_fsh);
  init_int n_bts
  !! write_log(n_bts);
  init_int n_eit
  !! write_log(n_eit);
  vector nagecomp(1,ngears)
  init_ivector yrs_fsh_data(1,n_fsh)
  !! write_log(yrs_fsh_data);
  init_ivector yrs_bts_data(1,n_bts)
  !! write_log(yrs_bts_data);
  init_ivector yrs_eit_data(1,n_eit)
  !! write_log(yrs_eit_data);
  init_ivector      sam_fsh(1,n_fsh)
  !! write_log(sam_fsh);
  init_ivector      sam_bts(1,n_bts)
  !! write_log(sam_bts);
  init_ivector      sam_eit(1,n_eit)
  !! write_log(sam_eit);
  !! cout<<endl<<"Sample size  fsh "<<endl;
  !! cout<<sam_fsh<<endl;
  !! cout<<endl<<"Sample size  bts "<<endl;
  !! cout<<sam_bts<<endl;
  !! cout<<endl<<"Sample size  eit "<<endl;
  !! cout<<sam_eit<<endl;
  init_matrix     oac_fsh_data(1,n_fsh,1,nages)
  !! cout<< " Index min and max for age comp data: "<<endl <<oac_fsh_data.indexmin()<<" Max "<<oac_fsh_data.indexmax()<<endl;
  init_vector     obs_bts_data(1,n_bts)
  init_vector std_obs_bts_data(1,n_bts)
  vector           var_obs_bts(1,n_bts)
  !! var_obs_bts = elem_prod(std_obs_bts_data,std_obs_bts_data);

  init_matrix  wt_bts(1,n_bts,1,nages)
  init_vector std_bts(1,n_bts)
  vector      var_bts(1,n_bts)
  !! cout<<endl<<"observed std bts "<<endl;
  !! cout<<std_bts<<endl;
  !! var_bts = elem_prod(std_bts,std_bts);
  init_matrix oac_bts_data(1,n_bts,1,nages)
  !! cout<<oac_bts_data.indexmin()<<" Max "<<oac_bts_data.indexmax()<<endl;
  init_vector std_eit(1,n_eit)
  vector var_eit(1,n_eit)
  !! var_eit = elem_prod(std_eit,std_eit);
  !! cout<<"Stdev Total EIT N "<<std_eit<<endl;
  vector lse_eit(1,n_eit_r)
  vector lvar_eit(1,n_eit_r)

  init_matrix oac_eit_data(1,n_eit,1,nages)
  matrix ln_oac_eit(1,n_eit,mina_eit,nages)
  !! for (i=1;i<=n_eit;i++) ln_oac_eit(i) = log(oac_eit_data(i)(mina_eit,nages));
  !! cout<<oac_eit_data.indexmin()<<" Max "<<oac_eit_data.indexmax()<<endl;
  !! cout<<oac_eit_data(n_eit)(5,15) <<endl;

  !! cout<<endl<<"observed EIT Age comps"<<endl;
  !! cout<<oac_eit_data<<endl;

  !! cout<<endl<<"observed BTS Age comps"<<endl;
  !! cout<<oac_bts_data<<endl;

  init_vector obs_eit_data(1,n_eit)
  init_vector std_obs_eit_data(1,n_eit)
  vector var_obs_eit(1,n_eit)
  !! var_obs_eit = elem_prod(std_obs_eit_data,std_obs_eit_data);
  init_matrix wt_eit(1,n_eit,1,nages)

  !! write_log(wt_eit);
  init_vector bottom_temp(1,n_bts)
  !! cout<<"BottomTemp:"<<endl<<bottom_temp<<endl;
  !! write_log(bottom_temp);

  init_matrix age_err(1,nages,1,nages)
  init_int nlbins;
  init_vector olc_fsh(1,nlbins)
  !! olc_fsh /= sum(olc_fsh);
  vector lens(1,nlbins);
    !! for (j=1;j<=nlbins;j++)
      !!  lens(j) = double(j)+25;
      !! lens(2,5) += 1;
      !! lens(3,5) += 1;
      !! lens(4,5) += 1;
      !! lens(5) += 1;
      !! lens(6,16) += 4.5;
      !! lens(17,nlbins) += 5;
      !! lens(18,nlbins) += 1.0;
      !! lens(19,nlbins) += 1.0;
      !! lens(20,nlbins) += 1.0;
      !! lens(21,nlbins) += 1.0;
      !! lens(22,nlbins) += 1.0;
      !! lens(23,nlbins) += 1.0;
      !! lens(24,nlbins) += 1.0;
  !! cout <<lens<<endl;// exit(1);
  init_matrix age_len(1,nages,1,nlbins)
  init_number test
  !! if(test!=1234){ cout<<"Failed on data read "<<test<<endl;exit(1);}
  // ____________________________________________________________________________________
  // Bit to simulate from covariance matrix on numbers at age in terminal year
  // 
  !! global_datafile= new cifstream("endyrN_est.dat");
  init_vector N_endyr_est(1,nages)
  init_vector s_endyr_est(1,nages)
  init_matrix c_endyr_est(1,nages,1,nages)
  matrix v_endyr_est(1,nages,1,nages)
  matrix chol(1,nages,1,nages)
  int j
  !! for (i=1;i<=nages;i++) for (j=1;j<=nages;j++) v_endyr_est(i,j)=c_endyr_est(i,j)*s_endyr_est(i)*s_endyr_est(j); 
  !! chol = (choleski_decomp(v_endyr_est));  

  int dim_sel_fsh
  int dim_sel_bts
  int dim_sel_eit
  int group_num_fsh
  int group_num_bts
  int group_num_eit
  int n_selages_fsh
  int n_selages_bts
  int n_selages_eit
  int nbins
  int n_fsh_r
  int n_bts_r
  int n_eit_r
  int n_avo_r
  int endyr_r

  number spawnmo
  !! spawnmo = 4.;
  number yrfrac        // Fraction of year prior to spawning 
  !! yrfrac= (spawnmo-1.)/12; 

  // settings from control file
  int phase_nosr
  int i
  int Use_78_on_only
  vector age_vector(1,nages);
    !! for (j=1;j<=nages;j++)
      !!  age_vector(j) = double(j);

  
  int iyr
  int endyr_fut;
  int styr_fut;
  int nFs;
  int phase_Rzero;

  int styr_est;
  int endyr_est;

  //
  // Code added to begin dealing better with irregularly spaced changes in time-varying selectivity changes
  //   Intended to apply to all gears generically but so far only implemented for the EIT (acoustic survey)
  //
  //   Reads in a matrix of 3 columns by nyrs rows.  Values for 3rd column are used to allow some variability in 
  //      EIT availability by year (a type of process error, Eqs. 4 and 5)
  !! ad_comm::change_datafile_name("pm_Fmsy_Alt.dat");
  // Vector of additional weight due to cost of travel (apply to Fmsy calc)
  init_vector Fmoney(1,nages);
  !! ad_comm::change_datafile_name(selchng_filename);
  int ii;
  init_matrix sel_data(styr,endyr,0,3);  // Columns: year, change by gear type (fsh, bts, eit)

  vector eit_ch_in(styr,endyr)
  int nch_eit;
 LOCAL_CALCS
   // Sets up sigmas (for each age) used to estimate future mean-wt-age (Eq. 21)
  for (i=1991;i<=endyr-1;i++) 
    for (j=1;j<=nages;j++) 
      wt_tmp(j,i) = wt_fsh(i,j);
  for (j=1;j<=nages;j++)
  {
    wt_mn(j) = mean(wt_tmp(j));
    wt_sigma(j) = sqrt(norm2(wt_tmp(j)-mean(wt_tmp(j)))/(endyr-1991) );
  }

  // Flag for changing period of estimating recruits  
  Use_78_on_only = int(ctrl_flag(24));
  if (Use_78_on_only)
  {
    if (styr>1978)
      styr_est = styr;
    else
      styr_est = 1978;
  }
  else
  {
    styr_est = styr;
  }

  // Set bounds for SRR params (depending on estimation and type)
  if (phase_sr>0) 
  {
    phase_nosr=-1;
    phase_Rzero=3;
    if (SrType==3)
      phase_Rzero=-2;
  }
  else
  {
    phase_nosr=1;
    phase_Rzero=-1;
  }

  if (SrType==4)
    Steepness_UB=4.;
  else
    Steepness_UB=.99;

  n_fsh_r=0;
  n_bts_r=0;
  n_eit_r=0;
  n_avo_r=0;
  endyr_r = endyr - int(ctrl_flag(28));
  endyr_est = endyr_r - int(ctrl_flag(29)); // lop off last couple of years 
  cout <<"Last yr of estimation..."<<endyr_est<<endl;

  // Used to count parameters changes in EIT survey selectivities
  eit_ch_in = column(sel_data,3);
  ii = 0;
  for(i=styr;i<=endyr_r;i++) if(eit_ch_in(i)>0) {ii++;} nch_eit=ii; 
  if (ctrl_flag(28)==0.)
  {
    n_fsh_r=n_fsh;
    n_bts_r=n_bts;
    n_eit_r=n_eit; 
    n_avo_r=n_avo; 
  }
  else
  {
    int ii=0;
    for (i=styr;i<endyr_r;i++)
    {
      ii++;
      if (i == yrs_fsh_data(ii)) 
        n_fsh_r++;
    }
    ii=0;
    for ( i=styr_bts;i<=endyr_r;i++)
    {
      ii++;
      if (i == yrs_bts_data(ii)) 
        n_bts_r++;
    }
    ii=0;
    if (endyr_r <= styr_eit) {cout <<" Not enough years for EIT survey"<<endl; exit(1);}
    n_eit_r = 0;
    for (i=styr_eit;i<endyr_r;i++)
    {
      ii++;
      if (i == yrs_eit_data(n_eit_r+1)) 
        n_eit_r++;
    }
  // init_int n_avo
  // init_ivector yrs_avo(1,n_avo)
    n_avo_r = 0;
    for (i=yrs_avo(1);i<endyr_r;i++)
    {
      // if (yrs_avo(i) <= endyr_r)
      if (i == yrs_avo(n_avo_r+1)) 
        n_avo_r++;
    }
  }
  cout <<"End year, n's "<<endyr_r<<" "<<n_fsh_r<<" "<<n_bts_r<<" "<<n_eit_r<<" "<<endl;
  write_log(endyr_r);
  write_log(endyr_est);
  write_log(n_fsh_r);
  write_log(n_bts_r);
  write_log(n_eit_r);
  write_log(n_avo_r);

  n_selages_fsh = nages-last_age_sel_group_fsh+1;
  n_selages_bts = nages-last_age_sel_group_bts+1;
  n_selages_eit = nages-last_age_sel_group_eit+1;
  group_num_fsh=int(ctrl_flag(16));
  group_num_bts=int(ctrl_flag(17));
  group_num_eit=int(ctrl_flag(18));
  int ifsh=0;
  int ibts=0;
  int ieit=0;
  // Old way of specifying parameter blocks over time (still in use for fishery and bts)
  for (i=styr;i<endyr_r;i++) 
  {
    if (!((i+sel_dev_shift)%group_num_fsh)) ifsh++; 
    cout <<i<<" "<<(i+sel_dev_shift)%group_num_fsh<<" "<<ifsh<<endl;
  }
  for ( i=styr_bts;i<endyr_r;i++)
    if (!(i%group_num_bts)) ibts++; 

  dim_sel_fsh=ifsh;
  dim_sel_bts=ibts;
  dim_sel_eit=nch_eit;cout<<"dim_sel_eit "<<dim_sel_eit<<endl;
  nbins=nages;
  nagecomp(1)=n_fsh_r;
  nagecomp(2)=n_bts_r;
  nagecomp(3)=n_eit_r; 

  // Future and SPR stuff
  endyr_fut=endyr_r+nyrs_future+1;
  styr_fut=endyr_r+1;
  nFs=3;
  if (phase_sr>0) 
    phase_nosr=-1;
  else
    phase_nosr=1;
 END_CALCS
  ivector    yrs_ch_eit(1,nch_eit);
  vector sel_ch_sig_eit(1,nch_eit);
  !! ii=0;for (i=styr;i<=endyr_r;i++) if(eit_ch_in(i)>0) {ii++;sel_ch_sig_eit(ii)=eit_ch_in(i);yrs_ch_eit(ii)=i;} 
  // Critical variables: yrs_ch_eit, sel_ch_sig_eit,  and nch_eit
  !! cout<<"Yrs EIT sel change: "<<yrs_ch_eit <<endl<< sel_ch_sig_eit<<endl;// exit(1);
  matrix oac_fsh(1,n_fsh_r,1,nbins)//--Observed proportion at age in Fishery
  matrix oac_bts(1,n_bts_r,1,nbins)//--Observed proportion at age in Survey

  // Set up whether last survey value is used or not (if ALK is from BTS instead of EIT)
  int n_eit_ac_r;
  !!  if (use_last_eit_ac>0) n_eit_ac_r = n_eit_r; else n_eit_ac_r = n_eit_r-1; 
  !!  if (use_last_eit_ac>0) nagecomp(3) = n_eit_r; else nagecomp(3) = n_eit_r-1; 

  matrix oac_eit(1,n_eit_ac_r,1,nbins)//--Observed proportion at age in Survey
  vector oa1_eit(1,n_eit_ac_r)       //--Observed age 1 index from hydro survey
  vector ot_fsh(1,n_fsh_r)         //--Observed total numbers in Fishery
  vector ot_bts(1,n_bts_r)         //--Observed total numbers in BTS
  vector obs_biom_bts(1,n_bts_r)        //--Observed age comp in BTS
  vector std_obs_bts(1,n_bts_r)    //--Observed total biomass in BTS

  vector ot_eit(1,n_eit_r)         //--Observed total numbers in HydroSurvey
   vector obs_eit(1,n_eit_r)        //--Observed total biomass in Survey
  vector std_obs_eit(1,n_eit_r)    //--Observed total biomass in Survey
  int ignore_last_eit_age1;

   
  // Stuff for passing seed via command line and for initializing new sequence
  int iseed;
  !! iseed=0;
  int do_fmort;
  !! do_fmort=0;
   vector    adj_1(1,10)
   vector    adj_2(1,10)
   vector    SSB_1(1,10)
   vector    SSB_2(1,10)
  int do_check  // Placeholder to have debug checker flag...
 LOCAL_CALCS
  do_check=0;  
  if (ad_comm::argc > 1)
  {
    int on=0;
    if ( (on=option_match(argc,argv,"-io"))>-1)
      do_check = 1;
    if ( (on=option_match(ad_comm::argc,ad_comm::argv,"-iseed"))>-1)
    {
      if (on>ad_comm::argc-2 | ad_comm::argv[on+1][0] == '-')
      {
        cerr << "Invalid number of iseed arguements, command line option -iseed ignored" << endl;
      }
      else
      {
        iseed = atoi(ad_comm::argv[on+1]);
        cout<<  "Currently using "<<adstring(ad_comm::argv[on+1])<<" as random number seed for sims"<<endl;
      }
    }
    if ( (on=option_match(ad_comm::argc,ad_comm::argv,"-uFmort"))>-1)
      do_fmort=1;
  }
  // Some defaults------------
   adj_1=1.0;
   adj_2=1.0; 
   SSB_1=1.0;
   SSB_2=1.0; 

 END_CALCS
  int phase_cpue_q
  int phase_avo_q
  !!long int lseed=iseed;
  !!CLASS random_number_generator rng(iseed);

 // ------------------------------------------------------
 // read in CPUE data by station (with temperatures)
 //   Section to do experimental survey CPUE tests using 
 //    temperatures as a covariate (not completed).  
 //    intent is to read all raw station data (for CPUE anyway) to accommodate potential
 //    for availability effects due to habitat preferences
 //    
 //    NOT IN DOCUMENT/NOT USED 
 // ------------------------------------------------------
 !! ad_comm::change_datafile_name("surveycpue.dat");
  init_int nstrata;
 !! cout <<nstrata<<endl;
  init_vector sqkm(1,nstrata);
  init_int ndata;
  init_matrix d(1,ndata,1,5);  // Columns: year, strataindex, RACEstrata, CPUE, Temperature
  imatrix nobs(1982,2008,1,14); // years and strata 
   matrix mnCPUE(1982,2008,1,14); // years and strata 
   matrix mntemp(1982,2008,1,14); // years and strata 
  3darray temp_in(1982,2008,1,14,1,70); // for reading in 
  3darray CPUE_in(1982,2008,1,14,1,70); // for reading in  
 LOCAL_CALCS
  nobs.initialize();
  // int iobs=0;
  int nextyr;
  int nextst;
  for (int i=1;i<=ndata;i++)
  {
    int iyr = int(d(i,1)) ;
    int ist = int(d(i,2));
    nextyr=iyr; nextst=ist; 
    nobs(iyr,ist)++;
    temp_in(iyr,ist,nobs(iyr,ist))=d(i,5);
    CPUE_in(iyr,ist,nobs(iyr,ist))=d(i,4);
  }
 END_CALCS
  3darray temp(1982,2008,1,14,1,nobs); // for actually using (ragged, since not same number of observations each year)
  3darray CPUE(1982,2008,1,14,1,nobs); // for actually using (ragged, since not same number of observations each year)
 LOCAL_CALCS
  temp.initialize();
  CPUE.initialize();
  mntemp.initialize();
  mnCPUE.initialize();
  for (int iyr=1982;iyr<=2008;iyr++)
  {
    for (int ist=1;ist<=nstrata;ist++)
    {
      for (int iobs=1;iobs<=nobs(iyr,ist);iobs++)
      {
         temp(iyr,ist,iobs)=temp_in(iyr,ist,iobs);
         CPUE(iyr,ist,iobs)=CPUE_in(iyr,ist,iobs);
      }
      if (nobs(iyr,ist)>0)
      {
        mnCPUE(iyr,ist) = mean(CPUE(iyr,ist));
        mntemp(iyr,ist) = mean(temp(iyr,ist));
      }
    }
  }
 END_CALCS

INITIALIZATION_SECTION
  sigr  sigrprior ;
  // To give initial (non-zero) values for future mean weights at age (estimated from historical mean wts-at-age)
  wt_fut  .8;
  steepness steepnessprior;
  log_avgrec 9.87558
  log_Rzero 9.2033
  log_avginit 4.8;
  log_avg_F -1.6;
  bt_slope  0.;
  log_q_eit -1.05313
  log_q_avo -9.6
  log_q_bts q_bts_prior;
  log_q_std_area 0.;
  log_q_cpue -0.16
  sel_coffs_fsh -.10
  sel_coffs_bts -.01
  sel_coffs_eit -.10
  sel_a50_bts 4.5
  sel_slp_bts 1.
  sel_dif1_fsh 1
  sel_a501_fsh 3
  sel_dif2_fsh 5
  sel_trm2_fsh .90

PARAMETER_SECTION
  init_number log_avgrec(1);
  init_number log_avginit(1);
  init_number log_avg_F(1)  ;
  init_number natmort_phi(phase_natmort)
  vector natmort(1,nages)
  vector base_natmort(1,nages)

  init_number log_q_bts(phase_q_bts);
  init_number log_q_std_area(phase_q_std_area);
  init_number bt_slope(phase_bt);
  !! cout <<phase_bt<<" Phase for bts temperature"<<endl;
  init_number log_q_eit(phase_q_eit);

  init_number         log_Rzero(phase_Rzero)
  init_bounded_number steepness(0.2,Steepness_UB,phase_steepness)
  !!   phase_cpue_q = 1; // always estimate historical CPUE q
  // estimate avo q if turned on 
  !!   if (n_avo_r < 6 || ctrl_flag(6)==0) phase_avo_q = -1; else phase_avo_q=1;  
  init_number log_q_cpue(phase_cpue_q);
  init_number log_q_avo(phase_avo_q);
  number q_avo;
  number q_bts;
  number q_eit;
  number q_cpue;

  init_bounded_vector log_initdevs(2,nages,-15.,15.,3)
  init_bounded_vector log_rec_devs(styr,endyr_r,-10.,10.,phase_rec_devs)
  vector rec_epsilons(styr,endyr_r) // These are the real rec-dev guys
  // 11x11 matrix of advection "cells" to use for testing explanatory power of post-spawning wind conditions on survival (rec)
  // Eq. 8
  init_bounded_matrix larv_rec_devs(1,11,1,11,-10.,10.,phase_larv)
  matrix larv_rec_trans(1,11,1,11)
  number alpha;
  number beta;
  number Rzero;
  likeprof_number q_all;
  sdreport_vector endyr_N(1,nages);
  sdreport_number B_Bnofsh;
  // Added to test for presence of alternative recruitment "regimes" :
  //  e.g., regime(1) = mean(pred_rec(1964,1977));
  // sdreport_vector regime(1,8);
  // Moved to regular vector since this broke during retrospective runs
  vector regime(1,8);
  // Some added stats
  sdreport_number Bzero;
  sdreport_number Percent_Bzero;
  sdreport_number Percent_Bzero_1;
  sdreport_number Percent_Bzero_2;
  sdreport_number Percent_B100;
  sdreport_number Percent_B100_1;
  sdreport_number Percent_B100_2;
  // Added to test for significance of "mean temperature" from earlier assessments (effect on survey q) 
  //   NOT presented as a sensitivity in 2008 nor 2009
  sdreport_vector q_temp(1,5);
  // vector q_temp(1,5);
  number phizero

  // Reserved for closer evaluation of station-station data NOT USED, hence commented out
  // 3darray pred_CPUE(1982,2006,1,14,1,nobs); // for actually using (ragged) d

  init_bounded_dev_vector log_F_devs(styr,endyr_r,-15.,15.,2)
  init_bounded_number sigr(0.1,2.,phase_sigr)

  number sigmaRsq;

  init_bounded_matrix sel_devs_fsh(1,dim_sel_fsh,1,n_selages_fsh,-5.,5.,phase_selcoffs_fsh_dev)
  init_bounded_matrix sel_devs_bts(1,dim_sel_bts,1,n_selages_bts,-5.,5.,phase_selcoffs_bts_dev)
  init_bounded_matrix sel_devs_eit(1,dim_sel_eit,mina_eit,n_selages_eit,-5.,5.,phase_selcoffs_eit_dev)

  init_vector sel_coffs_fsh(1,n_selages_fsh,phase_selcoffs_fsh)
  init_vector sel_coffs_bts(1,n_selages_bts,phase_selcoffs_bts)
  init_vector sel_coffs_eit(mina_eit,n_selages_eit,phase_selcoffs_eit)

  init_vector wt_fut(1,nages,wt_fut_phase)

  init_bounded_number sel_slp_bts(0.001,5.,phase_logist_bts)
  init_bounded_number sel_a50_bts(0.1,6,phase_logist_bts)
  init_number             sel_age_one(phase_logist_bts)                              // Special since age-1 are selected more than others...
  init_bounded_dev_vector sel_slp_bts_dev(styr_bts,endyr_r,-5,5,phase_logist_bts_dev)// allow for variability in survey selectivity inflection 
  init_bounded_dev_vector sel_a50_bts_dev(styr_bts,endyr_r,-5,5,phase_logist_bts_dev)// allow for variability in survey selectivity inflection 
  init_bounded_dev_vector sel_one_bts_dev(styr_bts,endyr_r,-5,5,phase_age1devs_bts)  // allow for variability in survey selectivity inflection 
   
  init_number sel_dif1_fsh(phase_logist_fsh)
  init_bounded_number sel_a501_fsh(0.1,7,phase_logist_fsh)
  init_bounded_number sel_trm2_fsh(0.0,0.999,phase_logist_fsh)
  !! int ph_log_fsh2;
  !! if (phase_logist_fsh>0) ph_log_fsh2 = phase_logist_fsh+1;else ph_log_fsh2 = phase_logist_fsh;
  init_number sel_dif2_fsh(ph_log_fsh2)

  init_bounded_dev_vector sel_dif1_fsh_dev(styr,endyr_r,-5,5,phase_logist_fsh_dev) // allow for variability in survey selectivity inflection 
  init_bounded_dev_vector sel_a501_fsh_dev(styr,endyr_r,-5,5,phase_logist_fsh_dev) // allow for variability in survey selectivity inflection 
  init_bounded_dev_vector sel_trm2_fsh_dev(styr,endyr_r,-.5,.5,-2) // allow for variability in survey selectivity inflection 

 // Parameters for computing SPR rates
           number SPR_ABC // (0.05,2.,phase_F40)
  sdreport_number SPR_OFL // (0.05,2.,phase_F40)
  sdreport_number F40 // (0.05,2.,phase_F40)
  sdreport_number F35 // (0.05,2.,phase_F40)
  sdreport_vector SSB(styr,endyr_r)

 // Stuff for SPR and yield projections
  number sigmarsq_out
  number ftmp
  number SB0
  number SBF40
  number SBF35
  number sprpen
  number F_pen
  number meanrec
  vector SR_resids(styr_est,endyr_est);
  matrix Nspr(1,4,1,nages)
  vector sel_fut(1,nages)
 
  3darray natage_future(1,nscen,styr_fut,endyr_fut,1,nages)
  init_vector rec_dev_future(styr_fut,endyr_fut,phase_F40);

  3darray F_future(1,nscen,styr_fut,endyr_fut,1,nages);
  matrix Z_future(styr_fut,endyr_fut,1,nages);
  matrix S_future(styr_fut,endyr_fut,1,nages);
  matrix catage_future(styr_fut,endyr_fut,1,nages);
  number avg_rec_dev_future

  matrix   eac_fsh(1,n_fsh_r,1,nbins)  //--Expected proportion at age in Fishery
  vector   elc_fsh(1,nlbins)           //--Expected proportion at length in Fishery
  matrix   eac_bts(1,n_bts_r,1,nbins)  //--Expected proportion at age in trawl survey
  matrix   eac_cmb(1,n_bts_r,1,nbins)  //--Expected proportion at age in combined surveys
  matrix   oac_cmb(1,n_bts_r,1,nbins)  //--observed proportion at age in combined surveys
  matrix   eac_eit(1,n_eit_ac_r,1,nbins)//--Expected proportion at age in hydro survey
  vector   ea1_eit(1,n_eit_ac_r)       //--Expected age 1 index from hydro survey
  vector    et_fsh(1,n_fsh_r)          //--Expected total numbers in Fishery
  vector    et_bts(1,n_bts_r)          //--Expected total numbers in Survey
  vector    et_cmb(1,n_bts_r)          //--Expected total numbers in HydroSurvey
  vector avail_bts(1,n_bts_r)          //--Availability estimates in BTS
  vector avail_eit(1,n_bts_r)          //--Availability estimates in HydroSurvey
  vector sigma_cmb(1,n_bts_r)          //--Std errors of combined surveys (by year) 
  vector   var_cmb(1,n_bts_r)          //--Std errors of combined surveys (by year) 
  vector    ot_cmb(1,n_bts_r)          //--Observed total numbers in combined Surveys
  vector    eb_bts(1,n_bts_r)          //--Expected total biomass in Survey
  vector    eb_eit(1,n_eit_r)          //--Expected total biomass in Survey
  vector    et_eit(1,n_eit_r)          //--Expected total numbers in HydroSurvey
  vector    et_avo(1,n_avo_r)          //--Expected total numbers in HydroSurvey
  vector   et_cpue(1,n_cpue)           //--Expected total numbers in CPUE
  vector     Fmort(styr,endyr_r)

  matrix catage(styr,endyr_r,1,nages)
  vector pred_catch(styr,endyr_r)
  vector Pred_N_bts(styr,endyr_r)
  vector Pred_N_eit(styr,endyr_r)
  vector pred_cpue(1,n_cpue)
  vector pred_avo(1,n_avo)
  // vector SSB(styr,endyr_r)
  matrix natage(styr,endyr_r,1,nages);
  vector srmod_rec(styr_est,endyr_est);
  matrix Z(styr,endyr_r,1,nages);
  matrix F(styr,endyr_r,1,nages);
  matrix S(styr,endyr_r,1,nages);
  matrix log_sel_fsh(styr,endyr_r,1,nages);
  matrix sel_fsh(styr,endyr_r,1,nages);
  matrix log_sel_bts(styr,endyr_r,1,nages);
  matrix log_sel_eit(styr,endyr_r,1,nages);
  number ff;
  number ssqcatch;
  number avgsel_fsh;
  number avgsel_bts;
  number avgsel_eit;
  number bzero;
  number surv;
  number nthisage;
  vector surv_like(1,3);
  number cpue_like;
  number avo_like;
  vector sel_like(1,3);
  vector sel_like_dev(1,3);
  vector age_like(1,ngears);
  number len_like;
  vector age_like_offset(1,ngears);
  number MN_const // Multinomial constant
  !! MN_const = 1e-3; // Multinomial constant
  vector Priors(1,4);
  vector rec_like(1,7);
  number sumtmp;
  number tmpsp;
  vector log_initage(2,nages);
  vector pred_biom(styr,endyr_r);
  vector fake_SSB(1,20)

  sdreport_number avg_age_msy;
  // sdreport_number avgln_msy;
  sdreport_number avgwt_msy;
  sdreport_number MSY;
  sdreport_number Fmsy;
  sdreport_number Fmsy2;
  sdreport_number lnFmsy;
  sdreport_number lnFmsy2;
  sdreport_number SER_Fmsy;
  sdreport_number Fendyr_Fmsy;
  sdreport_number Rmsy;
  sdreport_number Bmsy;
  sdreport_number Bmsy2;

  sdreport_vector Fcur_Fmsy(1,nscen);
  sdreport_vector Bcur_Bmsy(1,nscen);
  sdreport_vector Bcur_Bmean(1,nscen);
  sdreport_vector Bcur3_Bcur(1,nscen);
  sdreport_vector Bcur3_Bmean(1,nscen);
  sdreport_vector Bcur2_Bmsy(1,nscen);
  sdreport_vector Bcur2_B20(1,nscen);
  sdreport_vector LTA1_5R(1,nscen);   // long term average age 1_5 Ratio
  sdreport_vector LTA1_5(1,nscen);    // long term average age 1_5
  sdreport_vector MatAgeDiv1(1,nscen); // Diversity of Age structure in mature population
  sdreport_vector MatAgeDiv2(1,nscen); // Diversity of Age structure in mature population
  sdreport_vector RelEffort(1,nscen); // Effort relative to 2012 (endyr)   

  sdreport_number F40_spb;
  // sdreport_number F40_catch;
  sdreport_number begbiom;
  sdreport_number DepletionSpawners;
  sdreport_number SB100;
  sdreport_number Current_Spawners;
  sdreport_vector pred_rec(styr,endyr_r);
  sdreport_vector age_3_plus_biom(styr,endyr_r+2);
  sdreport_vector ABC_biom(1,10);
  sdreport_vector ABC_biom2(1,10);
  sdreport_vector rechat(1,20);
  sdreport_vector SER(styr,endyr_r);
  matrix SER_future(1,nscen,styr_fut,endyr_fut);
  matrix catch_future(1,nscen,styr_fut,endyr_fut);
  sdreport_matrix future_SSB(1,nscen,endyr_r,endyr_fut)
  vector age_1_7_biomass(styr,endyr_r);
  objective_function_value fff;

PRELIMINARY_CALCS_SECTION
  fixed_catch_fut1 = fixed_catch_fut1 + 0.1 ;
  if(wt_fut_phase>0)
    wt_fut = wt_mn;           // initializes estimates to correct values...Eq. 21
  else
    wt_fut = wt_fsh(endyr_r); // initializes estimates to correct values...Eq. 21
  base_natmort(1)=.9;
  base_natmort(2)=.45;
  for (j=3 ;j<=nages;j++)
    base_natmort(j)=natmortprior;

  natmort = base_natmort;
  cout <<"M input= "<<natmort <<endl;
  write_log(natmort);
  cout <<ctrl_flag<<endl;
  write_log(ctrl_flag);
  age_like_offset.initialize();

  //--Caculate offset for multinomials (if used)---------------
  for (int igear =1;igear<=ngears;igear++)
  {
    for (int i=1; i <= nagecomp(igear); i++)
    {
      if (igear==1)
      {
        oac_fsh(i)=oac_fsh_data(i)/sum(oac_fsh_data(i));
        age_like_offset(igear)-=sam_fsh(i)*oac_fsh(i)*log(oac_fsh(i) +MN_const);
      }
      else if (igear==2)
      {
        obs_biom_bts(i) = obs_bts_data(i)            ;
        std_obs_bts(i)  = std_obs_bts_data(i)        ;
        ot_bts(i)       = sum(oac_bts_data(i)(mina_bts,nages)); // mina_bts is for totals
        oac_bts(i )     = oac_bts_data(i)/sum(oac_bts_data(i));
        age_like_offset(igear)-=sam_bts(i)*oac_bts(i)*log(oac_bts(i) +MN_const);
      }
      else if (igear==3)
      {
        obs_eit(i)      = obs_eit_data(i)            ;
        std_obs_eit(i)  = std_obs_eit_data(i)        ;
        oa1_eit(i) = oac_eit_data(i,1); // set observed age 1 index
        ot_eit(i)  = sum(oac_eit_data(i)(mina_bts,nages));
        oac_eit(i)(mina_eit,nages)  = oac_eit_data(i)(mina_eit,nages)/sum(oac_eit_data(i)(mina_eit,nages));
        age_like_offset(igear)     -= sam_eit(i)*oac_eit(i)(mina_eit,nages)*
                                             log(oac_eit(i)(mina_eit,nages) +MN_const);
      }
    }     
  }
  ot_eit(n_eit_r) = sum(oac_eit_data(n_eit_r)(mina_bts,nages));
  // flag to ignore age 1's
  if (std_eit(n_eit_r)/ot_eit(n_eit_r) > 0.4 ) ignore_last_eit_age1 = 1; else ignore_last_eit_age1=0;
  cout <<" Last age comp in BTS: " << endl << oac_bts_data(n_bts) << endl;
  cout <<" Age_like_offset:      " << endl << age_like_offset     << endl;
  Cat_Fut(1) = next_yrs_catch; //  catch guess                            
  // Simple decrement of future cathes to capture relationship between adjustments (below Bmsy) w/in same year
  for (i=2;i<=10;i++) 
    Cat_Fut(i) = Cat_Fut(i-1)*.95;
  Cat_Fut(1) = 1375.;
  Cat_Fut(2) = 1200.;

  cout << "Next year's catch and decrements"<<endl<<Cat_Fut<<endl;
  lse_eit = elem_div(std_eit(1,n_eit_r),ot_eit);
  lse_eit = sqrt(log(square(lse_eit) + 1.));
  cout<<endl<<"observed std eit "<<endl;
  cout<<std_eit<<endl;
  lvar_eit = square(lse_eit);

RUNTIME_SECTION
   maximum_function_evaluations 50,50,350,500,5000
   convergence_criteria .001,.0001,.0001,1e-7

PROCEDURE_SECTION
  //cout<<"Gothere"<<endl;
  Get_Selectivity();
  //cout<<"Gothere"<<endl;
  Get_Mortality_Rates();
  //cout<<"Gothere"<<endl;
  GetNumbersAtAge();
  //cout<<"Gothere"<<endl;
  Get_Catch_at_Age();
  //cout<<"Gothere"<<endl;
  GetDependentVar();  // Includes MSY, F40% computations
  //cout<<"Gothere"<<endl;
  Evaluate_Objective_Function();
  //cout<<"Gothere"<<endl;
  if (do_fmort)
    Profile_F();
  if (mceval_phase()) 
    write_eval();

FUNCTION Get_Selectivity
  avgsel_fsh.initialize();
  avgsel_bts.initialize();
  avgsel_eit.initialize();
  //cout<<"InSel"<<endl;
  // Basic free-form penalized selectivities for fishery, calls different function if devs active or not
  if (active(sel_devs_fsh))
    log_sel_fsh = compute_fsh_selectivity(n_selages_fsh,styr,avgsel_fsh,sel_coffs_fsh,sel_devs_fsh,group_num_fsh);
  else 
    log_sel_fsh = compute_selectivity(n_selages_fsh,styr,avgsel_fsh,sel_coffs_fsh);
  //cout<<"InSel"<<endl;

  if (phase_logist_fsh>0) // For logistic fishery selectivity (not default, NOT USED)
  {
    dvariable dif;
    dvariable inf;
    if (active(sel_a501_fsh_dev))
    {
      dvar_matrix seldevs_tmp(1,3,styr,endyr_r);
      seldevs_tmp(1) = sel_dif1_fsh_dev;
      seldevs_tmp(2) = sel_a501_fsh_dev;
      seldevs_tmp(3) = sel_trm2_fsh_dev;
      log_sel_fsh = compute_selectivity1(styr,sel_dif1_fsh,sel_a501_fsh,sel_trm2_fsh,seldevs_tmp);
    }
    else 
      log_sel_fsh = compute_selectivity1(styr,sel_dif1_fsh,sel_a501_fsh,sel_trm2_fsh);
  }

  //cout<<"InSel"<<endl;
  // Eq. 4
  if (active(sel_a50_bts))
  {
    if (active(sel_a50_bts_dev))
      log_sel_bts = compute_selectivity(styr_bts,sel_slp_bts,sel_a50_bts,sel_slp_bts_dev,sel_a50_bts_dev); 
      // log_sel_bts = compute_selectivity(styr_bts,sel_slp_bts,sel_a50_bts,sel_a50_bts_dev);
    else 
      log_sel_bts = compute_selectivity(styr_bts,sel_slp_bts,sel_a50_bts);

  // Bottom trawl selectivity of age 1's independent of logistic selectivity
    for (i=styr_bts;i<=endyr_r;i++)
      log_sel_bts(i,1) = sel_age_one*exp(sel_one_bts_dev(i));
  }
  else
    if (active(sel_devs_bts))
      log_sel_bts = compute_selectivity(n_selages_bts,styr_bts,avgsel_bts,sel_coffs_bts,sel_devs_bts,group_num_bts);
    else 
      log_sel_bts = compute_selectivity(n_selages_bts,styr_bts,avgsel_bts,sel_coffs_bts);


  if (active(sel_devs_eit))
    log_sel_eit = compute_selectivity_eit(n_selages_eit,styr_eit,avgsel_eit,sel_coffs_eit,sel_devs_eit);
    // log_sel_eit = compute_selectivity(n_selages_eit,styr_eit,avgsel_eit,sel_coffs_eit,sel_devs_eit,group_num_eit);
  else 
    log_sel_eit = compute_selectivity_eit(n_selages_eit,styr_eit,avgsel_eit,sel_coffs_eit);
  //cout<<"InSel"<<endl;

  sel_fsh = mfexp(log_sel_fsh);

FUNCTION Get_Mortality_Rates
  if (active(natmort_phi)) // Sensitivity approach for estimating natural mortality (as offset of input vector, NOT USED, NOT IN DOC)
    natmort(3,nages) = base_natmort(3,nages) * mfexp(natmort_phi);

  Fmort=  mfexp(log_avg_F + log_F_devs); // Eq. 2
  for (i=styr; i<=endyr_r; i++)
  {
    F(i) = Fmort(i) * sel_fsh(i); // Eq. 2
    Z(i) = F(i) + natmort; // Eq. 1
  }
  S=mfexp(-1.0*Z); // Eq. 1
 
FUNCTION GetNumbersAtAge 
  //-This calculates the first year's numbers at age, estimated freely (no equil. assumptions)
  Get_Bzero();
  for (i=styr;i<=endyr_r;i++)
    rec_epsilons(i)=log_rec_devs(i)+larv_rec_devs(nsindex(i),ewindex(i));

  log_initage=log_initdevs+log_avginit; 

  natage(styr)(2,nages)=mfexp(log_initage); // Eq. 1

  // Recruitment in subsequent years
  for (i=styr;i<=endyr_r;i++)
  {
    natage(i,1) = mfexp(log_avgrec+rec_epsilons(i)); // Eq. 1
    pred_rec(i) = natage(i,1);
  }

  SSB(styr)  = elem_prod(elem_prod(natage(styr),pow(S(styr),yrfrac)),p_mature)*wt_ssb(styr); // Eq. 1
  natage(styr+1)(2,nages) = ++elem_prod(natage(styr)(1,nages-1), S(styr)(1,nages-1));   // Eq. 1
  natage(styr+1,nages)   += natage(styr,nages)*S(styr,nages); // Eq. 1

  for (i=styr+1;i<endyr_r;i++)
  {
    SSB(i)          = elem_prod(elem_prod(natage(i),pow(S(i),yrfrac)),p_mature)*wt_ssb(i); // Eq. 1
    natage(i+1)(2,nages) = ++elem_prod(natage(i)(1,nages-1), S(i)(1,nages-1));   // Eq. 1
    natage(i+1,nages)   += natage(i,nages)*S(i,nages); // Eq. 1
  }
  SSB(endyr_r)  = elem_prod(elem_prod(natage(endyr_r),pow(S(endyr_r),yrfrac)),p_mature)*wt_ssb(endyr_r); // Eq. 1

  meanrec = mean(pred_rec(styr_est,endyr_r)); 

FUNCTION GetDependentVar 
  // For making some output for spiffy output
  // Spiffy SR output
  tmpsp=1.1*max(SSB);
  if (tmpsp<Bzero) tmpsp=1.1*Bzero;
  dvariable Xspawn ;
  for (i=1;i<=20;i++)
  {
    fake_SSB(i)=tmpsp*double(i-.5)/19.5;
    Xspawn = fake_SSB(i); 
    rechat(i)=SRecruit(Xspawn); // Eq. 12
  }

  if (last_phase())
  {
  // Spiffy q-temperature relationship output
    for (i=1;i<=5;i++)
      q_temp(i)         = bt_slope * double(i-3)*.6666666667 + q_bts ;

    SB100   = meanrec*Bzero/Rzero ;
    F40_spb = 0.4*SB100;
    SBF40   = F40_spb;
    SBF35   = 0.35*SB100;
    compute_Fut_selectivity();
    compute_spr_rates();
  }
  ////For standard deviation report////////////////////
  // if (mceval_phase())
  if (sd_phase()||mceval_phase())
  {
    for (i=styr;i<=endyr_r;i++)
    {
      age_3_plus_biom(i) = natage(i)(3,nages) * wt_ssb(i)(3,nages); 
      age_1_7_biomass(i) = natage(i)(1,7) * wt_ssb(i)(1,7); 
    }
    regime(1) = mean(pred_rec(1964,1977));
    regime(4) = mean(pred_rec(1978,1989));

    if (endyr_r>=2005)
    {
      regime(2) = mean(pred_rec(1978,2005));
      regime(5) = mean(pred_rec(1990,2005));
      regime(7) = mean(pred_rec(2000,2005));
      regime(8) = mean(pred_rec(1964,2005));
      regime(6) = mean(pred_rec(1990,1999));
      regime(3) = mean(pred_rec(1978,1999));
    }
    else
    {
      if (endyr_r<=2000)
        regime(7) = pred_rec(endyr_r);
      else
        regime(7) = mean(pred_rec(2000,endyr_r));

      regime(2) = mean(pred_rec(1978,endyr_r));
      regime(5) = mean(pred_rec(1990,endyr_r));
      regime(8) = mean(pred_rec(1964,endyr_r));
      if (endyr_r<=1999)
      {
        regime(3) = mean(pred_rec(1978,endyr_r));
        regime(6) = mean(pred_rec(1990,endyr_r));
      }
      else
      {
        regime(6) = mean(pred_rec(1990,1999));
        regime(3) = mean(pred_rec(1978,1999));
      }
    }
    get_msy();
    i = endyr_r;
    dvar_vector Ntmp(1,nages);
    dvar_vector Ntmp2(1,nages);
    dvar_vector Stmp(1,nages);
    dvariable   Ftmp;
    Ntmp.initialize();
    Ntmp2.initialize();
    Stmp.initialize();

    endyr_N       = natage(endyr_r);
    Ntmp(2,nages) = ++elem_prod(natage(i)(1,nages-1), S(i)(1,nages-1));  
    Ntmp(nages)  += natage(i,nages)*S(i,nages);
    Ntmp(1)       = meanrec;
    age_3_plus_biom(i+1)  = Ntmp(3,nages) * wt_ssb(i)(3,nages); 

    Ftmp = SolveF2(Ntmp, obs_catch(endyr_r));
    Stmp = mfexp(-(Ftmp*sel_fut + natmort));
    Ntmp2(2,nages) = ++elem_prod(Ntmp(1,nages-1), Stmp(1,nages-1));  
    Ntmp2(nages)  += Ntmp(nages)*Stmp(nages);
    Ntmp2(1)       = meanrec;
    age_3_plus_biom(i+2)  = Ntmp2(3,nages) * wt_ssb(endyr_r)(3,nages); 

    // Loop over range of future catch levels
    for (int icat=1;icat<=10;icat++)
    {
      Ntmp(2,nages) = ++elem_prod(natage(i)(1,nages-1), S(i)(1,nages-1));  
      Ntmp(nages)  += natage(i,nages)*S(i,nages);
      Ntmp(1)       = meanrec;
      Ftmp = SolveF2(Ntmp, Cat_Fut(icat));
      Stmp = mfexp(-(Ftmp*sel_fut + natmort));
      // ABC_biom = age_3_plus_biom(i+1) ; // ABC_biom =  elem_prod(p_mature,Ntmp) * elem_prod(wt_ssb(i),pow(Stmp,yrfrac)); 
      ABC_biom(icat) =  elem_prod(sel_fut,Ntmp) * wt_ssb(i); 
      SSB_1(icat)    = value(elem_prod(elem_prod(Ntmp,pow(Stmp,yrfrac)),p_mature)*wt_ssb(endyr_r));
  
   // Add to get 2-yr projection of ABC based on harmonic mean
      Ntmp2(2,nages) = ++elem_prod(Ntmp(1,nages-1), Stmp(1,nages-1));  
      Ntmp2(nages)  += Ntmp(nages)*Stmp(nages);
      Ntmp2(1)       = meanrec;
      Ftmp = SolveF2(Ntmp2,Cat_Fut(icat)); // mean(obs_catch(endyr_r-3,endyr_r)));
      Stmp = mfexp(-(Ftmp*sel_fut + natmort));
      ABC_biom2(icat) =  elem_prod(sel_fut,Ntmp2) * wt_ssb(i); 
      SSB_2(icat)   = value(elem_prod(elem_prod(Ntmp2,pow(Stmp,yrfrac)),p_mature)*wt_ssb(endyr_r));
      if(SSB_1(icat) < value(Bmsy))
        adj_1(icat) = value((SSB_1(icat)/Bmsy - 0.05)/(1.-0.05));
      if(SSB_2(icat) < value(Bmsy))
        adj_2(icat) = value((SSB_2(icat)/Bmsy - 0.05)/(1.-0.05));
    }

    get_SER();
    begbiom=natage(styr)*wt_ssb(styr);
    Current_Spawners =SSB(endyr_r);
    DepletionSpawners=SSB(endyr_r)/SSB(styr);
    Future_projections_fixed_F();
    // F40_catch = catch_future(1,styr_fut);
    if (!mceval_phase())
    {
      // Re-run w/o F mort (for output)
      write_nofish();
      write_srec();
    }
    Percent_Bzero   = SSB(endyr_r) / Bzero;
    Percent_Bzero_1 = future_SSB(4,styr_fut) / Bzero;
    Percent_Bzero_2 = future_SSB(5,styr_fut) / Bzero;
    Percent_B100   = SSB(endyr_r) / SB100;
    Percent_B100_1 = future_SSB(4,styr_fut) / SB100;
    Percent_B100_2 = future_SSB(5,styr_fut) / SB100;
  }  // End of sd_phase

FUNCTION Future_projections_fixed_F
  catch_future.initialize();
  future_SSB.initialize();
  dvariable sumtmp1;
  dvariable sumtmp2;
  dvariable MeanSSB;
  dvar_vector ptmp(1,nages);
  dvar_vector H(styr,endyr_r);
  sumtmp1=0.;
  sumtmp2=0.;
  for (i=styr; i<=endyr_r; i++)
  {
    ptmp     = elem_prod(elem_prod(natage(i),wt_ssb(i)),p_mature)+0.0001;
    ptmp    /= sum(ptmp);
    H(i)     = mfexp(-ptmp*log(ptmp));
    sumtmp1 += sum(natage(i)(1,5));
    sumtmp2 += sum(natage(i)(6,nages));
  }
  MeanSSB    = mean(SSB(1978,endyr_r-1));
  // R_report(H);
  for (int k=1;k<=nscen;k++)
  {
    future_SSB(k,endyr_r)         = SSB(endyr_r);
    dvariable Xspawn ;
    if (phase_sr<0) 
      natage_future(k,styr_fut, 1)  = mfexp(log_avgrec + rec_dev_future(styr_fut));
    else //Stock-recruitment curve included---------
    {
      Xspawn =   future_SSB(k,endyr_r) ;
      natage_future(k,styr_fut,1)   = SRecruit(Xspawn) * mfexp(rec_dev_future(styr_fut) );
    }
    natage_future(k,styr_fut)(2,nages)  = ++elem_prod(natage(endyr_r)(1,nages-1), S(endyr_r)(1,nages-1));  
    natage_future(k,styr_fut,nages)    += natage(endyr_r,nages)*S(endyr_r,nages);
    // Set two-year olds in 1st future year to mean value...
    // natage_future(k,styr_fut,2)         = mean(column(natage,2));
    // natage_future(k,styr_fut,5)         = mean(column(natage,5));

    dvariable criterion;
    dvariable Bref ;
    for (i=styr_fut;i<=endyr_fut;i++)
    {
      switch (k)
      {
        case 1:
          ftmp = F40;
          // ftmp=0.0001*F(endyr_r,6);
          break;
        case 2:
          // ftmp = SolveF2(natage_future(k,styr_fut),500.);
          ftmp = F35;
          break;
        case 3:
          // ftmp = SolveF2(natage_future(k,styr_fut),750.);
          ftmp = Fmsy;
          break;
        case 4:
          ftmp = SolveF2(natage_future(k,styr_fut),1000.);
          // ftmp = SolveF2(natage_future(k,styr_fut),fixed_catch_fut1);
          // F_future(k,i) = F(endyr);
          break;
        case 5:
          ftmp = SolveF2(natage_future(k,styr_fut),1200.);
          // ftmp = SolveF2(natage_future(k,styr_fut),fixed_catch_fut2);
          // F_future(k,i) = F(endyr)*1.5;
          break;
        case 6:
          ftmp = SolveF2(natage_future(k,styr_fut),1500.);
          // ftmp = SolveF2(natage_future(k,styr_fut),fixed_catch_fut3);
          // F_future(k,i) = F(endyr)*2.;
          F_future(k,i) = sel_fut*ftmp;
          break;
        case 7:
          ftmp = SolveF2(natage_future(k,styr_fut),2000.);
          break;
          // ftmp = SolveF2(natage_future(k,styr_fut),fixed_catch_fut3);
          // F_future(k,i) = F(endyr)*2.;
      }
      F_future(k,i) = sel_fut*ftmp;
    // sel_fut is normalized to age 6 (=1)
    // ftmp = F_future(k,i,6);
    // Get future F's since these are the same in the future...
      Z_future(i) = F_future(k,i) + natmort;
      S_future(i) = exp(-Z_future(i));
      criterion = 0.40*SB100;
      Bref      = 0.35*SB100;
  /* natage_future(k,i+1)(2,nages) = ++elem_prod(natage_future(k,i)(1,nages-1), S_future(i)(1,nages-1));  
       natage_future(k,i+1,nages)   += natage_future(k,i,nages)* S_future(i,nages);

       future_SSB(k,i)       = elem_prod(elem_prod(natage_future(k,i),pow(S_future(i),yrfrac)), p_mature) * wt_ssb(endyr_r);

       Now for all subsequent future years 
       for (i=styr_fut;i<=endyr_fut;i++)
       {
         if ( k==3) 
         {
           criterion = Bmsy;
           Bref      = Bmsy;
         }
         else 
         {
           criterion = SBF40;
           Bref      = SBF40;
         }
         future_SSB(k,i)   = elem_prod(natage_future(i),p_mature)*wt_ssb(endyr_r);
  */

      // Compute this once for begin year (to compute the adjustment rate, loop over 4 times to get adequate convergence)
      for (int isp=1;isp<=4;isp++)
      {
        future_SSB(k,i)       = elem_prod(elem_prod(natage_future(k,i),pow(S_future(i),yrfrac)), p_mature) * wt_ssb(endyr_r);
        if(future_SSB(k,i) < criterion && k<4)
        {
          F_future(k,i)        = ftmp*sel_fut*(future_SSB(k,i)/Bref -0.05)/(1.-0.05);
          Z_future(i)          = F_future(k,i) + natmort;
          S_future(i)          = exp(-Z_future(i));
        }
      }
      cout<< "F Future : "<<k<<" "<<i<<" "<<F_future(k,i) <<" "<<future_SSB(k,i)<<" "<<Bref<<endl;
      // Now compute the time of spawning SSB for everything else....
      future_SSB(k,i)   = elem_prod(elem_prod(natage_future(k,i),pow(S_future(i),yrfrac)), p_mature) * wt_ssb(endyr_r);

      if (phase_sr<0) //No Stock-recruitment curve for future projections--------
        natage_future(k,i, 1)  = mfexp(log_avgrec + rec_dev_future(i));
      else //Use Stock-recruitment curve ---------
      {
        Xspawn =future_SSB(k,i-1);  
        natage_future(k,i,1)   = SRecruit(Xspawn)  * mfexp(rec_dev_future(i) );
      }
      
      if (i<endyr_fut)
      {
        natage_future(k,i+1)(2,nages) = ++elem_prod(natage_future(k,i)(1,nages-1), S_future(i)(1,nages-1));  
        natage_future(k,i+1,nages)   +=  natage_future(k,i,nages)*S_future(i,nages);
      }
    }
    if (phase_sr<0)
      natage_future(k,endyr_fut, 1) = mfexp(log_avgrec + rec_dev_future(endyr_fut));
    else
    {
      Xspawn =future_SSB(k,endyr_fut-1);  
      natage_future(k,endyr_fut,1)  = SRecruit(Xspawn) * mfexp(rec_dev_future(endyr_fut) );
    }
    future_SSB(k,endyr_fut)    = elem_prod(elem_prod(natage_future(k,endyr_fut),pow(S_future(endyr_fut),yrfrac)), p_mature) * wt_ssb(endyr_r);
  
    // Now get catch at future ages
    for (i=styr_fut; i<=endyr_fut; i++)
    {
      catage_future(i)  = elem_prod( elem_prod(natage_future(k,i) , F_future(k,i) ) , elem_div( (1. - S_future(i)) , Z_future(i) ));
      catch_future(k,i) = catage_future(i)*wt_fsh(endyr_r);
      SER_future(k,i)   = get_SER(natage_future(k,i),mean(F_future(k,i)));
    }

    Bcur_Bmsy(k)   = future_SSB(k,styr_fut+1)/Bmsy;
    Bcur_Bmean(k)  = future_SSB(k,styr_fut+1)/MeanSSB;
    Bcur2_B20(k)   = future_SSB(k,styr_fut+2)/(.2*Bzero);
    Bcur2_Bmsy(k)  = future_SSB(k,styr_fut+2)/Bmsy;
    Bcur3_Bcur(k)  = future_SSB(k,styr_fut+4)/future_SSB(k,styr_fut);
    Bcur3_Bmean(k) = future_SSB(k,styr_fut+4)/MeanSSB;
    Fcur_Fmsy(k)   = F_future(k,styr_fut,6)/Fmsy;
    
    RelEffort(k)   = F_future(k,styr_fut,6)/F(endyr_r,6) ; // Effort relative to 2012 (endyr)   
    LTA1_5(k)      = sum(natage_future(k,endyr_fut)(1,5))/sum(natage_future(k,endyr_fut)(6,nages));                                                   // long term average age 1_5
    LTA1_5R(k)     = LTA1_5(k)/(sumtmp1/sumtmp2);
    ptmp           = elem_prod(elem_prod(natage_future(k,styr_fut+1),wt_ssb(endyr_r)),p_mature)+0.0001;
    ptmp          /= sum(ptmp);
    MatAgeDiv1(k)  = mfexp(-ptmp*log(ptmp))/(H(1994));
    ptmp           = elem_prod(elem_prod(natage_future(k,endyr_fut),wt_ssb(endyr_r)),p_mature)+0.0001;
    ptmp          /= sum(ptmp);
    MatAgeDiv2(k)  = mfexp(-ptmp*log(ptmp))/(H(1994));
    // MatAgeDiv1(k)  = mfexp(-ptmp*log(ptmp))/mean(H);
    // R_report(k);
    // R_report(mfexp(-ptmp*log(ptmp)));
    /*  
    sdreport_number Fcur_Fmsy;
    sdreport_number Bcur_Bmsy;
    sdreport_number Bcur3_Bcur;
    sdreport_number Bcur2_Bmsy;
    sdreport_number Bcur2_B0;
    */
  }   //End of loop over F's


FUNCTION dvariable get_SER(_CONST dvariable& Ftmp)
  RETURN_ARRAYS_INCREMENT();
  dvar_vector NoFishSurvival=exp(-natmort);
  dvar_vector WiFishSurvival=exp(-natmort - Ftmp*sel_fut);
  dvar_vector Ntmp(1,nages);
  dvariable spawn_nofsh;
  dvariable spawn_wfsh;
  spawn_nofsh.initialize();
  spawn_wfsh.initialize();
  Ntmp.initialize();
  Ntmp(1) = 1.;
  for (j=2;j<nages;j++) 
    Ntmp(j) = Ntmp(j-1) * NoFishSurvival(j-1);
  Ntmp(nages) = Ntmp(nages-1)/(1-NoFishSurvival(nages));
  spawn_nofsh  = elem_prod(Ntmp,wt_ssb(endyr_r))  * p_mature; //Target computation, note restarts Ntmp in next line

  for (j=2;j<nages;j++) Ntmp(j) = Ntmp(j-1) * WiFishSurvival(j-1);
  Ntmp(nages) = Ntmp(nages-1)/(1-WiFishSurvival(nages));
  spawn_wfsh  = elem_prod(Ntmp,wt_ssb(endyr_r))  * p_mature; //Target computation, note restarts Ntmp in next line

  RETURN_ARRAYS_DECREMENT();
  return(1. - spawn_wfsh / spawn_nofsh) ;

FUNCTION dvariable get_SER(dvar_vector& Ntmp, _CONST dvariable& Ftmp)
  RETURN_ARRAYS_INCREMENT();
  dvar_vector NoFishSurvival=exp(-natmort);
  dvar_vector WiFishSurvival=exp(-natmort - Ftmp*sel_fut);
  dvar_vector Ntmpw(1,nages);
  dvar_vector Ntmpwo(1,nages);
  dvariable spawn_nofsh;
  dvariable spawn_wfsh;
  spawn_nofsh.initialize();
  spawn_wfsh.initialize();
  Ntmpwo.initialize();
  Ntmpw.initialize();
  // for (j=2;j<nages;j++) Ntmpwo(j) = Ntmpwo(j-1) * NoFishSurvival(j-1);
  Ntmpw(1)      = Ntmp(1);
  Ntmpw(2,nages)= ++elem_prod(Ntmp(1,nages-1), WiFishSurvival(1,nages-1));  
  Ntmpw(nages) += Ntmp(nages) * WiFishSurvival(nages);
  spawn_wfsh  = elem_prod(Ntmpw,wt_ssb(endyr_r))  * p_mature; //Target computation, note restarts Ntmp in next line

  // Now do without fishing
  Ntmpwo(1)      = Ntmp(1);
  Ntmpwo(2,nages)= ++elem_prod(Ntmp(1,nages-1), NoFishSurvival(1,nages-1));  
  Ntmpwo(nages) += Ntmp(nages) * NoFishSurvival(nages);
  spawn_nofsh  = elem_prod(Ntmpwo,wt_ssb(endyr_r))  * p_mature; //Target computation, note restarts Ntmp in next line

  RETURN_ARRAYS_DECREMENT();
  return(1. - spawn_wfsh / spawn_nofsh) ;

FUNCTION get_SER
  dvar_vector NoFishSurvival=exp(-natmort);
  dvar_vector Ntmp(1,nages); 
  dvariable spawn_nofsh;
  dvariable spawn_wfsh;
  // S = Matrix of survival (sel(j)*fmort(i)+natmort)
  for (i=styr;i < endyr_r;i++)
  {
    Ntmp(1)      = natage(i+1,1);
    Ntmp(2,nages)= ++elem_prod(natage(i)(1,nages-1), S(i)(1,nages-1));  
    Ntmp(nages) += Ntmp(nages) * S(i,nages);

    spawn_wfsh  = elem_prod(Ntmp,wt_ssb(i))  * p_mature; //Target computation, note restarts Ntmp in next line

    Ntmp(1)      = natage(i+1,1);
    Ntmp(2,nages)= ++elem_prod(natage(i)(1,nages-1), NoFishSurvival(1,nages-1));  
    Ntmp(nages) += Ntmp(nages) * NoFishSurvival(nages);//Accumulate last age group

    spawn_nofsh = elem_prod(Ntmp,wt_ssb(i)) * p_mature;  // Same as above target, but this time w/o fshing

    SER(i)       = 1. - spawn_wfsh/spawn_nofsh;
  }
  // This is just for the last year...
  Ntmp(1)       = natage(endyr_r,1);
  Ntmp(2,nages) = ++elem_prod(natage(endyr_r)(1,nages-1), S(endyr_r)(1,nages-1));  
  Ntmp(nages)  += Ntmp(nages) * S(endyr_r,nages);
  spawn_wfsh   = elem_prod(Ntmp,wt_ssb(endyr_r))  * p_mature;

  Ntmp(2,nages) = ++elem_prod(natage(endyr_r)(1,nages-1), NoFishSurvival(1,nages-1));  
  Ntmp(nages)  += Ntmp(nages) * NoFishSurvival(nages);
  spawn_nofsh  = elem_prod(Ntmp,wt_ssb(endyr_r)) * p_mature;

  SER(endyr_r)    = 1. - spawn_wfsh/spawn_nofsh;

FUNCTION compute_Fut_selectivity
  sel_fut.initialize();
  // Average future selectivity based on most recent years' (as read in from file)
  for (i=endyr_r-nyrs_sel_avg+1;i<=endyr_r;i++)
    sel_fut = sel_fut + sel_fsh(i);
  sel_fut/=nyrs_sel_avg;
  sel_fut/=sel_fut(6); // NORMALIZE TO AGE 6

FUNCTION compute_spr_rates
  //Compute SPR Rates 
  F35 = get_spr_rates(.35);
  F40 = get_spr_rates(.40);

FUNCTION dvariable get_spr_rates(double spr_percent,dvar_vector sel)
  RETURN_ARRAYS_INCREMENT();
  double df=1.e-3;
  dvariable F1 ;
  F1.initialize();
  F1 = .9*natmort(3);
  dvariable F2;
  dvariable F3;
  dvariable yld1;
  dvariable yld2;
  dvariable yld3;
  dvariable dyld;
  dvariable dyldp;
  // Newton Raphson stuff to go here
  for (int ii=1;ii<=6;ii++)
  {
    F2     = F1 + df;
    F3     = F1 - df;
    yld1   = -1000*square(log(spr_percent/spr_ratio(F1,sel)));
    yld2   = -1000*square(log(spr_percent/spr_ratio(F2,sel)));
    yld3   = -1000*square(log(spr_percent/spr_ratio(F3,sel)));
    dyld   = (yld2 - yld3)/(2*df);                          // First derivative (to find the root of this)
    dyldp  = (yld3-(2*yld1)+yld2)/(df*df);  // Newton-Raphson approximation for second derivitive
    F1    -= dyld/dyldp;
  }
  RETURN_ARRAYS_DECREMENT();
  return(F1);

FUNCTION dvariable spr_ratio(dvariable trial_F,dvar_vector& sel)
  RETURN_ARRAYS_INCREMENT();
  dvariable SBtmp;
  dvar_vector Ntmp(1,nages);
  dvar_vector srvtmp(1,nages);
  SBtmp.initialize();
  Ntmp.initialize();
  srvtmp.initialize();
  dvar_vector Ftmp(1,nages);
  Ftmp = sel*trial_F;
  srvtmp  = exp(-(Ftmp + natmort) );
  dvar_vector wttmp = wt_ssb(endyr_r);
  Ntmp(1)=1.;
  j=1;
  SBtmp  += Ntmp(j)*p_mature(j)*wttmp(j)*pow(srvtmp(j),yrfrac);
  for (j=2;j<nages;j++)
  {
    Ntmp(j) = Ntmp(j-1)*srvtmp(j-1);
    SBtmp  += Ntmp(j)*p_mature(j)*wttmp(j)*pow(srvtmp(j),yrfrac);
  }
  Ntmp(nages)=Ntmp(nages-1)*srvtmp(nages-1)/(1.-srvtmp(nages));

  SBtmp  += Ntmp(nages)*p_mature(nages)*wttmp(nages)*pow(srvtmp(nages),yrfrac);
  RETURN_ARRAYS_DECREMENT();
  return(SBtmp/phizero);

FUNCTION dvariable get_spr_rates(double spr_percent)
  RETURN_ARRAYS_INCREMENT();
  double df=1.e-3;
  dvariable F1 ;
  F1.initialize();
  F1 = .9*natmort(3);
  dvariable F2;
  dvariable F3;
  dvariable yld1;
  dvariable yld2;
  dvariable yld3;
  dvariable dyld;
  dvariable dyldp;
  // Newton Raphson stuff to go here
  for (int ii=1;ii<=6;ii++)
  {
    F2     = F1 + df;
    F3     = F1 - df;
    yld1   = -1000*square(log(spr_percent/spr_ratio(F1)));
    yld2   = -1000*square(log(spr_percent/spr_ratio(F2)));
    yld3   = -1000*square(log(spr_percent/spr_ratio(F3)));
    dyld   = (yld2 - yld3)/(2*df);                          // First derivative (to find the root of this)
    dyldp  = (yld3-(2*yld1)+yld2)/(df*df);  // Newton-Raphson approximation for second derivitive
    F1    -= dyld/dyldp;
  }
  RETURN_ARRAYS_DECREMENT();
  return(F1);

FUNCTION dvariable spr_ratio(double trial_F)
  RETURN_ARRAYS_INCREMENT();
  dvariable SBtmp;
  dvar_vector Ntmp(1,nages);
  dvar_vector srvtmp(1,nages);
  SBtmp.initialize();
  Ntmp.initialize();
  srvtmp.initialize();
  dvar_vector Ftmp(1,nages);
  dvar_vector wttmp = wt_ssb(endyr_r);
  Ftmp = sel_fut*trial_F;
  srvtmp  = exp(-(Ftmp + natmort) );
  Ntmp(1)=1.;
  j=1;
  SBtmp  += Ntmp(j)*p_mature(j)*wttmp(j)*pow(srvtmp(j),yrfrac);
  for (j=2;j<nages;j++)
  {
    Ntmp(j) = Ntmp(j-1)*srvtmp(j-1);
    SBtmp  += Ntmp(j)*p_mature(j)*wttmp(j)*pow(srvtmp(j),yrfrac);
  }
  Ntmp(nages)=Ntmp(nages-1)*srvtmp(nages-1)/(1.-srvtmp(nages));
  SBtmp  += Ntmp(nages)*p_mature(nages)*wttmp(nages)*pow(srvtmp(nages),yrfrac);
  RETURN_ARRAYS_DECREMENT();
  return(SBtmp/phizero);

FUNCTION dvariable spr_ratio(dvariable trial_F)
  RETURN_ARRAYS_INCREMENT();
  dvariable SBtmp;
  dvar_vector Ntmp(1,nages);
  dvar_vector srvtmp(1,nages);
  SBtmp.initialize();
  Ntmp.initialize();
  srvtmp.initialize();
  dvar_vector Ftmp(1,nages);
  dvar_vector wttmp = wt_ssb(endyr_r);
  Ftmp = sel_fut*trial_F;
  srvtmp  = exp(-(Ftmp + natmort) );
  Ntmp(1)=1.;
  j=1;
  SBtmp  += Ntmp(j)*p_mature(j)*wttmp(j)*pow(srvtmp(j),yrfrac);
  for (j=2;j<nages;j++)
  {
    Ntmp(j) = Ntmp(j-1)*srvtmp(j-1);
    SBtmp  += Ntmp(j)*p_mature(j)*wttmp(j)*pow(srvtmp(j),yrfrac);
  }
  Ntmp(nages)=Ntmp(nages-1)*srvtmp(nages-1)/(1.-srvtmp(nages));

  SBtmp  += Ntmp(nages)*p_mature(nages)*wttmp(nages)*pow(srvtmp(nages),yrfrac);
  RETURN_ARRAYS_DECREMENT();
  return(SBtmp/phizero);

FUNCTION dvariable spr_unfished()
  RETURN_ARRAYS_INCREMENT();
  dvariable Ntmp;
  dvariable SBtmp;
  dvar_vector wttmp = wt_ssb(endyr_r);
  SBtmp.initialize();
  Ntmp = 1.;
  for (j=1;j<nages;j++)
  {
    SBtmp += Ntmp*p_mature(j)*wttmp(j)*exp(-yrfrac * natmort(j));
    Ntmp  *= exp( -natmort(j));
  }
  Ntmp    /= (1.-exp(-natmort(nages)));
  SBtmp += Ntmp*p_mature(nages)*wttmp(nages)*exp(-yrfrac * natmort(nages) );

  RETURN_ARRAYS_DECREMENT();
  return(SBtmp);

FUNCTION Get_Catch_at_Age
  q_bts  = mfexp(log_q_bts);
  q_eit  = mfexp(log_q_eit);
  q_cpue = mfexp(log_q_cpue);
  q_avo  = mfexp(log_q_avo);

  // Define this to get to survey time of year.... 
  catage = elem_prod( elem_prod(natage , F ) , elem_div( (1. - S) , Z ));
  for (i=styr; i<=endyr_r; i++)
    pred_catch(i)  = catage(i) * wt_fsh(i);

 //Fishery expected values------------------------
  for (i=1;i<=n_fsh_r;i++)
  {
    iyr       = yrs_fsh_data(i);
    et_fsh(i) = sum(catage(iyr));
    if (use_age_err)
      eac_fsh(i) = age_err * catage(iyr)/et_fsh(i); 
    else 
      eac_fsh(i) =           catage(iyr)/et_fsh(i); 
  }
  // only do this for 3+ predicted fish...
  elc_fsh = elem_prod(selages,catage(endyr_r))/sum(catage(endyr_r)(3,nages)) * age_len;

 //CPUE predicted values..
    for (i=1;i<=n_cpue;i++)
    {
      iyr          = yrs_cpue(i);
      pred_cpue(i) = elem_prod(wt_fsh(iyr), natage(iyr) ) * sel_fsh(iyr) * q_cpue; 
    }
 //avo predicted values..
    for (i=1;i<=n_avo_r;i++)
    {
      iyr          = yrs_avo(i);
      pred_avo(i)  = elem_prod(wt_avo(i), natage(iyr) ) * mfexp(log_sel_eit(iyr)) * q_avo; 
      // pred_avo(i)  = wt_fsh(iyr) * elem_prod(natage(iyr)  , avo_sel) * q_avo; 
      // pred_avo(i)  = wt_fsh(iyr) * natage(iyr)  * q_avo; 
    }
   
   
  //Trawl survey expected values------------------------
  dvar_vector ntmp(1,nages); 
  dvariable qtmp;
  dvariable qtmp_early;
  for ( i=1;i<=n_bts_r;i++)
  {
    iyr          = yrs_bts_data(i);
    qtmp         = bt_slope * bottom_temp(i) + q_bts ;
    // For reduced survey strata years
    if (active(log_q_std_area)&&(iyr<1985||iyr==1986)) 
      qtmp *= exp(log_q_std_area); 

    ntmp(1,nages)= elem_prod(natage(iyr),pow(S(iyr),.5));

    if (use_age_err)
      eac_bts(i)  = age_err * elem_prod(ntmp,mfexp(log_sel_bts(iyr))) * qtmp; // Eq. 15
    else 
      eac_bts(i)  =           elem_prod(ntmp,mfexp(log_sel_bts(iyr))) * qtmp; 
    et_bts(i)   = sum(eac_bts(i)(mina_bts,nages)); 
    eb_bts(i)   = wt_bts(i) * eac_bts(i); 


    eac_bts(i) /= sum(eac_bts(i)); 
  }
 
 //Hydro survey expected values------------------------
  for (i=1;i<=n_eit_ac_r;i++)
  {
    iyr          = yrs_eit_data(i);
    ntmp(1,nages)= elem_prod(natage(iyr),pow(S(iyr),.5));
    if (use_age_err)
      eac_eit(i)  = age_err * elem_prod(ntmp,mfexp(log_sel_eit(iyr))) * q_eit; // Eq. 15
    else
      eac_eit(i)  =           elem_prod(ntmp,mfexp(log_sel_eit(iyr))) * q_eit; 

    ea1_eit(i)  = ntmp(1); // NOTE that this is independent of selectivity function...
    eb_eit(i)   = wt_eit(i) * eac_eit(i); 
    et_eit(i)   = sum(eac_eit(i)(mina_eit,nages)); 
    eac_eit(i)(mina_eit,nages) /= et_eit(i);
  }

  // This is to deal with et_eit being greater than the age comps
  for (i=n_eit_ac_r;i<=n_eit_r;i++)
  {
    iyr          = yrs_eit_data(i);
    ntmp(1,nages)= elem_prod(natage(iyr),pow(S(iyr),.5));
    et_eit(i)    = sum( elem_prod(ntmp,mfexp(log_sel_eit(iyr)))(mina_eit,nages) ) * q_eit; 
  }
  // Experimental (not implemented nor used) for combining both surveys
  // if (Do_Combined && current_phase()>3) get_combined_index();

FUNCTION get_combined_index
  {
    int i_eit=2; // NOTE Start at 2nd year of data (ignores 1979 survey for combined run)
    for ( i=1;i<=n_bts_r;i++)
    {
      dvar_vector bts_tmp(mina_bts,nages);
      dvariable Ng_bts;
      dvariable Ng_eit;
      iyr     = yrs_bts_data(i);
      bts_tmp = ot_bts(i)*oac_bts(i)(mina_bts,nages);
      Ng_bts  = bts_tmp *(1./mfexp(log_sel_bts(iyr)(mina_bts,nages)))/q_bts ;
      if (yrs_eit_data(i_eit) == yrs_bts_data(i))
      {
        dvar_vector eit_tmp(mina_bts,nages);
        eit_tmp = ot_eit(i_eit)*oac_eit(i_eit)(mina_bts,nages);
        Ng_eit = eit_tmp *(1./mfexp(log_sel_eit(iyr)(mina_bts,nages)))/q_eit ; 
        ot_cmb(i) = 0.5*(Ng_eit + Ng_bts);
        var_cmb(i)  = .25*square(std_bts(i)     * Ng_bts/sum(bts_tmp));
        var_cmb(i) += .25*square(std_eit(i_eit) * Ng_eit/sum(eit_tmp));
        avail_eit(i) = q_eit * mfexp(log_sel_eit(iyr)(mina_bts,nages)) * oac_eit(i_eit)(mina_bts,nages);
        //Increment eit year counter
        i_eit++;
      }
      else
      {
        ot_cmb(i) = Ng_bts;
        var_cmb(i)  = square(std_bts(i) * Ng_bts/sum(bts_tmp));
      }
      avail_bts(i) = q_bts * mfexp(log_sel_bts(iyr)(mina_bts,nages)) * oac_bts(i)(mina_bts,nages);
      et_cmb(i)    = natage(iyr)(mina_bts,nages)*pow(S(iyr)(mina_bts,nages),.5);
    }
  }

FUNCTION get_msy
 // NOTE THis will need to be conditional on SrType too
 /*Function calculates used in calculating MSY and MSYL for a designated component of the
  population, given values for stock recruitment and selectivity...  Fmsy is the trial value of MSY example of the use of "funnel" to reduce the amount of storage for derivative calculations */
  dvariable Fdmsy;
  dvariable Stmp;
  dvariable Rtmp;
  dvariable Btmp;
  double df=1.e-6;
  dvariable F1=.3;
  dvariable F2;
  dvariable F3;
  dvariable yld1;
  dvariable yld2;
  dvariable yld3;
  dvariable dyld;
  dvariable dyldp;
  // Newton Raphson stuff to go here //cout <<endl<<endl<<"Iter  F  Stock  1Deriv  Yld  2Deriv"<<endl; //for (int ii=1;ii<=500;ii++)
  for (int ii=1;ii<=8;ii++)
  {
    if (mceval_phase()&&(F1>5||F1<0.01)) 
    {
      ii=5;
      count_Ffail++;
      cout<<F1<<" Bombed at  "<<count_mcmc<<" "<<count_Ffail<<" ";
      F1=F35; // When things bomb (F <0 or F really big then just set it to F35...)
    }
    else
    {
      F2     = F1 + df*.5;
      F3     = F2 - df; 
      yld1   = get_yield(F1,Stmp,Rtmp,Btmp); 
      yld2   = get_yield(F2,Stmp,Rtmp,Btmp);
      yld3   = get_yield(F3,Stmp,Rtmp,Btmp);
      dyld   = (yld2 - yld3)/df;                          // First derivative (to find the root of this)
      dyldp  = (yld2 + yld3 - 2.*yld1)/(.25*df*df);   // Second derivative (for Newton Raphson)
      F1    -= dyld/dyldp;
    }
  }
  Fdmsy    = F1;
  Fmsy     = Fdmsy;
  SPR_OFL  = spr_ratio(Fmsy,sel_fut);
  lnFmsy   = log(Fmsy);
  SER_Fmsy = get_SER(Fmsy);
  MSY      = get_yield(Fmsy,Stmp,Rtmp,Btmp);
  avg_age_msy  = get_avg_age(Fmsy,Stmp,Rtmp,Btmp);
  Bmsy     = Stmp;
  Bmsy2    = Btmp; // Fishable      
  Fmsy2    = MSY/Btmp; 
  lnFmsy2  = log(Fmsy2);
  Fendyr_Fmsy = mean( F(endyr_r) ) / Fmsy;
  Rmsy     = Rtmp;
//+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+ 
FUNCTION dvariable get_yield(dvariable& Ftmp, dvariable& Stmp,dvariable& Rtmp,dvariable& Btmp)
  RETURN_ARRAYS_INCREMENT();
  // Note that two wt vectors are used: 1 for yield, the other for biomass.  
  dvariable yield;
  dvariable phi;
  phi.initialize();
  dvariable Req;
  dvar_vector Ntmp(1,nages);
  dvar_vector Ctmp(1,nages);
  dvar_vector wttmp   = wt_ssb(endyr_r);
  dvar_vector Fatmp   = Ftmp * sel_fut;
  dvar_vector Ztmp    = Fatmp+ natmort;
  dvar_vector survtmp = mfexp(-Ztmp);
  Ntmp(1) = 1.;
  for ( j=1 ; j < nages; j++ )
    Ntmp(j+1)  =   Ntmp(j) * survtmp(j); // Begin numbers in the next year/age class
  Ntmp(nages)  /= (1.- survtmp(nages)); 
  for ( j=1 ; j <= nages; j++ )
    Ctmp(j)     = Ntmp(j) * Fatmp(j) * (1. - survtmp(j)) / Ztmp(j);
  yield  = wt_fut * elem_prod(Fmoney,Ctmp); 
  phi    = elem_prod( elem_prod( Ntmp , pow(survtmp,yrfrac) ), p_mature ) * wttmp; 
  Req    = Requil(phi); 
  yield *= Req;
  Stmp   = phi*Req;
  Btmp   = Req *  elem_prod(Ntmp,sel_fut) * wttmp; // Fishable biomass, Eq. 23
  Rtmp   = Req;   
  RETURN_ARRAYS_DECREMENT();
  return yield;
//+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+ 

//+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+ 
FUNCTION dvariable get_avg_age(dvariable& Ftmp, dvariable& Stmp,dvariable& Rtmp,dvariable& Btmp)
  RETURN_ARRAYS_INCREMENT();
  // Note that two wt vectors are used: 1 for yield, the other for biomass.  
  dvariable avgage;
  dvariable phi;
  phi.initialize();
  dvariable Req;
  dvar_vector Ntmp(1,nages);
  dvar_vector Ctmp(1,nages);
  dvar_vector wttmp   = wt_ssb(endyr_r);
  dvar_vector Fatmp   = Ftmp * sel_fut;
  dvar_vector Ztmp    = Fatmp+ natmort;
  dvar_vector survtmp = mfexp(-Ztmp);
  Ntmp(1) = 1.;
  for ( j=1 ; j < nages; j++ )
    Ntmp(j+1)  =   Ntmp(j) * survtmp(j); // Begin numbers in the next year/age class
  Ntmp(nages)  /= (1.- survtmp(nages)); 
  for ( j=1 ; j <= nages; j++ )
    Ctmp(j)     = Ntmp(j) * Fatmp(j) * (1. - survtmp(j)) / Ztmp(j);
  phi    = elem_prod( elem_prod( Ntmp , pow(survtmp,yrfrac) ), p_mature ) * wttmp; 
  Req    = Requil(phi); 
  Ctmp  *= Req;
  avgwt_msy = (wt_fut * Ctmp)/sum(Ctmp); 
  // avgln_msy = (lens*(age_len*Ctmp))/sum(Ctmp); 
  avgage    = (age_vector * Ctmp)/sum(Ctmp); 
  Stmp   = phi*Req;
  Btmp   = Req *  elem_prod(Ntmp,sel_fut) * wttmp; // Fishable biomass, Eq. 23
  Rtmp   = Req;   
  RETURN_ARRAYS_DECREMENT();
  return avgage;
//+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+==+ 


FUNCTION dvariable SRecruit(_CONST dvariable& Stmp)
  RETURN_ARRAYS_INCREMENT();
  dvariable RecTmp;
  switch (SrType)
  {
    case 1: // Eq. 12
      RecTmp = (Stmp / phizero) * mfexp( alpha * ( 1. - Stmp / Bzero )) ; //Ricker form from Dorn
      break;
    case 2:
      RecTmp = Stmp / ( alpha + beta * Stmp);        //Beverton-Holt form
      break;
    case 3:
      RecTmp = mfexp(log_avgrec);                    //Avg recruitment
      break;
    case 4:
      RecTmp = Stmp * mfexp( alpha  - Stmp * beta) ; //old Ricker form
      break;
  }
  RETURN_ARRAYS_DECREMENT();
  return RecTmp;


FUNCTION dvar_vector SRecruit(_CONST dvar_vector& Stmp)
  RETURN_ARRAYS_INCREMENT();
  dvar_vector RecTmp(Stmp.indexmin(),Stmp.indexmax());
  switch (SrType)
  {
    case 1: // Eq. 12
      RecTmp = elem_prod((Stmp / phizero) , mfexp( alpha * ( 1. - Stmp / Bzero ))) ; //Ricker form from Dorn
      break;
    case 2:
      RecTmp = elem_prod(Stmp , 1. / ( alpha + beta * Stmp)); //Beverton-Holt form
      break;
    case 3:
      RecTmp = mfexp(log_avgrec);                              //Avg recruitment
      break;
    case 4:
      RecTmp = elem_prod(Stmp ,mfexp( alpha - Stmp * beta));  //old Ricker form
      break;
  }
  RETURN_ARRAYS_DECREMENT();
  //cout <<RecTmp<<endl;
  return RecTmp;

FUNCTION dvariable Requil(_CONST dvariable& phi)
  RETURN_ARRAYS_INCREMENT();
  dvariable RecTmp;
  switch (SrType)
  {
    case 1: // Eq. 12
      RecTmp =  Bzero * (alpha + log(phi) - log(phizero) ) / (alpha*phi);
      break;
    case 2:
      RecTmp =  (phi-alpha)/(beta*phi);
      break;
    case 3:
      RecTmp =  mfexp(log_avgrec);
      break;
    case 4:
      RecTmp =  (log(phi)+alpha) / (beta*phi); //RecTmp =  (log(phi)/alpha + 1.)*beta/phi;
      break;
  }
  RETURN_ARRAYS_DECREMENT();
  return RecTmp;

 //=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
FUNCTION Get_Bzero
  Bzero.initialize();
  Rzero    =  mfexp(log_Rzero); 
  dvar_vector Ntmp(1,nages);
  dvar_vector survtmp = mfexp(-natmort);
  Ntmp.initialize();

  Ntmp(1) = Rzero;
  for ( j=1 ; j < nages; j++ )
  {
    Ntmp(j+1)  =   Ntmp(j) * survtmp(j); // Begin numbers in the next year/age class
  }
  Ntmp(nages)  /= (1.- survtmp(nages)); 
  for ( j=1 ; j <= nages; j++ )
    Ntmp(j) *= mfexp(yrfrac*log(survtmp(j)));
    // Ntmp(j) *= pow(survtmp(j),yrfrac);

  Bzero = elem_prod(wt_ssb(endyr_r) , p_mature) * Ntmp ; // p_mature is of Females (half of adults)
  phizero = Bzero/Rzero;

  switch (SrType)
  {
    case 1:
      alpha = log(-4.*steepness/(steepness-1.)); // Eq. 13
      break;
    case 2:
      alpha  =  Bzero * (1. - (steepness - 0.2) / (0.8*steepness) ) / Rzero;
      beta   = (5. * steepness - 1.) / (4. * steepness * Rzero);
      break;
    case 4:
    //R = S * EXP(alpha - beta * S))
      beta  = log(5.*steepness)/(0.8*Bzero) ;
      alpha = log(Rzero/Bzero)+beta*Bzero;
      break;
  }

FUNCTION Recruitment_Likelihood
  sigmaRsq = sigr*sigr;
  rec_like.initialize();

  if (active(log_rec_devs));
    rec_like(2) =  1.*norm2(log_rec_devs);

  rec_like(4) =  .1*norm2(log_initdevs);

  // Tune recruits to spawners via functional form of Srec (to estimate srec params) RAM's exp. value form of -ln like
  //if (current_phase()<4)
  if (phase_sr<0) 
  {
    sigmarsq_out    = norm2(log_rec_devs)/size_count(log_rec_devs);
    // No stock-rec relationship------------------------
    rec_like(1) = norm2( log_rec_devs(styr_est,endyr_est )) / (2.*sigmaRsq) + 
                  size_count(pred_rec(styr_est,endyr_est))*log(sigr);
  }
  else
  {
    sigmarsq_out    = norm2(log_rec_devs(styr_est,endyr_est))/size_count(log_rec_devs(styr_est,endyr_est));

    // SRR estimated for a specified window of years
    for (i=styr_est;i<=endyr_est;i++)
    {
      srmod_rec(i) = SRecruit(SSB(i-1)); // 1 year lag w/ SSB
    }
    SR_resids = log(pred_rec(styr_est,endyr_est)+1.e-8) - log(srmod_rec + 1.e-8)  ;

    // Flag to ignore the impact of the 1978 YC on S-Rec estimation...
    if (ctrl_flag(25)<1)
    {
      rec_like(1) = 0.5 * norm2( SR_resids + sigmaRsq/2. ) / sigmaRsq + (endyr_est-styr_est+1)*log(sigr);
    }
    else
    {
      for (i=styr_est;i<=endyr_est;i++)
      {
      if (i!=1979)
        rec_like(1) += 0.5 * square(SR_resids(i) + sigmaRsq/2.)/sigmaRsq + log(sigr);
      }
    }
  }

 // This sets variability of future recruitment to same as in past....
  if (active(rec_dev_future)) rec_like(5) = norm2(rec_dev_future)/(2.*sigmarsq_out+.001);
  if (ctrl_flag(29) > 0)
    rec_like(5) += 10.*norm2(log_rec_devs(endyr_est,endyr_r))/(2.*sigmarsq_out+.001);// WILL BREAK ON RETROSPECTIVE

  // Larval drift contribution to recruitment prediction (not used in recent years) Eq. 8
  if (active(larv_rec_devs))
    rec_like(3) += ctrl_flag(23)*norm2(larv_rec_devs);
  if (ctrl_flag(27)>0)
  {
    larv_rec_trans=trans(larv_rec_devs);
    // Do third differencing on spatial aspect...
    for (i=1;i<=11;i++)
    {
      rec_like(6) += ctrl_flag(27)*norm2(first_difference( first_difference(first_difference( larv_rec_devs(i)))));
      rec_like(6) += ctrl_flag(27)*norm2(first_difference( first_difference(first_difference(larv_rec_trans(i)))));
    }
  }

 // +===+====+==+==+==+==+==+==+==+====+====+==+==+===+====+==+==+==+==+==+==+==+====+====+====+
FUNCTION Evaluate_Objective_Function 
  fff.initialize();
  // For logistic fishery selectivity option (sensitivity)
  if (active(sel_dif2_fsh)) 
  {
    fff += .01 * sel_dif2_fsh*sel_dif2_fsh ;
  }
  Recruitment_Likelihood();
  Surv_Likelihood();  //-survey Deviations
  Selectivity_Likelihood();  
  
  ssqcatch = norm2(log(obs_catch(styr,endyr_r)+1e-4)-log(pred_catch+1e-4));

  if (current_phase() >= robust_phase)
    Robust_Likelihood();       //-Robust AGE  Likelihood part
  else  
    Multinomial_Likelihood();  //-Multinomial AGE  Likelihood part

  fff += ctrl_flag(1) * ssqcatch;
  fff += ctrl_flag(2) * sum(surv_like);
  fff += ctrl_flag(12) * cpue_like;
  fff += ctrl_flag(6) * avo_like;
  fff += ctrl_flag(3) * sum(rec_like);
  F_pen = norm2(log_F_devs);
  fff += ctrl_flag(4) * F_pen;

  fff+= ctrl_flag(7)*age_like(1);
  fff+= ctrl_flag(8)*age_like(2);
  fff+= ctrl_flag(9)*age_like(3);

  if (use_endyr_len>0)
    fff+= ctrl_flag(7)*len_like;

  fff+= sum(sel_like);
  fff+= sum(sel_like_dev);

  // Condition model in early phases to stay reasonable
  if (current_phase()<3)
  {
      fff += 10.*square(log(mean(Fmort)/.2));
      fff += 10.*square(log_avginit-log_avgrec)  ; //This is to make the initial comp not stray too far 
  }

  Priors.initialize();
  // Prior on combined-survey catchability, idea is to have sum of two surveys tend to the prior distribution
  q_all.initialize();
  dvariable q_bts_tmp;
  q_bts_tmp.initialize();
  dvariable q_eit_tmp;
  q_eit_tmp.initialize();

  for (i=1;i<=n_bts_r;i++)
  {
    iyr = yrs_bts_data(i);
    // Note this is to correct for reduced survey strata coverage pre 1985 and in 86
    if (!(iyr<1985||iyr==1986)) 
    {
      q_bts_tmp += sum(mfexp(log_q_bts + log_sel_bts(iyr)(q_amin,q_amax)));
    }
  }
  q_bts_tmp /= ((q_amax-q_amin+1)*(n_bts_r-4)) ;
  for ( i=1;i<=n_eit_r;i++)
  {
    iyr = yrs_eit_data(i);
    q_eit_tmp += sum(mfexp(log_q_eit + log_sel_eit(iyr)(q_amin,q_amax)));
  }
  q_eit_tmp /= ((q_amax-q_amin+1)*n_eit_r) ;
  q_all= log(q_bts_tmp + q_eit_tmp)     ;

  // Note: optional ability to put a prior on "combined" surveys overall catchability
  if (q_all_sigma<1.)
    Priors(2) = square( q_all- q_all_prior )/(2.*q_all_sigma*q_all_sigma); 
  q_all = exp(q_all);
  
  // Prior on BTS catchability
  if (active(log_q_bts)&&q_bts_sigma<1.)
  {
    Priors(2) = square( log_q_bts - q_bts_prior )/(2.*q_bts_sigma*q_bts_sigma); 
    // cout<<Priors(2)<<" "<<log_q_bts<<endl;
  }
  // Beta prior on steepness....
  if (active(steepness)&&cvsteepnessprior<1.)
  {
    if (SrType==4) // Old Ricker prior (1999)
      Priors(1) = log(steepness*cvsteepnessprior) + square( log( steepness/steepnessprior ) )/(2.*cvsteepnessprior*cvsteepnessprior); 
    else
    {
      // Note that the prior for steepness has already been mapped to interval 0.2, 1.0
      Priors(1) = -((srprior_a-1.)*log(steepness) + (srprior_b-1)*log(1.-steepness)); 

      // Under development (use of F_SPR as a prior)
      if (use_spr_msy_pen&&last_phase())
      {
        get_msy();
        // cout<<"SPR: "<<SPR_OFL<<endl; cout<<"Fmsy "<<Fmsy   <<endl; 
        Priors(1) = lambda_spr_msy*square(log(SPR_OFL)-log(.35));
      }
    }
  }

  fff += sum(Priors);
  // Conditional bits
  fff += 10.*square(avgsel_fsh);
  fff += 10.*square(avgsel_bts);
  fff += 10.*square(avgsel_eit);

  dvariable ssq_wt;
  ssq_wt.initialize();

  if (active(wt_fut))
    for (j=1;j<=nages;j++)
    {
      dvariable res = wt_mn(j)-wt_fut(j);
      fff += res*res/ (2.*wt_sigma(j)*wt_sigma(j));
    }

 // +===+====+==+==+==+==+==+==+==+====+====+==+==+===+====+==+==+==+==+==+==+==+====+====+====+
  
FUNCTION Selectivity_Likelihood
  sel_like.initialize();

  for (i=styr;i<= endyr_r;i++) //--This is for limiting the dome-shapedness FISHERY
    for (j=1;j<=n_selages_fsh;j++)
      if (log_sel_fsh(i,j)>log_sel_fsh(i,j+1))
        sel_like(1)+=ctrl_flag(13)*square(log_sel_fsh(i,j)-log_sel_fsh(i,j+1));

  if (active(sel_coffs_bts))
  {
    for (i=styr_bts;i<= endyr_r;i++) //--This is for controlling the selectivity shape BOTTOM TRAWL SURVEY
      for (j=6;j<=n_selages_bts;j++)
        if (log_sel_bts(i,j)>log_sel_bts(i,j+1))
          sel_like(2)+=ctrl_flag(14)*square(log_sel_bts(i,j)-log_sel_bts(i,j+1));
  }
  
  for (i=styr_eit;i<= endyr_r;i++) //--This is for selectivity shape HYDROA TRAWL SURVEY
    for (j=mina_eit;j<=n_selages_eit;j++)
      if (log_sel_eit(i,j)<log_sel_eit(i,j+1))
        sel_like(3) += ctrl_flag(15) * square(log_sel_eit(i,j)-log_sel_eit(i,j+1));

  //////////////////////////////////////////////////////////////////////////////////
  //--If time changes turned on then do 2nd differencing 
  //  for curvature penalty on subsequent years, otherwise only first year matters
  sel_like_dev.initialize();
  if (active(sel_coffs_fsh))
  {
    if (active(sel_devs_fsh))
    {
      sel_like_dev(1)+=ctrl_flag(10)/group_num_fsh*norm2(sel_devs_fsh);
      sel_like_dev(1)+=ctrl_flag(11)/dim_sel_fsh*norm2(first_difference( first_difference(log_sel_fsh(styr))));
      for (i=styr;i<endyr_r;i++)
        if (!(i+sel_dev_shift)%group_num_fsh)
          sel_like_dev(1)+=ctrl_flag(11)/dim_sel_fsh*norm2(first_difference( first_difference(log_sel_fsh(i+1))));
    }
    else
      sel_like_dev(1)+=ctrl_flag(11)*norm2(first_difference( first_difference(log_sel_fsh(styr))));
  }
  if (active(sel_a501_fsh_dev))
  {
    sel_like_dev(1) +=  5.0*norm2(first_difference(sel_a501_fsh_dev)); 
    sel_like_dev(1) += 25.0*norm2(first_difference(sel_trm2_fsh_dev)); 
    sel_like_dev(1) += 12.5*norm2(first_difference(sel_dif1_fsh_dev)); 
  }

  
  //////////////////////////////////////////////////////////////////////////////////
  if (active(sel_coffs_bts))
  {
    if (active(sel_devs_bts))
    {
      sel_like_dev(2) += ctrl_flag(20)/group_num_bts*norm2(sel_devs_bts);
      sel_like_dev(2) += ctrl_flag(21)/dim_sel_bts * norm2(first_difference( first_difference(log_sel_bts(styr))));
      for (i=styr;i<endyr_r;i++)
        if (!(i%group_num_bts))
          sel_like_dev(2)+=ctrl_flag(21)/dim_sel_bts*norm2(first_difference( first_difference(log_sel_bts(i+1))));
    }
    else
      sel_like_dev(2)+=ctrl_flag(21)*norm2(first_difference( first_difference(log_sel_bts(styr))));
  }
  if (active(sel_a50_bts))
    sel_like_dev(2) += 12.5*square(log(sel_a50_bts/4.5)); 
  if (active(sel_a50_bts_dev))
  {
    sel_like_dev(2) += 12.5*norm2(first_difference(sel_a50_bts_dev)); 
    sel_like_dev(2) += 25.0*norm2(first_difference(sel_slp_bts_dev)); 
    if (active(sel_one_bts_dev))
      sel_like_dev(2) += 8.*norm2(first_difference(sel_one_bts_dev)); // 25% CV on this
      // sel_like_dev(2) += 3.125*norm2(first_difference(sel_one_bts_dev)); // 40% CV on this
  }
 
  //////////////////////////////////////////////////////////////////////////////////
  dvar_vector like_tmp(1,4);
  like_tmp.initialize();

  like_tmp(1)  = ctrl_flag(22) * norm2(first_difference( first_difference(log_sel_eit(styr))));
  if (active(sel_devs_eit))
  {
    for (i=1;i<=nch_eit;i++)
    {

      like_tmp(1) += ctrl_flag(22) * norm2(first_difference( 
                                           first_difference(log_sel_eit(yrs_ch_eit(i)))));

      like_tmp(2) += norm2(log_sel_eit(yrs_ch_eit(i)-1) - log_sel_eit(yrs_ch_eit(i))) / 
                         (2*sel_ch_sig_eit(i) * sel_ch_sig_eit(i));
    }
  }
  sel_like_dev(3)  = sum(like_tmp);

FUNCTION Surv_Likelihood
 //-Likelihood due to Survey Numbers-------------------
  surv_like.initialize();
  // survey index
  if (Do_Combined && current_phase()>3)
  {
    for (i=1;i<=n_bts_r;i++)
    {
      // Under development--not used (yet).  Idea to combine surveys for directly accounting for differences
      //  of water column densities...
      surv_like(1) += square(ot_cmb(i)-et_cmb(i))/(2.*var_cmb(i));
      // slight penalty here to get the q to scale correctly (note--development, not used)
      surv_like(1) += .01*square(ot_bts(i)-et_bts(i))/(2.*var_bts(i));
    }
    // slight penalty here to get the q to scale correctly
    for (i=1;i<=n_eit_r;i++)
      surv_like(2) += .01*square(log(ot_eit(i)+.01)-log(et_eit(i)+.01))/ (2.*lvar_eit(i)) ;
  }
  else
  {
    // This is used (standard approach) Eq. 19,  historically used normal distribution since year-by-year var_bts avail
    for (i=1;i<=n_bts_r;i++)
      surv_like(1) += square(ot_bts(i)-et_bts(i))/(2.*var_bts(i));

    for (i=1;i<=n_eit_r;i++) // Eq. 19
      surv_like(2) += square(log(ot_eit(i)+.01)-log(et_eit(i)+.01))/ (2.*lvar_eit(i)) ;
  }

  if (use_age1_eit) 
  {
    // Compute q for this age1 index...
    dvariable qtmp = mfexp(mean(log(oa1_eit)-log(ea1_eit)));
    if (ignore_last_eit_age1)
      surv_like(3) = 0.5*norm2(log(oa1_eit(1,n_eit_r-1)+.01)-log(ea1_eit(1,n_eit_r-1)*qtmp +.01))/(age1_sigma_eit*age1_sigma_eit) ; 
    else
      surv_like(3) = 0.5*norm2(log(oa1_eit+.01)-log(ea1_eit*qtmp +.01))/(age1_sigma_eit*age1_sigma_eit) ; 
  }
  avo_like.initialize();
  cpue_like.initialize();

  dvar_vector cpue_dev = obs_cpue-pred_cpue;
  for (i=1;i<=n_cpue;i++)
    cpue_like += square(cpue_dev(i))/(2.*obs_cpue_var(i));

  dvar_vector avo_dev = obs_avo-pred_avo;
  for (i=1;i<=n_avo_r;i++)
    avo_like += square(avo_dev(i))/(2.*obs_avo_var(i));


FUNCTION Robust_Likelihood
  age_like.initialize();
  len_like.initialize();
  // dvariable robust_p(_CONST dmatrix& obs,_CONST dvar_matrix& pred,_CONST dvariable& a,_CONST dvariable& b)
  dvariable  rf=.1/nages;
  age_like(1) = robust_p(oac_fsh,eac_fsh,rf,sam_fsh);
  age_like(2) = robust_p(oac_bts,eac_bts,rf,sam_bts);

  for (i=1;i<=n_eit_r;i++)
    surv_like(2) += square(log(ot_eit(i)+.01)-log(et_eit(i)+.01))/ (2.*lvar_eit(i)) ;

  if (current_phase() >= eit_robust_phase ) // eit robustness phase big number means do multinomial, not robust
    age_like(3) = robust_p(oac_eit,eac_eit,rf,sam_eit,mina_eit,nages);
  else // Multinomial for EIT
    for (i=1; i <= nagecomp(3); i++) 
      age_like(3) -= sam_eit(i)*oac_eit(i)(mina_eit,nages)*log(eac_eit(i)(mina_eit,nages) + MN_const);
  len_like    = robust_p(olc_fsh,elc_fsh,rf,50);

FUNCTION Multinomial_Likelihood
  age_like.initialize();
  len_like.initialize();
//-Likelihood due to Age compositions--------------------------------
  for (int igear =1;igear<=ngears;igear++)
  {
    for (i=1; i <= nagecomp(igear); i++)
    {
      switch (igear)
      {
        case 1:
          age_like(igear) -= sam_fsh(i)*oac_fsh(i)*log(eac_fsh(i) + MN_const);
          break;
        case 2:
          age_like(igear) -= sam_bts(i)*oac_bts(i)*log(eac_bts(i) + MN_const);
          break;
        default:
          age_like(igear) -= sam_eit(i)*oac_eit(i)(mina_eit,nages)*log(eac_eit(i)(mina_eit,nages) +MN_const);
          break;
      }
    }     
    age_like(igear)-=age_like_offset(igear);
  }
  len_like = sam_fsh(n_fsh_r)*olc_fsh*log(elc_fsh+MN_const);

  // this one allows a concentrated range of ages (last two args are min and max age range)
FUNCTION dvariable robust_p(dmatrix& obs,dvar_matrix& pred,_CONST dvariable& a, _CONST data_ivector& b, _CONST int amin, _CONST int amax)
    if (obs.indexmin() != pred.indexmin() || obs.indexmax() != pred.indexmax() )
      cerr << "Index limits on observed vector are not equal to the Index\n"
        "limits on the predicted vector in robust_p function\n";
    RETURN_ARRAYS_INCREMENT(); //Need this statement because the function returns a variable type
    dvar_matrix v(obs.indexmin(),obs.indexmax(),amin,amax);
    // dvar_matrix l  =  elem_div(square(pred - obs), v);
    dvariable log_likelihood = 0.;
    for (i=obs.indexmin();i<= obs.indexmax() ;i++) 
    {
      v(i) = a  + 2. * elem_prod(obs(i)(amin,amax) ,1.  - obs(i)(amin,amax));
      dvar_vector l  =  elem_div(square(pred(i)(amin,amax) - obs(i)(amin,amax)), v(i));
      log_likelihood -=  sum(log(mfexp(-1.* double(b(i)) * l) + .01));  
    }
    log_likelihood  += 0.5 * sum(log(v));
    RETURN_ARRAYS_DECREMENT(); // Need this to decrement the stack increment
    return(log_likelihood);

FUNCTION dvariable robust_p(_CONST dmatrix& obs,_CONST dvar_matrix& pred,_CONST dvariable& a, _CONST data_ivector& b)
    RETURN_ARRAYS_INCREMENT(); //Need this statement because the function returns a variable type
    if (obs.indexmin() != pred.indexmin() || obs.indexmax() != pred.indexmax() )
      cerr << "Index limits on observed vector are not equal to the Index\n"
        "limits on the predicted vector in robust_p function\n";
    // dvar_matrix v = a  + 2. * elem_prod(pred ,1.  - pred );
    dvar_matrix v = a  + 2. * elem_prod(obs ,1.  - obs );
    dvar_matrix l  =  elem_div(square(pred - obs), v);
    dvariable log_likelihood = 0.;
    for (i=obs.indexmin();i<= obs.indexmax() ;i++) 
    {
      log_likelihood -=  sum(log(mfexp(-1.* double(b(i)) * l(i)) + .01));  
    }
    log_likelihood  += 0.5 * sum(log(v));
    RETURN_ARRAYS_DECREMENT(); // Need this to decrement the stack increment
    return(log_likelihood);

FUNCTION dvariable robust_p(_CONST dmatrix& obs, _CONST dvar_matrix& pred, _CONST dvariable& a, _CONST dvariable& b)
    RETURN_ARRAYS_INCREMENT(); //Need this statement because the function
    if (obs.indexmin() != pred.indexmin() || obs.indexmax() != pred.indexmax() )
      cerr << "Index limits on observed vector are not equal to the Index\n"
        "limits on the predicted vector in robust_p function\n";
             //returns a variable type
  
    // dvar_matrix v = a  + 2. * elem_prod(pred ,1.  - pred );
    dvar_matrix v = a  + 2. * elem_prod(obs ,1.  - obs );
    dvar_matrix l  = mfexp(- b * elem_div(square(pred - obs), v ));
    dvariable log_likelihood = -1.0*sum(log(l + .01));  
    log_likelihood  += 0.5 * sum(log(v));
    RETURN_ARRAYS_DECREMENT(); // Need this to decrement the stack increment
    return(log_likelihood);

FUNCTION dvariable robust_p(_CONST dvector& obs, _CONST dvar_vector& pred, _CONST dvariable& a, _CONST dvariable& b)
    RETURN_ARRAYS_INCREMENT(); //Need this statement because the function
    if (obs.indexmin() != pred.indexmin() || obs.indexmax() != pred.indexmax() )
      cerr << "Index limits on observed vector are not equal to the Index\n"
        "limits on the predicted vector in robust_p function\n";
             //returns a variable type
  
    // dvar_matrix v = a  + 2. * elem_prod(pred ,1.  - pred );
    dvar_vector v = a  + 2. * elem_prod(obs ,1.  - obs );
    dvar_vector l  = mfexp(- b * elem_div(square(pred - obs), v ));
    dvariable log_likelihood = -1.0*sum(log(l + .01));  
    log_likelihood  += 0.5 * sum(log(v));
    RETURN_ARRAYS_DECREMENT(); // Need this to decrement the stack increment
    return(log_likelihood);

FUNCTION write_srec
  srecout << "Yearclass SSB Recruit Pred_Rec Residual"<<endl;
  for (i=styr+1;i<=endyr_r;i++)
  {
    dvariable tmpr=SRecruit(SSB(i-1)) ; 
    srecout << i-1               <<" "
            << SSB(i-1)          <<" "
            << tmpr              <<" "
            << pred_rec(i)       <<" "
            << log(pred_rec(i)) 
               - log(tmpr)       <<" "
            <<endl;
  }
FUNCTION write_eval
  count_mcmc++;
  // only use every 100th chain...
  // if (count_mcmc % 10 == 0)
  {
    count_mcsave++;
    if(iseed>0)
      SimulateData1();
    else
    {
      // if (!pflag) 
      for (int k=1;k<=nscen;k++)
      {
        write_mceval(future_SSB(k));
        write_mceval(catch_future(k));
      }
      write_mceval(Fcur_Fmsy);
      write_mceval(Bcur_Bmsy);
      write_mceval(Bcur_Bmean);
      write_mceval(Bcur2_Bmsy);
      write_mceval(Bcur2_B20);
      write_mceval(Bcur3_Bcur);
      write_mceval(Bcur3_Bmean);
      write_mceval(LTA1_5R);
      write_mceval(LTA1_5);
      write_mceval(MatAgeDiv1);
      write_mceval(MatAgeDiv2);
      write_mceval(RelEffort);
      write_mceval <<endl;
      // eval<< "Obj_Fun steep q AvgRec SER_endyr SSBendyr_B40 1989_YC 1992_YC 1996_YC 2000YC MSYR Bmsy3+ Fmsy F35 SER_Fmsy SER_Endyr SBF40 Bcur_Bmsy Cur_Sp F40Catch Steepness Q CC1_1 CC1_2 CC1_3 CC2_1 CC2_2 CC2_3"<<endl;
      // eval <<" Future ssb"<<endl;
      // pflag=1;
      // for (j=4;j<=5;j++) eval<< future_SSB(j) << " "; eval<<endl;
      // eval << Bmsy<<" "<< future_SSB(4)<<" "<<future_SSB(5)<<" "<<future_SSB(6) <<endl;
      // eval<<"SPR: "<<SPR_OFL<<" Fmsy "<<Fmsy   <<" "<< catch_future(3,endyr+1)    <<endl;
    }
  }

FUNCTION write_nofish
// function to write out results if there had been no fishing...
  dvar_vector a3p_nofsh(1978,endyr_r);
  dvar_vector SSB_nofsh(1977,endyr_r);
  dvariable adj_age1;
  adj_age1.initialize();
  a3p_nofsh.initialize();
  SSB_nofsh.initialize();

  log_initage=log_initdevs+log_avginit;
  natage(styr)(2,nages)=mfexp(log_initage);
    natage(styr,1) = mfexp(log_avgrec+rec_epsilons(styr));
  // Recruitment in subsequent years
  for (i=styr;i<=endyr_r;i++)
  {
    S(i)=mfexp(-1.*natmort);
    natage(i,1) = mfexp(log_avgrec+rec_epsilons(i));
  }
  
  SSB_nofsh(1978)         = elem_prod(elem_prod(natage(1978),pow(S(1978),yrfrac)),p_mature)*wt_ssb(1978);
  natage(1978+1)(2,nages) = ++elem_prod(natage(1978)(1,nages-1), S(1978)(1,nages-1));  
  natage(1978+1,nages)   += natage(1978,nages)*S(1978,nages);

  SSB_nofsh(1977)         = elem_prod(elem_prod(natage(1977),pow(S(1977),yrfrac)),p_mature)*wt_ssb(1977);
  for (i=1978;i<endyr_r;i++)
  {
    // S-R impact on recruitment (estimated_R/R_had_new) Eq. 14
    SSB_nofsh(i)         = elem_prod(elem_prod(natage(i),pow(S(i),yrfrac)),p_mature)*wt_ssb(i);
    adj_age1             = SRecruit(SSB_nofsh(i-1))/SRecruit(SSB(i-1));
    natage(i,1)         *= adj_age1;

    natage(i+1)(2,nages) = ++elem_prod(natage(i)(1,nages-1), S(i)(1,nages-1));  
    natage(i+1,nages)   += natage(i,nages)*S(i,nages);
  }
  SSB_nofsh(endyr_r)  = elem_prod(elem_prod(natage(endyr_r),pow(S(endyr_r),yrfrac)),p_mature)*wt_ssb(endyr_r);
  for (i=1978;i<=endyr_r;i++)
  {
    a3p_nofsh(i)  = natage(i)(3,nages)*wt_ssb(i)(3,nages); //cout<<a3p_nofsh(i)<<endl; cout<<natage(i)(3,nages)<<endl<< wt_ssb(i)(3,nages)<<endl; exit(1);
    // nofish<<i<<" "<<SSB_nofsh(i)<<" "<<" "<<SSB_nofsh_nosr(i)<<" "<<a3p_nofsh(i)<<" "<<a3p_nofsh(i)<<endl;
    nofish<<i<<" "<<SSB_nofsh(i)<<" "<<a3p_nofsh(i)<<" "<<endl;
  }
  nofish<<endl<<"N_Endyr "<<natage(endyr_r)<<endl;
  nofish<<endl<<"wt_Fut_fsh  "<<wt_fut     <<endl;
  nofish<<endl<<"wt_Fut_ssb  "<<wt_ssb(endyr_r) <<endl;
  nofish<<endl<<elem_prod(natage(endyr_r),wt_ssb(endyr_r)     )<<endl;
  B_Bnofsh = SSB(endyr_r)/SSB_nofsh(endyr_r);

FUNCTION write_projout2
// Function to write out data file for projection model....just an output for alternative projections
// Species part....
  /*
  Name
  Number  of  fsheries                                          
  Is_it_Const_popln?
  2000 Catch
  reserved
  TAC Split
  ABC Adjustment
  SpwnMonth
  NumAges
  Fratio  (must  sum  to  one)                                      
  M
  Maturity
  SpawnWtatAge
  FemWtatAge
  MaleWtatAge
  FemSelectivity
  MaleSelectivity
  Female NatAge
  Male Natage
  NumRecs
  Recs
  Spawning biomass
  */
 projout2 <<"EBS_Pollock"<<endl;
 projout2 <<"# Number of fsheries"<<endl<<"1" <<endl;;
 projout2 <<"# Const Popln?" <<endl  <<"0"     <<endl;;
 projout2 <<"# Recent TAC"   <<endl  <<obs_catch(endyr_r)  <<endl;;
 projout2 <<"# reserved "    <<endl  <<"0"     <<endl;;
 projout2 <<"# TAC Split "   <<endl  <<"1 1 "  <<endl;;
 projout2 <<"# ABC Adjustment "<<endl<<"1"     <<endl;;
 projout2 <<"# Spawnmonth "  <<endl    <<"4"   <<endl;;
 projout2 <<"# NumberAges "  <<endl    <<nages <<endl;;
 projout2 << " 1  # Fratio                  "  <<endl;
 projout2 << natmort<< " # Natural Mortality " <<endl;
 projout2 <<"# Maturity"<<endl<< p_mature/max(p_mature)<< endl;
 projout2 <<"# Wt spawn"<<endl<< wt_ssb(endyr_r)     << endl;
 projout2 <<"# Wt fsh"<<endl<< wt_fut     << endl;
 projout2 <<"# Wt fsh"<<endl<< wt_fut     << endl;
 projout2 <<"# selectivity"<<endl<< sel_fut << endl;
 projout2 <<"# selectivity"<<endl<< sel_fut << endl;
 projout2 <<"# natage"<<endl<< natage(endyr_r) << endl;
 projout2 <<"# natage"<<endl<< natage(endyr_r) << endl;
 projout2 <<"# Nrec"<<endl<< endyr_r-1978+1<< endl;
 projout2 <<"# rec"<<endl<< pred_rec(1978,endyr_r) << endl;
 projout2 <<"# SpawningBiomass"<<endl<< SSB(1978-1,endyr_r-1) << endl;
 
// Function to write out data file for projection model....just an output for alternative projections
FUNCTION write_newproj
 ofstream newproj("newproj.prj");
// Function to write out data file for new Ianelli 2005 projection model....
 newproj <<"#Ebspollock:"<<endl<<model_name<<endl;
 newproj <<"#SSL Species?"<<endl;
 newproj <<"1"<<endl;
 newproj <<"#Constant buffer of Dorn?"<<endl;
 newproj <<"0"<<endl;
 newproj <<"#Number of fisheries?"<<endl;
 newproj <<"1"<<endl;
 newproj <<"#Number of sexes?"<<endl;
 newproj <<"1"<<endl;
 newproj <<"#5year_Average_F(endyr-4,endyr_as_estimated_by_ADmodel)"<<endl;
  sel_fut.initialize(); 
  for (i=1;i<=5;i++)
    sel_fut += F(endyr_r-i-1);
  sel_fut /= 5.;
 newproj << "# "<< mean(Fmort(endyr_r-4,endyr_r))<<endl;
 newproj << max(sel_fut) << endl;
 newproj << "# Sel_fut " << sel_fut<<endl;
 sel_fut /= max(sel_fut);
 newproj << "# F at last yr catch and fut select: " << SolveF2(natage(endyr_r),obs_catch(endyr_r))<<endl;
 newproj << "# F at last yr catch and fut select: " << SolveF2(natage(endyr_r-1),obs_catch(endyr_r-1))<<endl;
 newproj << "# F at last yr catch and fut select: " << SolveF2(natage(endyr_r-2),obs_catch(endyr_r-2))<<endl;
 newproj << "# F at last yr catch and fut select: " << SolveF2(natage(endyr_r-3),obs_catch(endyr_r-3))<<endl;
 newproj << "# F at last yr catch and fut select: " << SolveF2(natage(endyr_r-4),obs_catch(endyr_r-4))<<endl;
 newproj << "# Sel_fut " << sel_fut<<endl;
 newproj <<"#_Author_F_as_fraction_F_40%"<<endl;
 newproj <<"1"<<endl;
 newproj <<"#ABC SPR" <<endl;
 newproj <<"0.4"<<endl;
 newproj <<"#MSY SPR" <<endl;
 newproj <<"0.35"<<endl;
 newproj <<"#_Spawn_month"<<endl;
 newproj << yrfrac*12+1<<endl;
 newproj <<"#_Number_of_ages"<<endl;
 newproj <<nages<<endl;
 newproj <<"#_F_ratio(must_sum_to_one_only_one_fishery)"<<endl;
 newproj <<"1"<<endl;
 newproj <<"#_Natural_Mortality" << endl;
 newproj << natmort << endl; // for (j=1;j<=nages;j++) newproj <<natmort<<" "; newproj<<endl;
 newproj <<"#_Maturity_divided_by_2(projection_program_uses_to_get_female_spawning_biomass_if_divide_by_2"<<endl<<2*p_mature<< endl;
 newproj <<"#_Wt_at_age_spawners"<<endl<<wt_ssb(endyr_r)     << endl;
 newproj <<"#_Wt_at_age_fishery" <<endl<<wt_fut      << endl;
 newproj <<"#" <<endl;

 newproj <<"#_Selectivity_fishery_scaled_to_max_at_one"<<endl;
 newproj << sel_fut/max(sel_fut)<<endl;
 newproj <<"#_Numbers_at_age_end_year"<<endl<<natage(endyr_r)<< endl;
 newproj <<"#_N_recruitment_years (including last 3 estimates)"<<endl<<endyr-(1977+recage) << endl;
 newproj <<"#_Recruitment_start_at_1977_yearclass=1978_for_age_1_recruits"<<endl
         <<pred_rec(1977+recage,endyr_r-1)<< endl;
 newproj <<"#_Spawning biomass "<<endl<<SSB(1977,endyr_r-recage-1)<< endl;
  // SSB_nofsh(endyr_r)  = elem_prod(elem_prod(natage(endyr_r),pow(S(endyr_r),yrfrac)),p_mature)*wt_fut     ;
 newproj.close();

// Function to write out data file for projection model....just an output for alternative projections
FUNCTION write_projout
// Function to write out data file for projection model....
 projout << "EBS_Pollock_"<<endyr_r<<endl;
 projout <<"1    # SSLn species..."<<endl;
 projout <<"0    # Buffer of Dorn"<<endl;
 projout <<"1    # Number of fsheries"<<endl;
 projout <<"1    # Number of sexes"<<endl;
 dvariable sumFtmp;
 sumFtmp=0.;
 for (i = endyr_r-4;i<=endyr_r;i++) sumFtmp += F(i,6); 
 sumFtmp /= 5.;
 projout << sumFtmp << "  # averagei 5yr f                  " <<endl;
 // projout << mean(Fmort(endyr_r-4,endyr_r))<<"  # averagei 5yr f                  " <<endl;
 projout << " 1  # author f                  " <<endl;
 projout <<" 0.4  # ABC SPR        "<<endl;
 projout <<" 0.35 # MSY/OFL SPR    "<<endl;
 projout << " 4  # Spawnmo                   " <<endl;
 projout <<nages<< " # Number of ages" <<endl;
 projout << " 1  # Fratio                  " <<endl;
 projout <<natmort<< " # Natural Mortality       " <<endl;
 projout <<"# Maturity"<<endl<< p_mature<< endl;
 projout <<"# Wt spawn"<<endl<< wt_ssb(endyr_r)     << endl;
 projout <<"# Wt fsh"<<endl<< wt_fut     << endl;
 projout <<"# selectivity"<<endl<< sel_fut << endl;
 projout <<"# natage"<<endl<< natage(endyr_r) << endl;
 projout <<"# Nrec"<<endl<< endyr_r-1978<< endl;
 projout <<"# rec"<<endl<< pred_rec(1978,endyr_r) << endl;
 projout <<"# SpawningBiomass"<<endl<< SSB(1978-1,endyr_r-1) << endl;

  
REPORT_SECTION
  dvariable qtmp = mfexp(mean(log(oa1_eit)-log(ea1_eit)));
  cout << endl<<"Changing phases: "<<current_phase()<<" ==============================================="<<endl<<endl;
  report << model_name<<" "<< datafile_name<<" "<<q_bts<<" "<<q_eit<<" "<<q_bts*exp(log_q_std_area)<< " "<<q_all<<" "<<qtmp<<" "<<sigr<<" q's and sigmaR"<<endl;
  report << "Estimated Catch and Observed" <<endl;
  report << pred_catch <<endl;
  report << obs_catch <<endl;
  report << "Estimated Survival at age" <<endl;
  for (i=styr;i<=endyr_r;i++) report << i<<" "<<S(i) <<endl;
  report << "Estimated N at age" <<endl;
  for (i=styr;i<=endyr_r;i++) report << i<<" "<<natage(i) <<endl;
  report << "selectivity Fishery, trawl survey, and hydro survey" <<endl;
  for (i=styr;i<=endyr_r;i++) report << i<<" "<<sel_fsh(i) <<endl;
                              report << "Future "<<sel_fut <<endl;
  for (i=styr;i<=endyr_r;i++) report << i<<" "<<mfexp(log_sel_bts(i)) <<endl;
  if (use_age1_eit)
    for (i=styr;i<=endyr_r;i++) report << i<<" 0 "<<mfexp(log_sel_eit(i)(mina_eit,nages)) <<endl;
  else
    for (i=styr;i<=endyr_r;i++) report << i<<" "<<mfexp(log_sel_eit(i)) <<endl;

  report << "Fishery observed P at age" <<endl;
  for (i=1;i<=n_fsh_r;i++) report << yrs_fsh_data(i)<<" "<<oac_fsh(i) <<endl;
  report << "Fishery observed P at size" <<endl;
  report << endyr          <<" "<<olc_fsh    <<endl;

  report << "Fishery Predicted P at age" <<endl;
  for (i=1;i<=n_fsh_r;i++) report << yrs_fsh_data(i)<<" "<<eac_fsh(i) <<endl;
  report << "Fishery Predicted P at size" <<endl;
                           report << endyr_r        <<" "<<elc_fsh    <<endl;

  report << "Survey Observed P at age"<<endl;
  for (i=1;i<=n_bts_r;i++) report << yrs_bts_data(i)<<" "<<oac_bts(i) <<endl;
  report << "Survey Predicted P at age"<<endl;
  for (i=1;i<=n_bts_r;i++) report << yrs_bts_data(i)<<" "<<eac_bts(i) <<endl;

  report << "Hydro Survey Observed P at age"<<endl;
  if (use_age1_eit) 
    for (i=1;i<=n_eit_r;i++) report << yrs_eit_data(i)<<" 0 "<<oac_eit(i)(mina_eit,nages) <<endl;
  else
    for (i=1;i<=n_eit_r;i++) report << yrs_eit_data(i)<<" "<<oac_eit(i) <<endl;

  if (use_last_eit_ac<=0) 
  {
    n_eit_ac_r = n_eit_r-1; 
    iyr          = yrs_eit_data(n_eit_r);
    report<<  (elem_prod(natage(iyr),mfexp(log_sel_eit(iyr))) * q_eit)/et_eit(n_eit_r)<<endl; 
  }

  report << "Hydro Survey Predicted P at age"<<endl;
  if (use_age1_eit) 
    for (i=1;i<=n_eit_r;i++) report << yrs_eit_data(i)<<" 0 "<<eac_eit(i)(mina_eit,nages) <<endl;
  else
    for (i=1;i<=n_eit_r;i++) report << yrs_eit_data(i)<<" "<<eac_eit(i) <<endl;

  if (use_last_eit_ac<=0) 
  {
    report<<  oac_eit_data(n_eit_r)/sum(oac_eit_data(n_eit_r))  <<endl;
  }
  report << "Pred. Survey numbers " <<endl;                 
  report << et_bts(1,n_bts_r)  <<" "<<endl;
  report << "Obs. Survey numbers " <<endl;
  report << ot_bts(1,n_bts_r)  <<" "<<endl;
  report << "Pred. hydro Survey numbers " <<endl;
  report << et_eit(1,n_eit_r) <<endl;
  report << "Obs. hydro Survey numbers " <<endl;   
  report << ot_eit(1,n_eit_r) <<endl;
  report << "Yrs. AVO biomass " <<endl;
  report << yrs_avo(1,n_avo) <<endl;
  report << "Pred. AVO biomass " <<endl;
  report << pred_avo(1,n_avo) <<endl;
  report << "Obs. AVO biomass biomass " <<endl;   
  report << obs_avo(1,n_avo) <<endl<<endl;

  report << "Obs. Japanese CPUE " <<endl;   
  report << obs_cpue(1,n_cpue) <<endl;
  report << "Pred Japanese CPUE " <<endl;   
  report << pred_cpue(1,n_cpue) <<endl<<endl;

  report << "Fmort " <<endl;
  report << Fmort  <<endl;
  report << "Natural Mortality" << endl;
  report << natmort  <<endl;
  report << "Catch at age year" << endl;
  for (i=styr;i<=endyr_r;i++) report << i<<" "<<catage(i)<<endl;
  report << "available biomass by year" << endl;
  for (i=styr;i<=endyr_r;i++)
  {
    dvar_vector real_sel=sel_fsh(i)/max(sel_fsh(i));
    dvar_vector avbio= elem_prod(elem_prod(natage(i),real_sel),wt_fsh(i));
    report << i<<" "<<sum(avbio) << "  " <<  avbio  << endl;
  }
  report << endl;
  report << "Catch_and_indices-----------------------------"<<endl;
  report << "Fishery_Catch  "
         << ctrl_flag(1) * ssqcatch      << endl;
  report << "CPUE_like "
         << ctrl_flag(12) * cpue_like    << endl;
  report << "Bottom_Trawl_Like "
         << ctrl_flag(2) * surv_like(1)  << endl;
  report << "EIT_N2+_Like "
         << ctrl_flag(2) * surv_like(2)  << endl;
  report << "EIT_N1_Like "
         << ctrl_flag(2) * surv_like(3)  << endl<<endl;
  report << "AVO_Biom_Like "
         << ctrl_flag(6) * avo_like      << endl<<endl;

  report << "AgeComps--------------------------------------"<<endl;
  report << "Fishery_age_Like "
         << ctrl_flag(7) * age_like(1) << endl;
  report << "Fishery_Length_Like "
         << ctrl_flag(7) * len_like    << endl;
  report << "Bottom_Trawl_age_Like "
         << ctrl_flag(8) * age_like(2) << endl;
  report << "EIT_Age_Like "
         << ctrl_flag(9) * age_like(3) << endl<<endl;

  report << "Priors ---------------------------------------"<<endl;
  report << "F_penalty " 
         << ctrl_flag(4) * F_pen       << endl;
  report << "Rec_Like_1 "   
         << ctrl_flag(3) * rec_like(1) << endl;
  report << "Rec_Like_2 "
         << ctrl_flag(3) * rec_like(2) << endl;
  report << "Rec_Like_3 "
         << ctrl_flag(3) * rec_like(3) << endl;
  report << "Rec_Like_4 "
         << ctrl_flag(3) * rec_like(4) << endl;
  report << "Rec_Like_5 "
         << ctrl_flag(3) * rec_like(5) << endl;
  report << "Rec_Like_6 "
         << ctrl_flag(3) * rec_like(6) << endl;

  report << "sel_Like_1 "
         << sel_like(1) << endl;
  report << "sel_Like_2 "
         << sel_like(2) << endl;
  report << "sel_Like_3 "
         << sel_like(3) << endl;

  report << "sel_Like_devs_1 "
         << sel_like_dev(1) << endl;
  report << "sel_Like_devs_2 "
         << sel_like_dev(2) << endl;
  report << "sel_Like_devs_3 "
         << sel_like_dev(3) << endl;

  report << "sel_avg_fishery "
      << 10.*square(avgsel_fsh)<<endl;
  report << "sel_avg_BTS     "
      << 10.*square(avgsel_bts)<<endl;
  report << "sel_avg_EIT "
      << 10.*square(avgsel_eit)<<endl;

  report << "Prior_h "
         << Priors(1) <<" "<<srprior_a<<" "<<srprior_b<<" Alpha_Beta_of_Prior "<<endl;
  report << "Prior_q "
         << Priors(2) <<endl<<endl;

  report << "Totals----------------------------------------"<<endl;
  report << "Without_prior " << fff - (sum(rec_like)+sum(sel_like)+sum(sel_like_dev)+
                                       sum(Priors)) <<endl;
  report << "With_Priors "<< fff<<endl<<endl;

  report << "Spawning_Biomass  "<<endl;
  report << SSB  <<endl;
  report << "larv_rec_devs"<<endl;
  report << larv_rec_devs<<endl;
  report << " Spawners and Rhat for plotting" <<endl;
  report << fake_SSB<<endl;
  report << rechat<<endl;
  for (i=1;i<=11;i++)
    for (j=1;j<=11;j++)
      report << 1.66679*(double(j)-6.) / 2. -164.4876 <<" "<< 0.62164*(double(i)-6.) / 2. + 56.1942 <<" "<< exp(larv_rec_devs(i,j)) <<endl;
  report << rec_epsilons-log_rec_devs<<endl;
  // Report on bottom temperature affect on survey q
  report << endl<<"Year"<<" Temperature q mean"<< endl;
  for (i=1;i<=n_bts_r;i++)
    report << yrs_bts_data(i)<<" "<< bottom_temp(i)<<" "<< bt_slope * bottom_temp(i) + q_bts<<" "<<q_bts <<endl;
  report<<endl;

  if(last_phase() )
  {

    // cout<<"Report"<<endl;
    get_msy();
    // cout<<"Report"<<endl;
    get_SER();
    cout<<"Last phase in report section"<<endl;
    report << "F40  F35"  <<endl;
    report << F40<<" "<<F35 <<endl;
    report << "Future_Selectivity"<<endl;
    report << sel_fut<<endl;

    Future_projections_fixed_F();
    report << "Future Fmsy " <<endl;
    for (i=styr_fut;i<=endyr_fut;i++) 
      report << mean(F_future(3,i))<<" ";
    report << endl;
    report << "Numbers at age in Future (for Fmsy)"<<endl;
    report << natage_future(3) <<endl;
    report <<"Fmsy, MSY, Steepness, Rzero, Bzero, PhiZero, Alpha, Beta, SPB0, SPRBF40, Fmsy2, Bmsy2"<<endl;
    report << Fmsy<<" "<<MSY<<" "<<steepness<<" "<<Rzero<<" "<<Bzero<<" "<<phizero<<" "<<alpha<<" "<<beta<<" "<<phizero*meanrec
    <<" "<<phizero*meanrec*.4
    <<" "<<Fmsy2
    <<" "<<Bmsy2
    <<endl;
    // Save the ABC/Catch for next year
    ofstream NewCatch(adstring(model_name)+adstring("_catch.dat"));
    cout<< adstring(model_name)+adstring("_catch.dat")<<endl;
    // NOTE: unallocated among fleets...
    NewCatch << max(0.1,min(1500.,1.*min(1.15*obs_catch(endyr_r),value(catch_future(1,styr_fut)))) ) << endl;
    NewCatch << 1.15*obs_catch(endyr_r)<<" "<<value(catch_future(1,styr_fut)) << endl;
    NewCatch.close();
  }
  report<<"Num_parameters_Estimated "<<initial_params::nvarcalc()<<endl;
  report<<  SSB(styr_est-1,endyr_est-1)<<endl
        <<  pred_rec(styr_est,endyr_est) <<endl
        <<  (SRecruit(SSB(styr_est-1,endyr_est-1))) <<endl
        <<  log(pred_rec(styr_est,endyr_est))- ++log(SRecruit(SSB(styr_est-1,endyr_est-1)))<<endl;

  if (last_phase())
  {
    if(iseed>0)
      SimulateData1();
    cout<< "Estimated and SR-predicted recruits"<<endl;
    cout<< pred_rec(styr,endyr_r)<<endl;
    cout<< SRecruit(SSB(styr,endyr_r))<<endl;

    write_projout2();
    write_projout();
    // write_newproj();
    ofstream temp_q("temp_q.dat");
    for (i=1;i<=5;i++)
    {
      q_temp(i)         = bt_slope * double(i-3)*.15 + q_bts ;
      temp_q<< double(i-3)*.15 <<" "<< q_temp(i)         <<  endl;
    }
  }
  report << "Age_1_EIT index"<<endl;
  report << yrs_eit_data     <<endl;
  report << oa1_eit          <<endl;
  report << ea1_eit*qtmp     <<endl;
  report << ea1_eit          <<endl;

  report << "Obs_Pred_BTS_Biomass"<<endl;
  report << yrs_bts_data     <<endl;
  report << obs_biom_bts     <<endl;
  report << eb_bts     <<endl;

  report << "Obs_Pred_EIT_Biomass"<<endl;
  report << yrs_eit_data     <<endl;
  report << obs_eit     <<endl;
  report << eb_eit     <<endl;
  report << "Pred_EIT_N_age"<<endl;
  for (i=1;i<=n_eit_r;i++)
    report << yrs_eit_data(i)<<" "<<et_eit(i)*eac_eit(i)(2,15)     <<endl;
  report << "Stock-Rec_Residuals"<<endl;
  for (i=styr_est;i<=endyr_est;i++)
    report << i<<" "<<SR_resids(i)     <<endl;
  report << "Years Combined_Indices_CV Observed Predicted Avail_BTS Avail_EIT Likelihood"<<endl;
  /* 
  if (last_phase())
  {
    get_combined_index();
    for (i=1;i<=n_bts_r;i++)
      report << yrs_bts_data(i)<<" "<<pow(var_cmb(i),.5)/ot_cmb(i)<< " "<< ot_cmb(i)<<" "<<et_cmb(i) <<" " 
             << avail_bts(i) <<" "
             << avail_eit(i) <<" "
             << square(ot_cmb(i)-et_cmb(i))/(2.*var_cmb(i))
             << endl;
  } 
  */ 
  report << F<<endl;

FUNCTION SimulateData1
  cout <<"Doing mcsim: "<< count_mcsave<<" for iseed "<<iseed<<endl;// exit(1);
  dvector ran_age_vect(1,nages);
  ran_age_vect.fill_randn(rng);

 // Simulate 1-year ahead..............................................
  int k=5;
  dvar_matrix natage_futsim(styr_fut,endyr_fut,1,nages);
  natage_futsim.initialize();
  F_future.initialize();
  Z_future.initialize();
  S_future.initialize();

  natage_futsim(styr_fut,1)  = mfexp(log_avgrec + rec_dev_future(styr_fut));

 // If 2005 yc is above average (by 2x)...
  // natage(endyr_r) = elem_prod(YC_mult,natage(endyr_r));
  // multivariate normal
  // cout<<natage(endyr_r)<<endl;

  ofstream ssb("ssb.rep");
  dvector ntmp1(1,nages);
  ntmp1.initialize();

  // Stochastic version
  // begin-yr 2006 N
  // ntmp1 = N2006 + chol*ran_age_vect; // for (i=1;i<=nages;i++) ntmp1(i) = max(0.1,ntmp1(i));
  // non stochastic version
  // 2009 N (current year)
  ntmp1 = value(natage(endyr_r));
  ssb << elem_prod(elem_prod(ntmp1, pow(S(endyr_r),yrfrac)), p_mature) * wt_ssb(endyr_r)     <<" ";

  // begin-yr 2010 N
  natage_futsim(styr_fut)(2,nages)  = ++elem_prod(ntmp1(1,nages-1), S(endyr_r)(1,nages-1));  
  natage_futsim(styr_fut,nages)    += ntmp1(nages)*S(endyr_r,nages);

  ftmp = SolveF2(natage_futsim(styr_fut),next_yrs_catch);
  F_future(k,styr_fut) = sel_fut * ftmp;
  Z_future(styr_fut)   = F_future(k,styr_fut) + natmort;
  S_future(styr_fut)   = exp(-Z_future(styr_fut));
  dvariable  Xspawn1    = elem_prod(elem_prod(natage_futsim(styr_fut),pow(S_future(styr_fut),yrfrac)), p_mature) * wt_ssb(endyr_r)     ;
  ssb << Xspawn1<<" ";

  // begin-yr 2011 N
  i=styr_fut+1;
  natage_futsim(i)(2,nages)  = ++elem_prod(natage_futsim(styr_fut)(1,nages-1), S(endyr_r)(1,nages-1));  
  natage_futsim(i,nages)    += natage_futsim(styr_fut,nages)*S(endyr_r,nages);
  natage_futsim(i,1)  = mfexp(log_avgrec + rec_dev_future(i));
  // get morts
  ftmp = SolveF2(natage_futsim(i),next_yrs_catch);
  F_future(k,i) = sel_fut * ftmp;
  Z_future(i)   = F_future(k,i) + natmort;
  S_future(i)   = exp(-Z_future(i));
  ssb << elem_prod(elem_prod(natage_futsim(i), pow(S_future(i),yrfrac)), p_mature)*wt_ssb(endyr_r)     <<" " ;

  // begin-yr 2012 N
  i=styr_fut+2;
  natage_futsim(i)(2,nages)  = ++elem_prod(natage_futsim(styr_fut)(1,nages-1), S(endyr_r)(1,nages-1));  
  natage_futsim(i,nages)    += natage_futsim(styr_fut,nages)*S(endyr_r,nages);
  natage_futsim(i,1)  = mfexp(log_avgrec + rec_dev_future(i));

  // get morts
  ftmp = SolveF2(natage_futsim(i),next_yrs_catch);
  F_future(k,i) = sel_fut * ftmp;
  Z_future(i)   = F_future(k,i) + natmort;
  S_future(i)   = exp(-Z_future(i));
  ssb << elem_prod(elem_prod(natage_futsim(i), pow(S_future(i),yrfrac)), p_mature)*wt_ssb(endyr_r)     <<" " ;
  ssb <<endl; ssb.close();


 // Numbers at age for survey (note based on styr_fut)
 // 2010 survey timing N
  dvector ntmp         = value(elem_prod(natage_futsim(styr_fut),pow(S_future(styr_fut),.5)));
  dvector eac_fsh_fut(1,nages);
  dvector eac_bts_fut(1,nages);
  dvector eac_eit_fut(1,nages);
  eac_fsh_fut.initialize();
  eac_bts_fut.initialize();
  eac_eit_fut.initialize();

  // simply the last year's catch expectation (e.g., since 2007 data unavailable in 2007, 2006 for 2007 projection)
  eac_fsh_fut = value(catage(endyr_r)); 

  // BTS expectation for 2007 projection
  eac_bts_fut = value(elem_prod(ntmp,mfexp(log_sel_bts(endyr_r))) * q_bts); 

  // EIT expectation for 2007 projection
  eac_eit_fut = value(elem_prod(ntmp,mfexp(log_sel_eit(endyr_r))) * q_eit); 

  // Simulate data for next  year...
  // stochastic in surveys and fishery
  // not in wt-age
  char simno[33];
  int nsims;
  if (mceval_phase()) nsims=1; else nsims=100;
  for (int isim=1;isim<=nsims;isim++)
  {
    if (mceval_phase()) 
      simname = "sim_"+ adstring(itoa(count_mcsave,simno,10)) + ".dat";
    else 
      simname = "sim_"+ adstring(itoa(isim,simno,10)) + ".dat";
    cout<<"Simulate 1-year ahead data, iseed:  "<<simname<<" "<<iseed<<endl;
    ofstream simdat(simname);
    simdat<< styr             <<endl;
    simdat<< styr_bts         <<endl;
    simdat<< styr_eit         <<endl;
    simdat<< endyr+1          <<endl;
    simdat<< recage           <<endl;
    simdat<< nages            <<endl;
    simdat<< 2*p_mature       <<endl;
    simdat<< ewindex<<" 4 "   <<endl;
    simdat<< nsindex<<" 4 "   <<endl;
    simdat<< "# Wt fishey    "<<endl;
    simdat<< wt_fsh           <<endl;
    // Simulate next year's wt-age from stochsastic history? xxx
    simdat<< wt_fut           <<endl;
    simdat<< wt_ssb           <<endl;
    simdat<< wt_ssb(endyr)   <<endl;
  
    dvariable sigmasq;
    dvariable sigma;
    simdat << obs_catch <<" "
           << next_yrs_catch      <<endl;
  
    simdat << obs_effort       <<endl;
    simdat << obs_effort(endyr)<<endl;
  
    simdat << n_cpue          <<endl;
    simdat << yrs_cpue        <<endl;
    simdat << obs_cpue        <<endl;
    simdat << obs_cpue_std    <<endl;
    
    simdat << ngears          <<endl;
    simdat << minind          <<endl;
    // Add one year of fishery and survey years
    simdat << n_fsh+1         <<endl;
    simdat << n_bts+1         <<endl;
    // if(do_EIT1) 
    // this was to test for value of EIT survey...
    //if (Sim_status==1)
      simdat << n_eit+1       <<endl;
    // else
      // simdat << n_eit         <<endl;
  
    simdat << yrs_fsh_data    <<endl;
    simdat << yrs_fsh_data(n_fsh)+1   <<endl;
  
    simdat << yrs_bts_data             <<endl;
    simdat << yrs_bts_data(n_bts)+1    <<endl;
  
    simdat << yrs_eit_data             <<endl;
    // if(Sim_status==1) 
      simdat << yrs_eit_data(n_eit)+1    <<endl;// Nota
  
    simdat << sam_fsh         <<endl;
    simdat << sam_fsh(n_fsh)  <<endl;
  
    simdat << sam_bts         <<endl;
    simdat << sam_bts(n_bts)  <<endl;

    simdat << sam_eit         <<endl;
  // if(Sim_status==1) 
    simdat << sam_eit(n_eit)<<endl;

    // dvector ran_age_vect(1,nages);
    sigma   = 0.100;
    sigmasq = sigma*sigma;
    ran_age_vect.fill_randn(rng);
  
    simdat << "# Fishery simulate age compositions "    <<endl;
    simdat << oac_fsh                                 <<endl;
    simdat << exp(-sigmasq/2.) * elem_prod(mfexp(sigma * ran_age_vect) , eac_fsh_fut)<<endl;
  
    sigma   = 0.150;
    sigmasq = sigma*sigma;
    simdat << "# BTS Biomass estimates "                    <<endl;
    simdat << obs_bts_data<<" "<<eac_bts_fut * wt_bts(n_bts)<<endl;
    simdat << "# BTS Biomass std errors "                   <<endl;
    simdat << std_obs_bts_data 
    // 20% CV for new data
           << " "<< eac_bts_fut*wt_bts(n_bts)*.2            <<endl;
    simdat << "# BTS wt at age "                            <<endl;
    simdat <<   wt_bts << endl << wt_bts(n_bts)             <<endl;
    simdat << "# BTS Numbers std errors "                   <<endl;
    simdat << std_bts                                              
           << " "<< sum(eac_bts_fut)*.15                    <<endl;
  
    //matrix eac_bts(1,n_bts_r,1,nbins)  //--Expected proportion at age in trawl survey
    // Need to correct for lognormal bias ------------------------
    sigmasq=norm2(log(et_bts+.01)-log(ot_bts+.01))/size_count(et_bts);
    sigma=sqrt(sigmasq);
  
    ran_age_vect.fill_randn(rng);
    simdat << "# Bottom Trawl survey simulate age compositions " <<endl;
    simdat <<  oac_bts_data         <<endl;
    // Simulate next year's bt survey age compositions and totals....TODO
    dvector bts_tmp(1,nages);
    bts_tmp = value(mfexp(-sigmasq/2.) * elem_prod(mfexp(sigma * ran_age_vect) , eac_bts_fut));
    simdat << bts_tmp <<endl;
    // simdat <<  eac_bts_fut <<endl;
  
    // sigmasq=norm2(log(et_eit+.01)-log(ot_eit+.01))/size_count(et_eit);
    // sigma=sqrt(sigmasq);
    sigma   = 0.200;
    sigmasq = sigma*sigma;
    ran_age_vect.fill_randn(rng);
    dvector eit_tmp(1,nages);
    eit_tmp = value(mfexp(-sigmasq/2.) * elem_prod(mfexp(sigma * ran_age_vect) , eac_eit_fut));
  
    simdat << "# EIT Trawl survey stdevs for total N "<<endl;
    simdat <<   std_eit <<" "<< sum(eit_tmp)*.2<<endl;
    simdat << "# EIT Trawl survey simulate age compositions "<<endl;
    simdat <<  oac_eit_data         <<endl;
    // Simulate next year's EIT survey age compositions and totals....TODO
    simdat << eit_tmp <<endl;
  
    simdat << "# EIT Biomass estimates "                    <<endl;
    simdat << obs_eit_data<<" "<<eac_eit_fut * wt_eit(n_eit)<<endl;
    simdat << "# BTS Biomass std errors "                   <<endl;
    simdat << std_obs_eit_data<<" "<<std_obs_eit_data(n_eit)<<endl; 
    simdat << "# EIT wt at age "                            <<endl;
    simdat <<   wt_eit << endl << wt_eit(n_eit)             <<endl;
    simdat << "# BTS Bottom  temperatures (Habitat area) "  <<endl;
    simdat << bottom_temp<<" 0 "                            <<endl;
    simdat << "# Ageing error                            "  <<endl;
    simdat << age_err                                       <<endl;
  
    simdat << "# Expected length composition             "  <<endl;
    simdat << nlbins                                        <<endl;
    simdat << olc_fsh(1,nlbins)                             <<endl;
    simdat << age_len << endl;
    simdat << "1234567"                                     <<endl;                                                                                            
    simdat.close();
  }

 // Selectivity functions........................
FUNCTION dvar_matrix compute_selectivity(int stsel,_CONST dvariable& slp,_CONST dvariable& a50)
  // Single logistic, no trends....
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  for (i=stsel;i<=endyr_r;i++)
    log_sel(i)  =  -1.*log( 1.0 + mfexp(-slp * ( age_vector - a50 )  ))  ;
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);

FUNCTION dvar_matrix compute_selectivity(int stsel,_CONST dvariable& slp,_CONST dvariable& a50,_CONST dvar_vector& a50_dev)
  // Single logistic, trends in inflection....
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  for (i=stsel;i<=endyr_r;i++)
    log_sel(i)  =  -1.*log( 1.0 + mfexp(-slp * ( age_vector - a50*exp(a50_dev(i)) )  ))  ;
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);

FUNCTION dvar_matrix compute_selectivity(int stsel,_CONST dvariable& slp,_CONST dvariable& a50,_CONST dvar_vector& se,_CONST dvar_vector& ae)
  // Single logistic, trends in inflection and slope....
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  for (i=stsel;i<=endyr_r;i++)
    log_sel(i)  =  -1.*log( 1.0 + mfexp(-exp(se(i)) * slp * ( age_vector - a50*exp(ae(i)) )  ))  ;
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);

FUNCTION dvar_matrix compute_selectivity(int stsel,_CONST dvar_vector& slp,_CONST dvar_vector& a50)
  // double logistic, NO trends in inflection and slope....
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  dvariable slp1;
  dvariable inf1;
  dvariable slp2;
  dvariable inf2;
  for (i=stsel;i<=endyr_r;i++)
  {
    slp1        = mfexp(slp(1));
    inf1        = a50(1) ;
    slp2        = mfexp(slp(2));
    inf2        = a50(2) ;
    log_sel(i)  = -1.*log( 1.0 + mfexp(-slp1             * ( age_vector - inf1 ) )  - ( 1.0 - 1.0 / ( 1 + mfexp( -slp2 *( age_vector - inf2 ) ) )) );
    log_sel(i) -= max(log_sel(i));
  }
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);


FUNCTION dvar_matrix compute_selectivity1(int stsel,_CONST dvariable& dif,_CONST dvariable& a50,_CONST dvariable& trm)
  // slow_gistic, NO trends in ascending inflection, slope, and terminal Selectivity value....
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  dvariable inf1;
  dvariable slp1;
  dvariable slp2;
  dvariable dif1;
  dvariable   x1;
  dvariable trm1;
  dvariable intrcpt;
  for (i=stsel;i<=endyr_r;i++)
  {
    dif1        = mfexp(dif);
    slp1        = 2.9444389791664400/ dif1;
    inf1        = a50 ;
    trm1        = trm ;

    x1          =  dif/2 + inf1;
    slp2        = (0.95-trm)/(dif/2. + inf1 - nages);
    intrcpt     =  0.95-x1*slp2;

    int tt;
    tt = int(value(x1));
    for ( j = 1  ; j < tt ; j++ )
      log_sel(i,j)  = -1.*log( 1.0 + mfexp(-slp1 * ( double(j) - inf1 ) ))  ;
    for (j = tt  ;j<=nages; j++ )
      log_sel(i,j)  = log( intrcpt + slp2 * double(j) ) ;

    log_sel(i) -= max(log_sel(i));
  }
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);

FUNCTION dvar_matrix compute_selectivity1(int stsel,_CONST dvariable& dif,_CONST dvariable& a50,_CONST dvariable& trm,_CONST dvar_matrix& devs)
  // slow_gistic, trends in ascending inflection, slope, and terminal Selectivity value....
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  dvariable inf1;
  dvariable slp1;
  dvariable slp2;
  dvariable dif1;
  dvariable   x1;
  dvariable trm1;
  dvariable intrcpt;
  for (i=stsel;i<=endyr_r;i++)
  {
    dif1        = mfexp(devs(1,i) + dif);
    slp1        = 2.9444389791664400/ dif1;
    inf1        = mfexp(devs(2,i)) * a50 ;
    trm1        = mfexp(devs(3,i)) * trm ;

    x1          =  dif/2 + inf1;
    slp2        = (0.95-trm)/(dif/2. + inf1 - nages);
    intrcpt     =  0.95-x1*slp2;

    for (j=1;j<=nages;j++)
    {
      if (j < x1)
        log_sel(i,j)  = -1.*log( 1.0 + mfexp(-slp1 * ( double(j) - inf1 ) ) ) ;
      else
        log_sel(i,j)  = log( intrcpt + slp2 * double(j) ) ;
    }
    log_sel(i) -= max(log_sel(i));
  }
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);

FUNCTION dvar_matrix compute_selectivity2(int stsel,_CONST dvar_vector& slp,_CONST dvar_vector& a50,_CONST dvar_matrix& devs)
  // double logistic, trends in ascending (only) inflection and both slopes....
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  dvariable inf1;
  dvariable inf2;
  dvariable slp1;
  dvariable slp2;
  for (i=stsel;i<=endyr_r;i++)
  {
    slp1        = mfexp(devs(1,i)  + slp(1));
    inf1        = mfexp(devs(2,i)) * a50(1) ;
    slp2        = mfexp(devs(3,i)  + slp(2));
    inf2        = a50(2) ;
    log_sel(i)  = -1.*log( 1.0 + mfexp(-slp1 * ( age_vector - inf1 ) )  - ( 1.0 - 1.0 / ( 1 + mfexp( -slp2 *( age_vector - inf2 ) ) )) );
    log_sel(i) -= max(log_sel(i));
  }
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);

FUNCTION dvar_matrix compute_selectivity3(int stsel,_CONST dvar_vector& dif,_CONST dvar_vector& a50)
  // double logistic, Andre's parameterization, NO trends in inflection and slope....
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  dvariable dif1;
  dvariable inf1;
  dvariable dif2;
  dvariable inf2;
  for (i=stsel;i<=endyr_r;i++)
  {
    dif1        = 2.9444389791664400/( mfexp(dif(1)));
    inf1        = a50(1) ;
    dif2        = 2.9444389791664400/ (mfexp(dif(2)));
    inf2        = a50(2) ;
    log_sel(i)  = -1.*log( 1.0 + mfexp(-dif1 * ( age_vector - inf1 ) )  
                  -(1.0 - 1.0/(1.0 + mfexp( -dif2 *( age_vector - inf2 ) ) )) );

    log_sel(i) -= max(log_sel(i));
  }
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);
FUNCTION dvar_matrix compute_selectivity3(int stsel,_CONST dvar_vector& dif,_CONST dvar_vector& a50,_CONST dvar_matrix& devs)
  // double logistic, Andre's parameterization, trends in inflection and slope....
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  dvariable dif1;
  dvariable inf1;
  dvariable dif2;
  dvariable inf2;
  for (i=stsel;i<=endyr_r;i++)
  {
    dif1        = 2.9444389791664400/( mfexp(devs(1,i) + dif(1)));
    inf1        = mfexp(devs(2,i)) * a50(1) ;
    dif2        = 2.9444389791664400/ (mfexp(devs(3,i) + dif(2)));
    inf2        = mfexp(devs(4,i)) * a50(2) ;
    log_sel(i)  = -1.*log( 1.0 + mfexp(-dif1 * ( age_vector - inf1 ) )  
                  -(1.0 - 1.0 / ( 1 + mfexp( -dif2 *( age_vector - inf2 ) ) )) );
    log_sel(i) -= max(log_sel(i));
  }
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);

FUNCTION dvar_matrix compute_selectivity(int stsel,_CONST dvar_vector& slp,_CONST dvar_vector& a50,_CONST dvar_matrix& devs)
  // double logistic, trends in inflection and slope....
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  dvariable slp1;
  dvariable inf1;
  dvariable slp2;
  dvariable inf2;
  for (i=stsel;i<=endyr_r;i++)
  {
    slp1        = mfexp(devs(1,i)  + slp(1));
    inf1        = mfexp(devs(2,i)) * a50(1) ;
    slp2        = mfexp(devs(3,i)  + slp(2));
    inf2        = mfexp(devs(4,i)) * a50(2) ;
    log_sel(i)  = -1.*log( 1.0 + mfexp(-slp1 * ( age_vector - inf1 ) )  - ( 1.0 - 1.0 / ( 1 + mfexp( -slp2 *( age_vector - inf2 ) ) )) );
    log_sel(i) -= max(log_sel(i));
  }
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);

 // Coefficient selectivity forms from here down===================================================
FUNCTION dvar_matrix compute_fsh_selectivity(int nsel,int stsel,dvariable& avgsel,_CONST dvar_vector& coffs,_CONST dvar_matrix& sel_devs,int  gn)
  // Coefficient selectivity, with deviations...
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  avgsel  = log(mean(mfexp(coffs)));
  log_sel(stsel)(1,nsel)      = coffs;
  log_sel(stsel)(nsel+1,nages)      = coffs(nsel);
  int ii;
  log_sel(stsel)-=log(mean(exp(log_sel(stsel))));
  ii=1;
  for (i=stsel;i<endyr_r;i++)
  {
    if (!((i+sel_dev_shift)%gn)) // note that this makes the shift occurr in different years....
    {
      log_sel(i+1)(1,nsel)       = log_sel(i)(1,nsel) + sel_devs(ii); // Next year's selectivity has a deviation from this (the 3rd yr)
      log_sel(i+1)(nsel+1,nages) = log_sel(i+1,nsel);
      ii++;
    }
    else
      log_sel(i+1)=log_sel(i);

    log_sel(i+1)-=log(mean(exp(log_sel(i+1))));
  }
  // log_sel(endyr_r)=log_sel(endyr_r-1); // This avoids uncertainty in last age group
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);

FUNCTION dvar_matrix compute_selectivity_eit(int nsel,int stsel,dvariable& avgsel,_CONST dvar_vector& coffs)
  // Coefficient selectivity, withOUT deviations...
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  avgsel  = log(mean(mfexp(coffs)));
  log_sel(stsel)(mina_eit,nsel)     = coffs;
  log_sel(stsel)(nsel+1,nages)      = coffs(nsel);
  int ii;
  log_sel(stsel)-=log(mean(exp(log_sel(stsel))));
  for (i=stsel;i<endyr_r;i++)
    log_sel(i+1)=log_sel(i);
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);

FUNCTION dvar_matrix compute_selectivity_eit(int nsel,int stsel,dvariable& avgsel,_CONST dvar_vector& coffs,_CONST dvar_matrix& sel_devs)
    // log_sel_eit = compute_selectivity(n_selages_eit,styr_eit,avgsel_eit,sel_coffs_eit,sel_devs_eit);
  // Coefficient selectivity, with deviations...
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  avgsel  = log(mean(mfexp(coffs)));
  log_sel(stsel)(mina_eit,nsel)     = coffs;
  log_sel(stsel)(nsel+1,nages)      = coffs(nsel);
  int ii;
  log_sel(stsel)-=log(mean(exp(log_sel(stsel))));
  ii=1;
  for (i=stsel+1;i<=endyr_r;i++) // Starts in 1979
  {
    // if (i==yrs_eit_data(ii)&&ii<=dim_sel_eit)
    if (i==yrs_ch_eit(ii))
    {
      // log_sel(i+1)(mina_eit,nsel) = log_sel(i)(mina_eit,nsel) + sel_devs(ii);
      // log_sel(i+1)(nsel+1,nages)  = log_sel(i+1,nsel);
      log_sel(i)(mina_eit,nsel) = log_sel(i-1)(mina_eit,nsel) + sel_devs(ii);
      log_sel(i)(nsel+1,nages)  = log_sel(i,nsel);
      // cout<<yrs_eit_data(ii)<<endl;
      if(ii<dim_sel_eit)
        ii++;
    }
    else
      log_sel(i)=log_sel(i-1);
    log_sel(i)-=log(mean(exp(log_sel(i))));
  }
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);

FUNCTION dvar_matrix compute_selectivity(int nsel,int stsel,dvariable& avgsel,_CONST dvar_vector& coffs,_CONST dvar_matrix& sel_devs,int  gn)
  // Coefficient selectivity, with deviations...
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  avgsel  = log(mean(mfexp(coffs)));
  log_sel(stsel)(1,nsel)      = coffs;
  log_sel(stsel)(nsel+1,nages)      = coffs(nsel);
  int ii;
  log_sel(stsel)-=log(mean(exp(log_sel(stsel))));
  ii=1;
  for (i=stsel;i<endyr_r;i++)
  {
    if (!(i%gn))
    {
      log_sel(i+1)(1,nsel)       = log_sel(i)(1,nsel) + sel_devs(ii);
      log_sel(i+1)(nsel+1,nages) = log_sel(i+1,nsel);
      ii++;
    }
    else
      log_sel(i+1)=log_sel(i);

    log_sel(i+1)-=log(mean(exp(log_sel(i+1))));
  }
  RETURN_ARRAYS_DECREMENT();
  return(log_sel);

FUNCTION dvar_matrix compute_selectivity(int nsel,int stsel,dvariable& avgsel,_CONST dvar_vector& coffs)
  // Coefficient selectivity, without deviations...
  RETURN_ARRAYS_INCREMENT();
  dvar_matrix log_sel(styr,endyr_r,1,nages);
  log_sel.initialize();
  dvar_vector log_sel_tmp(1,nages);
  avgsel  = log(mean(mfexp(coffs )));
  log_sel_tmp(1,nsel)       = coffs;
  log_sel_tmp(nsel+1,nages) = coffs(nsel);
  log_sel_tmp                   -= log(mean(exp(log_sel_tmp)));
  for (i=stsel;i<=endyr_r;i++)
    log_sel(i) = log_sel_tmp;

  RETURN_ARRAYS_DECREMENT();
  return(log_sel);

FUNCTION dvariable SolveF2(_CONST dvar_vector& N_tmp, double  TACin)
  RETURN_ARRAYS_INCREMENT();
  dvariable dd = 10.;
  dvariable cc = TACin;
  dvariable btmp =  elem_prod(sel_fut,N_tmp) * wt_fut ;
  dvariable ftmp;
  ftmp = 1.2*TACin/btmp;

  dvar_vector Fatmp = ftmp * sel_fut;
  dvar_vector Z_tmp = Fatmp + natmort;
  dvar_vector S_tmp = exp(-Z_tmp);

  int icount;
  icount=0;
  for (icount=1;icount<=20;icount++)
  {
    ftmp += (TACin-cc) / btmp;
    Fatmp = ftmp * sel_fut;
    Z_tmp = Fatmp + natmort;
    S_tmp = mfexp( -Z_tmp );
    cc = (wt_fut * elem_prod(elem_div(Fatmp,  Z_tmp),elem_prod(1.-S_tmp,N_tmp))); // Catch equation (vectors)
    dd = cc / TACin - 1.;
    dd = sfabs(dd);
  }
  RETURN_ARRAYS_DECREMENT();
  return(ftmp);

FUNCTION Profile_F
 /* NOTE THis will need to be conditional on SrType too
  Function calculates used in calculating MSY and MSYL for a designated component of the
  population, given values for stock recruitment and selectivity...  Fmsy is the trial value of MSY example of the use of "funnel" to reduce the amount of storage for derivative calculations */
  dvariable Fdmsy;
  dvariable Stmp;
  dvariable Rtmp;
  dvariable Btmp;
  dvariable F1;
  dvariable yld1;
  cout <<"sel_fut"<<endl;
  cout <<sel_fut<<endl;
  cout <<natmort<<endl;
  cout <<p_mature<<endl;
  cout <<wt_ssb(endyr_r)     <<endl;
  cout <<sigmarsq_out<<endl;
  cout <<"Fmsy, MSY, Steepness, Rzero, Bzero, PhiZero, Alpha, Beta, SPB0, SPRBF40"<<endl;
  cout << Fmsy<<" "<<MSY<<" "<<steepness<<" "<<Rzero<<" "<<Bzero<<" "<<phizero<<" "<<alpha<<" "<<beta<<" "<<SB0/meanrec<<" "<<SBF40/meanrec<<endl;;
  cout <<endl<<endl<<"Iter  F  Stock  Yld  Recruit"<<endl;
  for (int ii=1;ii<=500;ii++)
  {
    F1    = double(ii)/100;
    yld1   = get_yield(F1,Stmp,Rtmp,Btmp);
    cout <<ii<<" " <<F1<<" "<< Stmp <<" "<<yld1<<" "<<Rtmp<<" "<<" "<< endl; 
  } 
 exit(1);

FUNCTION write_sd
 //   ofstream sd_output("extra_sd.rep");
  // sd_output << "GM_Biom GM_Biom2 HM_Fmsyr AM_Fmsyr Avg_3yr_Catch C_F40 C_F35 SSB_F40 SSB_F35 ABC_HM OFL_AM ABC_HM2 OFL_AM2 Adjust_1 Adjust_2 SPR_ABC SPR_OFL 1989YC 1989YC.sd "
           // << "1992YC 1992YC.sd 1996YC 1996YC.sd 2000YC 2000YC.sd Fut_2yr_Catch"<<endl;
  dvar_vector  cv_b(1,10);
  dvar_vector  cv_b2(1,10);
  dvar_vector  gm_b(1,10);
  dvar_vector  gm_b2(1,10);
 
  cv_b = elem_div(ABC_biom.sd , ABC_biom); 
  cv_b2= elem_div(ABC_biom2.sd , ABC_biom2); 
 
  gm_b = exp(log(ABC_biom) -elem_prod(cv_b ,cv_b )/2.); // Eq. 22
  gm_b2= exp(log(ABC_biom2)-elem_prod(cv_b2,cv_b2)/2.); // Eq. 22
 
  dvariable hm_f = exp(lnFmsy2 - lnFmsy2.sd*lnFmsy2.sd /2.); // Eq. 22
  dvariable am_f = exp(lnFmsy2 + lnFmsy2.sd*lnFmsy2.sd /2.); // Eq. 22

 // get spr rates for ABC and OFL
  SPR_ABC = SPR_OFL * am_f / hm_f;

  dvariable ABC  = gm_b(1)*hm_f*adj_1(1); 
  dvariable OFL  = gm_b(1)*am_f*adj_1(1); 
  for_sd << endyr_r+1<<", " ;
  for_sd(ABC);
  for_sd << endyr_r+1<<", " ;
  for_sd(OFL);
  ABC  = gm_b2(1)*hm_f*adj_2(1); 
  OFL  = gm_b2(1)*am_f*adj_2(1); 
  for_sd << endyr_r+2<<", " ;
  for_sd(ABC);
  for_sd << endyr_r+1<<", " ;
  for_sd(OFL);

  for_sd(future_SSB(1,styr_fut) ); 
  for_sd(future_SSB(2,styr_fut)); 
  for_sd(catch_future(1,styr_fut)); 
  for_sd(catch_future(2,styr_fut)); 
  for_sd(hm_f); 
  for_sd(am_f ); 
  for_sd(gm_b(1)); 
  for_sd(gm_b2(1)); 
  for_sd(adj_1(1)); 
  for_sd(adj_2(1)); 
  for_sd(mean(obs_catch(endyr_r-3,endyr_r))); 
  for_sd(SPR_ABC); 
  for_sd(SPR_OFL); 
  for_sd(pred_rec(1990)); 
  // for_sd(pred_rec.sd(1990)); 
  for_sd(pred_rec(1993)); 
  // for_sd(pred_rec.sd(1993)); 
  for_sd(pred_rec(1997)); 
  // for_sd(pred_rec.sd(1997)); 
  for_sd(pred_rec(2001)); 
  // for_sd(pred_rec.sd(2001)); 
  for_sd(Cat_Fut);
  for_sd<<"Catch  SSBNext  AdjNext  ABC1  OFL1  SSB2yrs  Adj2yrs  ABC2  OFL2"<<endl;
   for (i=1;i<=10;i++)
   {
      for_sd(  Cat_Fut(i));
      for_sd(  SSB_1(i)   );
      for_sd(  adj_1(i)   );
      for_sd(  gm_b(i)   * hm_f * adj_1(i));
      for_sd(  gm_b(i)   * am_f * adj_1(i));
      for_sd(  SSB_2(i)                   );
      for_sd(  adj_2(i)                   );
      for_sd(  gm_b2(i)  * hm_f * adj_2(i));
      for_sd(  gm_b2(i)  * am_f * adj_2(i));
   }
   for_sd<<endl<<"Stock in year "<<endyr_r<< " relative to Bzero (and SD):"<<endl;
   for_sd<< endyr_r<<" "<<Percent_Bzero <<" "<<Percent_Bzero.sd<<endl;
   for_sd<< endyr_r+1<<" "<<Percent_Bzero_1 <<" "<<Percent_Bzero_1.sd<<endl;
   for_sd<< endyr_r+1<<" "<<Percent_Bzero_2 <<" "<<Percent_Bzero_2.sd<<endl;
   for_sd<< endyr_r<<" "<<Percent_B100 <<" "<<Percent_B100.sd<<endl;
   for_sd<< endyr_r+1<<" "<<Percent_B100_1 <<" "<<Percent_B100_1.sd<<endl;
   for_sd<< endyr_r+1<<" "<<Percent_B100_2 <<" "<<Percent_B100_2.sd<<endl;
   for_sd<< " Plus and minus 5% from mode (normal approx) sensitivity"<<endl;
  // 0.12566 sigma
   double tweaklo;
   double tweakhi;
   tweaklo = (1.- 0.12566*value(cv_b(5)));
   tweakhi = (1.+ 0.12566*value(cv_b(5)));
   double adjlo=1.;
   double adjhi=1.;
   if(tweaklo*SSB_1(5) < value(Bmsy))
        adjlo = value((tweaklo*SSB_1(5)/Bmsy - 0.05)/(1.-0.05));
   if(tweakhi*SSB_1(5) < value(Bmsy))
        adjhi = value((tweakhi*SSB_1(5)/Bmsy - 0.05)/(1.-0.05));

   for_sd << "low:  "<< tweaklo*gm_b(5)   * hm_f * adjlo      << endl;
   for_sd << "mode: "<< gm_b(5)   * hm_f * adj_1(5)           << endl;
   for_sd << "high: "<< tweakhi*gm_b(5)   * hm_f * adjhi      << endl;

   for_sd << "No adjustment below Bmsy..."<<endl;
   for_sd << "low:  "<< tweaklo*gm_b(5)   * hm_f              << endl;
   for_sd << "mode: "<< gm_b(5)   * hm_f                      << endl;
   for_sd << "high: "<< tweakhi*gm_b(5)   * hm_f              << endl;
   for_sd << "Fut_Cat ABC_biom gm_b "<< endl;
   for (int icat=1;icat<=10;icat++)
     for_sd<<Cat_Fut(icat)<< " "<<ABC_biom(icat)<<" "<<gm_b(icat)<<endl;
  
  // Change sel_fut and recompute stuff
   for_sd <<" MSYR with younger selectivity (shifted down one age group)"<<endl;
   for_sd <<"Shift hm_f am_f "<<endl;
   for_sd << "0 " << hm_f<<" "<<am_f<<" "<<sel_fut<<endl;
   dvar_vector seltmp(1,nages);
   seltmp = sel_fut;

   for (j=1;j<nages;j++)
     sel_fut(j) = .5*(seltmp(j)+seltmp(j+1));
   sel_fut(nages) = sel_fut(nages-1);
   get_msy();
   hm_f = exp(lnFmsy2 - lnFmsy2.sd*lnFmsy2.sd /2.);
   am_f = exp(lnFmsy2 + lnFmsy2.sd*lnFmsy2.sd /2.);
   for_sd << "-0.5 " << hm_f<<" "<<am_f<<" "<<sel_fut<<endl;

   for (j=1;j<nages;j++)
     sel_fut(j) = seltmp(j+1);
   sel_fut(nages) = sel_fut(nages-1);
   get_msy();
   hm_f = exp(lnFmsy2 - lnFmsy2.sd*lnFmsy2.sd /2.);
   am_f = exp(lnFmsy2 + lnFmsy2.sd*lnFmsy2.sd /2.);
   for_sd << "-1 " << hm_f<<" "<<am_f<<" "<<sel_fut<<endl;

   for (j=1;j<nages;j++)
     sel_fut(j+1) = seltmp(j);
   sel_fut(1) = seltmp(1);
   get_msy();
   hm_f = exp(lnFmsy2 - lnFmsy2.sd*lnFmsy2.sd /2.);
   am_f = exp(lnFmsy2 + lnFmsy2.sd*lnFmsy2.sd /2.);
   for_sd << "1 " << hm_f<<" "<<am_f<<" "<<sel_fut<<endl;

   for (j=1;j<nages;j++)
     sel_fut(j+1) = .5*(seltmp(j)+seltmp(j+1));
   sel_fut(1) = seltmp(1);
   get_msy();
   hm_f = exp(lnFmsy2 - lnFmsy2.sd*lnFmsy2.sd /2.);
   am_f = exp(lnFmsy2 + lnFmsy2.sd*lnFmsy2.sd /2.);
   for_sd << "0.5 " << hm_f<<" "<<am_f<<" "<<sel_fut<<endl;
   for_sd.close();

   ofstream F40_out("F40_t.dat");

   F40_out << "Year B/Bmsy HR/MSYR SER/SERmsy F/Fmsy Bmsy SSB Bmsy2 Bfshble AM_fmsyr ";
   F40_out << "C/Bfshble SPRMSY_F Implied_SPR SPRMSY meanF F35 Fmsy Age3+ A3PRatio_Bmsy2 ";
   F40_out << "B35 F_Fmsyr avgAgeMSY avgWtMSY"<<endl;
   double fshable;
   double AM_fmsyr;
   for (i=styr;i<=endyr_r;i++)
   {
     sel_fut = sel_fsh(i);
     age_3_plus_biom(i)  = natage(i)(3,nages) * wt_ssb(i)(3,nages); 
     fshable = value(elem_prod(natage(i),sel_fut) * wt_ssb(endyr_r)); // fishable biomass
     AM_fmsyr =  value(exp(lnFmsy2 + lnFmsy2.sd*lnFmsy2.sd /2.));
     get_msy();
     F40_out << i       // Year
         <<" "<< SSB(i)/Bmsy   // Fshable Bmsy
         <<" "<< (obs_catch(i)/fshable) /AM_fmsyr  // Realized harvest rate
         <<" "<< (SER(i)/SER_Fmsy)                 // SER harvest rate
         <<" "<< mean(F(i))/Fmsy
         <<" "<< Bmsy
         <<" "<< SSB(i)
         <<" "<< Bmsy2   // Fshable Bmsy
         <<" "<< fshable // fishable biomass
         <<" "<< AM_fmsyr// AM Msyr
         <<" "<< obs_catch(i)/fshable // Realized harvest rate
         <<" "<< get_spr_rates(value(SPR_OFL),sel_fsh(i)) // F at MSY
         <<" "<< Implied_SPR(F(i))    // Implied SPR Given F
         <<" "<< SPR_OFL 
         <<" "<< mean(F(i))
         <<" "<<get_spr_rates(.35,sel_fsh(i))
         <<" "<<Fmsy 
         <<" "<<age_3_plus_biom(i) 
         <<" "<<value(age_3_plus_biom(i))/value(Bmsy2)
         <<" "<<value(SB100)*.35
         <<" "<<(obs_catch(i)/value(age_3_plus_biom(i)))/value(Fmsy2)
         <<" "<<value(avg_age_msy)
         <<" "<<value(avgwt_msy)
         <<endl; 
    }
    F40_out.close();
   ofstream SelGrid("selgrid.dat");
   for (i=1;i<=5;i++)
   {
     sel_fut = 0.0;
     sel_fut(i+1,nages) = 1.;

     get_msy();

     SelGrid << i+1       // knife-selection
         <<" "<<value(MSY)
         <<" "<<value(Bmsy)
         <<" "<<value(avg_age_msy)
         <<" "<<value(avgwt_msy)
         <<endl; 
   }
   for (i=styr;i<=endyr_r;i++)
   {
     sel_fut = sel_fsh(i);
     get_msy();
    SelGrid << i       // 
         <<" "<<value(MSY)
         <<" "<<value(Bmsy)
         <<" "<<value(avg_age_msy)
         <<" "<<value(avgwt_msy)
         <<endl; 
   }
   compute_Fut_selectivity();
   get_msy();
    SelGrid << "sel_fut"        // knife-selection
         <<" "<<value(MSY)
         <<" "<<value(Bmsy)
         <<" "<<value(avg_age_msy)
         <<" "<<value(avgwt_msy)
         <<endl; 
    SelGrid.close();

FINAL_SECTION
  Write_R();
  write_sd();

TOP_OF_MAIN_SECTION
  gradient_structure::set_MAX_NVAR_OFFSET(1600);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(200000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(8100); 
  gradient_structure::set_CMPDIF_BUFFER_SIZE(2000000);
  arrmblsize=10000000;

GLOBALS_SECTION
  #include <float.h>
  #include <admodel.h>

  ofstream for_sd("extra_sd.rep");
  #undef for_sd
  #define for_sd(object) for_sd << #object "," << object << endl;

  ofstream write_mceval("mceval.rep");
  #undef write_mceval
  #define write_mceval(object) write_mceval << " " << object ;
  ofstream write_log("Input_Log.rep");
  #undef write_log
  #define write_log(object) write_log << #object "\n" << object << endl;
  ofstream R_report("For_R.rep");
  #undef R_report
  #define R_report(object) R_report << "$"<<#object "\n" << object << endl;

  adstring simname;
  adstring model_name;
  adstring datafile_name;
  adstring selchng_filename; 

FUNCTION double mn_age(_CONST dvar_vector& pobs)
  int lb1 = pobs.indexmin();
  int ub1 = pobs.indexmax();
  dvector av = age_vector(lb1,ub1)  ;
  double mobs = value(pobs*av);
  return mobs;

FUNCTION double Sd_age(_CONST dvar_vector& pobs, _CONST dvar_vector& phat)
  int lb1 = pobs.indexmin();
  int ub1 = pobs.indexmax();
  dvector av = age_vector(lb1,ub1)  ;
  double mobs = value(pobs*av);
  double stmp = value(sqrt(elem_prod(av,av)*pobs - mobs*mobs));
  return stmp;

FUNCTION double Eff_N_adj(_CONST double, _CONST dvar_vector& pobs, _CONST dvar_vector& phat)
  // Eq. 6
  int lb1 = pobs.indexmin();
  int ub1 = pobs.indexmax();
  dvector av = age_vector(lb1,ub1)  ;
  double mobs = value(pobs*av);
  double mhat = value(phat*av );
  double rtmp = mobs-mhat;
  double stmp = value(sqrt(elem_prod(av,av)*pobs - mobs*mobs));
  return square(stmp)/square(rtmp);

FUNCTION double Eff_N2(_CONST dvar_vector& pobs, _CONST dvar_vector& phat)
  // Eq. 6
  int lb1 = pobs.indexmin();
  int ub1 = pobs.indexmax();
  dvector av = age_vector(lb1,ub1)  ;
  double mobs = value(pobs*av);
  double mhat = value(phat*av );
  double rtmp = mobs-mhat;
  double stmp = value(sqrt(elem_prod(av,av)*pobs - mobs*mobs));
  return square(stmp)/square(rtmp);

FUNCTION double Eff_N(_CONST dvar_vector& pobs, _CONST dvar_vector& phat)
  // Eq. 6
  dvar_vector rtmp = elem_div((pobs-phat),sqrt(elem_prod(phat,(1-phat))));
  double vtmp;
  vtmp = value(norm2(rtmp)/size_count(rtmp));
  return 1/vtmp;

FUNCTION Write_R
  // Development--just start to get some output into R
  R_report<<"$Yr"<<endl; for (i=styr;i<=endyr_r;i++) R_report<<i<<" "; R_report<<endl;
  R_report(future_SSB);
  R_report(future_SSB.sd);
  R_report(catch_future);
  R_report(Fcur_Fmsy);
  R_report(Fcur_Fmsy.sd);
  R_report(Bcur_Bmsy);
  R_report(Bcur_Bmsy.sd);
  R_report(Bcur_Bmean);
  R_report(Bcur_Bmean.sd);
  R_report(Bcur2_Bmsy);
  R_report(Bcur2_Bmsy.sd);
  R_report(Bcur2_B20);
  R_report(Bcur2_B20.sd);
  R_report(Bcur3_Bcur);
  R_report(Bcur3_Bcur.sd);
  R_report(Bcur3_Bmean);
  R_report(Bcur3_Bmean.sd);
  R_report(LTA1_5R);
  R_report(LTA1_5R.sd);
  R_report(MatAgeDiv1);
  R_report(MatAgeDiv1.sd);
  R_report(MatAgeDiv2);
  R_report(MatAgeDiv2.sd);
  R_report(RelEffort);
  R_report(RelEffort.sd);
  R_report(LTA1_5);
  R_report(LTA1_5.sd);
  R_report<<"$SER"<<endl; 
  for (i=styr;i<=endyr_r;i++) 
  {
    double lb=value(SER(i)/exp(2.*sqrt(log(1+square(SER.sd(i))/square(SER(i))))));
    double ub=value(SER(i)*exp(2.*sqrt(log(1+square(SER.sd(i))/square(SER(i))))));
    R_report<<i<<" "<<SER(i)<<" "<<SER.sd(i)<<" "<<lb<<" "<<ub<<endl;
  }
  R_report<<"$SSB"<<endl; 
  for (i=styr;i<=endyr_r;i++) 
  {
    double lb=value(SSB(i)/exp(2.*sqrt(log(1+square(SSB.sd(i))/square(SSB(i))))));
    double ub=value(SSB(i)*exp(2.*sqrt(log(1+square(SSB.sd(i))/square(SSB(i))))));
    R_report<<i<<" "<<SSB(i)<<" "<<SSB.sd(i)<<" "<<lb<<" "<<ub<<endl;
  }
  R_report<<"$R"<<endl; for (i=styr;i<=endyr_r;i++) 
  {
    double lb=value(pred_rec(i)/exp(2.*sqrt(log(1+square(pred_rec.sd(i))/square(pred_rec(i))))));
    double ub=value(pred_rec(i)*exp(2.*sqrt(log(1+square(pred_rec.sd(i))/square(pred_rec(i))))));
    R_report<<i<<" "<<pred_rec(i)<<" "<<pred_rec.sd(i)<<" "<<lb<<" "<<ub<<endl;
  }
    R_report << "$N"<<endl;
  R_report << "$AVO_yrs"<<  endl;
  R_report << "$AVO"<<  endl;
  R_report << "$AVO_sd"<<  endl;
  R_report << "$pobs_fsh"<<  endl;
  for (i=1;i<=n_fsh_r;i++) 
    R_report << yrs_fsh_data(i)<< " "<< oac_fsh(i) << endl;
    R_report   << endl;
    R_report << "$phat_fsh"<< endl;
    for (i=1;i<=n_fsh_r;i++) 
      R_report << yrs_fsh_data(i)<< " "<< eac_fsh(i) << endl;
    R_report   << endl;

    R_report << "$phat_bts"<<endl;
    for (i=1;i<=n_bts_r;i++) 
      R_report << yrs_bts_data(i)<< " "<< eac_bts(i) << endl;
    R_report   << endl;
    R_report << "$phat_eit"<<endl;
    for (i=1;i<=n_eit_r;i++) 
      R_report << yrs_eit_data(i)<< " "<< eac_eit(i) << endl;
      R_report   << endl;
      
      R_report << "$pobs_biom_bts"<<endl;
      for (i=1;i<=n_bts_r;i++) 
        R_report << yrs_bts_data(i)<< " "<< oac_bts(i) << endl;
        R_report   << endl;

      R_report << "$pobs_eit"<<endl;
      for (i=1;i<=n_eit_r;i++) 
        R_report << yrs_eit_data(i)<< " "<< oac_eit(i) << endl;
        R_report   << endl;
      
    R_report <<"$EffN_Fsh_1"<<endl;
    double sda_tmp = Sd_age(oac_fsh(i),eac_fsh(i));
    for (i=1;i<=n_fsh_r;i++) 
    {
      R_report << yrs_fsh_data(i)
               << " "<<Eff_N(oac_fsh(i),eac_fsh(i)) 
               << " "<<Eff_N2(oac_fsh(i),eac_fsh(i))
               << " "<<mn_age(oac_fsh(i))
               << " "<<mn_age(eac_fsh(i))
               << " "<<sda_tmp
               << " "<<mn_age(oac_fsh(i)) - sda_tmp *2. / sqrt(sam_fsh(i))
               << " "<<mn_age(oac_fsh(i)) + sda_tmp *2. / sqrt(sam_fsh(i))
               <<endl;
    }
  R_report(F);
  R_report.close();

FUNCTION dvariable Implied_SPR( const dvar_vector& F_age) 
  RETURN_ARRAYS_INCREMENT();
  // Function that returns SPR percentage given a realized value of F...
    dvar_vector ntmp0(1,nages);
    dvar_vector ntmp1(1,nages);
    ntmp0(1) = 1.;
    ntmp1(1) = 1.;
    for (j=2;j<nages;j++)
    {
      ntmp0(j)  = ntmp0(j-1)* exp( -natmort(j-1));
      ntmp1(j)  = ntmp1(j-1)* exp(-(natmort(j-1) + F_age(j-1) ));
    }
    ntmp0(nages)  =  ntmp0(nages-1)* exp(-natmort(nages-1))/ (1.- exp(-natmort(nages-1)));
    ntmp1(nages)  =  ntmp1(nages-1)* exp(-(natmort(nages-1) + F_age(nages-1)))/ (1.- mfexp(-(natmort(nages) + F_age(nages) )));
    dvariable sb0_tmp;
    dvariable sb1_tmp;

    sb0_tmp.initialize();
    sb1_tmp.initialize();
    for (j=1;j<=nages;j++)
    {
      // natmort till spawning 
      sb0_tmp += ntmp0(j)*wt_ssb(endyr_r,j) * mfexp(-yrfrac * natmort(j));
      sb1_tmp += ntmp1(j)*wt_ssb(endyr_r,j) * mfexp(-yrfrac * ( natmort(j) + F_age(j) ));
    }
  RETURN_ARRAYS_DECREMENT();
    return(sb1_tmp / sb0_tmp);

 /* ******************************************************
 FUNCTION dvariable Check_Parm(const double& Pmin, const double& Pmax, const double& jitter, const prevariable& Pval)
  {
    dvariable NewVal;
    dvariable temp;
    NewVal=Pval;
    if(Pval<Pmin)
    {N_warn++; warning<<" parameter init value is less than parameter min "<<Pval<<" < "<<Pmin<<endl; NewVal=Pmin;}
    if(Pval>Pmax)
    {N_warn++; warning<<" parameter init value is greater than parameter max "<<Pval<<" > "<<Pmax<<endl; NewVal=Pmax;}
    if(jitter>0.0)
    {
      temp=log((Pmax-Pmin+0.0000002)/(NewVal-Pmin+0.0000001)-1.)/(-2.);   // transform the parameter
      temp += randn(radm) * jitter;
      NewVal=Pmin+(Pmax-Pmin)/(1.+mfexp(-2.*temp));
    }
    return NewVal;
  }
  */
