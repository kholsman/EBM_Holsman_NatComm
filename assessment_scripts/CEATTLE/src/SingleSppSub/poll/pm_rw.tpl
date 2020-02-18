//////////////////////////////////////////////////////////// 
// 
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
  !! avo_sel(1)=0.0;	avo_sel(2)=1;	avo_sel(3)=1;	avo_sel(4)=0.85;	avo_sel(5)=0.7;	avo_sel(6)=0.55;	avo_sel(7)=0.3;	avo_sel(8)=0.15;	avo_sel(9)=0.05;	avo_sel(10)=0.01;	avo_sel(11)=0.01;	avo_sel(12)=0.01;	avo_sel(13)=0.01;	avo_sel(14)=0.01;	avo_sel(15)=0.01;
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
  !!if (max(p_mature)>.51) p_mature *= 0.5;
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
  init_int n_fsh
  init_int n_bts
  init_int n_eit
  vector nagecomp(1,ngears)
  init_ivector yrs_fsh_data(1,n_fsh)
  init_ivector yrs_bts_data(1,n_bts)
  init_ivector yrs_eit_data(1,n_eit)
  init_ivector      sam_fsh(1,n_fsh)
  init_ivector      sam_bts(1,n_bts)
  init_ivector      sam_eit(1,n_eit)
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

  init_vector bottom_temp(1,n_bts)
  !! cout<<"BottomTemp:"<<endl<<bottom_temp<<endl;

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
  endyr_fut=endyr_r+nyrs_future;
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
  sel_a50_bts 5.5
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

  init_number temp_coef(3);
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
  sdreport_vector regime(1,9);
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
 
  3darray natage_future(1,6,styr_fut,endyr_fut,1,nages)
  init_vector rec_dev_future(styr_fut,endyr_fut,phase_F40);

  3darray F_future(1,6,styr_fut,endyr_fut,1,nages);
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
  sdreport_number Bcur_Bmsy;
  sdreport_number F40_spb;
  sdreport_number F40_catch;
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
  matrix SER_future(1,6,styr_fut,endyr_fut);
  matrix catch_future(1,6,styr_fut,endyr_fut);
  sdreport_matrix future_SSB(1,6,endyr_r,endyr_fut)
  vector age_1_7_biomass(styr,endyr_r);
  objective_function_value fff;

PRELIMINARY_CALCS_SECTION
  WriteNewData();
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
  
TOP_OF_MAIN_SECTION
  gradient_structure::set_MAX_NVAR_OFFSET(1600);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(200000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(8100); 
  gradient_structure::set_CMPDIF_BUFFER_SIZE(2000000);
  arrmblsize=10000000;

GLOBALS_SECTION
  #include <admodel.h>
  #include <pm_rw.htp>
  adstring simname;
  adstring model_name;
  adstring datafile_name;
  adstring selchng_filename; 

  ofstream for_sd("extra_sd.rep");
	#undef for_sd
	#define for_sd(object) for_sd << #object "," << object << endl;

  ofstream write_log("Input_Log.rep");
	#undef write_log
	#define write_log(object) write_log << #object "\n" << object << endl;
  
  ofstream NewData("poll_new.dat");
  #undef write_new
  #define write_new(object) NewData << "# " #object "\n" << object << endl;

  ifstream poll_msm("poll_msm.dat");
  #undef read_new
  // #define read_new(object) poll_msm >> object ;
  // #define read_new(object) cout<<"Hi" <<endl;
  #define read_new(object) poll_msm >> object;
FUNCTION WriteNewData
  write_new( styr);
  write_new( styr_bts);
  write_new( styr_eit);
  write_new( endyr+1);
  write_new( recage);
  write_new( nages);
  write_new( p_mature*2.);
  write_new( ewindex);
  write_new( ewindex(endyr));
  write_new( nsindex);
  write_new( nsindex(endyr));
  write_new( wt_fsh);
  write_new( wt_fsh(endyr));
  write_new( wt_ssb);
  write_new( wt_ssb(endyr));
  write_new( obs_catch);
  double catchtmp;
    read_new(catchtmp);
  write_new( catchtmp     );
  write_new( obs_effort);
  write_new( obs_effort(endyr));
  write_new( n_cpue);
  write_new( yrs_cpue);
  write_new( obs_cpue);
  write_new( obs_cpue_std);
  write_new( n_avo);
  write_new( yrs_avo);
  write_new( obs_avo);
  write_new( obs_avo_std);
  write_new( wt_avo);
  write_new( ngears);
  write_new( minind);
  write_new( n_fsh+1);
  write_new( n_bts+1);
  if (endyr==(yrs_eit_data(n_eit)+1)) 
  {
    write_new( n_eit+1); 
  }
  else
  {
    write_new( n_eit);
  }
  write_new( yrs_fsh_data);
  write_new( yrs_fsh_data(n_fsh)+1);
  write_new( yrs_bts_data);
  write_new( yrs_bts_data(n_bts)+1);
  // if it's been two years since survey, then do new one
  write_new( yrs_eit_data);
  if (endyr==(yrs_eit_data(n_eit)+1)) 
    write_new(yrs_eit_data(n_eit)+2);
  write_new(      sam_fsh);
  write_new(      sam_fsh(n_fsh)); // addon
  write_new(      sam_bts);
  write_new(      sam_bts(n_bts)); // addon
  write_new(      sam_eit);
  if (endyr==(yrs_eit_data(n_eit)+1)) 
    write_new(sam_eit(n_eit) );
  write_new(     oac_fsh_data);
  dvector agetmp(1,nages);
    read_new( agetmp);  // Fishery age composition
  write_new( agetmp); // written 

  double bbbtmp;
  write_new(     obs_bts_data);
    read_new( bbbtmp);  // read in new biom
  write_new(bbbtmp);  // Write biomass estimate
  write_new( std_obs_bts_data);
    read_new( bbbtmp);  // read in new biom std err
  write_new(bbbtmp);  // Write biomass estimate std err
  write_new(  wt_bts);
  write_new(  wt_bts(n_bts));
  write_new( std_bts);
    read_new( bbbtmp);  // read in new survey N std err
  write_new(bbbtmp);  // Write survey N estimate std err
  write_new( oac_bts_data);
    read_new( agetmp);  // read in new bts age comp
  write_new( agetmp); // write new age comp
  write_new( std_eit);
    read_new( bbbtmp);  // read in new survey N std err
  if (endyr==(yrs_eit_data(n_eit)+1)) 
  {
    write_new(bbbtmp);  // Write eit survey N estimate std err
  }
  write_new( oac_eit_data);
    read_new(agetmp);
  if (endyr==(yrs_eit_data(n_eit)+1)) 
  {
    write_new(agetmp);
  }
  write_new( obs_eit_data);
  read_new(bbbtmp);
  if (endyr==(yrs_eit_data(n_eit)+1)) 
  {
    write_new(bbbtmp);
  }
  write_new( std_obs_eit_data);
  read_new( bbbtmp);  // read in new survey N std err
  if (endyr==(yrs_eit_data(n_eit)+1)) 
  {
    write_new(bbbtmp);  // Write eit survey N estimate std err
  }
  write_new( wt_eit);
  if (endyr==(yrs_eit_data(n_eit)+1)) 
  {
    cout <<"Eit status: ALL GO"<<yrs_eit_data<<" "<<endyr<<endl;
    write_new(wt_eit(n_eit));
  }
  write_new( bottom_temp);
  write_new( bottom_temp(n_bts));
  write_new( age_err);
  write_new( nlbins);
  write_new( olc_fsh);
  write_new( age_len);
  write_new( test);
  exit(1);
