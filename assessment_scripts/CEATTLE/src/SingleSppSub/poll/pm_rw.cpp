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
#include <admodel.h>
#include <contrib.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <poll/pm_rw.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
 count_mcmc=0;
 count_mcsave=0;
 q_amin = 3; q_amax= 15; // age range overwhich q applies (for prior specifications)
  selages.allocate(1,15);
 selages=1.0;selages(1)=0;selages(2)=0;
  avo_sel.allocate(1,15);
 avo_sel(1)=0.0;	avo_sel(2)=1;	avo_sel(3)=1;	avo_sel(4)=0.85;	avo_sel(5)=0.7;	avo_sel(6)=0.55;	avo_sel(7)=0.3;	avo_sel(8)=0.15;	avo_sel(9)=0.05;	avo_sel(10)=0.01;	avo_sel(11)=0.01;	avo_sel(12)=0.01;	avo_sel(13)=0.01;	avo_sel(14)=0.01;	avo_sel(15)=0.01;
  Cat_Fut.allocate(1,10);
 do_EIT1=1; // flag to carry EIT out in future year (for simulations only)
 pflag=0;
  pad_srecout = new ofstream("srec_Ass_out.dat");
  pad_projout = new ofstream("pm.prj");
  pad_nofish = new ofstream("nofish.out");
  pad_projout2 = new ofstream("pmsr.prj");
  pad_eval = new ofstream("pm_eval.dat")     ;
  *(ad_comm::global_datafile) >>  model_name; 
  *(ad_comm::global_datafile) >>  datafile_name; 
  *(ad_comm::global_datafile) >>  selchng_filename; 
 write_log(datafile_name);
 write_log(model_name);
  SrType.allocate("SrType");
  Do_Combined.allocate("Do_Combined");
  use_age_err.allocate("use_age_err");
  use_age1_eit.allocate("use_age1_eit");
  age1_sigma_eit.allocate("age1_sigma_eit");
 mina_bts=2;
 if (use_age1_eit) mina_eit=2; else mina_eit=1;
 write_log(mina_eit);
 cout<<" Minimum age for EIT is: "<<mina_eit<<endl;// exit(1);
  use_endyr_len.allocate("use_endyr_len");
  use_popwts_ssb.allocate("use_popwts_ssb");
  natmortprior.allocate("natmortprior");
  cvnatmortprior.allocate("cvnatmortprior");
  q_all_prior.allocate("q_all_prior");
  q_all_sigma.allocate("q_all_sigma");
  q_bts_prior.allocate("q_bts_prior");
  q_bts_sigma.allocate("q_bts_sigma");
  sigrprior.allocate("sigrprior");
  cvsigrprior.allocate("cvsigrprior");
  phase_sigr.allocate("phase_sigr");
  steepnessprior.allocate("steepnessprior");
  cvsteepnessprior.allocate("cvsteepnessprior");
  phase_steepness.allocate("phase_steepness");
  use_spr_msy_pen.allocate("use_spr_msy_pen");
  sigma_spr_msy.allocate("sigma_spr_msy");
 lambda_spr_msy = .5/(sigma_spr_msy*sigma_spr_msy);
  use_last_eit_ac.allocate("use_last_eit_ac");
  nyrs_sel_avg.allocate("nyrs_sel_avg");
  cv_bts_tmp.allocate("cv_bts_tmp");
  cv_eit_tmp.allocate("cv_eit_tmp");
  srprior_a.allocate("srprior_a");
  srprior_b.allocate("srprior_b");
  nyrs_future.allocate("nyrs_future");
  next_yrs_catch.allocate("next_yrs_catch");
  fixed_catch_fut1.allocate("fixed_catch_fut1");
  fixed_catch_fut2.allocate("fixed_catch_fut2");
  fixed_catch_fut3.allocate("fixed_catch_fut3");
 write_log(nyrs_future);
 write_log(next_yrs_catch);
 write_log(fixed_catch_fut1);
 write_log(fixed_catch_fut2);
 write_log(fixed_catch_fut3);
 write_log(nyrs_sel_avg);
  phase_F40.allocate("phase_F40");
  robust_phase.allocate("robust_phase");
  eit_robust_phase.allocate("eit_robust_phase");
  eit_like_type.allocate("eit_like_type");
  phase_logist_fsh.allocate("phase_logist_fsh");
  phase_logist_bts.allocate("phase_logist_bts");
 write_log(phase_F40);
 write_log(robust_phase);
 write_log(phase_logist_fsh);
 write_log(phase_logist_bts);
 phase_selcoffs_bts   = 2;
 phase_selcoffs_fsh   = 2;
  phase_seldevs_fsh.allocate("phase_seldevs_fsh");
  phase_seldevs_bts.allocate("phase_seldevs_bts");
  phase_age1devs_bts.allocate("phase_age1devs_bts");
 write_log(phase_seldevs_bts);
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
  phase_selcoffs_eit.allocate("phase_selcoffs_eit");
  phase_selcoffs_eit_dev.allocate("phase_selcoffs_eit_dev");
 cout <<"Phase fsh coef: "<<phase_selcoffs_fsh<<" "<<phase_selcoffs_fsh_dev<<endl;
 cout <<"Phase bts coef: "<<phase_selcoffs_bts<<" "<<phase_selcoffs_bts_dev<<endl;
 cout <<"Phase eit coef: "<<phase_selcoffs_eit<<" "<<phase_selcoffs_eit_dev<<endl;
 cout <<"Phase fsh logist: "<<phase_logist_fsh<<" "<<phase_logist_fsh_dev<<endl;
 cout <<"Phase bts logist: "<<phase_logist_bts<<" "<<phase_logist_bts_dev<<endl;
  phase_natmort.allocate("phase_natmort");
  phase_q_bts.allocate("phase_q_bts");
  phase_q_std_area.allocate("phase_q_std_area");
  phase_q_eit.allocate("phase_q_eit");
  phase_bt.allocate("phase_bt");
  phase_rec_devs.allocate("phase_rec_devs");
  phase_larv.allocate("phase_larv");
  phase_sr.allocate("phase_sr");
  wt_fut_phase.allocate("wt_fut_phase");
  last_age_sel_group_fsh.allocate("last_age_sel_group_fsh");
  last_age_sel_group_bts.allocate("last_age_sel_group_bts");
  last_age_sel_group_eit.allocate("last_age_sel_group_eit");
  ctrl_flag.allocate(1,29,"ctrl_flag");
 cout<<ctrl_flag<<endl;
  sel_dev_shift.allocate("sel_dev_shift");
  Sim_status.allocate("Sim_status");
  iseed_junk.allocate("iseed_junk");
  YC_mult.allocate(1,15,"YC_mult");
 cout << "Simulation status = "<<Sim_status<<endl;
 cout << "iseed             = "<<iseed_junk<<endl;
 cout << "YC                = "<<YC_mult<<endl;
 global_datafile= new cifstream(datafile_name);
  styr.allocate("styr");
  styr_bts.allocate("styr_bts");
  styr_eit.allocate("styr_eit");
  endyr.allocate("endyr");
 cout <<datafile_name<<" "<<styr<<" "<<endyr<<endl;;
  recage.allocate("recage");
  nages.allocate("nages");
 write_log(nages);write_log(recage);write_log(endyr);
  p_mature.allocate(1,nages,"p_mature");
if (max(p_mature)>.51) p_mature *= 0.5;
  ewindex.allocate(styr,endyr,"ewindex");
  nsindex.allocate(styr,endyr,"nsindex");
  wt_fsh.allocate(styr,endyr,1,nages,"wt_fsh");
  wt_ssb.allocate(styr,endyr,1,nages,"wt_ssb");
 if(use_popwts_ssb==0)  wt_ssb = wt_fsh; // this is the historical default (and continued use) Eq. 9
  wt_tmp.allocate(1,nages,1991,endyr-1);
  wt_mn.allocate(1,nages);
  wt_sigma.allocate(1,nages);
  obs_catch.allocate(styr,endyr,"obs_catch");
 write_log(p_mature);write_log(obs_catch);write_log(wt_fsh);
  obs_effort.allocate(styr,endyr,"obs_effort");
  n_cpue.allocate("n_cpue");
  yrs_cpue.allocate(1,n_cpue,"yrs_cpue");
  obs_cpue.allocate(1,n_cpue,"obs_cpue");
  obs_cpue_std.allocate(1,n_cpue,"obs_cpue_std");
  obs_cpue_var.allocate(1,n_cpue);
 obs_cpue_var = square(obs_cpue_std);
  n_avo.allocate("n_avo");
  yrs_avo.allocate(1,n_avo,"yrs_avo");
  obs_avo.allocate(1,n_avo,"obs_avo");
  obs_avo_std.allocate(1,n_avo,"obs_avo_std");
  wt_avo.allocate(1,n_avo,1,nages,"wt_avo");
  obs_avo_var.allocate(1,n_avo);
 obs_avo_var = square(obs_avo_std); 
 write_log(yrs_avo);write_log(obs_avo);write_log(wt_avo);
  ngears.allocate("ngears");
  minind.allocate(1,ngears,"minind");
  n_fsh.allocate("n_fsh");
  n_bts.allocate("n_bts");
  n_eit.allocate("n_eit");
  nagecomp.allocate(1,ngears);
  yrs_fsh_data.allocate(1,n_fsh,"yrs_fsh_data");
  yrs_bts_data.allocate(1,n_bts,"yrs_bts_data");
  yrs_eit_data.allocate(1,n_eit,"yrs_eit_data");
  sam_fsh.allocate(1,n_fsh,"sam_fsh");
  sam_bts.allocate(1,n_bts,"sam_bts");
  sam_eit.allocate(1,n_eit,"sam_eit");
 cout<<endl<<"Sample size  fsh "<<endl;
 cout<<sam_fsh<<endl;
 cout<<endl<<"Sample size  bts "<<endl;
 cout<<sam_bts<<endl;
 cout<<endl<<"Sample size  eit "<<endl;
 cout<<sam_eit<<endl;
  oac_fsh_data.allocate(1,n_fsh,1,nages,"oac_fsh_data");
 cout<< " Index min and max for age comp data: "<<endl <<oac_fsh_data.indexmin()<<" Max "<<oac_fsh_data.indexmax()<<endl;
  obs_bts_data.allocate(1,n_bts,"obs_bts_data");
  std_obs_bts_data.allocate(1,n_bts,"std_obs_bts_data");
  var_obs_bts.allocate(1,n_bts);
 var_obs_bts = elem_prod(std_obs_bts_data,std_obs_bts_data);
  wt_bts.allocate(1,n_bts,1,nages,"wt_bts");
  std_bts.allocate(1,n_bts,"std_bts");
  var_bts.allocate(1,n_bts);
 cout<<endl<<"observed std bts "<<endl;
 cout<<std_bts<<endl;
 var_bts = elem_prod(std_bts,std_bts);
  oac_bts_data.allocate(1,n_bts,1,nages,"oac_bts_data");
 cout<<oac_bts_data.indexmin()<<" Max "<<oac_bts_data.indexmax()<<endl;
  std_eit.allocate(1,n_eit,"std_eit");
  var_eit.allocate(1,n_eit);
 var_eit = elem_prod(std_eit,std_eit);
 cout<<"Stdev Total EIT N "<<std_eit<<endl;
  lse_eit.allocate(1,n_eit_r);
  lvar_eit.allocate(1,n_eit_r);
  oac_eit_data.allocate(1,n_eit,1,nages,"oac_eit_data");
  ln_oac_eit.allocate(1,n_eit,mina_eit,nages);
 for (i=1;i<=n_eit;i++) ln_oac_eit(i) = log(oac_eit_data(i)(mina_eit,nages));
 cout<<oac_eit_data.indexmin()<<" Max "<<oac_eit_data.indexmax()<<endl;
 cout<<oac_eit_data(n_eit)(5,15) <<endl;
 cout<<endl<<"observed EIT Age comps"<<endl;
 cout<<oac_eit_data<<endl;
 cout<<endl<<"observed BTS Age comps"<<endl;
 cout<<oac_bts_data<<endl;
  obs_eit_data.allocate(1,n_eit,"obs_eit_data");
  std_obs_eit_data.allocate(1,n_eit,"std_obs_eit_data");
  var_obs_eit.allocate(1,n_eit);
 var_obs_eit = elem_prod(std_obs_eit_data,std_obs_eit_data);
  wt_eit.allocate(1,n_eit,1,nages,"wt_eit");
  bottom_temp.allocate(1,n_bts,"bottom_temp");
 cout<<"BottomTemp:"<<endl<<bottom_temp<<endl;
  age_err.allocate(1,nages,1,nages,"age_err");
  nlbins.allocate("nlbins");
  olc_fsh.allocate(1,nlbins,"olc_fsh");
 olc_fsh /= sum(olc_fsh);
  lens.allocate(1,nlbins);
 for (j=1;j<=nlbins;j++)
  lens(j) = double(j)+25;
 lens(2,5) += 1;
 lens(3,5) += 1;
 lens(4,5) += 1;
 lens(5) += 1;
 lens(6,16) += 4.5;
 lens(17,nlbins) += 5;
 lens(18,nlbins) += 1.0;
 lens(19,nlbins) += 1.0;
 lens(20,nlbins) += 1.0;
 lens(21,nlbins) += 1.0;
 lens(22,nlbins) += 1.0;
 lens(23,nlbins) += 1.0;
 lens(24,nlbins) += 1.0;
 cout <<lens<<endl;// exit(1);
  age_len.allocate(1,nages,1,nlbins,"age_len");
  test.allocate("test");
 if(test!=1234){ cout<<"Failed on data read "<<test<<endl;exit(1);}
 global_datafile= new cifstream("endyrN_est.dat");
  N_endyr_est.allocate(1,nages,"N_endyr_est");
  s_endyr_est.allocate(1,nages,"s_endyr_est");
  c_endyr_est.allocate(1,nages,1,nages,"c_endyr_est");
  v_endyr_est.allocate(1,nages,1,nages);
  chol.allocate(1,nages,1,nages);
 for (i=1;i<=nages;i++) for (j=1;j<=nages;j++) v_endyr_est(i,j)=c_endyr_est(i,j)*s_endyr_est(i)*s_endyr_est(j); 
 chol = (choleski_decomp(v_endyr_est));  
 spawnmo = 4.;
 yrfrac= (spawnmo-1.)/12; 
  age_vector.allocate(1,nages);
 for (j=1;j<=nages;j++)
  age_vector(j) = double(j);
 ad_comm::change_datafile_name("pm_Fmsy_Alt.dat");
  Fmoney.allocate(1,nages,"Fmoney");
 ad_comm::change_datafile_name(selchng_filename);
  sel_data.allocate(styr,endyr,0,3,"sel_data");
  eit_ch_in.allocate(styr,endyr);
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
  yrs_ch_eit.allocate(1,nch_eit);
  sel_ch_sig_eit.allocate(1,nch_eit);
 ii=0;for (i=styr;i<=endyr_r;i++) if(eit_ch_in(i)>0) {ii++;sel_ch_sig_eit(ii)=eit_ch_in(i);yrs_ch_eit(ii)=i;} 
 cout<<"Yrs EIT sel change: "<<yrs_ch_eit <<endl<< sel_ch_sig_eit<<endl;// exit(1);
  oac_fsh.allocate(1,n_fsh_r,1,nbins);
  oac_bts.allocate(1,n_bts_r,1,nbins);
  if (use_last_eit_ac>0) n_eit_ac_r = n_eit_r; else n_eit_ac_r = n_eit_r-1; 
  if (use_last_eit_ac>0) nagecomp(3) = n_eit_r; else nagecomp(3) = n_eit_r-1; 
  oac_eit.allocate(1,n_eit_ac_r,1,nbins);
  oa1_eit.allocate(1,n_eit_ac_r);
  ot_fsh.allocate(1,n_fsh_r);
  ot_bts.allocate(1,n_bts_r);
  obs_biom_bts.allocate(1,n_bts_r);
  std_obs_bts.allocate(1,n_bts_r);
  ot_eit.allocate(1,n_eit_r);
  obs_eit.allocate(1,n_eit_r);
  std_obs_eit.allocate(1,n_eit_r);
 iseed=0;
 do_fmort=0;
  adj_1.allocate(1,10);
  adj_2.allocate(1,10);
  SSB_1.allocate(1,10);
  SSB_2.allocate(1,10);
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
long int lseed=iseed;
  pad_rng = new random_number_generator(iseed);;
 ad_comm::change_datafile_name("surveycpue.dat");
  nstrata.allocate("nstrata");
 cout <<nstrata<<endl;
  sqkm.allocate(1,nstrata,"sqkm");
  ndata.allocate("ndata");
  d.allocate(1,ndata,1,5,"d");
  nobs.allocate(1982,2008,1,14);
  mnCPUE.allocate(1982,2008,1,14);
  mntemp.allocate(1982,2008,1,14);
  temp_in.allocate(1982,2008,1,14,1,70);
  CPUE_in.allocate(1982,2008,1,14,1,70);
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
  temp.allocate(1982,2008,1,14,1,nobs);
  CPUE.allocate(1982,2008,1,14,1,nobs);
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
}

void model_parameters::initializationfunction(void)
{
  sigr.set_initial_value(sigrprior);
  wt_fut.set_initial_value(.8);
  steepness.set_initial_value(steepnessprior);
  log_avgrec.set_initial_value(9.87558);
  log_Rzero.set_initial_value(9.2033);
  log_avginit.set_initial_value(4.8);
  log_avg_F.set_initial_value(-1.6);
  bt_slope.set_initial_value(0.);
  log_q_eit.set_initial_value(-1.05313);
  log_q_avo.set_initial_value(-9.6);
  log_q_bts.set_initial_value(q_bts_prior);
  log_q_std_area.set_initial_value(0.);
  log_q_cpue.set_initial_value(-0.16);
  sel_coffs_fsh.set_initial_value(-.10);
  sel_coffs_bts.set_initial_value(-.01);
  sel_coffs_eit.set_initial_value(-.10);
  sel_a50_bts.set_initial_value(5.5);
  sel_slp_bts.set_initial_value(1.);
  sel_dif1_fsh.set_initial_value(1);
  sel_a501_fsh.set_initial_value(3);
  sel_dif2_fsh.set_initial_value(5);
  sel_trm2_fsh.set_initial_value(.90);
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  log_avgrec.allocate(1,"log_avgrec");
  log_avginit.allocate(1,"log_avginit");
  log_avg_F.allocate(1,"log_avg_F");
  natmort_phi.allocate(phase_natmort,"natmort_phi");
  natmort.allocate(1,nages,"natmort");
  #ifndef NO_AD_INITIALIZE
    natmort.initialize();
  #endif
  base_natmort.allocate(1,nages,"base_natmort");
  #ifndef NO_AD_INITIALIZE
    base_natmort.initialize();
  #endif
  log_q_bts.allocate(phase_q_bts,"log_q_bts");
  log_q_std_area.allocate(phase_q_std_area,"log_q_std_area");
  bt_slope.allocate(phase_bt,"bt_slope");
 cout <<phase_bt<<" Phase for bts temperature"<<endl;
  log_q_eit.allocate(phase_q_eit,"log_q_eit");
  log_Rzero.allocate(phase_Rzero,"log_Rzero");
  steepness.allocate(0.2,Steepness_UB,phase_steepness,"steepness");
   phase_cpue_q = 1; // always estimate historical CPUE q
   if (n_avo_r < 6 || ctrl_flag(6)==0) phase_avo_q = -1; else phase_avo_q=1;  
  log_q_cpue.allocate(phase_cpue_q,"log_q_cpue");
  log_q_avo.allocate(phase_avo_q,"log_q_avo");
  q_avo.allocate("q_avo");
  #ifndef NO_AD_INITIALIZE
  q_avo.initialize();
  #endif
  q_bts.allocate("q_bts");
  #ifndef NO_AD_INITIALIZE
  q_bts.initialize();
  #endif
  q_eit.allocate("q_eit");
  #ifndef NO_AD_INITIALIZE
  q_eit.initialize();
  #endif
  q_cpue.allocate("q_cpue");
  #ifndef NO_AD_INITIALIZE
  q_cpue.initialize();
  #endif
  temp_coef.allocate(3,"temp_coef");
  log_initdevs.allocate(2,nages,-15.,15.,3,"log_initdevs");
  log_rec_devs.allocate(styr,endyr_r,-10.,10.,phase_rec_devs,"log_rec_devs");
  rec_epsilons.allocate(styr,endyr_r,"rec_epsilons");
  #ifndef NO_AD_INITIALIZE
    rec_epsilons.initialize();
  #endif
  larv_rec_devs.allocate(1,11,1,11,-10.,10.,phase_larv,"larv_rec_devs");
  larv_rec_trans.allocate(1,11,1,11,"larv_rec_trans");
  #ifndef NO_AD_INITIALIZE
    larv_rec_trans.initialize();
  #endif
  alpha.allocate("alpha");
  #ifndef NO_AD_INITIALIZE
  alpha.initialize();
  #endif
  beta.allocate("beta");
  #ifndef NO_AD_INITIALIZE
  beta.initialize();
  #endif
  Rzero.allocate("Rzero");
  #ifndef NO_AD_INITIALIZE
  Rzero.initialize();
  #endif
  q_all.allocate("q_all");
  endyr_N.allocate(1,nages,"endyr_N");
  B_Bnofsh.allocate("B_Bnofsh");
  regime.allocate(1,9,"regime");
  Bzero.allocate("Bzero");
  Percent_Bzero.allocate("Percent_Bzero");
  Percent_Bzero_1.allocate("Percent_Bzero_1");
  Percent_Bzero_2.allocate("Percent_Bzero_2");
  Percent_B100.allocate("Percent_B100");
  Percent_B100_1.allocate("Percent_B100_1");
  Percent_B100_2.allocate("Percent_B100_2");
  q_temp.allocate(1,5,"q_temp");
  phizero.allocate("phizero");
  #ifndef NO_AD_INITIALIZE
  phizero.initialize();
  #endif
  log_F_devs.allocate(styr,endyr_r,-15.,15.,2,"log_F_devs");
  sigr.allocate(0.1,2.,phase_sigr,"sigr");
  sigmaRsq.allocate("sigmaRsq");
  #ifndef NO_AD_INITIALIZE
  sigmaRsq.initialize();
  #endif
  sel_devs_fsh.allocate(1,dim_sel_fsh,1,n_selages_fsh,-5.,5.,phase_selcoffs_fsh_dev,"sel_devs_fsh");
  sel_devs_bts.allocate(1,dim_sel_bts,1,n_selages_bts,-5.,5.,phase_selcoffs_bts_dev,"sel_devs_bts");
  sel_devs_eit.allocate(1,dim_sel_eit,mina_eit,n_selages_eit,-5.,5.,phase_selcoffs_eit_dev,"sel_devs_eit");
  sel_coffs_fsh.allocate(1,n_selages_fsh,phase_selcoffs_fsh,"sel_coffs_fsh");
  sel_coffs_bts.allocate(1,n_selages_bts,phase_selcoffs_bts,"sel_coffs_bts");
  sel_coffs_eit.allocate(mina_eit,n_selages_eit,phase_selcoffs_eit,"sel_coffs_eit");
  wt_fut.allocate(1,nages,wt_fut_phase,"wt_fut");
  sel_slp_bts.allocate(0.001,5.,phase_logist_bts,"sel_slp_bts");
  sel_a50_bts.allocate(0.1,6,phase_logist_bts,"sel_a50_bts");
  sel_age_one.allocate(phase_logist_bts,"sel_age_one");
  sel_slp_bts_dev.allocate(styr_bts,endyr_r,-5,5,phase_logist_bts_dev,"sel_slp_bts_dev");
  sel_a50_bts_dev.allocate(styr_bts,endyr_r,-5,5,phase_logist_bts_dev,"sel_a50_bts_dev");
  sel_one_bts_dev.allocate(styr_bts,endyr_r,-5,5,phase_age1devs_bts,"sel_one_bts_dev");
  sel_dif1_fsh.allocate(phase_logist_fsh,"sel_dif1_fsh");
  sel_a501_fsh.allocate(0.1,7,phase_logist_fsh,"sel_a501_fsh");
  sel_trm2_fsh.allocate(0.0,0.999,phase_logist_fsh,"sel_trm2_fsh");
 int ph_log_fsh2;
 if (phase_logist_fsh>0) ph_log_fsh2 = phase_logist_fsh+1;else ph_log_fsh2 = phase_logist_fsh;
  sel_dif2_fsh.allocate(ph_log_fsh2,"sel_dif2_fsh");
  sel_dif1_fsh_dev.allocate(styr,endyr_r,-5,5,phase_logist_fsh_dev,"sel_dif1_fsh_dev");
  sel_a501_fsh_dev.allocate(styr,endyr_r,-5,5,phase_logist_fsh_dev,"sel_a501_fsh_dev");
  sel_trm2_fsh_dev.allocate(styr,endyr_r,-.5,.5,-2,"sel_trm2_fsh_dev");
  SPR_ABC.allocate("SPR_ABC");
  #ifndef NO_AD_INITIALIZE
  SPR_ABC.initialize();
  #endif
  SPR_OFL.allocate("SPR_OFL");
  F40.allocate("F40");
  F35.allocate("F35");
  SSB.allocate(styr,endyr_r,"SSB");
  sigmarsq_out.allocate("sigmarsq_out");
  #ifndef NO_AD_INITIALIZE
  sigmarsq_out.initialize();
  #endif
  ftmp.allocate("ftmp");
  #ifndef NO_AD_INITIALIZE
  ftmp.initialize();
  #endif
  SB0.allocate("SB0");
  #ifndef NO_AD_INITIALIZE
  SB0.initialize();
  #endif
  SBF40.allocate("SBF40");
  #ifndef NO_AD_INITIALIZE
  SBF40.initialize();
  #endif
  SBF35.allocate("SBF35");
  #ifndef NO_AD_INITIALIZE
  SBF35.initialize();
  #endif
  sprpen.allocate("sprpen");
  #ifndef NO_AD_INITIALIZE
  sprpen.initialize();
  #endif
  F_pen.allocate("F_pen");
  #ifndef NO_AD_INITIALIZE
  F_pen.initialize();
  #endif
  meanrec.allocate("meanrec");
  #ifndef NO_AD_INITIALIZE
  meanrec.initialize();
  #endif
  SR_resids.allocate(styr_est,endyr_est,"SR_resids");
  #ifndef NO_AD_INITIALIZE
    SR_resids.initialize();
  #endif
  Nspr.allocate(1,4,1,nages,"Nspr");
  #ifndef NO_AD_INITIALIZE
    Nspr.initialize();
  #endif
  sel_fut.allocate(1,nages,"sel_fut");
  #ifndef NO_AD_INITIALIZE
    sel_fut.initialize();
  #endif
  natage_future.allocate(1,6,styr_fut,endyr_fut,1,nages,"natage_future");
  #ifndef NO_AD_INITIALIZE
    natage_future.initialize();
  #endif
  rec_dev_future.allocate(styr_fut,endyr_fut,phase_F40,"rec_dev_future");
  F_future.allocate(1,6,styr_fut,endyr_fut,1,nages,"F_future");
  #ifndef NO_AD_INITIALIZE
    F_future.initialize();
  #endif
  Z_future.allocate(styr_fut,endyr_fut,1,nages,"Z_future");
  #ifndef NO_AD_INITIALIZE
    Z_future.initialize();
  #endif
  S_future.allocate(styr_fut,endyr_fut,1,nages,"S_future");
  #ifndef NO_AD_INITIALIZE
    S_future.initialize();
  #endif
  catage_future.allocate(styr_fut,endyr_fut,1,nages,"catage_future");
  #ifndef NO_AD_INITIALIZE
    catage_future.initialize();
  #endif
  avg_rec_dev_future.allocate("avg_rec_dev_future");
  #ifndef NO_AD_INITIALIZE
  avg_rec_dev_future.initialize();
  #endif
  eac_fsh.allocate(1,n_fsh_r,1,nbins,"eac_fsh");
  #ifndef NO_AD_INITIALIZE
    eac_fsh.initialize();
  #endif
  elc_fsh.allocate(1,nlbins,"elc_fsh");
  #ifndef NO_AD_INITIALIZE
    elc_fsh.initialize();
  #endif
  eac_bts.allocate(1,n_bts_r,1,nbins,"eac_bts");
  #ifndef NO_AD_INITIALIZE
    eac_bts.initialize();
  #endif
  eac_cmb.allocate(1,n_bts_r,1,nbins,"eac_cmb");
  #ifndef NO_AD_INITIALIZE
    eac_cmb.initialize();
  #endif
  oac_cmb.allocate(1,n_bts_r,1,nbins,"oac_cmb");
  #ifndef NO_AD_INITIALIZE
    oac_cmb.initialize();
  #endif
  eac_eit.allocate(1,n_eit_ac_r,1,nbins,"eac_eit");
  #ifndef NO_AD_INITIALIZE
    eac_eit.initialize();
  #endif
  ea1_eit.allocate(1,n_eit_ac_r,"ea1_eit");
  #ifndef NO_AD_INITIALIZE
    ea1_eit.initialize();
  #endif
  et_fsh.allocate(1,n_fsh_r,"et_fsh");
  #ifndef NO_AD_INITIALIZE
    et_fsh.initialize();
  #endif
  et_bts.allocate(1,n_bts_r,"et_bts");
  #ifndef NO_AD_INITIALIZE
    et_bts.initialize();
  #endif
  et_cmb.allocate(1,n_bts_r,"et_cmb");
  #ifndef NO_AD_INITIALIZE
    et_cmb.initialize();
  #endif
  avail_bts.allocate(1,n_bts_r,"avail_bts");
  #ifndef NO_AD_INITIALIZE
    avail_bts.initialize();
  #endif
  avail_eit.allocate(1,n_bts_r,"avail_eit");
  #ifndef NO_AD_INITIALIZE
    avail_eit.initialize();
  #endif
  sigma_cmb.allocate(1,n_bts_r,"sigma_cmb");
  #ifndef NO_AD_INITIALIZE
    sigma_cmb.initialize();
  #endif
  var_cmb.allocate(1,n_bts_r,"var_cmb");
  #ifndef NO_AD_INITIALIZE
    var_cmb.initialize();
  #endif
  ot_cmb.allocate(1,n_bts_r,"ot_cmb");
  #ifndef NO_AD_INITIALIZE
    ot_cmb.initialize();
  #endif
  eb_bts.allocate(1,n_bts_r,"eb_bts");
  #ifndef NO_AD_INITIALIZE
    eb_bts.initialize();
  #endif
  eb_eit.allocate(1,n_eit_r,"eb_eit");
  #ifndef NO_AD_INITIALIZE
    eb_eit.initialize();
  #endif
  et_eit.allocate(1,n_eit_r,"et_eit");
  #ifndef NO_AD_INITIALIZE
    et_eit.initialize();
  #endif
  et_avo.allocate(1,n_avo_r,"et_avo");
  #ifndef NO_AD_INITIALIZE
    et_avo.initialize();
  #endif
  et_cpue.allocate(1,n_cpue,"et_cpue");
  #ifndef NO_AD_INITIALIZE
    et_cpue.initialize();
  #endif
  Fmort.allocate(styr,endyr_r,"Fmort");
  #ifndef NO_AD_INITIALIZE
    Fmort.initialize();
  #endif
  catage.allocate(styr,endyr_r,1,nages,"catage");
  #ifndef NO_AD_INITIALIZE
    catage.initialize();
  #endif
  pred_catch.allocate(styr,endyr_r,"pred_catch");
  #ifndef NO_AD_INITIALIZE
    pred_catch.initialize();
  #endif
  Pred_N_bts.allocate(styr,endyr_r,"Pred_N_bts");
  #ifndef NO_AD_INITIALIZE
    Pred_N_bts.initialize();
  #endif
  Pred_N_eit.allocate(styr,endyr_r,"Pred_N_eit");
  #ifndef NO_AD_INITIALIZE
    Pred_N_eit.initialize();
  #endif
  pred_cpue.allocate(1,n_cpue,"pred_cpue");
  #ifndef NO_AD_INITIALIZE
    pred_cpue.initialize();
  #endif
  pred_avo.allocate(1,n_avo,"pred_avo");
  #ifndef NO_AD_INITIALIZE
    pred_avo.initialize();
  #endif
  natage.allocate(styr,endyr_r,1,nages,"natage");
  #ifndef NO_AD_INITIALIZE
    natage.initialize();
  #endif
  srmod_rec.allocate(styr_est,endyr_est,"srmod_rec");
  #ifndef NO_AD_INITIALIZE
    srmod_rec.initialize();
  #endif
  Z.allocate(styr,endyr_r,1,nages,"Z");
  #ifndef NO_AD_INITIALIZE
    Z.initialize();
  #endif
  F.allocate(styr,endyr_r,1,nages,"F");
  #ifndef NO_AD_INITIALIZE
    F.initialize();
  #endif
  S.allocate(styr,endyr_r,1,nages,"S");
  #ifndef NO_AD_INITIALIZE
    S.initialize();
  #endif
  log_sel_fsh.allocate(styr,endyr_r,1,nages,"log_sel_fsh");
  #ifndef NO_AD_INITIALIZE
    log_sel_fsh.initialize();
  #endif
  sel_fsh.allocate(styr,endyr_r,1,nages,"sel_fsh");
  #ifndef NO_AD_INITIALIZE
    sel_fsh.initialize();
  #endif
  log_sel_bts.allocate(styr,endyr_r,1,nages,"log_sel_bts");
  #ifndef NO_AD_INITIALIZE
    log_sel_bts.initialize();
  #endif
  log_sel_eit.allocate(styr,endyr_r,1,nages,"log_sel_eit");
  #ifndef NO_AD_INITIALIZE
    log_sel_eit.initialize();
  #endif
  ff.allocate("ff");
  #ifndef NO_AD_INITIALIZE
  ff.initialize();
  #endif
  ssqcatch.allocate("ssqcatch");
  #ifndef NO_AD_INITIALIZE
  ssqcatch.initialize();
  #endif
  avgsel_fsh.allocate("avgsel_fsh");
  #ifndef NO_AD_INITIALIZE
  avgsel_fsh.initialize();
  #endif
  avgsel_bts.allocate("avgsel_bts");
  #ifndef NO_AD_INITIALIZE
  avgsel_bts.initialize();
  #endif
  avgsel_eit.allocate("avgsel_eit");
  #ifndef NO_AD_INITIALIZE
  avgsel_eit.initialize();
  #endif
  bzero.allocate("bzero");
  #ifndef NO_AD_INITIALIZE
  bzero.initialize();
  #endif
  surv.allocate("surv");
  #ifndef NO_AD_INITIALIZE
  surv.initialize();
  #endif
  nthisage.allocate("nthisage");
  #ifndef NO_AD_INITIALIZE
  nthisage.initialize();
  #endif
  surv_like.allocate(1,3,"surv_like");
  #ifndef NO_AD_INITIALIZE
    surv_like.initialize();
  #endif
  cpue_like.allocate("cpue_like");
  #ifndef NO_AD_INITIALIZE
  cpue_like.initialize();
  #endif
  avo_like.allocate("avo_like");
  #ifndef NO_AD_INITIALIZE
  avo_like.initialize();
  #endif
  sel_like.allocate(1,3,"sel_like");
  #ifndef NO_AD_INITIALIZE
    sel_like.initialize();
  #endif
  sel_like_dev.allocate(1,3,"sel_like_dev");
  #ifndef NO_AD_INITIALIZE
    sel_like_dev.initialize();
  #endif
  age_like.allocate(1,ngears,"age_like");
  #ifndef NO_AD_INITIALIZE
    age_like.initialize();
  #endif
  len_like.allocate("len_like");
  #ifndef NO_AD_INITIALIZE
  len_like.initialize();
  #endif
  age_like_offset.allocate(1,ngears,"age_like_offset");
  #ifndef NO_AD_INITIALIZE
    age_like_offset.initialize();
  #endif
  MN_const.allocate("MN_const");
  #ifndef NO_AD_INITIALIZE
  MN_const.initialize();
  #endif
 MN_const = 1e-3; // Multinomial constant
  Priors.allocate(1,4,"Priors");
  #ifndef NO_AD_INITIALIZE
    Priors.initialize();
  #endif
  rec_like.allocate(1,7,"rec_like");
  #ifndef NO_AD_INITIALIZE
    rec_like.initialize();
  #endif
  sumtmp.allocate("sumtmp");
  #ifndef NO_AD_INITIALIZE
  sumtmp.initialize();
  #endif
  tmpsp.allocate("tmpsp");
  #ifndef NO_AD_INITIALIZE
  tmpsp.initialize();
  #endif
  log_initage.allocate(2,nages,"log_initage");
  #ifndef NO_AD_INITIALIZE
    log_initage.initialize();
  #endif
  pred_biom.allocate(styr,endyr_r,"pred_biom");
  #ifndef NO_AD_INITIALIZE
    pred_biom.initialize();
  #endif
  fake_SSB.allocate(1,20,"fake_SSB");
  #ifndef NO_AD_INITIALIZE
    fake_SSB.initialize();
  #endif
  avg_age_msy.allocate("avg_age_msy");
  avgwt_msy.allocate("avgwt_msy");
  MSY.allocate("MSY");
  Fmsy.allocate("Fmsy");
  Fmsy2.allocate("Fmsy2");
  lnFmsy.allocate("lnFmsy");
  lnFmsy2.allocate("lnFmsy2");
  SER_Fmsy.allocate("SER_Fmsy");
  Fendyr_Fmsy.allocate("Fendyr_Fmsy");
  Rmsy.allocate("Rmsy");
  Bmsy.allocate("Bmsy");
  Bmsy2.allocate("Bmsy2");
  Bcur_Bmsy.allocate("Bcur_Bmsy");
  F40_spb.allocate("F40_spb");
  F40_catch.allocate("F40_catch");
  begbiom.allocate("begbiom");
  DepletionSpawners.allocate("DepletionSpawners");
  SB100.allocate("SB100");
  Current_Spawners.allocate("Current_Spawners");
  pred_rec.allocate(styr,endyr_r,"pred_rec");
  age_3_plus_biom.allocate(styr,endyr_r+2,"age_3_plus_biom");
  ABC_biom.allocate(1,10,"ABC_biom");
  ABC_biom2.allocate(1,10,"ABC_biom2");
  rechat.allocate(1,20,"rechat");
  SER.allocate(styr,endyr_r,"SER");
  SER_future.allocate(1,6,styr_fut,endyr_fut,"SER_future");
  #ifndef NO_AD_INITIALIZE
    SER_future.initialize();
  #endif
  catch_future.allocate(1,6,styr_fut,endyr_fut,"catch_future");
  #ifndef NO_AD_INITIALIZE
    catch_future.initialize();
  #endif
  future_SSB.allocate(1,6,endyr_r,endyr_fut,"future_SSB");
  age_1_7_biomass.allocate(styr,endyr_r,"age_1_7_biomass");
  #ifndef NO_AD_INITIALIZE
    age_1_7_biomass.initialize();
  #endif
  fff.allocate("fff");
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
}

void model_parameters::preliminary_calculations(void)
{

#if defined(USE_ADPVM)

  admaster_slave_variable_interface(*this);

#endif
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
}

void model_parameters::set_runtime(void)
{
  dvector temp1("{50,50,350,500,5000}");
  maximum_function_evaluations.allocate(temp1.indexmin(),temp1.indexmax());
  maximum_function_evaluations=temp1;
  dvector temp("{.001,.0001,.0001,1e-7}");
  convergence_criteria.allocate(temp.indexmin(),temp.indexmax());
  convergence_criteria=temp;
}

void model_parameters::userfunction(void)
{
  fff =0.0;
  ofstream& srecout= *pad_srecout;
  ofstream& projout= *pad_projout;
  ofstream& nofish= *pad_nofish;
  ofstream& projout2= *pad_projout2;
  ofstream& eval= *pad_eval;
  random_number_generator& rng= *pad_rng;
}

void model_parameters::WriteNewData(void)
{
  ofstream& srecout= *pad_srecout;
  ofstream& projout= *pad_projout;
  ofstream& nofish= *pad_nofish;
  ofstream& projout2= *pad_projout2;
  ofstream& eval= *pad_eval;
  random_number_generator& rng= *pad_rng;
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
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{
  delete pad_srecout;
  pad_srecout = NULL;
  delete pad_projout;
  pad_projout = NULL;
  delete pad_nofish;
  pad_nofish = NULL;
  delete pad_projout2;
  pad_projout2 = NULL;
  delete pad_eval;
  pad_eval = NULL;
  delete pad_rng;
  pad_rng = NULL;
}

void model_parameters::report(const dvector& gradients){}

void model_parameters::final_calcs(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
  gradient_structure::set_MAX_NVAR_OFFSET(1600);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(200000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(8100); 
  gradient_structure::set_CMPDIF_BUFFER_SIZE(2000000);
  arrmblsize=10000000;
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
    if (!arrmblsize) arrmblsize=15000000;
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
