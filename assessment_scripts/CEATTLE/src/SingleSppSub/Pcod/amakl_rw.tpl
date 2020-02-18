///////////////////////////////////////////////////////////////////
// AMAK--              
// Naming Conventions:
//
//  GENERAL:
//    styr, endyr begining year and ending year of model (catch data available)
//    nages       number of age groups considered
//    nyrs_        number of observations available to specific data set
//
//  DATA SPECIFIC:

//    catch_bio   Observed catch biomass
//    fsh        fishery data
//
//  Define indices
//    nind        number of indices
//  Index values
//    nyrs_ind      Number of years of index value (annual)
//    yrs_ind        Years of index value (annual)
//    obs_ind        Observed index value (annual)
//    obs_se_ind    Observed index standard errors (annual)
//  Age-comp values
//    nyrs_ind_age  Number of years index age data available
//    yrs_ind_age   Years of index age value (annual)
//    oac_ind       Observed age comp from index
//    n_sample_ind_age    Observed age comp sample sizes from index
//
//    eac_ind       Expected age comp from index
//
//    sel_ind       selectivity for egg production index
//
//    pred_ind ...
//
//    oac_fsh      Observed age comp from index
//    obs_ind_size  Observed size comp from index
//
//    pred_fsh_age    Predicted age comp from fishery data
//    eac_fsh            Expected age comp for fishery data (only years where data available)
//    eac_ ...
//
//    pred_tmp_ind   Predicted index value for trawl index
//
//    sel_fsh    selectivity for fishery                
//  
//     sel_ch indicates time-varying selectivity change
//  
//    Add bit for historical F
//    Added length part for selectivity
//
//////////////////////////////////////////////////////////////////////////////
 // To ADD/FIX:
 //   parameterization of steepness to work the same (wrt prior) for ricker and bholt
 //   splines for selectivity
 //   two projection outputs need consolidation
//////////////////////////////////////////////////////////////////////////////

DATA_SECTION
  !!version_info+="Amak.1;July_2012";
  int iseed 
  !! iseed=1313;
  int cmp_no // candidate management procedure
  int nnodes_tmp;
  !!CLASS ofstream mceval("mceval.dat")
  !!long int lseed=iseed;
  !!CLASS random_number_generator rng(iseed);
  
  int oper_mod
  int mcmcmode
  int mcflag

  !! oper_mod = 0;
  !! mcmcmode = 0;
  !! mcflag   = 1;
 LOCAL_CALCS
  write_input_log<<version_info<<endl;
  tmpstring=adprogram_name + adstring(".dat");
  int on=0;
  if ( (on=option_match(argc,argv,"-ind"))>-1)
  {
    if (on>argc-2 | argv[on+1][0] == '-') 
    { 
      cerr << "Invalid input data command line option"
         " -- ignored" << endl;  
    }
    else
    {
      cntrlfile_name = adstring(argv[on+1]);
    }
  }
  else
  {
      cntrlfile_name =   tmpstring;
  }
  if ( (on=option_match(argc,argv,"-om"))>-1)
  {
    oper_mod  = 1;
    cmp_no = atoi(argv[on+1]);
    cout<<"Got to operating model option "<<oper_mod<<endl;
  }
  if ( (on=option_match(argc,argv,"-mcmc"))>-1)
  {
    mcmcmode = 1;
  }
  global_datafile= new cifstream(cntrlfile_name);
  if (!global_datafile)
  {
  }
  else
  {
    if (!(*global_datafile))
    {
      delete global_datafile;
      global_datafile=NULL;
    }
  }
 END_CALCS
 // Read in "name" of this model...
  !! *(ad_comm::global_datafile) >>  datafile_name; // First line is datafile (not used by this executable)
  !! *(ad_comm::global_datafile) >>  model_name; 
  !! ad_comm::change_datafile_name(datafile_name);
  init_int styr
  init_int endyr
  init_int rec_age
  init_int oldest_age
  !! log_input(styr);
  !! log_input(endyr);
  !! log_input(rec_age);
  !! log_input(oldest_age);
  //------------LENGTH INTERVALS
  init_int nlength
  init_vector len_bins(1,nlength)
  !! log_input(nlength);
  !! log_input(len_bins);

  int nages
  !!  nages = oldest_age - rec_age + 1;
  int styr_rec
  int styr_sp
  int endyr_sp
  int nyrs
  !! nyrs          = endyr - styr + 1;
  int mc_count;
  !!  mc_count=0;
  !! styr_rec = (styr - nages) + 1;     // First year of recruitment
  !! styr_sp  = styr_rec - rec_age - 1 ;    // First year of spawning biomass  
  vector yy(styr,endyr);
  !! yy.fill_seqadd(styr,1) ;
  vector aa(1,nages);
  !! aa.fill_seqadd(rec_age,1) ;
  int junk;
  // Fishery specifics
  init_int nfsh                                   //Number of fisheries
  imatrix pfshname(1,nfsh,1,2)
  init_adstring fshnameread;
 LOCAL_CALCS
  for(k=1;k<=nfsh;k++) 
  {
    pfshname(k,1)=1; 
    pfshname(k,2)=1;
  }    // set whole array to equal 1 in case not enough names are read
  adstring_array CRLF;   // blank to terminate lines
  CRLF+="";
  k=1;
  for(i=1;i<=strlen(fshnameread);i++)
  if(adstring(fshnameread(i))==adstring("%")) {
    pfshname(k,2)=i-1; 
    k++;  
    pfshname(k,1)=i+1;
  }
  pfshname(nfsh,2)=strlen(fshnameread);
  for(k=1;k<=nfsh;k++)
  {
    fshname += fshnameread(pfshname(k,1),pfshname(k,2))+CRLF(1);
  }
  log_input(datafile_name);
  log_input(model_name);
  log_input(styr);
  log_input(endyr);
  log_input(rec_age);
  log_input(oldest_age);
  log_input(nfsh);
  log_input(fshname);
 END_CALCS
  init_matrix catch_bio_in(1,nfsh,styr,endyr)
  init_matrix catch_bio_sd_in(1,nfsh,styr,endyr)   // Specify catch-estimation precision
  // !! for (i=1;i<=nfsh;i++) catch_bio(i) += .01; 
  !! log_input(catch_bio_in);
  !! log_input(catch_bio_sd_in);
  //  Define fishery age compositions
  init_ivector nyrs_fsh_age(1,nfsh)
  !! log_input(nyrs_fsh_age);
  init_ivector nyrs_fsh_length(1,nfsh)
  !! log_input(nyrs_fsh_length);
  init_imatrix yrs_fsh_age_in(1,nfsh,1,nyrs_fsh_age)
  !! log_input(yrs_fsh_age_in);
  init_imatrix yrs_fsh_length_in(1,nfsh,1,nyrs_fsh_length)
  !! log_input(yrs_fsh_length_in);
  init_matrix n_sample_fsh_age_in(1,nfsh,1,nyrs_fsh_age)    //Years of index index value (annual)
  init_matrix n_sample_fsh_length_in(1,nfsh,1,nyrs_fsh_length)    //Years of index index value (annual)
  !! log_input(n_sample_fsh_length_in);
  init_3darray oac_fsh_in(1,nfsh,1,nyrs_fsh_age,1,nages)
  init_3darray olc_fsh_in(1,nfsh,1,nyrs_fsh_length,1,nlength)
  !! log_input(olc_fsh_in);
  init_3darray wt_fsh(1,nfsh,styr,endyr,1,nages)  //values of weights at age
  //  Define indices
  init_int nind                                   //number of indices
  !! log_input(nind);
  int nfsh_and_ind
  !! nfsh_and_ind = nfsh+nind;
  imatrix pindname(1,nind,1,2)
  init_adstring indnameread;
 LOCAL_CALCS
  for(int k=1;k<=nind;k++) 
  {
    pindname(k,1)=1; 
    pindname(k,2)=1;
  }    // set whole array to equal 1 in case not enough names are read
  int k=1;
  for(i=1;i<=strlen(indnameread);i++)
  if(adstring(indnameread(i))==adstring("%")) {
    pindname(k,2)=i-1; 
    k++;  
    pindname(k,1)=i+1;
  }
  pindname(nind,2)=strlen(indnameread);
  for(k=1;k<=nind;k++)
  {
    indname += indnameread(pindname(k,1),pindname(k,2))+CRLF(1);
  }
  log_input(indname);
 END_CALCS
  //  Index values
  init_ivector nyrs_ind(1,nind)                   //Number of years of index value (annual)
  init_imatrix yrs_ind_in(1,nind,1,nyrs_ind)         //Years of index value (annual)
  init_vector mo_ind(1,nind)                      //Month occur 
  init_matrix obs_ind_in(1,nind,1,nyrs_ind)          //values of index value (annual)
  init_matrix obs_se_ind_in(1,nind,1,nyrs_ind)       //values of indices serrs

  vector ind_month_frac(1,nind)
  !! log_input(nyrs_ind);
  !! log_input(yrs_ind_in);
  !! log_input(mo_ind);
  !! ind_month_frac = (mo_ind-1.)/12.;
  !! log_input(obs_ind_in);
  !! log_input(obs_se_ind_in);
  matrix        corr_dev(1,nind,1,nyrs_ind) //Index standard errors (for lognormal)
  matrix        corr_eff(1,nfsh,styr,endyr) //Index standard errors (for lognormal)
  matrix         act_eff(1,nfsh,styr,endyr) //Index standard errors (for lognormal)
  vector              ac(1,nind);

  init_ivector nyrs_ind_age(1,nind)               //Number of years of index value (annual)
  !! log_input(nyrs_ind_age);

  init_ivector nyrs_ind_length(1,nfsh)
  !! log_input(nyrs_ind_length);

  init_imatrix yrs_ind_age_in(1,nind,1,nyrs_ind_age)  //Years of index value (annual)
  !! log_input(yrs_ind_age_in);

  init_imatrix yrs_ind_length_in(1,nind,1,nyrs_ind_length)
  !! log_input(yrs_ind_length_in);

  init_matrix n_sample_ind_age_in(1,nind,1,nyrs_ind_age)         //Years of index value (annual)
  !! log_input(yrs_ind_age_in);

  init_matrix n_sample_ind_length_in(1,nind,1,nyrs_ind_length)         //Years of index lengths (annual)
  !! log_input(n_sample_ind_length_in);

  init_3darray oac_ind_in(1,nind,1,nyrs_ind_age,1,nages)  //values of Index proportions at age
  init_3darray olc_ind_in(1,nind,1,nyrs_ind_length,1,nlength)
  !! log_input(olc_ind_in);

  !! log_input(oac_ind_in);
  init_3darray  wt_ind(1,nind,styr,endyr,1,nages)      //values of Index proportions at age
  !! log_input(wt_ind);

  vector age_vector(1,nages);
  !! for (j=1;j<=nages;j++)
  !!  age_vector(j) = double(j+rec_age-1);
  init_vector wt_pop(1,nages)
  !! log_input(wt_pop);
  init_vector maturity(1,nages)
  !! log_input(maturity);
  !! if (max(maturity)>.9) maturity /=2.;
  vector wt_mature(1,nages);
  !! wt_mature = elem_prod(wt_pop,maturity) ;

  //Spawning month-----
  init_number spawnmo
  number spmo_frac
  !! spmo_frac = (spawnmo-1)/12.;

  init_matrix age_err(1,nages,1,nages)
  !! log_input(age_err);

  int k // Index for fishery or index
  int i // Index for year
  int j // Index for age
 LOCAL_CALCS
  // Rename data file to the control data section... 
  ad_comm::change_datafile_name(cntrlfile_name);
  *(ad_comm::global_datafile) >>  datafile_name; 
  *(ad_comm::global_datafile) >>  model_name; 
  log_input(cntrlfile_name);
  log_input(model_name);
 END_CALCS
  // Matrix of selectivity mappings--row 1 is type (1=fishery, 2=index) and row 2 is index within that type
  //  e.g., the following for 2 fisheries and 4 indices means that index 3 uses fishery 1 selectivities,
  //         the other fisheries and indices use their own parameterization
  //  1 1 2 2 1 2 
  //  1 2 1 2 1 4
  init_imatrix sel_map(1,2,1,nfsh_and_ind) 
  // maps fisheries and indices into sequential sel_map for sharing purposes

  init_int    SrType        // 2 Bholt, 1 Ricker
  init_int use_age_err      // nonzero value means use...
  init_int retro            // Retro years to peel off (0 means full dataset)
 LOCAL_CALCS
  log_input(sel_map);
  log_input(datafile_name);
  log_input(model_name);
  projfile_name = cntrlfile_name(1,length(cntrlfile_name)-4) + ".prj";
  log_input(SrType);
  log_input(use_age_err);
  log_input(retro);
 END_CALCS
  init_number steepnessprior
  init_number cvsteepnessprior
  init_int    phase_srec

  init_number sigmarprior
  number log_sigmarprior
  init_number cvsigmarprior
  init_int    phase_sigmar
  !! log_input(sigmarprior);
  !! log_input(cvsigmarprior);
  !! log_input(phase_sigmar);
  init_int    styr_rec_est
  init_int    endyr_rec_est
  !! log_input(styr_rec_est);
  !! log_input(endyr_rec_est);
  int nrecs_est;
  //-----GROWTH PARAMETERS--------------------------------------------------
  init_number Linfprior
  init_number cvLinfprior
  init_int    phase_Linf
  number log_Linfprior
  !! log_Linfprior = log(Linfprior);
  !! log_input(Linfprior)

  init_number kprior
  init_number cvkprior
  init_int    phase_k
  number log_kprior
  !! log_kprior = log(kprior);
  !! log_input(kprior)

  init_number Loprior
  init_number cvLoprior
  init_int    phase_Lo
  number log_Loprior
  !! log_Loprior = log(Loprior);
  !! log_input(Loprior)

  init_number sdageprior
  init_number cvsdageprior
  init_int    phase_sdage
  number log_sdageprior
  !! log_sdageprior = log(sdageprior);
  !! log_input(sdageprior)
  //---------------------------------------------------------------------------
  // Basic M
  init_number natmortprior
  init_number cvnatmortprior
  init_int    phase_M
  !! log_input(natmortprior);
  !! log_input(cvnatmortprior);
  !! log_input(phase_M);

  // age-specific M
  init_int     npars_Mage
  init_ivector ages_M_changes(1,npars_Mage)
  init_vector  Mage_in(1,npars_Mage)
  init_int     phase_Mage
  vector       Mage_offset_in(1,npars_Mage)
  // convert inputs to offsets from prior for initialization purposes
  !! if (npars_Mage>0) Mage_offset_in = log(Mage_in / natmortprior);
  !! log_input(npars_Mage);
  !! log_input(ages_M_changes);
  !! log_input(Mage_in);
  !! log_input(Mage_offset_in);

  // time-varying M
  init_int    phase_rw_M
  init_int npars_rw_M
  init_ivector  yrs_rw_M(1,npars_rw_M);
  init_vector sigma_rw_M(1,npars_rw_M)
 LOCAL_CALCS
  log_input(phase_rw_M);
  log_input(npars_rw_M);
  log_input(yrs_rw_M);
  log_input(sigma_rw_M);
 END_CALCS

  init_vector qprior(1,nind)      
  vector log_qprior(1,nind)      
  init_vector cvqprior(1,nind)     
  init_ivector phase_q(1,nind)
  !! log_input(qprior);
  !! log_input(cvqprior);
  !! log_input(phase_q);

  init_vector q_power_prior(1,nind)      
  vector log_q_power_prior(1,nind)      
  init_vector cvq_power_prior(1,nind)     
  init_ivector phase_q_power(1,nind)
  // Random walk definition for indices
  init_ivector phase_rw_q(1,nind)
  init_ivector npars_rw_q(1,nind)
  init_imatrix  yrs_rw_q(1,nind,1,npars_rw_q); // Ragged array
  init_matrix sigma_rw_q(1,nind,1,npars_rw_q); // Ragged array
 LOCAL_CALCS
  log_input(phase_rw_q);
  log_input(npars_rw_q);
  log_input(yrs_rw_q);
  log_input(sigma_rw_q);
 END_CALCS

  init_ivector    q_age_min(1,nind)     // Age that q relates to...
  init_ivector    q_age_max(1,nind)     // Age that q relates to...
  !! log_input(q_age_min);
  !! log_input(q_age_max);
  // Need to map to age index range...
  !! for (k=1;k<=nind;k++) {q_age_min(k) =  q_age_min(k) - rec_age + 1; q_age_max(k) = q_age_max(k) - rec_age + 1;}
  !! log_input(q_age_min);
  !! log_input(q_age_max);

  init_number cv_catchbiomass
  !! log_input(cv_catchbiomass);
  number catchbiomass_pen
  !!catchbiomass_pen= 1./(2*cv_catchbiomass*cv_catchbiomass);
  init_int nproj_yrs
  !! log_input(nproj_yrs);

  int styr_fut
  int endyr_fut            // LAst year for projections
  int phase_Rzero
  int phase_nosr
  number Steepness_UB
  !! phase_Rzero =  4;
  !! phase_nosr  = -3;

  // Selectivity controls
  // read in options for each fishery
  // Loop over fisheries and indices to read in data (conditional on sel_options)
  ivector   fsh_sel_opt(1,nfsh)
  ivector phase_sel_fsh(1,nfsh)
  vector   curv_pen_fsh(1,nfsh)
  matrix   sel_slp_in_fsh(1,nfsh,1,nyrs)
  matrix   logsel_slp_in_fsh(1,nfsh,1,nyrs)
  matrix   sel_inf_in_fsh(1,nfsh,1,nyrs)
  vector   logsel_slp_in_fshv(1,nfsh)
  vector   sel_inf_in_fshv(1,nfsh)
  vector   logsel_dslp_in_fshv(1,nfsh)
  vector   sel_dinf_in_fshv(1,nfsh)
  matrix   sel_dslp_in_fsh(1,nfsh,1,nyrs)
  matrix   logsel_dslp_in_fsh(1,nfsh,1,nyrs)
  matrix   sel_dinf_in_fsh(1,nfsh,1,nyrs)

  vector seldec_pen_fsh(1,nfsh) ;
  vector nnodes_fsh(1,nfsh) ;
  int seldecage ;
  !! seldecage = int(nages/2);
  ivector nselages_in_fsh(1,nfsh)

  ivector n_sel_ch_fsh(1,nfsh);
  ivector n_sel_ch_ind(1,nind);
  imatrix yrs_sel_ch_tmp(1,nind,1,endyr-styr+1);
  imatrix yrs_sel_ch_tmp_ind(1,nind,1,endyr-styr+1);
  !! yrs_sel_ch_tmp_ind.initialize();

  ivector   ind_sel_opt(1,nind)
  ivector phase_sel_ind(1,nind)

  vector   curv_pen_ind(1,nind)

  matrix   logsel_slp_in_ind(1,nind,1,nyrs)
  matrix   sel_inf_in_ind(1,nind,1,nyrs)
  matrix   sel_dslp_in_ind(1,nind,1,nyrs)
  matrix   logsel_dslp_in_ind(1,nind,1,nyrs)
  matrix   sel_dinf_in_ind(1,nind,1,nyrs)
  matrix   sel_slp_in_ind(1,nind,1,nyrs)

  vector   logsel_slp_in_indv(1,nind)
  vector   sel_inf_in_indv(1,nind)
  vector   logsel_dslp_in_indv(1,nind)
  vector   sel_dinf_in_indv(1,nind)


  vector seldec_pen_ind(1,nind) ;
  matrix sel_change_in_ind(1,nind,styr,endyr);
  ivector nselages_in_ind(1,nind)
  matrix sel_change_in_fsh(1,nfsh,styr,endyr);
  imatrix yrs_sel_ch_fsh(1,nfsh,1,endyr-styr);
  matrix sel_sigma_fsh(1,nfsh,1,endyr-styr);
  imatrix yrs_sel_ch_ind(1,nind,1,endyr-styr);
  matrix sel_sigma_ind(1,nind,1,endyr-styr);

  // Phase of estimation
  ivector phase_selcoff_fsh(1,nfsh)
  ivector phase_logist_fsh(1,nfsh)
  ivector phase_dlogist_fsh(1,nfsh)
  ivector phase_sel_spl_fsh(1,nfsh)

  ivector phase_selcoff_ind(1,nind)
  ivector phase_logist_ind(1,nind)
  ivector phase_dlogist_ind(1,nind)
  vector  sel_fsh_tmp(1,nages); 
  vector  sel_ind_tmp(1,nages); 
  3darray log_selcoffs_fsh_in(1,nfsh,1,nyrs,1,nages)
  3darray log_selcoffs_ind_in(1,nind,1,nyrs,1,nages)
  3darray  log_sel_spl_fsh_in(1,nfsh,1,nyrs,1,nages) // use nages for input to start
  // 3darray log_selcoffs_ind_in(1,nind,1,nyrs,1,nages)
  ivector nopt_fsh(1,2) // number of options...

 LOCAL_CALCS
  logsel_slp_in_fshv.initialize();
  sel_inf_in_fshv.initialize();
  logsel_dslp_in_fshv.initialize();
  sel_inf_in_fshv.initialize();
  sel_dinf_in_fshv.initialize();

  sel_inf_in_indv.initialize();
  logsel_dslp_in_indv.initialize();
  sel_inf_in_indv.initialize();
  sel_dinf_in_indv.initialize();

  phase_selcoff_ind.initialize();
  phase_logist_ind.initialize();
  phase_dlogist_ind.initialize();
  sel_fsh_tmp.initialize() ;
  sel_ind_tmp.initialize() ;
  log_selcoffs_fsh_in.initialize();
  log_selcoffs_ind_in.initialize();

  // nselages_in_fsh.initialize()   ;  
  // nselages_in_ind.initialize()   ;  
  nselages_in_fsh = nages-1;
  nselages_in_ind = nages-1;
  sel_change_in_ind.initialize()   ;  
  sel_slp_in_fsh.initialize()   ;  // ji
  sel_slp_in_ind.initialize()   ;  // ji
  sel_inf_in_fsh.initialize()   ;  // ji
  sel_inf_in_ind.initialize()   ;  // ji
  logsel_slp_in_fsh.initialize();  // ji
  logsel_slp_in_fshv.initialize();  // ji
  logsel_dslp_in_fsh.initialize(); // ji
  logsel_slp_in_ind.initialize();  // ji
  logsel_slp_in_indv.initialize();  // ji
  logsel_dslp_in_ind.initialize(); // ji
  sel_change_in_fsh.initialize()   ;  
  for (k=1;k<=nfsh;k++)
  {
    log_input(fsh_sel_opt(k));
    *(ad_comm::global_datafile) >> fsh_sel_opt(k)  ;  
    log_input(fsh_sel_opt(k));
    switch (fsh_sel_opt(k))
    {
      case 1 : // Selectivity coefficients 
      {
        *(ad_comm::global_datafile) >> nselages_in_fsh(k)   ;  
        log_input(nselages_in_fsh(k));
        *(ad_comm::global_datafile) >> phase_sel_fsh(k);  
        log_input(phase_sel_fsh(k));
        *(ad_comm::global_datafile) >> curv_pen_fsh(k) ;
        log_input(curv_pen_fsh(k));
        *(ad_comm::global_datafile) >> seldec_pen_fsh(k) ;
        log_input(seldec_pen_fsh(k));
        seldec_pen_fsh(k) *= seldec_pen_fsh(k) ;
        *(ad_comm::global_datafile) >>  n_sel_ch_fsh(k) ;  
        n_sel_ch_fsh(k) +=1;
        yrs_sel_ch_fsh(k,1) = styr; // first year always estimated
        for (int i=2;i<=n_sel_ch_fsh(k);i++)
          *(ad_comm::global_datafile) >>  yrs_sel_ch_fsh(k,i) ;  
        for (int i=2;i<=n_sel_ch_fsh(k);i++)
          *(ad_comm::global_datafile) >>  sel_sigma_fsh(k,i) ;  
        log_input(nselages_in_fsh(k)) ;  
        log_input(phase_sel_fsh(k)) ;  
        log_input(curv_pen_fsh(k)) ;  
        log_input(seldec_pen_fsh(k)) ;  
        log_input(n_sel_ch_fsh(k)) ;  
        log_input(yrs_sel_ch_fsh(k)) ;  
        log_input(sel_sigma_fsh(k)) ;  
        // for (int i=styr;i<=endyr;i++) *(ad_comm::global_datafile) >> sel_change_in_fsh(k,i) ;
        sel_change_in_fsh(k,styr)=1.; 
       // Number of selectivity changes is equal to the number of vectors (yr 1 is baseline)
        // This to read in pre-specified selectivity values...
        sel_fsh_tmp.initialize();
        log_selcoffs_fsh_in.initialize();
        for (int j=1;j<=nages;j++) 
          *(ad_comm::global_datafile) >> sel_fsh_tmp(j);  
        for (int jj=2;jj<=n_sel_ch_fsh(k);jj++) 
        {
          // Set the selectivity for the oldest group
          for (int j=nselages_in_fsh(k)+1;j<=nages;j++) 
          {
            sel_fsh_tmp(j)  = sel_fsh_tmp(nselages_in_fsh(k));  
          }
          // Set tmp to actual initial vectors...
          log_selcoffs_fsh_in(k,jj)(1,nselages_in_fsh(k)) = log((sel_fsh_tmp(1,nselages_in_fsh(k))+1e-7)/mean(sel_fsh_tmp(1,nselages_in_fsh(k))+1e-7) );
          write_input_log<<"Sel_in_fsh "<< mfexp(log_selcoffs_fsh_in(k,jj))<<endl;
        }
        // exit(1);
        phase_selcoff_fsh(k) = phase_sel_fsh(k);
        phase_logist_fsh(k)  = -1;
        phase_dlogist_fsh(k) = -1;
        phase_sel_spl_fsh(k) = -1;
      }
        break;
      case 2 : // Single logistic
      {
        *(ad_comm::global_datafile) >> phase_sel_fsh(k);  
        *(ad_comm::global_datafile) >>  n_sel_ch_fsh(k) ;  
        n_sel_ch_fsh(k) +=1;
        yrs_sel_ch_fsh(k,1) = styr;
        for (int i=2;i<=n_sel_ch_fsh(k);i++)
          *(ad_comm::global_datafile) >>  yrs_sel_ch_fsh(k,i) ;  
        for (int i=2;i<=n_sel_ch_fsh(k);i++)
          *(ad_comm::global_datafile) >>  sel_sigma_fsh(k,i) ;  
        // This to read in pre-specified selectivity values...
        *(ad_comm::global_datafile) >> sel_slp_in_fsh(k,1) ;
        *(ad_comm::global_datafile) >> sel_inf_in_fsh(k,1) ;
        logsel_slp_in_fsh(k,1)   = log(sel_slp_in_fsh(k,1)) ;
        for (int jj=2;jj<=n_sel_ch_fsh(k);jj++) 
        {
          sel_inf_in_fsh(k,jj)    =     sel_inf_in_fsh(k,1) ;
          logsel_slp_in_fsh(k,jj) = log(sel_slp_in_fsh(k,1)) ;
        }
        log_input(phase_sel_fsh(k));
        log_input(n_sel_ch_fsh(k));
        log_input(sel_slp_in_fsh(k)(1,n_sel_ch_fsh(k)));
        log_input(sel_inf_in_fsh(k)(1,n_sel_ch_fsh(k)));
        log_input(logsel_slp_in_fsh(k)(1,n_sel_ch_fsh(k)));
        log_input(yrs_sel_ch_fsh(k)(1,n_sel_ch_fsh(k)));

        phase_selcoff_fsh(k) = -1;
        phase_logist_fsh(k) = phase_sel_fsh(k);
        phase_dlogist_fsh(k) = -1;
        phase_sel_spl_fsh(k) = -1;

        logsel_slp_in_fshv(k) = logsel_slp_in_fsh(k,1);
           sel_inf_in_fshv(k) =    sel_inf_in_fsh(k,1);
        break;
      }
      case 3 : // Double logistic 
      {
        write_input_log << "Double logistic abandoned..."<<endl;exit(1);
        break;
      }
      case 4 : // Splines         
      {
      }
      break;
      write_input_log << fshname(k)<<" fish sel opt "<<endl<<fsh_sel_opt(k)<<" "<<endl<<"Sel_change"<<endl<<sel_change_in_fsh(k)<<endl;
    }
  }
  // Indices here..............
  yrs_sel_ch_ind.initialize() ;  
  sel_sigma_ind.initialize();
  for(k=1;k<=nind;k++)
  {
    *(ad_comm::global_datafile) >> ind_sel_opt(k)  ;  
    write_input_log << endl<<"Survey "<<indname(k)<<endl;
    log_input(ind_sel_opt(k));
    switch (ind_sel_opt(k))
    {
      case 1 : // Selectivity coefficients  indices
      {
        *(ad_comm::global_datafile) >> nselages_in_ind(k)   ;  
        *(ad_comm::global_datafile) >> phase_sel_ind(k);  
        *(ad_comm::global_datafile) >> curv_pen_ind(k) ;
        *(ad_comm::global_datafile) >> seldec_pen_ind(k) ;
        seldec_pen_ind(k) *= seldec_pen_ind(k);
        *(ad_comm::global_datafile) >>  n_sel_ch_ind(k) ;  
        n_sel_ch_ind(k)+=1;
        yrs_sel_ch_ind(k,1) = styr;
        yrs_sel_ch_tmp_ind(k,1) = styr;
        for (int i=2;i<=n_sel_ch_ind(k);i++)
          *(ad_comm::global_datafile) >>  yrs_sel_ch_ind(k,i) ;  
        for (int i=2;i<=n_sel_ch_ind(k);i++)
          *(ad_comm::global_datafile) >>  sel_sigma_ind(k,i) ;  
        sel_change_in_ind(k,styr)=1.; 
       // Number of selectivity changes is equal to the number of vectors (yr 1 is baseline)
        log_input(indname(k));
        log_input(nselages_in_ind(k));
        log_input(phase_sel_ind(k));
        log_input(seldec_pen_ind(k));
        log_input(n_sel_ch_ind(k));
        log_input(sel_change_in_ind(k));
        log_input(n_sel_ch_ind(k));
        log_input(yrs_sel_ch_ind(k)(1,n_sel_ch_ind(k)));
        // This to read in pre-specified selectivity values...
        for (j=1;j<=nages;j++) 
          *(ad_comm::global_datafile) >> sel_ind_tmp(j);  
        log_input(sel_ind_tmp);
        log_selcoffs_ind_in(k,1)(1,nselages_in_ind(k)) = log((sel_ind_tmp(1,nselages_in_ind(k))+1e-7)/mean(sel_fsh_tmp(1,nselages_in_ind(k))+1e-7) );
        // set all change selectivity to initial values
        for (int jj=2;jj<=n_sel_ch_ind(k);jj++) 
        {
          for (int j=nselages_in_ind(k)+1;j<=nages;j++) // This might be going out of nages=nselages
          {
            sel_ind_tmp(j)  = sel_ind_tmp(nselages_in_ind(k));  
          }
          // Set tmp to actual initial vectors...
          log_selcoffs_ind_in(k,jj)(1,nselages_in_ind(k)) = log((sel_ind_tmp(1,nselages_in_ind(k))+1e-7)/mean(sel_fsh_tmp(1,nselages_in_ind(k))+1e-7) );
          write_input_log<<"Sel_in_ind "<< mfexp(log_selcoffs_ind_in(k,jj))<<endl;
        }
        phase_selcoff_ind(k) = phase_sel_ind(k);
        phase_logist_ind(k)  = -2;
        phase_dlogist_ind(k)  = -1;
      }
      break;
      case 2 : // Single logistic
      {
        *(ad_comm::global_datafile) >> phase_sel_ind(k);  
        *(ad_comm::global_datafile) >>  n_sel_ch_ind(k) ;  
        n_sel_ch_ind(k) +=1;
        yrs_sel_ch_ind(k,1) = styr; // first year always estimated
        yrs_sel_ch_tmp_ind(k,1) = styr;
        for (int i=2;i<=n_sel_ch_ind(k);i++)
          *(ad_comm::global_datafile) >>  yrs_sel_ch_ind(k,i) ;  
        for (int i=2;i<=n_sel_ch_ind(k);i++)
          *(ad_comm::global_datafile) >>  sel_sigma_ind(k,i) ;  
        sel_change_in_ind(k,styr)=1.; 

        log_input(indname(k));
        log_input(nselages_in_ind(k));
        log_input(phase_sel_ind(k));
        log_input(sel_change_in_ind(k));
        log_input(n_sel_ch_ind(k));
        log_input(yrs_sel_ch_ind(k)(1,n_sel_ch_ind(k)));
        // This to read in pre-specified selectivity values...
       // Number of selectivity changes is equal to the number of vectors (yr 1 is baseline)
        for (int i=styr+1;i<=endyr;i++) { if(sel_change_in_ind(k,i)>0) { j++; yrs_sel_ch_tmp_ind(k,j) = i; } }
        // This to read in pre-specified selectivity values...
        *(ad_comm::global_datafile) >> sel_slp_in_ind(k,1) ;
        *(ad_comm::global_datafile) >> sel_inf_in_ind(k,1) ;
        logsel_slp_in_ind(k,1) =   log(sel_slp_in_ind(k,1)) ;
        for (int jj=2;jj<=n_sel_ch_ind(k);jj++) 
        {
          sel_inf_in_ind(k,jj)    =     sel_inf_in_ind(k,1) ;
          logsel_slp_in_ind(k,jj) = log(sel_slp_in_ind(k,1)) ;
        }
        log_input(sel_slp_in_ind(k,1));
        log_input(sel_inf_in_ind(k,1));
        log_input(logsel_slp_in_ind(k,1));

        phase_selcoff_ind(k) = -1;
        phase_logist_ind(k) = phase_sel_ind(k);
        phase_dlogist_ind(k)  = -1;

        logsel_slp_in_indv(k) = logsel_slp_in_ind(k,1);
           sel_inf_in_indv(k) =    sel_inf_in_ind(k,1);
        log_input(logsel_slp_in_indv(k));
      }
      break;
      case 3 : // Double logistic 
      {
        write_input_log << "Double logistic abandoned..."<<endl;exit(1);
      }
        break;
      case 4 : // spline for indices
      {
      }
      break;
    }
    write_input_log << indname(k)<<" ind sel opt "<<ind_sel_opt(k)<<" "<<sel_change_in_ind(k)<<endl;
  }
  write_input_log<<"Phase indices Sel_Coffs: "<<phase_selcoff_ind<<endl; 
  write_input_log<<" Test: "<<test<<endl;
 END_CALCS
  init_number test
 LOC_CALCS
  if (test!=123456789) {cerr<<"Control file not read in correctly... "<<endl;exit(1);}
  nopt_fsh.initialize();
  for (k=1;k<=nfsh;k++) 
    if(fsh_sel_opt(k)==1) 
     nopt_fsh(1)++;
    else nopt_fsh(2)++;
  // Fishery selectivity description:
  // type 1
  // Number of ages
  write_input_log << "# Fshry Selages: " << nselages_in_fsh  <<endl;
  write_input_log << "# Srvy  Selages: " << nselages_in_ind <<endl;

  write_input_log << "# Phase for age-spec fishery "<<phase_selcoff_fsh<<endl;
  write_input_log << "# Phase for logistic fishery "<<phase_logist_fsh<<endl;
  write_input_log << "# Phase for dble logistic fishery "<<phase_dlogist_fsh<<endl;

  write_input_log << "# Phase for age-spec indices  "<<phase_selcoff_ind<<endl;
  write_input_log << "# Phase for logistic indices  "<<phase_logist_ind<<endl;
  write_input_log << "# Phase for dble logistic ind "<<phase_dlogist_ind<<endl;

  for (k=1;k<=nfsh;k++) if (phase_selcoff_fsh(k)>0) curv_pen_fsh(k) = 1./ (square(curv_pen_fsh(k))*2.);
  write_input_log<<"# Curv_pen_fsh: "<<endl<<curv_pen_fsh<<endl;
  for (k=1;k<=nind;k++) if (phase_selcoff_ind(k)>0) curv_pen_ind(k) = 1./ (square(curv_pen_ind(k))*2.);
  write_input_log<<"# Curv_pen_ind: "<<endl<<curv_pen_fsh<<endl;
 END_CALCS

  int  phase_fmort;
  int  phase_proj;
  ivector   nselages_fsh(1,nfsh);
  matrix xnodes_fsh(1,nfsh,1,nnodes_fsh)
  matrix xages_fsh(1,nfsh,1,nages)

  ivector   nselages_ind(1,nind);
  //Resetting data here for retrospectives////////////////////////////////////////////
 LOCAL_CALCS
  for (int k=1;k<=nfsh;k++) 
  {
    if ((endyr-retro)<=yrs_sel_ch_fsh(k,n_sel_ch_fsh(k))) n_sel_ch_fsh(k)-=retro ;  
    for (int i=1;i<=retro;i++) 
    {
        cout<<"here"<<max(yrs_fsh_age_in(k)(1,nyrs_fsh_age(k)))<<endl;
      if (max(yrs_fsh_age_in(k)(1,nyrs_fsh_age(k)))>=(endyr-retro)) 
      {
         nyrs_fsh_age(k) -= 1;
          if (max(yrs_fsh_age_in(k)(1,nyrs_fsh_age(k)))>=(endyr-retro)) 
             nyrs_fsh_age(k) -= 1;
      }
    }
    if (nyrs_fsh_length(k) >0)
    {
      for (int i=1;i<=retro;i++) 
      {
       //  cout<<"Here "<<max(yrs_fsh_length_in(k)(1,nyrs_fsh_length(k)))<<endl;
        if (nyrs_fsh_length(k) >0)
          if (max(yrs_fsh_length_in(k)(1,nyrs_fsh_length(k)))>=(endyr-retro)) 
           nyrs_fsh_length(k) -= 1;
      }
    }
  }
  // now for indices
  for (int k=1;k<=nind;k++) 
  {
    if ((endyr-retro)<=yrs_sel_ch_ind(k,n_sel_ch_ind(k))) n_sel_ch_ind(k)-=retro ;  
    for (int i=1;i<=retro;i++) 
    {
      // index values
      if (max(yrs_ind_in(k)(1,nyrs_ind(k)))>=(endyr-retro)) 
         nyrs_ind(k) -= 1;
      // Ages (since they can be different than actual index years)
      if (max(yrs_ind_age_in(k)(1,nyrs_ind_age(k)))>=(endyr-retro)) 
         nyrs_ind_age(k) -= 1;
    }
  }
  endyr_rec_est = endyr_rec_est - retro;
  endyr         = endyr - retro;
  styr_fut      = endyr+1;
  endyr_fut     = endyr + nproj_yrs; 
  endyr_sp      = endyr   - rec_age - 1;// endyr year of (main) spawning biomass
 END_CALCS
 // now use redimensioned data for retro
  matrix catch_bio(1,nfsh,styr,endyr)         //Catch biomass 
  matrix catch_bio_sd(1,nfsh,styr,endyr)      //Catch biomass standard errors 
  matrix catch_bio_lsd(1,nfsh,styr,endyr)     //Catch biomass standard errors (for lognormal)
  matrix catch_bio_lva(1,nfsh,styr,endyr)     //Catch biomass variance (for lognormal)
  matrix catch_bioT(styr,endyr,1,nfsh)
  imatrix yrs_fsh_age(1,nfsh,1,nyrs_fsh_age)
  imatrix yrs_fsh_length(1,nfsh,1,nyrs_fsh_length)
  matrix  n_sample_fsh_age(1,nfsh,1,nyrs_fsh_age)    //Years of index index value (annual)
  matrix n_sample_fsh_length(1,nfsh,1,nyrs_fsh_length)    //Years of index index value (annual)
  3darray oac_fsh(1,nfsh,1,nyrs_fsh_age,1,nages)
  3darray olc_fsh(1,nfsh,1,nyrs_fsh_length,1,nlength)

  imatrix yrs_ind(1,nind,1,nyrs_ind)         //Years of index value (annual)
  matrix obs_ind(1,nind,1,nyrs_ind)          //values of index value (annual)
  matrix obs_se_ind(1,nind,1,nyrs_ind)       //values of indices serrs

  imatrix yrs_ind_age(1,nind,1,nyrs_ind_age)  //Years of index value (annual)
  imatrix yrs_ind_length(1,nind,1,nyrs_ind_length)
  matrix n_sample_ind_age(1,nind,1,nyrs_ind_age)         //Years of index value (annual)
  matrix n_sample_ind_length(1,nind,1,nyrs_ind_length)    //Years of index index value (annual)
  3darray oac_ind(1,nind,1,nyrs_ind_age,1,nages)  //values of Index proportions at age
  3darray olc_ind(1,nind,1,nyrs_ind_length,1,nlength)

  matrix     obs_lse_ind(1,nind,1,nyrs_ind) //Index standard errors (for lognormal)
  matrix     obs_lva_ind(1,nind,1,nyrs_ind) //Index standard errors (for lognormal)
 LOCAL_CALCS
  for (int k=1;k<=nfsh;k++)
  {
    catch_bio(k) = catch_bio_in(k)(styr,endyr);
    catch_bio_sd(k) = catch_bio_sd_in(k)(styr,endyr);
    yrs_fsh_age(k) = yrs_fsh_age_in(k)(1,nyrs_fsh_age(k));
    n_sample_fsh_age(k) = n_sample_fsh_age_in(k)(1,nyrs_fsh_age(k));
    yrs_fsh_length(k) = yrs_fsh_length_in(k)(1,nyrs_fsh_length(k));
    n_sample_fsh_length(k) = n_sample_fsh_length_in(k)(1,nyrs_fsh_length(k));
    for (int i=1;i<=nyrs_fsh_age(k);i++)
      oac_fsh(k,i) = oac_fsh_in(k,i) ;
    for (int i=1;i<=nyrs_fsh_length(k);i++)
      olc_fsh(k,i) = olc_fsh_in(k,i) ;
  }
  catch_bio_lsd = sqrt(log(square(catch_bio_sd) + 1.));
  catch_bio_lva = log(square(catch_bio_sd) + 1.);
  catch_bioT = trans(catch_bio);
  for (int k=1;k<=nind;k++)
  {
    yrs_ind(k)  = yrs_ind_in(k)(1,nyrs_ind(k));
    obs_ind(k)  = obs_ind_in(k)(1,nyrs_ind(k));
    obs_se_ind(k)  = obs_se_ind_in(k)(1,nyrs_ind(k));

    yrs_ind_age(k) = yrs_ind_age_in(k)(1,nyrs_ind_age(k));
    n_sample_ind_age(k) = n_sample_ind_age_in(k)(1,nyrs_ind_age(k));
    yrs_ind_length(k) = yrs_ind_length_in(k)(1,nyrs_ind_length(k));
    n_sample_ind_length(k) = n_sample_ind_length_in(k)(1,nyrs_ind_length(k));
    for (int i=1;i<=nyrs_ind_age(k);i++)
      oac_ind(k,i) = oac_ind_in(k,i) ;
    for (int i=1;i<=nyrs_ind_length(k);i++)
      olc_ind(k,i) = olc_ind_in(k,i) ;
  }
  log_input(nyrs_fsh_age);
  log_input(yrs_fsh_age);
  log_input(n_sample_fsh_age);
  log_input(oac_fsh);
  log_input(olc_fsh);
  log_input(wt_fsh);

  log_input(nyrs_ind_age);
  log_input(yrs_ind_age);
  log_input(n_sample_ind_age);
  log_input(oac_ind);
  log_input(olc_ind);
  obs_lse_ind = elem_div(obs_se_ind,obs_ind);
  obs_lse_ind = sqrt(log(square(obs_lse_ind) + 1.));
  log_input(obs_lse_ind);
  obs_lva_ind = square(obs_lse_ind);
 END_CALCS

  ////////////////////////////////////////////////////////////////////////////////////
 LOCAL_CALCS
  for (k=1; k<=nfsh;k++)
  {
    // xages_fsh increments from 0-1 by number of ages, say
    xages_fsh.initialize();
    log_input(xages_fsh);
    xages_fsh(k).fill_seqadd(0.,1.0/(nages-1));
    log_input(xages_fsh);
    //  xnodes increments from 0-1 by number of nodes
    xnodes_fsh.initialize();
    xnodes_fsh(k).fill_seqadd(0.,1.0/(nnodes_fsh(k)-1));
    log_input(xnodes_fsh);
    // xages_fsh(k).fill_seqadd(0,1.0/(nselages_in_fsh(k)-1)); //prefer to use nselages but need 3d version to work
  }
  write_input_log<<"Yrs fsh_sel change: "<<yrs_sel_ch_fsh<<endl;
  // for (k=1; k<=nind;k++) yrs_sel_ch_ind(k) = yrs_sel_ch_tmp_ind(k)(1,n_sel_ch_ind(k));
  write_input_log<<"Yrs ind_sel change: "<<yrs_sel_ch_ind<<endl;
    log_sigmarprior = log(sigmarprior);
    log_input(steepnessprior);
    log_input(sigmarprior);
    nrecs_est = endyr_rec_est-styr_rec_est+1;
    nrecs_est = endyr_rec_est-styr_rec_est+1;
    write_input_log<<"#  SSB estimated in styr endyr: " <<styr_sp    <<" "<<endyr_sp      <<" "<<endl;
    write_input_log<<"#  Rec estimated in styr endyr: " <<styr_rec    <<" "<<endyr        <<" "<<endl;
    write_input_log<<"#  SR Curve fit  in styr endyr: " <<styr_rec_est<<" "<<endyr_rec_est<<" "<<endl;
    write_input_log<<"#             Model styr endyr: " <<styr        <<" "<<endyr        <<" "<<endl;
    log_qprior = log(qprior);
    log_input(qprior);
    log_q_power_prior = log(q_power_prior);
    write_input_log<<"# q_power_prior " <<endl<<q_power_prior<<" "<<endl;
    write_input_log<<"# cv_catchbiomass " <<endl<<cv_catchbiomass<<" "<<endl;
    write_input_log<<"# CatchbiomassPen " <<endl<<catchbiomass_pen<<" "<<endl;
    write_input_log<<"# Number of projection years " <<endl<<nproj_yrs<<" "<<endl;// cin>>junk;

 END_CALCS
  number R_guess;

  vector offset_ind(1,nind)
  vector offset_fsh(1,nfsh)
  vector offset_lfsh(1,nfsh)
  vector offset_lind(1,nind)

  int do_fmort;
  !! do_fmort=0;
  int Popes;
 LOCAL_CALCS
  Popes=0; // option to do Pope's approximation (not presently flagged outside of code)
  if (Popes) 
    phase_fmort = -2;
  else
    phase_fmort = 1;

  phase_proj  =  5;

  Steepness_UB = .9999; // upper bound of steepness
  offset_ind.initialize();
  offset_fsh.initialize();
  offset_lfsh.initialize();
  offset_lind.initialize();
  double sumtmp;
  for (k=1;k<=nfsh;k++)
    for (i=1;i<=nyrs_fsh_age(k);i++)
    {
      oac_fsh(k,i) /= sum(oac_fsh(k,i)); // Normalize to sum to one
      offset_fsh(k) -= n_sample_fsh_age(k,i)*(oac_fsh(k,i) + 0.001) * log(oac_fsh(k,i) + 0.001 ) ;
    }
  for (k=1;k<=nfsh;k++)
    for (i=1;i<=nyrs_fsh_length(k);i++)
    {
      olc_fsh(k,i) /= sum(olc_fsh(k,i)); // Normalize to sum to one
      offset_lfsh(k) -= n_sample_fsh_length(k,i)*(olc_fsh(k,i) + 0.001) * log(olc_fsh(k,i) + 0.001 ) ;
    }

  for (k=1;k<=nind;k++)
  {
    for (i=1;i<=nyrs_ind_age(k);i++)
    {
      oac_ind(k,i) /= sum(oac_ind(k,i)); // Normalize to sum to one
      offset_ind(k) -= n_sample_ind_age(k,i)*(oac_ind(k,i) + 0.001) * log(oac_ind(k,i) + 0.001 ) ;
    }
    for (i=1;i<=nyrs_ind_length(k);i++)
    {
      olc_ind(k,i) /= sum(olc_ind(k,i)); // Normalize to sum to one
      offset_lind(k) -= n_sample_ind_length(k,i)*(olc_ind(k,i) + 0.001) * log(olc_ind(k,i) + 0.001 ) ;
    }
  }
  log_input(offset_fsh); 
  log_input(offset_ind); 

  if (ad_comm::argc > 1) // Command line argument to profile Fishing mortality rates...
  {
    int on=0;
    if ( (on=option_match(ad_comm::argc,ad_comm::argv,"-uFmort"))>-1)
      do_fmort=1;
  }

  // Compute an initial Rzero value based on exploitation 
   double btmp=0.;
   double ctmp=0.;
   dvector ntmp(1,nages);
   ntmp(1) = 1.;
   for (int a=2;a<=nages;a++)
     ntmp(a) = ntmp(a-1)*exp(-natmortprior-.05);
   btmp = wt_pop * ntmp;
   write_input_log << "Mean Catch"<<endl;
   ctmp = mean(catch_bio);
   write_input_log << ctmp <<endl;
   R_guess = log((ctmp/.02 )/btmp) ;
   write_input_log << "R_guess "<<endl;
   write_input_log << R_guess <<endl;
 END_CALCS
 // vector len_bins(1,nlength)
 // !! len_bins.fill_seqadd(stlength,binlength);
PARAMETER_SECTION
 // Biological Parameters
  init_bounded_number Mest(.02,4.8,phase_M)
  init_bounded_vector Mage_offset(1,npars_Mage,-3,3,phase_Mage)
  vector Mage(1,nages)
  init_bounded_vector  M_rw(1,npars_rw_M,-10,10,phase_rw_M)
  vector natmort(styr,endyr)
  matrix  natage(styr,endyr+1,1,nages)
  matrix N_NoFsh(styr,endyr_fut,1,nages);
  // vector Sp_Biom(styr_sp,endyr)
  vector pred_rec(styr_rec,endyr)
  vector mod_rec(styr_rec,endyr) // As estimated by model
  matrix  M(styr,endyr,1,nages)
  matrix  Z(styr,endyr,1,nages)
  matrix  S(styr,endyr,1,nages)


 //-----GROWTH PARAMETERS--------------------------------------------------
  init_number log_Linf(phase_Linf);
  init_number log_k(phase_k);
  init_number log_Lo(phase_Lo);
  init_number log_sdage(phase_sdage);
  //---------------------------------------------------------------------------


 // Stock rectuitment params
  init_number mean_log_rec(1); 
  init_bounded_number steepness(0.21,Steepness_UB,phase_srec)
  init_number log_Rzero(phase_Rzero)  
  // OjO
  // init_bounded_vector initage_dev(2,nages,-15,15,4)
  init_bounded_vector rec_dev(styr_rec,endyr,-15,15,2)
  // init_vector rec_dev(styr_rec,endyr,2)
  init_number log_sigmar(phase_sigmar);
  number m_sigmarsq  
  number m_sigmar
  number sigmarsq  
  number sigmar
  number alpha   
  number beta   
  number Bzero   
  number Rzero   
  number phizero
  number avg_rec_dev   

 // Fishing mortality parameters
  // init_vector         log_avg_fmort(1,nfsh,phase_fmort)
  // init_bounded_matrix fmort_dev(1,nfsh,styr,endyr,-15,15.,phase_fmort)
  init_bounded_matrix fmort(1,nfsh,styr,endyr,0.00,5.,phase_fmort)
  vector Fmort(styr,endyr);  // Annual total Fmort
  number hrate
  number catch_tmp
  number Fnew 

  !! for (k=1;k<=nfsh;k++) nselages_fsh(k)=nselages_in_fsh(k); // Sets all elements of a vector to one scalar value...
  !! for (k=1;k<=nind;k++) nselages_ind(k)=nselages_in_ind(k); // Sets all elements of a vector to one scalar value...

 //  init_3darray log_selcoffs_fsh(1,nfsh,1,n_sel_ch_fsh,1,nselages_fsh,phase_selcoff_fsh)
  init_matrix_vector log_selcoffs_fsh(1,nfsh,1,n_sel_ch_fsh,1,nselages_fsh,phase_selcoff_fsh) // 3rd dimension out...
  // option to estimate smoother for selectivity penalty
  // init_number_vector logSdsmu_fsh(1,nfsh,1,phase_selcoff_fsh) 
  !! if (fsh_sel_opt(1)==4) nnodes_tmp=nnodes_fsh(1);  // NOTE THIS won't work in general
  //init_matrix_vector  log_sel_spl_fsh(1,nfsh,1,n_sel_ch_fsh,1,nnodes_tmp,phase_sel_spl_fsh)
  init_matrix_vector  log_sel_spl_fsh(1,nfsh,1,n_sel_ch_fsh,1,4,phase_sel_spl_fsh)

  !! log_input(nfsh);
  !! log_input(n_sel_ch_fsh);
  !! log_input(nselages_fsh);
  !! log_input(phase_selcoff_fsh);
  init_vector_vector logsel_slope_fsh(1,nfsh,1,n_sel_ch_fsh,phase_logist_fsh)
  matrix                sel_slope_fsh(1,nfsh,1,n_sel_ch_fsh)
  init_vector_vector     sel50_fsh(1,nfsh,1,n_sel_ch_fsh,phase_logist_fsh)
  init_vector_vector logsel_dslope_fsh(1,nfsh,1,n_sel_ch_fsh,phase_dlogist_fsh)
  matrix                sel_dslope_fsh(1,nfsh,1,n_sel_ch_fsh)
  !! int lb_d50=nages/2;
  init_bounded_vector_vector     seld50_fsh(1,nfsh,1,n_sel_ch_fsh,lb_d50,nages,phase_dlogist_fsh)

  // !!exit(1);
  3darray log_sel_fsh(1,nfsh,styr,endyr,1,nages)
  3darray sel_fsh(1,nfsh,styr,endyr,1,nages)
  matrix avgsel_fsh(1,nfsh,1,n_sel_ch_fsh);

  matrix  Ftot(styr,endyr,1,nages)
  3darray F(1,nfsh,styr,endyr,1,nages)
  3darray eac_fsh(1,nfsh,1,nyrs_fsh_age,1,nages)
  //-----------------------------------------------NEW--------
  3darray elc_fsh(1,nfsh,1,nyrs_fsh_length,1,nlength)
  3darray elc_ind(1,nind,1,nyrs_ind_length,1,nlength)
  //----------------------------------------------------------
  matrix  pred_catch(1,nfsh,styr,endyr)
  3darray catage(1,nfsh,styr,endyr,1,nages)
  matrix catage_tot(styr,endyr,1,nages)
  matrix expl_biom(1,nfsh,styr,endyr)

 // Parameters for computing SPR rates 
  vector F50(1,nfsh)
  vector F40(1,nfsh)
  vector F35(1,nfsh)

 // Stuff for SPR and yield projections
  number sigmar_fut
  vector f_tmp(1,nfsh)
  number SB0
  number SBF50
  number SBF40
  number SBF35
  vector Fratio(1,nfsh)
  !! Fratio = 1;
  !! Fratio /= sum(Fratio);

  matrix Nspr(1,4,1,nages)
 
  matrix nage_future(styr_fut,endyr_fut,1,nages)

  init_vector rec_dev_future(styr_fut,endyr_fut,phase_proj);
  vector Sp_Biom_future(styr_fut-rec_age,endyr_fut);
  3darray F_future(1,nfsh,styr_fut,endyr_fut,1,nages);
  matrix Z_future(styr_fut,endyr_fut,1,nages);
  matrix S_future(styr_fut,endyr_fut,1,nages);
  matrix catage_future(styr_fut,endyr_fut,1,nages);
  number avg_rec_dev_future
  vector avg_F_future(1,5)

 // Survey Observation parameters
  init_number_vector log_q_ind(1,nind,phase_q) 
  init_number_vector log_q_power_ind(1,nind,phase_q_power) 
  init_vector_vector log_rw_q_ind(1,nind,1,npars_rw_q,phase_rw_q) 
  init_matrix_vector log_selcoffs_ind(1,nind,1,n_sel_ch_ind,1,nselages_ind,phase_selcoff_ind)

  // init_vector_vector logsel_slope_ind(1,nind,1,n_sel_ch_ind,phase_logist_ind) // Need to make positive or reparameterize
  init_vector_vector logsel_slope_ind(1,nind,1,n_sel_ch_ind,phase_logist_ind+1) // Need to make positive or reparameterize
  init_bounded_vector_vector        sel50_ind(1,nind,1,n_sel_ch_ind,1,20,phase_logist_ind)

  init_vector_vector  logsel_dslope_ind(1,nind,1,n_sel_ch_ind,phase_dlogist_ind) // Need to make positive or reparameterize
  init_bounded_vector_vector seld50_ind(1,nfsh,1,n_sel_ch_ind,lb_d50,nages,phase_dlogist_ind)

  matrix                sel_slope_ind(1,nind,1,n_sel_ch_ind)
  matrix                sel_dslope_ind(1,nind,1,n_sel_ch_ind)

  3darray log_sel_ind(1,nind,styr,endyr,1,nages)
  3darray sel_ind(1,nind,styr,endyr,1,nages)
  matrix avgsel_ind(1,nind,1,n_sel_ch_ind);

  matrix pred_ind(1,nind,1,nyrs_ind)
  3darray eac_ind(1,nind,1,nyrs_ind_age,1,nages)

 // Likelihood value names         
  number sigma
  vector rec_like(1,4)
  vector catch_like(1,nfsh)
  vector age_like_fsh(1,nfsh)
  //---------------------------------NEW
  vector length_like_fsh(1,nfsh)
  vector length_like_ind(1,nfsh)
  //---------------------------------NEW

  vector age_like_ind(1,nind)
  matrix sel_like_fsh(1,nfsh,1,4)       
  matrix sel_like_ind(1,nind,1,4)       
  vector ind_like(1,nind)
  vector fpen(1,6)    
  vector post_priors(1,8)
  vector post_priors_indq(1,nind)
  objective_function_value obj_fun
  vector obj_comps(1,13)
  init_number repl_F(5)

  sdreport_number repl_yld
  sdreport_number repl_SSB
  sdreport_number B100
  number F50_est
  number F40_est
  number F35_est
  matrix q_ind(1,nind,1,nyrs_ind)
  vector q_power_ind(1,nind)
  // sdreport_vector q_ind(1,nind)
  sdreport_vector totbiom(styr,endyr+1)
  sdreport_vector totbiom_NoFish(styr,endyr)
  sdreport_vector Sp_Biom(styr_sp,endyr+1)
  sdreport_vector Sp_Biom_NoFish(styr_sp,endyr)
  sdreport_vector Sp_Biom_NoFishRatio(styr,endyr)
  sdreport_number ABCBiom;
  sdreport_vector recruits(styr,endyr+1)
  // vector recruits(styr,endyr+1)
  sdreport_number depletion
  sdreport_number depletion_dyn
  sdreport_number MSY;
  sdreport_number MSYL;
  sdreport_number Fmsy;
  sdreport_number lnFmsy;
  sdreport_number Fcur_Fmsy;
  sdreport_number Rmsy;
  sdreport_number Bmsy;
  sdreport_number Bcur_Bmsy;
  sdreport_vector pred_ind_nextyr(1,nind);
  sdreport_number OFL;
  // NOTE TO DAVE: Need to have a phase switch for sdreport variables(
  matrix catch_future(1,4,styr_fut,endyr_fut); // Note, don't project for F=0 (it will bomb)
  sdreport_matrix SSB_fut(1,5,styr_fut,endyr_fut)
  !! write_input_log <<"logRzero "<<log_Rzero<<endl;
  !! write_input_log <<"logmeanrec "<<mean_log_rec<<endl;
  !! write_input_log<< "exp(log_sigmarprior "<<exp(log_sigmarprior)<<endl;
  //-----GROWTH PARAMETERS--------------------------------------------------
 number Linf;
 number k_coeff;
 number Lo;
 number sdage;
 vector mu_age(1,nages);
 vector sigma_age(1,nages);
 matrix P1(1,nages,1,nlength);
 matrix P2(1,nages,1,nlength);
 matrix P3(1,nages,1,nlength);
 vector Ones_length(1,nlength);
 matrix P_age2len(1,nages,1,nlength);
  //-----------------------------------------------------------------------
 // Initialize coefficients (if needed)
 LOCAL_CALCS
  for (k=1;k<=nfsh;k++) 
  {
    write_input_log<<"Fish sel phase: "<<phase_selcoff_fsh(k)<<" "<<fshname(k)<<endl;
    switch (fsh_sel_opt(k))
    {
      case 1 : // Selectivity coefficients 
      {
        if(phase_selcoff_fsh(k)<0)
        {
          write_input_log<<"Initial fixing fishery sel to"<<endl<<n_sel_ch_fsh(k)<<endl;
          for (int jj=1;jj<=n_sel_ch_fsh(k);jj++) 
          {
            log_selcoffs_fsh(k,jj)(1,nselages_in_fsh(k)) = log_selcoffs_fsh_in(k,jj)(1,nselages_in_fsh(k));
            write_input_log <<"Init coef:"<<endl<<exp(log_selcoffs_fsh(k,jj)(1,nselages_in_fsh(k))) <<endl;
          }
        }
      }
        break;
      case 2 : // Single logistic
      {
        if(phase_logist_fsh(k)<0)
        {
          logsel_slope_fsh(k,1) = logsel_slp_in_fsh(k,1)  ;
          write_input_log<<"Fixing fishery sel to"<<endl<<n_sel_ch_fsh(k)<<endl;
          for (int jj=1;jj<=n_sel_ch_fsh(k);jj++) 
          {
            logsel_slope_fsh(k,jj) = logsel_slp_in_fsh(k,jj)  ;
            sel50_fsh(k,jj)        =    sel_inf_in_fsh(k,jj)  ;
          }
        }
      }
      case 3 : // Double logistic 
      {
        if(phase_dlogist_fsh(k)<0)
        {
          write_input_log<<"Fixing fishery sel to"<<endl<<n_sel_ch_fsh(k)<<endl;
          for (int jj=1;jj<=n_sel_ch_fsh(k);jj++) 
          {
            logsel_slope_fsh(k,jj) = logsel_slp_in_fsh(k,jj)  ;
            sel50_fsh(k,jj)        =    sel_inf_in_fsh(k,jj)  ;
          }
        }
      }
      case 4 : // Selectivity spline initialize 
      /* {
        if(phase_sel_spl_fsh(k)<0)
        {
          write_input_log<<"Initial fishery spline to"<<endl<<n_sel_ch_fsh(k)<<endl;
          for (int jj=1;jj<=n_sel_ch_fsh(k);jj++) 
          {
            log_sel_spl_fsh(k,jj)(1,nnodes_tmp) = log_sel_spl_fsh_in(k,jj)(1,nnodes_tmp);
            // write_input_log <<"Init coef:"<<endl<<exp(log_sel_spl_fsh(k,jj)(1,nselages_in_fsh(k))) <<endl;
          }
          log_input(log_sel_spl_fsh);
        }
       }*/
     break;
    }
  }
  for (k=1;k<=nind;k++) 
  {
    write_input_log<<"Srvy sel phase: "<<phase_selcoff_ind(k)<<endl;
    if(phase_selcoff_ind(k)<0)
    {
      write_input_log<<"Fixing "<<indname(k)<<" indices sel to"<<endl<<n_sel_ch_ind(k)<<endl;
      for (int jj=1;jj<=n_sel_ch_ind(k);jj++) 
      {
        log_selcoffs_ind(k,jj)(1,nselages_in_ind(k)) = log_selcoffs_ind_in(k,jj)(1,nselages_in_ind(k));
        // write_input_log <<"Init coef:"<<endl<<exp(log_selcoffs_ind(k,jj)(1,nselages_in_ind(k))) <<endl;
      }
    }
    if(phase_logist_ind(k)<0)
    {
      write_input_log<<"Fixing index sel to"<<endl<<n_sel_ch_ind(k)<<endl;
      for (int jj=1;jj<=n_sel_ch_ind(k);jj++) 
      {
        logsel_slope_ind(k,jj) = logsel_slp_in_ind(k,jj)  ;
        // logsel_slope_ind(k,jj)    = 0.   ;
        sel50_ind(k,jj)           = sel_inf_in_ind(k,jj)  ;
      }
    }
  }
  log_input( logsel_slp_in_indv);
  write_input_log <<"Leaving parameter init secton"<<endl;
 END_CALCS
PRELIMINARY_CALCS_SECTION
  WriteNewData();
  
  // Initialize age-specific changes in M if they are specified
  M(styr) = Mest;
  if (npars_Mage>0)
  {
    Mage_offset = Mage_offset_in;
    int jj=1;
    for (j=1;j<=nages;j++)
    {
     if (j==ages_M_changes(jj))
      {
        M(styr,j) = M(styr,1)*mfexp(Mage_offset(jj));
        jj++;
        if (npars_Mage < jj) jj=npars_Mage;
      }
      else
        if(j>1) 
          M(styr,j) = M(styr,j-1);
    }
  }
  //Initialize matrix of M
  for (i=styr+1;i<=endyr;i++)
    M(i) = M(i-1);
  log_input(M);
INITIALIZATION_SECTION
  Mest natmortprior; 
  steepness steepnessprior
  log_sigmar log_sigmarprior;


  log_Rzero R_guess;
  mean_log_rec R_guess;
  
  log_Linf    log_Linfprior
  log_k       log_kprior
  log_Lo      log_Loprior
  log_sdage   log_sdageprior

  // log_avg_fmort -2.065
  log_q_ind log_qprior; 
  log_q_power_ind log_q_power_prior; 
  repl_F .1;

  sel50_fsh sel_inf_in_fshv 

  logsel_dslope_fsh logsel_dslp_in_fshv ;
  seld50_fsh sel_dinf_in_fshv 

  logsel_slope_ind logsel_slp_in_indv ;
  sel50_ind sel_inf_in_indv ;

  logsel_dslope_ind logsel_dslp_in_indv ;
  seld50_ind sel_dinf_in_indv ;
PROCEDURE_SECTION
RUNTIME_SECTION
  convergence_criteria 1.e-1,1.e-01,1.e-03,1e-5,1e-5
  maximum_function_evaluations 100,100,200,300,2500
TOP_OF_MAIN_SECTION
  gradient_structure::set_MAX_NVAR_OFFSET(4500);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(4500);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(1000000);
  gradient_structure::set_CMPDIF_BUFFER_SIZE(10000000);
  arrmblsize=500000000;
GLOBALS_SECTION
  #include <admodel.h>  
	#undef write_SIS_rep 
	#define write_SIS_rep(object) SIS_rep << #object "\n" << object << endl;
	#undef truth 
	#define truth(object) trudat << #object "\n" << object << endl;
	#undef REPORT 
	#define REPORT(object) REPORT << #object "\n" << object << endl;
	#undef R_Report 
	#define R_Report(object) R_report << "$"#object "\n" << object << endl;
	/**
	 \def log_input(object)
	Prints name and value of \a object on ADMB report %ofstream file.
	*/
	#undef log_input
	#define log_input(object) write_input_log << "# " #object "\n" << object << endl;
	#undef log_param
  // #define log_param(object) for(int i=0;i<initial_params::num_initial_params;i++) {if(withinbound(0,(initial_params::varsptr[i])->phase_start, initial_params::current_phase)) { int sc= (initial_params::varsptr[i])->size_count(); if (sc>0) { write_input_log << "# " << initial_params::varsptr[i] ->label() << "\n" << object<<endl; } }}
  //
	#define log_param(object) if (active(object)) write_input_log << "# " #object "\n" << object << endl;
  ofstream write_input_log("input.log");
  ofstream SIS_rep("SIS_out.rep");

 // void get_sel_changes(int& k);
  adstring_array fshname;
  adstring_array indname;
  adstring truname;
  adstring simname;
  adstring model_name;
  adstring projfile_name;
  adstring datafile_name;
  adstring cntrlfile_name;
  adstring tmpstring;
  adstring repstring;
  adstring version_info;
  ofstream NewCtl("amakl_new.ctl");
  #undef write_ctl
  #define write_ctl(object) NewCtl << "# " #object "\n" << object << endl;
  ofstream NewData("pcodnew.dat");
  #undef write_new
  #define write_new(object) NewData << "# " #object "\n" << object << endl;
  ifstream poll_msm("pcod_msm.dat");
  #undef read_new
  #define read_new(object) poll_msm >> object;
FUNCTION WriteNewData
 ifstream from_msm("pcod_msm.dat");
  //
  // Read in "new" data from tmsm first then append to past data...
  //
  ofstream NewData(adstring(model_name)+adstring("_new.dat"));
  write_new( styr);
  write_new( endyr+1);
  write_new( rec_age);
  write_new( oldest_age);
  write_new( nlength);
  write_new( len_bins);
  write_new( nfsh); 
  write_new( fshnameread);
  write_new( catch_bio_in);
  double catchtmp;
  read_new(catchtmp);
  write_new(catchtmp);
  write_new( catch_bio_sd_in);
  write_new( catch_bio_sd_in(1,endyr));
  write_new( nyrs_fsh_age+1);
  write_new( nyrs_fsh_length);
  write_new( yrs_fsh_age_in);
  write_new( yrs_fsh_age_in(1,nyrs_fsh_age(1))+1);
  write_new( yrs_fsh_length_in);
  write_new( n_sample_fsh_age_in);
  write_new( n_sample_fsh_age_in(1,nyrs_fsh_age(1)));
  write_new( n_sample_fsh_length_in);
  write_new( oac_fsh_in);
  dvector ptmp(1,nages);
   read_new( ptmp);
  write_new( ptmp);
  write_new( olc_fsh_in); // No length data from fishery...
  write_new( wt_fsh);
  write_new( wt_fsh(1,endyr)); // Constant mean wt in fishery
  write_new( nind);
  write_new( indnameread);
  write_new( nyrs_ind+1);
  write_new( yrs_ind_in);
  write_new( yrs_ind_in(1,nyrs_ind(1))+1 );
  write_new( mo_ind);
  write_new( obs_ind_in);
  double bbtmp;
   read_new( bbtmp);
  write_new( bbtmp);
  write_new( obs_se_ind_in);
  double bbsetmp;
   read_new( bbsetmp);
  write_new( bbsetmp);
  write_new( nyrs_ind_age);
  write_new( nyrs_ind_length(1)+1);
  write_new( yrs_ind_age_in);
  write_new( yrs_ind_length_in);
  write_new( yrs_ind_length_in(1,nyrs_ind_length(1))+1);
  write_new( n_sample_ind_age_in);
  write_new( n_sample_ind_length_in);
  write_new( n_sample_ind_length_in(1,nyrs_ind_length(1)));
  write_new( oac_ind_in);
  write_new( olc_ind_in);
  dvector pltmp(1,nlength);
   read_new(pltmp);
  write_new( pltmp);
  write_new(  wt_ind);
  write_new(  wt_ind(1,endyr));
  write_new( wt_pop);
  write_new( maturity*2.);
  write_new( spawnmo);
  write_new( age_err);
  NewData.close();
  ofstream NewCtl(adstring(model_name)+adstring("_new.ctl"));
  NewCtl << adstring(model_name)+adstring(".dat")<<endl; 
  NewCtl << model_name <<endl;
  write_ctl(sel_map); 
  write_ctl(SrType);        
  write_ctl(use_age_err);        
  write_ctl(retro);        
  write_ctl(steepnessprior);        
  write_ctl(cvsteepnessprior);        
  write_ctl(phase_srec);        
  write_ctl(sigmarprior);        
  write_ctl(cvsigmarprior);        
  write_ctl(phase_sigmar);        
  write_ctl(styr_rec_est);        
  write_ctl(endyr_rec_est+1);        
  write_ctl(Linfprior);        
  write_ctl(cvLinfprior);        
  write_ctl(phase_Linf);        
  write_ctl(kprior);        
  write_ctl(cvkprior);        
  write_ctl(phase_k);        
  write_ctl(Loprior);        
  write_ctl(cvLoprior);        
  write_ctl(phase_Lo);        
  write_ctl(sdageprior);        
  write_ctl(cvsdageprior);        
  write_ctl(phase_sdage);        
  write_ctl(natmortprior);        
  write_ctl(cvnatmortprior);        
  write_ctl(phase_M);        
  write_ctl(npars_Mage);        
  write_ctl(ages_M_changes);        
  write_ctl(Mage_in);        
  write_ctl(phase_Mage);        
  write_ctl(phase_rw_M);        
  write_ctl(npars_rw_M);        
  write_ctl(yrs_rw_M);        
  write_ctl(sigma_rw_M);        
  write_ctl(qprior);        
  write_ctl(cvqprior);        
  write_ctl(phase_q);        
  write_ctl(q_power_prior);        
  write_ctl(cvq_power_prior);        
  write_ctl(phase_q_power);        
  write_ctl(phase_rw_q);        
  write_ctl(npars_rw_q);        
  write_ctl(yrs_rw_q);        
  write_ctl(sigma_rw_q);        
  write_ctl(q_age_min);        
  write_ctl(q_age_max);        
  write_ctl(cv_catchbiomass);        
  write_ctl(nproj_yrs);        
  for (k=1;k<=nfsh;k++)
  {
    write_ctl(fsh_sel_opt(k));
    switch (fsh_sel_opt(k))
    {
      case 1 : // Selectivity coefficients 
      {
        write_ctl( nselages_in_fsh(k)   );  
        write_ctl(phase_sel_fsh(k));  
       // curv_pen_fsh(k) = 1./ (square(curv_pen_fsh(k))*2.); 
       // x = 1./ (y*y*2.); 
       // sqrt(1/2x) = y
        curv_pen_fsh(k) = sqrt(1./(2*curv_pen_fsh(k))); 
        write_ctl(curv_pen_fsh(k) );
        seldec_pen_fsh(k) = sqrt(seldec_pen_fsh(k));
        write_ctl(seldec_pen_fsh(k) );
        write_ctl( n_sel_ch_fsh(k)-1 );  
        for (int i=2;i<=n_sel_ch_fsh(k);i++)
          write_ctl( yrs_sel_ch_fsh(k,i) );  
        for (int i=2;i<=n_sel_ch_fsh(k);i++)
          write_ctl( sel_sigma_fsh(k,i) );  
       // Number of selectivity changes is equal to the number of vectors (yr 1 is baseline)
        // This to read in pre-specified selectivity values...
        write_ctl(sel_fsh_tmp);  
      }
        break;
      case 2 : // Single logistic
      {
        write_ctl(phase_sel_fsh(k));  
        write_ctl( n_sel_ch_fsh(k)-1 );  
        for (int i=2;i<=n_sel_ch_fsh(k);i++)
          write_ctl( yrs_sel_ch_fsh(k,i) );  
        for (int i=2;i<=n_sel_ch_fsh(k);i++)
          write_ctl( sel_sigma_fsh(k,i) );  
        // This to read in pre-specified selectivity values...
        write_ctl(sel_slp_in_fsh(k,1) );
        write_ctl(sel_inf_in_fsh(k,1) );
        break;
      }
    }
  }
  // Indices here..............
  for(k=1;k<=nind;k++)
  {
    write_ctl(ind_sel_opt(k) ) ;  
    switch (ind_sel_opt(k))
    {
      case 1 : // Selectivity coefficients  indices
      {
        write_ctl(nselages_in_ind(k)   );  
        write_ctl(phase_sel_ind(k));  
        curv_pen_ind(k) = sqrt(1./(2*curv_pen_ind(k))); 
        write_ctl(curv_pen_ind(k) );
        seldec_pen_ind(k) = sqrt(seldec_pen_ind(k));
        write_ctl(seldec_pen_ind(k) );
        write_ctl( n_sel_ch_ind(k)-1 );  
        for (int i=2;i<=n_sel_ch_ind(k);i++)
          write_ctl( yrs_sel_ch_ind(k,i) );  
        for (int i=2;i<=n_sel_ch_ind(k);i++)
          write_ctl( sel_sigma_ind(k,i) );  
        write_ctl(sel_ind_tmp);  
      }
      break;
      case 2 : // Single logistic
      {
        write_ctl(phase_sel_ind(k));  
        write_ctl( n_sel_ch_ind(k) );  
        for (int i=2;i<=n_sel_ch_ind(k);i++)
          write_ctl( yrs_sel_ch_ind(k,i)-1 );  
        for (int i=2;i<=n_sel_ch_ind(k);i++)
          write_ctl( sel_sigma_ind(k,i) );  
        write_ctl(sel_slp_in_ind(k,1) );
        write_ctl(sel_inf_in_ind(k,1) );
      }
      break;
    }
    write_ctl("123456789");
  }
  exit(1);
