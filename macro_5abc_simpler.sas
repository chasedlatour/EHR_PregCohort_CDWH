/*
**** program: Section 5 macros that cover 5b.1 to 5b.2c                                     ****;
**** purpose: Apply Section 5 macros that cover 5a, 5b.1 to 5b.2d                           ****;
***           Check outcomes groups attached to index prenatal encounter episode (step 5) or   *;
***           outcomegrps remaining after linking pregnancy with prenatal encounters (step 10) *;
***           -are they for same pregnancy? (using table 3 Time between outcome groups)        *;
***            if same pregnancy, evaluate paird outcomegrps to assign Pregnancy Outcome       *; 
***            for each iteration use the results from previous pairing for comparison         *;
***                                                                                            *;
***     Note: 5a - 1 outcomegrp, 5b.1 - 2 outcomegrps, 5b.2c - 3+ outcomegrps                  *;
***           method of comparing outcomegrp pair is the same.                                 *;
***     Note: mode pgm from chase_step5abc... - changes include
              revising varnames: PAIRcount to OUTCOMECOUNT PairsCompared to OUTCOMESCOMPARED
                      PAIREDoutcome to PREG_OUTCOME, DT_PAIREDoutcome to DT_PREGNANCYOUTCOME
***          
*/run;;;;

 OPTIONS MINOPERATOR MINDELIMITER=',';           

*setup datasets for PairEval results;
%Macro CombineResultsFaux (rdp=);
    Data &RDP._OutcomeNeedIndex_&NUM.;
      set &RDP._OutcomeNeedIndex_1 (obs=0);
    run;

    Data &RDP._deletedrows_&NUM.; *outcome groups included in preg outcome pair;
     set &RDP._deletedrows_1(obs=0);
    run;
 
    Data &RDP._Index_Outcome_Fin_&NUM.;
      set &RDP._Index_outcome_Fin_1 (obs=0)
          indexprenataloutcomes_&NUM. (keep = patient_deid idxpren prennum dt_indexprenatal outcomegroupsfound);

      keep patient_deid idxpren dt_indexprenatal dt_pregnancyoutcome_start dt_pregnancyoutcome_end 
           PREG_Outcome PREG_CodeType pregnancyoutcomegroupsevaluated outcomegrp OUTCOMEGROUPSFOUND
           PREG_OutcomeCount PREG_OutcomesCompared DT_PREGNANCYOUTCOME_start DT_PREGNANCYOUTCOME_end  DT_PREG_OUTCOME
           PRENNUM PREG_MISO PREG_MIFE
      ;
      Format dt_: date. ;
    run;
    proc sort nodups Data= &RDP._index_outcome_fin_&NUM.; 
      by patient_deid dt_indexprenatal ;
    run;


        data &RDP._outidsfin_&NUM.;
         set &RDP._outidsfin_1; 
           resultant_&NUM. = 'tbd';
          ;
          Format dt_: date. ;
        run;
        proc sort nodups data=&RDP._outidsfin_&NUM.;by patient_deid outcomegrp;run;
       

    proc datasets lib=work nolist;
     delete %do i=1 %to &max.; r&i._:  %end; ipo_: ;
    quit;
%Mend;


*instead of runnig separately including as last step in pair eval;
%macro CombineResults;

Data &RDP._OutcomeNeedIndex_&NUM.;
  set %do i=1 %to &max.; r&i._D_NewPren (in=a) r&i._D_NewPren2 (in=b) %end; ;
  Format dt_: date. ;
  *only retain info re outcomegroup - other info irrelevant since not linked to this prenatal pregnancy;
  if a then NeedPren='a'; if b then Needpren='b'; *for tracking only;
  KEEP patient_deid outcomegrp dt_outcomegroup_start dt_outcomegroup_end prennum dt_indexprenatal needpren;
run;
proc sort Data=&RDP._OutcomeNeedIndex_&NUM.;
 by patient_deid dt_indexprenatal dt_outcomegroup_start ;
run;

Data &RDP._deletedrows_&NUM._base; *outcome groups included in preg outcome pair;
  set %do i=1 %to &max.; r&i._Delete %end; ;
  Format dt_: date. ;
  *since deleted row info re preg not captured on this outcomegrp rec (can find on Paired row);
  KEEP patient_deid outcomegrp dt_outcomegroupstart dt_outcomegroupend idxpren dt_indexprenatal prennum;
run;
proc sort Data=&RDP._deletedrows_&NUM._base ;
 by patient_deid dt_indexprenatal;
run;

Data &RDP._Index_Outcome_Fin_&NUM.;
  set %do i=1 %to &max.; r&i._F_Fin  r&i._I_Fin %end; ;
/*  set %do i=1 %to &max.; r&i._F_Fin r&i._G_Fin %end; ;*/
  *drop pairedcodetypex count;

  keep patient_deid idxpren dt_indexprenatal dt_pregnancyoutcome_start dt_pregnancyoutcome_end 
       PREG_Outcome PREG_CodeType pregnancyoutcomegroupsevaluated outcomegrp OUTCOMEGROUPSFOUND
       PREG_OutcomeCount PREG_OutcomesCompared DT_PREGNANCYOUTCOME_start DT_PREGNANCYOUTCOME_end  DT_PREG_OUTCOME
       PRENNUM PREG_MISO PREG_MIFE
  ;
  Format dt_: date. ;
run;*04.18 RETAIN MISO,MIFE FLAGS;
proc sort nodups Data= &RDP._index_outcome_fin_&NUM.; 
  by patient_deid dt_indexprenatal ;
run;

*add preg info to deletedrows (i.e. outcomegrps rolled up, accounted for in pairedoutcome);
proc sql;
 create table &RDP._deletedrows_&num. as
  select a.*, b.*
  from &RDP._deletedrows_&NUM._base a join &RDP._index_outcome_fin_&num. b 
    on a.patient_deid=b.patient_deid and a.dt_indexprenatal=b.dt_indexprenatal;
quit;

    data &RDP._outidsfin_&NUM.;
     set &RDP._outcomeneedindex_&NUM.(in=a) &RDP._index_outcome_fin_&NUM.(in=b) &RDP._deletedrows_&NUM.(in=c);
     *keep patient_deid outcomegrp resultant_&NUM.  IDXPREN;
        if a then resultant_&NUM. ='need index';
        if b then resultant_&NUM.='paired'; 
        if c then resultant_&NUM.='rollup'; *change from dropped to rollup 01.23;
  *    keep patient_deid outcomegrp idxpren dt_indexprenatal outcomegroupsfound 
           paircount pairscompared pairedoutcome dt_pairedoutcome pairedcodetype
           dt_pregnancyoutcome_start dt_pregnancyoutcome_end resultant_&num.; 
      ;
      Format dt_: date. ;
    run;
    proc sort nodups data=&RDP._outidsfin_&NUM.;by patient_deid outcomegrp;run;
   
    proc sql;
/*     create table &RDP._missed_index_&NUM. as select a.* from indexprenataloutcomes_&NUM. a left join &RDP._index_outcome_fin_&NUM. b*/
     create table &RDP._missed_index_&NUM. as select a.* from &PODSN. a left join &RDP._index_outcome_fin_&NUM. b
       on a.idxpren=b.idxpren where b.idxpren=. and a.outcomegroupsfound>0;

/*     create table &RDP._lost_outgrp_&NUM. as select a.* from indexprenataloutcomes_&NUM. a left join &RDP._outidsfin_&NUM. b*/
     create table &RDP._lost_outgrp_&NUM. as select a.* from &PODSN. a left join &RDP._outidsfin_&NUM. b
       on a.patient_deid=b.patient_deid and a.outcomegrp=b.outcomegrp
        where b.outcomegrp=.  and a.outcomegrp ne .;
    quit;

    proc datasets lib=work nolist;
     delete %do i=1 %to &max.; r&i._:  %end; ipo_: ;
    quit;


%mend;

*podsn - prenatal+outcomegrp or remaining outcomegrps datasets dataset;
*rdp - results dataset prefix (using S5 (step 5) and S10 (step 10);
%macro PairEval(podsn, rdp);
%if &ocw= %then &ocw=1;  

    proc sort data= &podsn. ;*indexprenataloutcomes_1; 
     by  patient_deid outcomegrp dt_outcomegroup_start;
    run; 

  %do round= 1 %to &Max. ;

  /*%put ------------- &ROUND. / &MAX. ----------------;*/

         %if &Round = 1 %then %do;

            data ipo_5abc_&Round.    
                 ipo_5abc_next_&round. (drop=PREG_Outcome dt_pregnancy: count) ;

             set &podsn. ;*IndexPrenatalOutcomes_1   ;
              by patient_deid;
              where outcomegroupsfound>0;

              *8.15 add PairCounter;
              PREG_OutcomeCount=1; *note: was var PairCount;
              length PREG_OutcomesCompared $100.; *enough room to capture 25 pairs;
                PREG_OutcomesCompared=Outcome_Assigned&ALG.; *start with 1st outcome, add more in macro;

                *select the outcome for the paired outcomegrps - 5b.2;*moved from 5abc;
                attrib PREG_Outcome length=$10 label="Outcome for paired prenatal on same pregnancy";
                PREG_Outcome='';

              *8.17 add initial date values;
                Dt_PregnancyOutcome_Start = dt_outcomegroup_start;
                Dt_PregnancyOutcome_End = dt_outcomegroup_end;

              if first.patient_deid then count=0;
              count+1;

               if count le 2 then output ipo_5abc_1 ;**keep 1st set of outcomes ;*8962;
                             else output ipo_5abc_next_1 ;*11547;
            run;
            *108217+11180;
        %end;


    *prep the set of outcomegrps to be evaluated for same pregnancy;
    proc sort data= ipo_5abc_&Round.  ; 
        by patient_deid descending dt_outcomegroup_start;
    run;*reversing the order so can attach Next outcome info to record;

    *** setup - for each outcome attached to prenatal index get the subsequent outcome to check for timing;
    data R&round._A_prep ; 
      set ipo_5abc_&Round.  ;  
      by patient_deid;


      lptid= lag1(patient_deid);
      dt_lds = lag1(dt_outcomegroup_start);
      dt_lde = lag1(dt_outcomegroup_end);
      lot = lag1(outcome_assigned&ALG.);
      loc = lag1(outcome_assigned_codetype&ALG.);
      lmf = lag1(mifepristone);
      lms = lag1(misoprostol); 
      lid = lag1(inpatPRDelivery);
      LDA = LAG1(DT_OUTCOME_ASSIGNED&ALG. ); 

      *for preg w/ prev outcome group this will be >0;
      *IF missing(PregnancyOutcomeGroupsEvaluated) then PregnancyOutcomeGroupsEvaluated = 1; 

      if patient_deid = lptid then do;
          dt_OutNext_start = dt_lds;
          dt_OutNext_end = dt_lde ;
          Days_OutEndNextStart = dt_lds - dt_PregnancyOUtcome_end ;

          OutNext_Assigned = lot ;
          OutNext_CodeType = loc ;
            outnext_codetype_cd = put(outnext_codetype, $outcd.);
          OutNext_Mifepristone = lmf;
          OutNext_Misoprostol = lms;
          OutNext_InpatPRDelivery= lid;

          OUTNEXT_OUTCOMEDT = LDA;

          PREG_OutcomesCompared = cats(PREG_OutcomesCompared,'-', outnext_assigned);
          PREG_OutcomeCount+1 ;
      end;
      drop lot loc lptid dt_lds dt_lde  lmf lms;
      format dt_: mmddyy10.;
      label
        dt_outnext_start = 'Date of subsequent outcome start'
        dt_outnext_end = 'Date of subsequent outcome group end'
        days_outendnextstart = 'Days from Outcome group end to start of subsequent outcome'
        outnext_assigned = 'Subsequent outcome assigned'
        outnext_codetype = 'Subsequent outcome code (PR/DX/RX)'
        outnext_mifepristone= 'Mifrepristone RX on subsequent outcome'
        outnext_misoprostol = 'Misoprostol RX on subsequent outcome'
/*        PregnancyOutcomeGroupsEvaluated = "Number of outcome groups included in pregnancy"*/
        PREG_OutcomeCount = "Number of outcome groups included in pregnancy"
      ;
    run;*121437;

    proc sort data= R&ROUND._A_prep ;*r&round.a_indexoutnext_1;
      by patient_deid dt_outcomegroup_start;
    run;*put back in order;


    *** Now for Index with multiple groups check for time between 1st and 2nd to;
    *** determine if outcomes represent possibly 2 pregnancies and should be ;
    *** attached to different Index prenatal + outcome groups;

    data R&Round._B_sameck R&round._delete ; *r&round.b_indexouttime_1;
     set R&round._A_prep ; *r&round.a_indexoutnext_1;
     by patient_deid;

     * Time between outcomes within bounds in 5b.1 table 3 to check if outcomes can be tied to same index;
     * if first/last then only 1 record to be evaluated in the round, otherwise check pair for timing;
     * note first/last always true for those with one outcomegroup;
     if first.patient_deid and last.patient_deid then OnlyRec=1;
     else do;
       SamePregnancy=0;
        
       *flag for pregnancy outcome fields (set to 1st outcomegrp values round 1 / if not prev set);
       *after round1 the Preg/Paired values are set from evaluation of previous outcomegrp pairs;*02.2024;
        %if &round. = 1 %then %do;
         Preg_PRDelivery = inpatPRdelivery; *true if 1st round;
         Preg_MIFE = mifepristone;
         Preg_MISO = misoprostol;
         PREG_Outcome = outcome_assigned&ALG.;
         PREG_CodeType =  put(outcome_assigned_codetype&ALG. , $outcd.);
         Dt_PREG_Outcome = dt_outcome_assigned&ALG.;
        %end;      

       *****01.25 - NEED TO COMPARE THE NEXT OUTCOMEGROPU TO THE PAIREDOUTCOME FOR THE PREG AT THIS STAGE****;
         if PREG_Outcome in ('LBS' 'LBM' 'MLS') then do;
              if outnext_assigned in ('LBS' 'LBM' 'MLS') and days_outEndnextStart < 168 then SamePregnancy=1;
              else if outnext_assigned in ('SB') and days_outEndnextStart < 168 then SamePregnancy=1;
              else if outnext_assigned in ('EM') and days_outEndnextStart < 70 then SamePregnancy=1;
              else if outnext_assigned in ('SAB' 'IAB' 'UAB') and days_outEndnextStart < 70 then SamePregnancy=1;
              else if outnext_assigned in ('UDL') and days_outEndnextStart < 168 then SamePregnancy=1;
         end;
         else if PREG_Outcome in ('SB') then do;
              if outnext_assigned in ('LBS' 'LBM' 'MLS') and days_outEndnextStart < 168 then SamePregnancy=1;
              else if outnext_assigned in ('SB') and days_outEndnextStart < 168 then SamePregnancy=1;
              else if outnext_assigned in ('EM') and days_outEndnextStart < 70 then SamePregnancy=1;
              else if outnext_assigned in ('SAB' 'IAB' 'UAB') and days_outEndnextStart < 70 then SamePregnancy=1;
              else if outnext_assigned in ('UDL') and days_outEndnextStart < 168 then SamePregnancy=1;
         end;
         else if PREG_Outcome in ('EM') then do;
              if outnext_assigned in ('LBS' 'LBM' 'MLS') and days_outEndnextStart < 154 then SamePregnancy=1;
              else if outnext_assigned in ('SB') and days_outEndnextStart < 154 then SamePregnancy=1;
              else if outnext_assigned in ('EM') and days_outEndnextStart < 56 then SamePregnancy=1;
              else if outnext_assigned in ('SAB' 'IAB' 'UAB') and days_outEndnextStart < 56 then SamePregnancy=1;
              else if outnext_assigned in ('UDL') and days_outEndnextStart < 154 then SamePregnancy=1;
         end;

/*         *12.13 ACCt for outcome1 re-assigned RX-MIS,RX-MIF;*/
         *else if outcome_assigned1 in ('SAB' 'IAB' 'UAB') then do;  
         else if PREG_Outcome in: ('SAB' 'IAB' 'UAB' 'RX') then do;
          if outnext_assigned in ('LBS' 'LBM' 'MLS') and days_outEndnextStart < 140 then SamePregnancy=1;
              else if outnext_assigned in ('SB') and days_outEndnextStart < 140 then SamePregnancy=1;
              else if outnext_assigned in ('EM') and days_outEndnextStart < 42 then SamePregnancy=1;
              else if outnext_assigned in ('SAB' 'IAB' 'UAB') and days_outEndnextStart < 42 then SamePregnancy=1;
              else if outnext_assigned in ('UDL') and days_outEndnextStart < 154 then SamePregnancy=1;
         end;
         else if PREG_Outcome in ('UDL') then do;
              if outnext_assigned in ('LBS' 'LBM' 'MLS') and days_outEndnextStart < 168 then SamePregnancy=1;
              else if outnext_assigned in ('SB') and days_outEndnextStart < 168 then SamePregnancy=1;
              else if outnext_assigned in ('EM') and days_outEndnextStart <  70 then SamePregnancy=1;
              else if outnext_assigned in ('SAB' 'IAB' 'UAB') and days_outEndnextStart < 70 then SamePregnancy=1;
              else if outnext_assigned in ('UDL') and days_outEndnextStart < 168 then SamePregnancy=1;
         end;
     end;

      *Reset pregnancy end dates - end date will change if outgrps represent same pregnancy;
          if samepregnancy=1 then dt_PregnancyOutcome_End = dt_outnext_end;

      *drop rows accounted for already - only works for 2 -can drop 2nd row as its captured as Next in 1st otucomegrp;
      lsi=lag1(samepregnancy);
       if lsi=1 then output r&round._delete; *not needed but will help track counts, now all pair for same preg on 1 obs ;
       *else output R&round._B_sameck ;
        ELSE DO;
          *11.28 ugh pairs adj was not correct - need to back out last outc evaluated and shift to next step;
          *pairscompared = substr(pairscompared, 1, length(pairscompared)-4);;  *need to take off last outcome (-ABC);
          output R&round._B_sameck ;
        END;
       drop lsi;

    run;

    *** now separate  ;
    Data R&Round._C_eval (label="index w/ identified outcomes, clear (outgrp marked as same pregnancy for evaluation)")
         R&Round._D_newpren (label="outcomes that need a new index prenatal")
         R&Round._E_done (label="outcomes groups where Only 1 for index Or most recent pairing was not for same pregnancy")
     ;
     set R&Round._B_sameck ; 
      by patient_deid;

       *revise to work with dsn that only has 1-2 per prenatal (works with macro rounds);
       IF first.patient_deid and Last.patient_deid then do;
          if OnlyRec then  OUT_E=1; *output R&Round._E_Done ;*no need to evaluate if there was only 1 remaining rec for pt;  
          else if samepregnancy then OUT_C=1; *output R&ROund._C_eval ;      *evaluate if paired outcomes are same pregnancy;
       end;

       else if  samepregnancy=0 then do;          *2 potential outcomes too far apart to count as same preg;
          if first.patient_deid then do;   *the first rec is the fin prenatal-outcome pair;
            Out_E=1; 
            PREG_OutcomesCompared = substr(PREG_OutcomesCompared,1, (length(PREG_OutcomesCompared)-(length(scan(PREG_OutcomesCompared,-1,'-'))+1)) );  *need to take off last outcome (-ABC);
            PREG_OutcomeCount =PREG_OutcomeCount-1;
          end; 
          else out_d=1;* output R&ROUND._D_newpren;                       *the subsequent should be paired with new index;
       end;

       else if pregnancyoutcomegroupsevaluated >0 then OUT_E=1; *output R&Round._E_done;*this pair not the same preg so revert to prev pairing;
       else  OUT_D=1; *output R&ROUND._D_newpren ;                         *all others are not the same pregnancy so link to new prenatal;

        if out_c then output R&Round._C_eval;  *paired outcomegrps found to be for same pregnancy - evaluate pairs for pregnancy outcome (step G);
        if out_d then output R&Round._D_newpren; 
        if out_e then output R&Round._E_Done ; *no need to evaluate further as only 1 outcome or added outcomegrp timing indicates diff pregnancy;
         DROP out_c out_d out_e;
    run;

    **** get previous record for cases where last outcome group was in previous iteration;
    **** for 1st pass of a grp where there are 2 pairs but not for same pregnancy just use the rec as is (next outgrp leave for now);
    ***  postmtg 12.14 - ADJUST RX-MIS to use original abortion type and change the pairedcodetype to RS (MIS) and RF (MIF);

    %IF &Round. # 1  %THEN %DO;  
      data R&round._F_Fin; 
       set R&Round._E_done;
         if outcome_assigned_codetype&ALG.='RX' and mifepristone =1  then PREG_CodeType='RF'; *PairedCodeType='RX-MIF';
         else if outcome_assigned_Codetype&ALG.='RX' then PREG_CodeType='RS'; *pairedcodetype='RX-MIS';
         else PREG_CodeType= Put(outcome_assigned_codetype&ALG., $outcd.); *assigns to RX,DX,PR;
         *else Pairedcodetype= outcome_assigned_codetype1 ;
        PREG_Outcome = outcome_assigned&ALG.;
        DT_PREG_Outcome = DT_OUTCOME_ASSIGNED&ALG. ;
/*        drop dt_outnext: days_Outendnextstart Outnext: onlyrec sampPregnancy comp_outcome;*/
      run;
    %END;
    %ELSE %DO;
      proc sql;
       create table R&round._F_Fin as
        select b.* from r&Round._E_done a join R%eval(&round.-1)_G_OUTC b
            on a.patient_deid=b.patient_deid and a.dt_indexprenatal =b.dt_indexprenatal ;
      quit;
     %END;

    *** find pregnancy outcome based on paired outcome groups  ;
    data R&round._G_Outc  (label="pairs to pregnancy outcome"); 
     set R&Round._C_eval 
     ;  
      by patient_deid;

      Paired=1; *indicate that pregnancy outcome based on paired results;

        /***** done CHANGE - Add start/end dates for outcome ASSIGNEd to the Pregnancy, if outcomegrps match keep earlier date*/
        ** 8.18 check this - paired dates are off (see id Z1003395);

        *Determine pregnancy outcome based on paired outcome groups selecting based on priority in revised 5.2a+5.2b;
        *order: inptdel-pr, pr-pr (hier3), rx-rx (mife, miso), dx-dx (hier4),  any pr, any rx-mif, any rx-mis, any dx;

        Array prord(*) LBM LBS MLS SB UDL SAB IAB UAB EM AEM;  *this is hierarchy 3 5b.2b (1st 5 are hierarcy #2);
        Array dxord(*) SAB IAB UAB EM AEM SB LBM LBS MLS UDL;  *this is hiearchy 4 5b.2b;

        *select the outcome for the paired outcomegrps - 5b.2;
        PREG_CodeTypex = cats(PREG_CodeType,'-', OutNext_Codetype); *DX-DX, DX-PR, DX-PR; *better to have 3dig;

        tmp_found=0;

        *if EITHER outcome proc-based inpatient delivery this is prioritized in 5b.2b;
        *take the highest priorty outcome in the pair;
          if  PREG_PRDELIVERY=1 And OUTNEXT_INPATPRDELIVERY = 1 THEN DO i=1 to dim(prord) Until (tmp_found=1);
            if PREG_Outcome = vname(prord(i)) or outnext_assigned = vname(prord(i)) then  DO;
               PREG_Outcome= vname(prord(i));
               Preg_PRDelivery =1;
               PREG_CodeType = "PR";
               **NOT QUITE RIGHT - IF HIGHER PRIORITY IS ON 2ND DATE THEN CHANGE DT_PREG_OUTCOME TO 2ND DATE;
                if PREG_Outcome = vname(prord(i)) then Dt_PREG_Outcome = Dt_PREG_Outcome;  *can use 1st as that is earlier date;
                else Dt_Preg_Outcome = outnext_outcomedt;
               tmp_found=1;
            end;
          end;
          %IF &ROUND # 1 %then %do;
          else if PREG_PRDELIVERY=1 THEN DO i=1 to dim(prord) Until (tmp_found=1); *unncessary after round 1, resetting to itself;
              if PREG_Outcome = vname(prord(i)) THEN DO;
               PREG_Outcome= vname(prord(i));
               Preg_PRDelivery =1;
               PREG_CodeType = "PR";
               tmp_found=1;
                *no need to update date since code type did not change;
              end;
          end;
          %END;
          else if Outnext_InpatPRDELIVERY=1 THEN DO i=1 to dim(prord) Until (tmp_found=1); 
              if outnext_assigned = vname(prord(i)) THEN DO; 
               PREG_Outcome= vname(prord(i));
               Preg_PRDelivery =1;
               PREG_CodeType = "PR";
                Dt_PREG_Outcome = outnext_outcomedt ; *Reset preg outcome date to best guess given pairs;
               tmp_found=1;
             end;
          end;

        *if paired outcomes are defined the same (e.g. dx-dx) then go to hierarchy 5b.2b;
                *only update pregnancy date if changing type (if same type keeping earliest);
        ELSE if PREG_CodeTypex in ( "PR-PR" ) then do i=1 to dim(prord) Until (tmp_found=1);
            if PREG_Outcome = vname(prord(i)) or outnext_assigned = vname(prord(i)) then do;
               PREG_Outcome= vname(prord(i));
               PREG_CodeType ="PR";
               *no need to update date since code type did not change;
                *IF 2ND DATE IS HIGHER PRIORITY OUTCOME (IN DXORD) THEN RESET DATE;
                if PREG_Outcome = vname(prord(i)) then Dt_PREG_Outcome = Dt_PREG_Outcome;  *can use 1st as that is earlier date;
                else Dt_Preg_Outcome = outnext_outcomedt;
                tmp_found=1;
            end;
        end;

        *pregmife pregmiso;
        else if PREG_CodeTypex in ( "RX-RX" ) then do; *same as 001,001 - only UAB by rx;
            if PREG_MIFE =1 or outnext_mifepristone=1 then PREG_MIFE=1;
            else if PREG_MISO = 1 or outnext_misoprostol=1 then PREG_MISO=1;
              *no need to update date since code type did not change;
                *IF 2ND DATE IS HIGHER PRIORITY OUTCOME (IN DXORD) THEN RESET DATE;
               PREG_Outcome = 'UAB';
        end;  

        else if PREG_CodeTypex in ( "DX-DX" ) then do i=1 to dim(dxord) until (tmp_found=1);
            if PREG_Outcome = vname(dxord(i)) or outnext_assigned = vname(dxord(i)) then do;
                PREG_Outcome= vname(dxord(i));
                PREG_CodeType="DX";
                *no need to update date since code type did not change - WRONG;
                *IF 2ND DATE IS HIGHER PRIORITY OUTCOME (IN DXORD) THEN RESET DATE;
                tmp_found=1;
            end;
        end;
 
        *order: inptdel-pr, pr-pr (hier3), dx-dx (hier4), rx-rx (mife, miso), any pr, any rx-mif, any rx-mis, any dx;
        *11.30 - oops add the date update here;
        else if PREG_CodeType='PR' then do; 
         *no change needed, resultant pregnancy outcome same;
        end;
        else if Outnext_codetype_cd = 'PR' then do;
          PREG_Outcome = outnext_assigned;
          PREG_CodeType='PR';
          dt_PREG_Outcome = outnext_outcomedt;
        end;

        else if PREG_CodeType='RX' and PREG_MIFE=1 then do;;
         *no change - already mife defined preg outcome;
        end;
        else if Outnext_codetype_cd = 'RX' And Outnext_mifepristone=1 then do;
          PREG_MIFE=1;
          PREG_Outcome = 'UAB';
          PREG_CodeType='RX';
          dt_PREG_Outcome = outnext_outcomedt;
        end;
        
        else if PREG_CodeType='RX' and PREG_MISO=1 then do;;
         *no change - already mife defined preg outcome;
        end;
        else if Outnext_codetype_cd = 'RX' And Outnext_misoprostol=1 then do;
          PREG_MISO=1;
          PREG_Outcome = 'UAB';
          PREG_CodeType='RX';
          dt_PREG_Outcome = outnext_outcomedt;
        end;

        else if PREG_CodeType='DX' then do; 
         *no change needed, resultant pregnancy outcome same;
        end;
        else if Outnext_codetype_cd = 'DX' then do;
          PREG_Outcome = outnext_assigned;
          PREG_CodeType='DX';
          dt_PREG_Outcome = outnext_outcomedt;
        end;

        drop i tmp_found LBM LBS MLS SB UDL SAB IAB UAB EM AEM;
        format dt_: mmddyy10.;
        label dt_pregnancyoutcome_start = "Date start of 1st outcomegrp for pregnancy"
              dt_pregnancyoutcome_end = "Date end of last outcomegrp for pregnancy"
              dt_preg_outcome = "Date of pregnancy outcome"
              preg_outcome = "Pregnancy outcome assigned"
              preg_codetype = "Pregnancy outcome assigned codetype (DX/PR/RX)"
              preg_miso = "Misoprostol RX on pregnancy"
              preg_mife = "Mifepristone RX on pregnancy"
              ;
    run;

   *** now take findings and place back with outcome groups waiting for evaluation;
     * exclude those who have already been decided;


    *pull out those outcomegrps retained for next iteration that are after found pregnancy outcome dates;
    *(these would need a new prenatal index);
    *I.E. if outcomegrp for pt needs new Index, then all remaining Outc grp for patient need new index;
    *     so remove those patients from the Next Round of outcgrp evaluated for this Index Prenatal ;
    data ipo_5abc_next_&ROUND._adj (Label="Cases eligible for next pair evaluation") 
         R&Round._D_NewPren2  (label="Addl remaining outcgrps for those found to need index"
                               keep=patient_deid outcomegrp idxpren);
      merge r&round._D_NewPren(keep=patient_deid in=a) 
            ipo_5abc_next_&ROUND. (in=b);
       by patient_deid;
        if a and b then output R&round._D_NewPren2;
        else if b then output ipo_5abc_next_&ROUND._adj ;
    run;

    data R&Round._H_nextrnd (label="Info needed for next pair comparison i.e. next round"); 
     set  ipo_5abc_next_&Round._adj
          r&round._G_Outc  (in=a drop= dt_outnext: outnext: samepregnancy onlyrec) ;

     drop count; *gonna reset in prep below;
    run;
    proc sort data=R&Round._H_Nextrnd; by patient_deid dt_outcomegroup_start;
    run;

    Data ipo_5abc_%eval(1+&round.) (label="outcome pair to evaulate in next round")
         ipo_5abc_next_%eval(1+&round.)  (label="outcome groups to pair in successive rounds") 
         R&round._I_Fin (label="done - only rec left was pair just evaluated")
         ;
     set R&ROUND._H_nextrnd;  ;
     by patient_deid;

      if first.patient_deid then count=0;
      count+1;
       IF first.patient_deid and last.patient_deid then output R&Round._I_Fin ;
       else do;
         if count le 2 then output ipo_5abc_%eval(1+&round.);  
                       else output ipo_5abc_next_%eval(1+&round.);  
       end;
    run;

    Data R&Round._Final ; set r&round._F_fin r&round._I_fin ; run;

%end;

    %CombineResults 
            %lognote5ab

%mend;

