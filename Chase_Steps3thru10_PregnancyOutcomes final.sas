/*
************************************************************************************************;          
**** program: Chase_Steps3thru10_PregnancyOutcomes.sas                                      ****;
**** purpose: Apply algorithm steps 3 thru 10 which defines pregnancy episodes and outcomes    *;
***                                                                                            *;
***     Note: Call series of macros that look for outcomegrp w/in specified days of an index   *;
***           prenatal enc (step3,4) and assign an outcome for the pregnancy based on the      *;
***           outcomegroups present within that time (step 5ab) and verifying the prenatal     *;
***           encounter is w/in set pregnancy timeline for the preg outcome assigned (step 5cd)*; 
***           Repeat until all prenatal+outcomegrp pregnancies identified. Remaining Prenatal  *;
***           encounters are grouped into pregnancies with unk/missing preg outcome (step8)    *;
***           and remaining OutcomeGrps are grouped into pregnancies (step 10) with preg outc  *;
***           assigned (step 5ab)                                                              *;
***                                                                                            *;
************************************************************************************************;          
*/run;;;;

ods listing;
options nocenter noreplace mprint mlogic  minoperator mindelimiter=',';

%let xdr= %str(\\ad.unc.edu\med\tracs\groups\Research\CDWH\Latour_Chase_22-0689\);
/*X:\DATA\20221220\analysis*/
libname x "&xdr\data\20221220\analysis" access=r;

libname int "&xdr./DATA/20230328_Data_Pull_01/analysis/int_20230516" ;
libname out "&xdr./DATA/20230328_Data_Pull_01/analysis/int_20230516/20231102" ;
libname oldint "&xdr\data\20221220\analysis\int_20230922" access=r;*was20230329;

 %inc "&Xdr.\PROGRAMS\sharon\Preg_Outcomes\chase_Step0_FormatStatements.sas";


*==============================================================================================*;
*  STEP 3 (prep) - Get all prenatal encounters (only need date and base-class)
*==============================================================================================*;
/*proc sql stimer; */
/* create table _codeprenatal_meg1 as */
/*  select distinct patient_deid,enc_date, megan_primary_prenatal,*/
/*         case enc_base_class when '' then "unknown" else enc_base_class end as BASE_CLASS*/
/*   from int.codeprenatal where megan_primary_prenatal order by patient_deid, enc_date;*/
/* create table prenids as select distinct patient_deid,1 as AnyPrenatal */
/*   from _codeprenatal_meg1;*157353;*/
/*quit;*/
/**/
/*proc transpose data=_codeprenatal_meg1*/
/*      out=_codeprenatal_meg1_dts (drop= _name_ _label_) prefix=pren_ ;*/
/* by patient_deid enc_date; id  base_class; var megan_primary_prenatal;*/
/*run;*933210;*/

proc sql;
 create table _codeprenatal_meg1_dts as select * from  int.codeprenatal_meg1_dts;
 create table prenids as select distinct patient_deid,1 as AnyPrenatal 
   from _codeprenatal_meg1_dts;*157353;
quit;


/*%inc 'c:\users\peacocks\macros_step3to10.sas';*/
 %inc "&Xdr.\PROGRAMS\sharon\Preg_Outcomes\working_MacroStep3to10.sas"  ;

/*
macro variables to set on execution:
    OCW - outcomeassigned dataset, window to group outcomes (step 1)
    ALG - outcome concordance algorithm (step 1, options 1-4)
    POW - prenatal outcome window (default 286d)

macro variables initialized during processing (control loops)
   MAXenc - max number of possible prenatal encounters
   MAXOCG - max number of outcomegroups for single pt
   POTMAT - potential index prenatals (have 1+ outcomegrp in OCW)
   MAX - number of outcomegroups found for index - set for each index prenatal 
*/

/*  
%let ocw=7 ; %let alg=1; %let pow=286; 
%let num=1; %let maxenc=5;  %let max=5;
*/
option NOTES MPRINT mlogic;

%Macro Runit(OCW= 7, Alg=1, POW=286 );

%global POTMAT MAXENC MAX NUM MAXOCG;

    *==============================================================================================*;
    *  STEP 3 (prep) - Get Outcomes (step1), with grouping using [OCW]days and concordance [ALG]
    *  creates datasets PrenatalDates_Outc_0 and OutcomeAssigned_Pren_0 needed for steps 4-7
    *==============================================================================================*;

     %PrepOCG 
    
    /*
    *------------------------------------------------------------------------------------------------*;
    * Repeat Steps 3-6 for every Index Prenatal encounter long as there are potential matches[POTMAT] 
    * (IDs with prenatal encounter + outcomegrp), up to MAXENC (max loops if 1:1 outcomegrp and index 
    * prenatal). For 1st run place full notes in log but suppress notes for subsequent runs  
    *------------------------------------------------------------------------------------------------*;
    */

    %DO num = 1 %to  &MAXENC.;*&MaxOCG. ;%*1;

    /*
    *==============================================================================================*;
    *  STEP 3+4 - Find Indexing Prenatal and all OutcomeGrp within [POW] days
    *  (Step 3 - identify index prenatal then Step 4 - get outcome groups w/in [POW] days of index) 
    *  then capture the number of outcomegroups found per index. Go thru steps 5a-5c up to that [MAX]
    *  note - for each iteration, will use prenatal encounters and outcomegroups marked as AVAIL(lc)
    *  after the previous round (NUM-1) of processing 
    *===============================================================================================*;
    */

/*    %if &num. > 1 %then %do; OPTIONS NONOTES NOMPRINT NOSYMBOLGEN; %end; */


     %if &PotMat = 0 %then %do;%*2;
        %put -------------------------------------------------------------------;
        %put ---- loop &num. - Potential Matches: &POTMAT. (0) - move to wrapup ;
        %put -------------------------------------------------------------------;
       %goto RollupStep6 ;

     %end; %*2;

     %else %do; %*when PotMat>0 ;%*3;

       %put;%put ------ index prenatal &num  of &potmat. potential;%put;

     /*
     *------------------------------------------------------------------------------------------------*;
     * create tables IndexPre_&NUM. (index prenatal enc) and IndexPrenatalOutcomes_&NUM. (adds Outcgrp)
     * IndexPrenatalOutcomes_&NUM. is fed into PairEval macro to define pregnancy outcome
     *------------------------------------------------------------------------------------------------*;
     */
        %step34_Link_IdxOCG

        %lognote34

     /*
     *------------------------------------------------------------------------------------------------*;
     * continue if at least 1 outcomegrp is found for any of the Index prenatal encounters [MAX] 
     *------------------------------------------------------------------------------------------------*;
     */

         %if &Max = 0 %then %do; %*max=0;  %*5;

           %CombineResultsFaux(rdp=s5);

                %put ---- NO outcomegrps per index found ;
                %put ---- max=0 &MAX.;
                %put &potmat.;

        %end; %*max =0 loop end; %*5;

        %else %if &Max. > 0 %then %do; %*4;

        %put ---- up to &MAX. outcomegrps per index found ;
 
         /*
         *================================================================================================*;
         * STEP 5a - STEP 5b2d - Evaluate Index+OutcGrp pairs to determine if outcomes part of the same  
         * pregnancy episode (timing as noted in 5b.1-table 3) then determine the pregnancy outcome using
         * priortization of outcomes and outcome types (5b.2a, 5b.2b hierarchy lists). 
         * create *index_outcome_fin_1 -> index+outcomes=preg *this is Key dataset (cont step5)* ;
                  *deletedrows -> included in above preg (used outcomegrp) ;
                  *outcomeneedindex -> outc groups need new prenatal index (step 6);
                  *outidsfin -> status of all outcomegrp in indexprentaloutcomes ;
         *================================================================================================*;
         */
            option nomprint nomlogic nonotes;
            %PAIREVAL(podsn=INDEXPRENATALOUTCOMES_&num. , rdp= S5);

            option mlogic notes;
        %end; %*max>0 loop; %*4;

     %*continue potmat>0 ;
     /*
     *================================================================================================*;
     * STEP 5c - STEP 5d - Find pregnancy timeline based on the pregnancy outcome found (steps 5a-5b)
     * setting the DT_PREGNANCYLOOKBACK (table 4) and checking index prenatal date is w/in the lookback

            *** 5C - maximum lookback period for assigned pregnancy outcome per table 4;
            **     - rephrase - wantto be sure the DT_INDEXPRENATAL is within the Lookback for the Outcome;
            **       so from the Determined DT_PREGNANCYSTART use the table 4 PregnancyLookback period to ;
            **       verify that DT_INDEXPRENATAL is between dt_pregnancylookback and dt_pregnancystart;
            **       dt_pregnancylookback <= dt_indexprenatal <= dt_pregnancystart == Same Pregnancy :-) ;
            **       dt_indexprenatal < dt_pregnancylookback == Lost to F/Up (cant be > dt_pregnancystart);   
     *================================================================================================*;
     */;

     %put;%put ----- STEP 5C 5D ---- ;

        %Step5cd_PREGTIMELINE 

            ;
        *---------------------------------------------------------------------;
        *---------------------------------------------------------------------;

            %put;%put continuing index prenatal NUM &NUM. ;
/*            *Taking index-prenatal-final and linking to all prenatals to mark those visits that are timing-wise with this pregnancy;*/
/*            *resultant dsn has 1 row per orig prenatal encounter for patients in index prenatal dataset;*/

            %PDO_Updt  

            %OGP_Updt  


        ods listing close;

/*        **are there any more potential matches (pts with Find/Need index for prenatal and outcomes );*/
/*        **(actual count doesnt matter as long as > 0) - macvar PotMat >0 will continue processing;*/
        proc sql noprint;  
         select count(distinct a.patient_deid) into :potmat
           from prenataldates_outc_&num. a join outcomeassigned_pren_&num. b
             on a.patient_deid=b.patient_deid where a.status_&num="avail" and b.result_&num.="find index"
             And (dt_outcomegroup_start - enc_date between 0 and &OCW. OR dt_outcomegroup_end - enc_date between 0 and &OCW.) ;
        quit;
        %put;%put ----- After index prenatal &NUM., &potmat. remaining potential index prenatals;

/*   **No more index+outc grps to eval so wrap up pregnancy info, move to steps 8,10;*/


 %end; %*potmat >0 loop end; %*3;

 %put ========== potmat &potmat. num &num. ;

%end; %*1 reach last MaxENC; %*1;

%RollupStep6:

/*    %*==============================================================================================*;*/
/*      %put  ------ WRAP UP step 6  ---------------------------------------------------------------;*/
/*      %put; %put;*/
/*    %*==============================================================================================*;*/
/*      %put  --------------------------------------------------------------------------------------;*/
/*      %put  ------ Roll up findings from prenatal-outcomegrp evals once all pairs checked  ------ ;*/
/*      %put  ------ Add step 7 (Any pregnancies with the AEM outcome recategorized as UAB)*/
/*      %put  --------------------------------------------------------------------------------------;*/
/*    %*==============================================================================================*;*/

/*
     *------------------------------------------------------------------------------------------------*;
     * create dataset: ogp_result_oc - all outcome groups final resul (will further updt step10);
     *                 pdo_result_oc - all prenatal outcomes final status (will further updt step8);
     *                 pregnancy_Indexoutc - all pregnancies defined by index prenatal and outcomegrp
     *------------------------------------------------------------------------------------------------*;
*/
    %Rollup_IdxOutc
        

        *==============================================================================================*;
          %put  --- move to STEP 8 -  PRENATAL encounter dates not linked to outcome groups  ------ ;
        *==============================================================================================*;
/*
     *------------------------------------------------------------------------------------------------*;
     * input datasets: pdo_result_oc, prenataldates_none ;
     * create dataset: pregnancy_prenatalonly - pregnancies defined by prenatal encounters only
                       prenataldates_all_status - all prenatal encounter dates
     *------------------------------------------------------------------------------------------------*;
*/
    %Step8_leftoverpren;


        *==============================================================================================*;
          %put  --- move to STEp 10 -  NO MORE Outcome Groups w/in  &pow. d of Prenatal Index Dates ------ ;
        *==============================================================================================*;
           
/*          proc freq data=outcomeassigned_pren_25;table result_25;run;*/
/*
     *------------------------------------------------------------------------------------------------*;
     * input datasets: ogp_result_oc, pregnancy_indexoutc pregnancy_prenatalonly, _OUTCOMEsASSIGNED_&OCW. ;
     * create dataset: Pregnancy_OutcOnly - pregnancies defined by outcome groups only
                       outcomes_leftoverdates - all remainig dates
     *------------------------------------------------------------------------------------------------*;
*/

    %Step10_LeftoverOutc;

    data pregnancy_indexoutc_&ocw._&alg.; set pregnancy_indexoutc;run;
    data pregnancy_outconly_&ocw._&alg.; set pregnancy_outconly;run;
    data pregnancy_prenatalonly_&ocw._&alg.; set pregnancy_prenatalonly;run;
    data ogp_result_oc_&ocw._&alg. ; set ogp_result_oc;run;
    data pdo_result_oc_&ocw._&alg. ; set pdo_result_oc;run;

    
    *prefer one dataset;
    data pregnancy_&ocw._&alg. (label="Pregnancy data found prenatal encounters and outcome groups (&OCW.d, &alg.)");
     set pregnancy_indexoutc_&ocw._&alg. 
         pregnancy_prenatalonly_&ocw._&alg. (in=a)
         pregnancy_outconly_&ocw._&alg. (in=b);
       prenonly=a; outconly=b;

       *only changes 4.15 - add algorithm variables, mod outconly recs, remove redundant/temp vars, add labels;
       Algorithm="&OCW-&ALG" ;
       Algorithm_Desc="&OCW-&ALG (combined outcomegroups w/in &ocw days for outcome algorithm &alg)" ;

       *04.18 - create distinct pregnancy id using idxpren;
       PregID = compress(idxpren||'-'||algorithm);

      *tech no indexprenatal date for outconly cases (take out filler dt) ;
       if outconly then dt_indexprenatal=.;

      *mod needed - preg_outcome_clean set w/o outconly pregs;
       **YES use clean and Adjust outc-only pregnancies for AEM to UAB;
       if outconly then prenatal_outcome=0;
       if outconly then preg_outcome_clean=preg_outcome;
        if preg_outcome='AEM' then preg_outcome='UAB';

       drop enc_date outcomegroupsfound outcomegroup_count prennum index_finding pno outcomegrp
            anyprenatal dt_index140 dt_index168 pregnancy_counter pregnancy_count status: anyoutcomegrp
       ;
       format dt_: mmddyy10.;

       label prenonly = "Pregnancy - prenatal encounters only"
             outconly = "Pregnancy - outcome groups only"
             Algorithm = "Parameters used to define pregnancy"
             Algorithm_desc = "Parameters used to define pregnancy (long desc)"
             preg_outcomescompared = "Outcomes evaluated in defining pregnancy outcome"
             dt_ltfu = "Date loss to follow-up (prenonly)"
             pregid = "Unique pregnancy id (includes algorithm id)"
             preg_outcome = "Pregnancy outcome defined by evaluation of outcome groups" /*changed*/
             preg_outcome_clean="Pregnancy outcome clean (final)"
             ;
    run;
    *04.16 - one more step - sort by dt_pregoutcome and update preg counter;
    proc sort data=pregnancy_&ocw._&alg. ; by patient_deid dt_preg_outcome;run;
    data pregnancy_&ocw._&alg. ;
     attrib Pregnancy_Number length=4 label="Patient Pregnancy number (counter)";
      set pregnancy_&ocw._&alg. ;
      by patient_deid; if first.patient_deid then Pregnancy_Number=0;
      Pregnancy_number+1;
    run;

%exit:
%mend Runit;


%runit(OCW= 7, Alg=1, POW=286 );

option nomprint nomlogic ;
%runit(OCW= 7, Alg=2, POW=286 );
%runit(OCW= 7, Alg=3, POW=286 );
%runit(OCW= 7, Alg=4, POW=286 );


%runit(OCW= 14, Alg=1, POW=286 );
%runit(OCW= 14, Alg=2, POW=286 );
%runit(OCW= 14, Alg=3, POW=286 );
%runit(OCW= 14, Alg=4, POW=286 );

%runit(OCW= 30, Alg=1, POW=286 );
%runit(OCW= 30, Alg=2, POW=286 );
%runit(OCW= 30, Alg=3, POW=286 );
%runit(OCW= 30, Alg=4, POW=286 );

option dlcreatedir;
libname po  "&xdr./DATA/20230328_Data_Pull_01/analysis/int_20230516/%sysfunc(date(),yymmddn8.)" ;
proc copy  inlib=work outlib=po;
 select pregnancy_: ogp_result_: pdo_result:;
quit;

/**/
/*proc compare data=out.pregnancy_7_1 compare=pregnancy_7_1;*/
/*where prenonly=0 and outconly=0;*/
/*run;*/
/*proc sort data=out.pregnancy_7_1 out=old_prenout71;by idxpren;where prenonly=0 and outconly=0;run;*/
/*proc sort data= pregnancy_7_1 out= prenout71;by idxpren;where prenonly=0 and outconly=0;run;*/
/*proc compare data=prenout71 compare=old_prenout71;run;*/
/**/
/*proc sort data=out.pregnancy_7_1 out=old_pren71;by idxpren;where prenonly=1;run;*/
/*proc sort data= pregnancy_7_1 out= pren71;by idxpren;where prenonly=1;run;*/
/*proc compare data=pren71 compare=old_pren71;run;*/
/**/
/*proc sort data=out.pregnancy_7_1 out=old_OUTC71;by idxpren;where OUTConly=1;run;*/
/*proc sort data= pregnancy_7_1 out= OUTC71;by idxpren;where OUTCOnly=1;run;*/
/*PROC SORT DATA=OUTC71;BY PATIENT_DEID DT_PREG_OUTCOME;*/
/*PROC SORT DATA=OLD_OUTC71;BY PATIENT_DEID DT_PREG_OUTCOME;*/
/*proc compare data=OUTC71 compare=old_OUTC71;*/
/*ID PATIENT_DEID DT_PREG_OUTCOME;*/
/*run;*/
/*PROC FREQ DATA=OLD_OUTC71;TABLE PREG_OUTCOME;RUN;*/
/**/
/*DATA INNEW;SET PREGNANCY_7_1;WHERE PATIENT_DEID='Z101446';*/
/*DATA INOLD;SET OUT.PREGNANCY_7_1;WHERE PATIENT_DEID='Z101446';*/
/*RUN;*/
/*DATA INOLDNEW;SET INOLD INNEW;RUN;*/
/*DATA INOUTG;SET _OUTCOMESASSIGNED_7; *OGP_RESULT_OC_7_1;WHERE PATIENT_DEID='Z101446';*/
/*RUN;*/
/*DATA INPREN;SET PDO_RESULT_OC_7_1;WHERE PATIENT_DEID='Z101446';*/
/*RUN;*/
