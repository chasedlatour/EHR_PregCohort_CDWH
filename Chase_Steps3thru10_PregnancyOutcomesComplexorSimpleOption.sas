/************************************************************************************************          
**** PROGRAM: Chase_Steps3thru10_PregnancyOutcomes_COMPLEX.sas                               ****
**** PURPOSE: Apply algorithm steps 3 thru 10 which defines pregnancy episodes and outcomes     *
***           using complex approach to Step8 (prenatal only pregnancies)                       *
***                                                                                             *
***     NOTE: Call series of macros that look for outcomegrp w/in specified days of an index    *
***           prenatal enc (step3,4) and assign an outcome for the pregnancy based on the       *
***           outcomegroups present within that time (step 5ab) and verifying the prenatal      *
***           encounter is w/in set pregnancy timeline for the preg outcome assigned (step 5cd) * 
***           Repeat until all prenatal+outcomegrp pregnancies identified. Remaining Prenatal   *
***           encounters are grouped into pregnancies with unk/missing preg outcome (step8)     *
***           and remaining OutcomeGrps are grouped into pregnancies (step 10) with preg outc   *
***           assigned (step 5ab)                                                               *
***                                                                                             *
***                                                                                             *
***   INPUTS:                                                                                   *
***                                                                                             *
***          *macro variables to set on execution:                                              *
***           Runit(OCW= 7, Alg=1, POW=286, PRENSIMPLE=Y, POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 ) *
***            OCW - outcomeassigned dataset, window to group outcomes (step 1) (default 7)     *
***            ALG - outcome concordance algorithm (step 1, options 1-4) (default 1)            *
***            POW - prenatal outcome window (step 4) (default 286d)                            *
***            PRENSIMPLE - prenatal-only pregnancies, simple approach (step 8) (default Y)     *
***            POSPW - prenatal-only same preg window (step 8, step8-complex) (default 140)     *
***            PRENCOMPLEX - prenatal-only pregnancies, using available Gestational age (GA)    *
***                          estimates (step 8-complex) (default Y)                             *
***            DAYADJ - GA adj to link encounters to pregnancy (step 8-complex) (default 60)    *
***                                                                                             *
***                                                                                             *
***                                                                                             *
***	MODIFICATIONS:                                                                              *
***		- 05-01-24: CDL reviewed, added comments, set up, etc.								    *
***		- 05-2024: CDL conducted QC. SPH and CDL reviewed. All modifications were agreed upon   *
***			by both.                                                                            *
***     - 06.18.24: sph - correction to pregnancy_outcome_clean (change start approx line 492)  *
***     - 07.01.24: sph - modify to include options to use Simple Prenatal-only pregnancy or    *
***                 Complex prenatal pregnancy approach (or both). Each approach results in     *
***                 separate final pregnancy datasets                                           *
***                                                                                             *
***     - 08.05.24: sph - point to libraryoptions file, remove _codeprenatal_meg datastep and   *
***                  call permanent dataset codeprenatal_meg (removed commented out code)       *
************************************************************************************************/ 



/***********************************************************************************************

TABLE OF CONTENTS
	- 00 - SET UP LIBRARIES, PATHS, OPTIONS
	- 01 - DATA PREPARATION
	- 02 - RUN NECESSARY MACROS
	- 03 - CREATE MACRO TO RUN STEPS 3-10
	- 04 - RUN STEPS 3-10

***********************************************************************************************/














/***********************************************************************************************

							00 - SET UP LIBRARIES, PATHS, OPTIONS

***********************************************************************************************/

*Set up options;
ods listing;
options nocenter noreplace mprint mlogic  minoperator mindelimiter=',';



/*Specify the location of files on the server*/
/*Point to location of files - string/text will be added to libname statements; */

%let xdr= %str(\\ad.unc.edu\med\tracs\groups\Research\CDWH\Latour_Chase_22-0689);
/*CDL version:
%let xdr= %str(W:\);
*/


%inc "&xdr.\PROGRAMS\preg_encounter_algorithm\chase_All_LibrariesAndOptions.sas";

/* ******* ALTERNATIVE to running %inc above, use the following libname statements ******; */

/**Data received - most ^raw^ code files, manipulations but no linkage to the reference files;*/
/*libname Raw "&xdr\data\20230328_Data_Pull_01\analysis"  ;*/
/**/
/**/
/**Data received and divided according to reference files as of date run;*/
/**note: add datestamp to filename (option DLCREATEDIR above will create folder if doesnt exist);*/
/**/
/*libname int "&xdr.\data\20230328_Data_Pull_01\analysis\int_202408" ;                *<====== CHECK PATH;*/
/**/
/**/
/**Data received and divided according to reference files as of date run;*/
/**note: add datestamp to filename (option DLCREATEDIR above will create folder if doesnt exist);*/
/**/
/*libname Out "&xdr.\data\20230328_Data_Pull_01\analysis\int_202408\preg_alg" ;       *<====== CHECK PATH;*/
/*libname Outd "&xdr.\data\20230328_Data_Pull_01\analysis\int_202408\preg_alg\details" ;       *<====== CHECK PATH;*/
/**/
/**/
/*****Specify the path for algorithm programs;*/
/*%let algpath=&xdr.\programs\preg_encounter_algorithm;             *<===== CHECK PATH; */
*/
;;;;
%put &algpath.;

*to match libs referenced below;
libname po(outd);





*Pull in the format statements;
%inc "&Xdr.\PROGRAMS\preg_encounter_algorithm\chase_Step0_FormatStatements.sas";
 













/***********************************************************************************************

									01 - DATA PREPARATION


Get all prenatal encounters (only need date and base-class).
***********************************************************************************************/






*Create necessary datasets;

proc sql;

	create table _codeprenatal_meg1_dts as 
	select * 
	from int.codeprenatal_meg1_dts
	;

	create table prenids as 
	select distinct patient_deid 
	from _codeprenatal_meg1_dts
	;
	quit;











/***********************************************************************************************

								02 - RUN NECESSARY MACROS


These will be used to execute steps 3-10.
***********************************************************************************************/

*These are the primary macros to be included;

%inc "&AlgPath.\Chase_Steps3thru10_Macros_file1.SAS"  ; *was working_macrostep3to10;

**added for complex step 8 **;
%inc "&algPath.\Chase_Step8Complex_Macro_file3.sas"  ;




*Macros specific to Step 5;
 /*
 *------------------------------------------------------------------------------------------------*;
 * Step 5a - Step 5b2d - Evaluate Index+OutcGrp pairs to determine if outcomes part of the same
 * pregnancy episode (timing as noted in 5b.1-table 3) then determine the pregnancy outcome using
 * priortization of outcomes and outcome types (5b.2a, 5b.2b hierarchy lists).
 *------------------------------------------------------------------------------------------------*;
 */;


%inc "&AlgPath.\Chase_5abc_simpler_macros_file2.SAS"  ; *was macro_5abc_simpler;





/***********************************************************************************************

							03 - CREATE MACRO TO RUN STEPS 3-10

Create a macro to run the steps 3-10 using the macros defined in the steps above. This macro will then
be run for each algorithm and window for grouping pregnancy outcomes (e.g., 7, 14, 30 days)
***********************************************************************************************/


/*
macro variables to set on execution:
    OCW - outcomeassigned dataset, window to group outcomes (step 1) (default 7)
    ALG - outcome concordance algorithm (step 1, options 1-4) (default 1)
    POW - prenatal outcome window (default 286d)

macro variables initialized during processing (control loops)
   MAXENC - max number of possible prenatal encounters
   MAXOCG - max number of outcomegroups for single pt
   POTMAT - potential index prenatals (have 1+ outcomegrp in OCW)
   MAX - number of outcomegroups found for index - set for each index prenatal 
*/



/*  
Testing:
%let ocw=7 ; %let alg=1; %let pow=286; 
%let num=1; %let maxenc=5;  %let max=5;

%let prensimple=Y;  %let pospw=140;
%let prencomplex=Y; %let dayadj=60;
%let pods=simp ;
*/
option NOTES MPRINT mlogic;

%Macro Runit(OCW= 7, Alg=1, POW=286, PRENSIMPLE=Y, POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 ) ;

	/*Create global macro varibles that going to establish in the macro*/
	%global POTMAT MAXENC MAX NUM MAXOCG;

    /*==============================================================================================*
    *  STEP 3 (prep) - Get Outcomes (step1), with grouping using [OCW]days and concordance [ALG]
    *  creates datasets PrenatalDates_Outc_0 and OutcomeAssigned_Pren_0 needed for steps 4-7
    *==============================================================================================*/

	/*This creates multiple datasets. The prenatal encounters are separated according
	to whether the person has at least one pregnancy outcome group or not. Similarly, the 
	pregnancy outcome groups are separated according to whether teh person has at least one
	prenatal encounter in the data.
	This outputs potmax maxenc and maxocg.*/

     %PrepOCG 
    



    /*
    *------------------------------------------------------------------------------------------------*
    * Repeat Steps 3-6 for every Index Prenatal encounter long as there are potential matches[POTMAT] 
    * (IDs with prenatal encounter + outcomegrp), up to MAXENC (max loops if 1:1 outcomegrp and index 
    * prenatal). For 1st run place full notes in log but suppress notes for subsequent runs  
    *------------------------------------------------------------------------------------------------*
    */

	%DO num = 1 %to  &MAXENC.;/*&MaxOCG. ;%*1;*/

    	/*
    	*==============================================================================================*;
    	*  STEP 3+4 - Find Indexing Prenatal and all OutcomeGrp within [POW] days
    	*  (Step 3 - identify index prenatal then Step 4 - get outcome groups w/in [POW] days of index) 
    	*  then capture the number of outcomegroups found per index. Go thru steps 5a-5c up to that [MAX]
    	*  note - for each iteration, will use prenatal encounters and outcomegroups marked as AVAIL(lc)
    	*  after the previous round (NUM-1) of processing 
    	*===============================================================================================*;
    	*/

     	%if &PotMat = 0 %then %do;%*2; /*If no one left with a prenatal encounter needing a pregnancy outcome group*/
        	%put -------------------------------------------------------------------;
        	%put ---- loop &num. - Potential Matches: &POTMAT. (0) - move to wrapup ;
        	%put -------------------------------------------------------------------;
       		%goto RollupStep6 ; /*Goes to that step in the macro*/
     	%end; %*2;

     	%else %do; %*when PotMat>0 ;%*3;

       		%put;%put ------ index prenatal &num  of &potmat. potential;%put;

     		/*
     		*------------------------------------------------------------------------------------------------*;
     		* create tables IndexPre_&NUM. (index prenatal enc) and IndexPrenatalOutcomes_&NUM. (adds Outcgrp)
     		* IndexPrenatalOutcomes_&NUM. is fed into PairEval macro to define pregnancy outcome
     		*------------------------------------------------------------------------------------------------*;
     		*/

			/*This outputs a dataset of indexing prenatal encounters and pregnancy outcome groups. If more 
			than one pregnancy outcome group (with distinct information) links to the prenatal encounter, 
			each pregnancy outcome group will be a row in the dataset.*/
        	%step34_Link_IdxOCG

			/*This determines the maximum number of pregnancy outcome groups for each prenatal encounter*/
        	%lognote34

     		/*
     		*------------------------------------------------------------------------------------------------*;
     		* continue if at least 1 outcomegrp is found for any of the Index prenatal encounters [MAX] 
     		*------------------------------------------------------------------------------------------------*;
     		*/

			/*Deal with the situation where no outcomegroups are found for any index prenatal encounter*/
         	%if &Max = 0 %then %do; %*max=0;  %*5;

				/*Re-set the datasets so that they are ready to go for the next indexing prenatal encounter.*/
				/*Macro located in: macro_5abc_simpler.sas*/
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
                * deletedrows -> included in above preg (used outcomegrp) ;
                * outcomeneedindex -> outc groups need new prenatal index (step 6);
                * outidsfin -> status of all outcomegrp in indexprentaloutcomes ;
         		*================================================================================================*;
         		*/
            	option nomprint nomlogic nonotes;
            	%PAIREVAL(podsn=INDEXPRENATALOUTCOMES_&num. , rdp= S5);

            	*option mlogic notes;

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

        	%Step5cd_PREGTIMELINE;

        	*---------------------------------------------------------------------;
        	*---------------------------------------------------------------------;

            %put;%put continuing index prenatal NUM &NUM. ;
/*          *Taking index-prenatal-final and linking to all prenatals to mark those visits that are timing-wise with this pregnancy;*/
/*          *resultant dsn has 1 row per orig prenatal encounter for patients in index prenatal dataset;*/

			/*Macro in WORKING_MACROSTEP3TO10*/
            %PDO_Updt  

            %OGP_Updt  


        	ods listing close;

/*        	**are there any more potential matches (pts with Find/Need index for prenatal and outcomes );*/
/*        	**(actual count doesnt matter as long as > 0) - macvar PotMat >0 will continue processing;*/
        	proc sql noprint;  
         		select count(distinct a.patient_deid) into :potmat
           		from prenataldates_outc_&num. a 
				join outcomeassigned_pren_&num. b
             	on a.patient_deid=b.patient_deid 
				where a.status_&num="avail" and b.result_&num.="find index"
             		And (dt_outcomegroup_start - enc_date between 0 and &OCW. OR dt_outcomegroup_end - enc_date between 0 and &OCW.) ;
        		quit;
        	%put;%put ----- After index prenatal &NUM., &potmat. remaining potential index prenatals;

/*   		**No more index+outc grps to eval so wrap up pregnancy info, move to steps 8,10;*/


 		%end; %*potmat >0 loop end; %*3;

 		%put ========== potmat &potmat. num &num. ;

	%end; %*1 reach last MaxENC; %*1;



	%RollupStep6:

/*  %*==============================================================================================*;*/
/*  	%put  ------ WRAP UP step 6  ---------------------------------------------------------------;*/
/*  	%put; %put;*/
/*  %*==============================================================================================*;*/
/*  	%put  --------------------------------------------------------------------------------------;*/
/*      %put  ------ Roll up findings from prenatal-outcomegrp evals once all pairs checked  ------ ;*/
/*      %put  ------ Add step 7 (Any pregnancies with the AEM outcome recategorized as UAB)*/
/*      %put  --------------------------------------------------------------------------------------;*/
/*  %*==============================================================================================*;*/

/*
     *------------------------------------------------------------------------------------------------*;
     * create dataset: ogp_result_oc - all outcome groups final resul (will further updt step10);
     *                 pdo_result_oc - all prenatal outcomes final status (will further updt step8);
     *                 pregnancy_Indexoutc - all pregnancies defined by index prenatal and outcomegrp
     *------------------------------------------------------------------------------------------------*;
*/

	%Rollup_IdxOutc
        

/*  *==============================================================================================*;*/
   	%put  --- move to STEP 8 -  PRENATAL encounter dates not linked to outcome groups  ------ ;
/*  *==============================================================================================*;*/

	Data Prenatal_nooutc;
    set pdo_result_oc (where=(status_oc_fin in ("NO_OUTC", "avail" ,"LTFU")))
	    prenataldates_none(in=b);

    	%*set outcome to missing/unknown step8 - variables match steps 3-6;
        preg_outcome='UNK';
        preg_outcome_clean='UNK';
        prenatal_outcome = 0;
        Index_finding = "unknown"; *replaces prior values tbd (no outcgp found)/ltfu (outcgp found but timing off);
        keep patient_deid  prennum enc_date status_oc_fin index_finding
             prenatal_outcome preg_outcome preg_outcome_clean anyoutcomegrp;

                    dt_pren_enc = enc_date; keep dt_pren_enc; *added for step8 complex;
   	run;

    proc sort;
		by patient_deid enc_date prennum;
	run;

    %if &PrenSimple # Y, y, 1 %then %do;
        %Step8_leftoverpren;
    %end;

    %if &PrenComplex # Y, y, 1 %then %do;
        %STEP8_COMPLEX 
    %end;


/*
     *------------------------------------------------------------------------------------------------*;
     * input datasets: pdo_result_oc, prenataldates_none ;
     * create dataset: pregnancy_prenatalonly - pregnancies defined by prenatal encounters only
                       prenataldates_all_status - all prenatal encounter dates
     *------------------------------------------------------------------------------------------------*;
*/ 

        *==============================================================================================*;
          %put  --- move to STEp 10 -  NO MORE Outcome Groups w/in  &pow. d of Prenatal Index Dates ------ ;
        *==============================================================================================*;
           
/*  proc freq data=outcomeassigned_pren_25;table result_25;run;*/
/*
     *------------------------------------------------------------------------------------------------*;
     * input datasets: ogp_result_oc, pregnancy_indexoutc pregnancy_prenatalonly, _OUTCOMEsASSIGNED_&OCW. ;
     * create dataset: Pregnancy_OutcOnly - pregnancies defined by outcome groups only
                       outcomes_leftoverdates - all remainig dates
     *------------------------------------------------------------------------------------------------*;
*/

    %if &PrenSimple # Y, y, 1 %then %do;

	/*Steps 9-10*/
       %Step10_LeftoverOutc(pods=SIMP);

        Data Pregnancy_OutcOnly_SIMP_&ocw._&alg. ; set Pregnancy_OutcOnly_simp;run;
        Data Pregnancy_PrenatalOnly_SIMP_&ocw._&alg. ; set Pregnancy_PrenatalOnly_simp;run;

    %end;

    %if &PrenComplex # Y, y, 1 %then %do;
 
	/*Steps 9-10*/
       %Step10_LeftoverOutc(pods=COMP)   ;

        Data Pregnancy_OutcOnly_COMP_&ocw._&alg. ; set Pregnancy_OutcOnly_comp;run;
        Data Pregnancy_PrenatalOnly_COMP_&ocw._&alg. ; set Pregnancy_PrenatalOnly_comp ;run;
    %end;


	/*Get all the pregnancy datasets for each encgap and alg*/
    data pregnancy_indexoutc_&ocw._&alg.; set pregnancy_indexoutc;run;

    *use prenonly outconly preg dsn above instead 07.06.24;
/*    data pregnancy_outconly_&ocw._&alg.; set pregnancy_outconly;run;*/
/*    data pregnancy_prenatalonly_&ocw._&alg.; set pregnancy_prenatalonly;run;*/ 

	/*Get all the prenatal encounter and outcome group datasets for each encgap and alg*/
    data ogp_result_oc_&ocw._&alg. ; set ogp_result_oc;run;
    data pdo_result_oc_&ocw._&alg. ; set pdo_result_oc;run;

    **07.09  move final pregnancy datastep to a macro WrapUpPreg saved in master working macros file;
option mprint;
   %if &PrenSimple # Y, y, 1 %then %do;
       %WrapUpPreg(pods=SIMP) 
    %end;

   %if &PrenComplex # Y, y, 1 %then %do;
        %WrapUpPreg(pods=COMP) 
    %end;

/*    *prefer one dataset of all of the pregnancies;*/
/*    data pregnancy_&ocw._&alg. (label="Pregnancy data found prenatal encounters and outcome groups (&OCW.d, &alg.)");*/
/*    set pregnancy_indexoutc_&ocw._&alg. */
/*		pregnancy_prenatalonly_&ocw._&alg. (in=a)*/
/*        pregnancy_outconly_&ocw._&alg. (in=b);*/
/**/
/*       	prenonly=a; outconly=b;*/
/**/
/*       	*only changes 4.15 - add algorithm variables, mod outconly recs, remove redundant/temp vars, add labels;*/
/*       	Algorithm="&OCW-&ALG" ;*/
/*       	Algorithm_Desc="&OCW-&ALG (combined outcomegroups w/in &ocw days for outcome algorithm &alg)" ;*/
/**/
/*       	*04.18 - create distinct pregnancy id using idxpren;*/
/*       	PregID = compress(idxpren||'-'||algorithm);*/
/**/
/*      	*tech no indexprenatal date for outconly cases (take out filler dt) ;*/
/*       	if outconly then dt_indexprenatal=.;*/
/**/
/*		*STEP 10: use clean and Adjust outc-only pregnancies for AEM to UAB;*/
/*       	if outconly then prenatal_outcome=0;*/
/*       	if outconly then preg_outcome_clean=preg_outcome;*/
/*        *if preg_outcome='AEM' then preg_outcome='UAB';*/
/*        if preg_outcome='AEM' then preg_outcome_clean ='UAB'; *fix 6.18;*/
/**/
/*       	drop enc_date outcomegroupsfound outcomegroup_count prennum index_finding pno outcomegrp*/
/*            	anyprenatal dt_index140 dt_index168 pregnancy_counter pregnancy_count status: anyoutcomegrp*/
/*       	;*/
/*       	format dt_: mmddyy10.;*/
/**/
/*       	label prenonly = "Pregnancy - prenatal encounters only"*/
/*             	outconly = "Pregnancy - outcome groups only"*/
/*             	Algorithm = "Parameters used to define pregnancy"*/
/*             	Algorithm_desc = "Parameters used to define pregnancy (long desc)"*/
/*             	preg_outcomescompared = "Outcomes evaluated in defining pregnancy outcome"*/
/*             	dt_ltfu = "Date loss to follow-up (prenonly)"*/
/*             	pregid = "Unique pregnancy id (includes algorithm id)"*/
/*             	preg_outcome = "Pregnancy outcome defined by evaluation of outcome groups" /*changed*/*/
/*             	preg_outcome_clean="Pregnancy outcome clean (final)"*/
/**/
/*                preg_outcomecodetypescompared = "Codetypes for outcomes evaluated in defining pregancy outcome"*/
/*         ;*/
/**/
/*    run;*/
/**/
/*    *04.16 - one more step - sort by dt_pregoutcome and update preg counter;*/
/*    proc sort data=pregnancy_&ocw._&alg. ; */
/*		by patient_deid dt_preg_outcome; */
/*	run;*/
/*/*    data pregnancy_&ocw._&alg. ;*/*/
/*    data OUT_pregnancy_complex_&ocw._&alg. (label="Pregnancies identified using simple step8 for algorithm parameters &ocw. - &alg.");*/
/*    attrib Pregnancy_Number length=4 label="Patient Pregnancy number (counter)";*/
/*    set pregnancy_&ocw._&alg. ;*/
/*      	by patient_deid; */
/*		if first.patient_deid then Pregnancy_Number=0;*/
/*     	Pregnancy_number+1;*/
/*    run;*/
;;


	%exit:

    %put ocw=&ocw  alg=&alg  pow=&pow  num=&num maxenc=&maxenc  max=&max 
         prensimple=&prensimple pospw=&pospw prencomplex=&prencomplex dayadj=&dayadj ;

%mend Runit;



/***********************************************************************************************

									04 - RUN STEPS 3-10

***********************************************************************************************/


option  mlogic;

/*%runit(OCW= 7, Alg=1, POW=286 );*/
%Runit(OCW= 7, Alg=1, POW=286, PRENSIMPLE=y , POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 ) ;

%Runit(OCW= 7, Alg=2, POW=286, PRENSIMPLE=y , POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 );
%Runit(OCW= 7, Alg=3, POW=286, PRENSIMPLE= y, POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 );
%Runit(OCW= 7, Alg=4, POW=286, PRENSIMPLE=y , POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 );

%Runit(OCW= 14, Alg=1, POW=286, PRENSIMPLE=y , POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 );
%Runit(OCW= 14, Alg=2, POW=286, PRENSIMPLE= y, POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 );
%Runit(OCW= 14, Alg=3, POW=286, PRENSIMPLE=y , POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 );
%Runit(OCW= 14, Alg=4, POW=286, PRENSIMPLE=y , POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 );

%Runit(OCW= 30, Alg=1, POW=286, PRENSIMPLE=y , POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 );
%Runit(OCW= 30, Alg=2, POW=286, PRENSIMPLE=y , POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 );
%Runit(OCW= 30, Alg=3, POW=286, PRENSIMPLE=y , POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 );
%Runit(OCW= 30, Alg=4, POW=286, PRENSIMPLE=y , POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 );

option nomprint nomlogic notes;
/*%runit(OCW= 7, Alg=2, POW=286 );*/
/*%runit(OCW= 7, Alg=3, POW=286 );*/
/*%runit(OCW= 7, Alg=4, POW=286 );*/
/**/
/**/
/*%runit(OCW= 14, Alg=1, POW=286 );*/
/*%runit(OCW= 14, Alg=2, POW=286 );*/
/*%runit(OCW= 14, Alg=3, POW=286 );*/
/*%runit(OCW= 14, Alg=4, POW=286 );*/
/**/
/*%runit(OCW= 30, Alg=1, POW=286 );*/
/*%runit(OCW= 30, Alg=2, POW=286 );*/
/*%runit(OCW= 30, Alg=3, POW=286 );*/
/*%runit(OCW= 30, Alg=4, POW=286 );*/

/*option dlcreatedir;*/
/*libname po  "&xdr./DATA/20230328_Data_Pull_01/analysis/int_20230516/%sysfunc(date(),yymmddn8.)" ;*/

/*proc copy  inlib=work outlib=po;*/
/** select pregnancy_:    ogp_result_: pdo_result:;*/
/* select  ogp_result_: pdo_result:  prenataldates_none;*/
/*quit;*/

