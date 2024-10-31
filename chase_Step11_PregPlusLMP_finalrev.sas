/*
************************************************************************************************;          
**** program: Chase_PregnancyPlusLMPs.sas                                                   ****;
**** purpose: Apply gestational age / LMP algorithm steps 1-3 from                             *;
***                                                                                            *;
***     Note: single macro that will pull pregnancy dataset of interest (outcgrp, outc alg)    *;
***           that will run all linked gestational encounters through the algorithm to select  *;
***           ^best^ LMP estimate for those that have eligible GA encounters based on the preg *;
***           outcome and GA encounter timing or table-based timing for outcome (step3)        *;

***	MODIFICATIONS:
	- 	05.2024 - CDL reformatted and added annotation. Conducted QC. SPH and CDL agreed on
		any changes added to the program.
    - 	07.21.2024 - (SPH) modify for simple/complex prenatal-only pregnancy datasets by 
        placing GetGA macro calls and final Pregnancy_LMP_all dataset in new macro GetLMP
        (adds macvar PODS - needs values SIMP (for simple) and COMP (for complex)) 
    -   08.05.24 - sph change library setup to point to libname setup file and remove gestage dsn
	- 	10.18.24 - CDL added code to derive information on the number of prenatal encounters between
		the pregnancy LMP and the outcome or LTFU date. 

************************************************************************************************;          
*/run;;;;


/***********************************************************************************************

TABLE OF CONTENTS:
	- 00 - SET LIBRARIES, ETC.
	- 01 - PULL IN REFERENCE FILES
	- 02 - CALL GESTATIONAL AGE MACRO
	- 03 - RUN GESTATIONAL AGE MACRO
	- 04 - CREATE FINAL STACKED PREGNANCY DATASETS

***********************************************************************************************/









/***********************************************************************************************

									00 - SET LIBRARIES, ETC.

***********************************************************************************************/

ods listing;
options nocenter noreplace mprint mlogic  minoperator mindelimiter=',';

*Set up paths - will need to be modified dependent upon file structure.
This points to the TraCS server folder where all our data, programs, etc. are stored;

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
/*%let algpath=&xdr.\programs\preg_encounter_algorithm;                 *<===== CHECK PATH; */
*/
;;;;

%put &algpath.;


*Pull in the format statements;
*Sharon;
%inc "&Xdr.\PROGRAMS\preg_encounter_algorithm\chase_Step0_FormatStatements.sas";













/***********************************************************************************************

								01 - PULL IN REFERENCE FILES

***********************************************************************************************/

/***Pull data of interest - processed CDWH + Ref files;run;*/
/*proc copy inlib=int outlib=work;*/
/*	select  GESTAGE; *codeage refage;*/
/*quit;*/
/**This all encounter data for gestational age codes;*/
/**/

** creates gestage dsn - adding prenatal encounter flag to those with gestational age codes;
**(formerly chase_gestagedsn.sas);
proc sql;
 create table gestage as
  select * from int.gestage
;

quit;












/***********************************************************************************************

								02 - CALL GESTATIONAL AGE MACRO

***********************************************************************************************/


**********************************************************************************************************;
**********************************************************************************************************;
*** Summary of steps from pregnancy algorithm:
*** GA-STEP 0 - to prepare, get GA code encounters and pregnancy data of interest (outcgrp window and alg)
*** GA-STEP 1 - link all pregnancies to GA codes then estimate date of LMP using Gestational Days value
***             find the ^best^ LMP using dates based on GA-codes that are good fits for the pregnancy
***             i.e. GA codes for pregnancy outcome (GAMatchOutc) and w/in 7d of outcome date (GADaysMatch)
*** GA-Step 2 - for those w/o LMP in step 1, use GA codes on prenatal encounters between 1st prenatal and
***             date LTFU (dt_gapreg) to find ^best^ guess LMP or use Zhu
*** GA-Step 3 - for all pregnancies use table to calculate LMP from pregnancy outcome (technically this
***             is applied when creating AnyGAEnc dataset
**********************************************************************************************************;
**********************************************************************************************************;

*** call macro that processes pregnancy+GA data to find LMP estimate;
%inc "&Xdr.\PROGRAMS\preg_encounter_algorithm\chase_Step11_GestationalAge_macro.sas"; *07.24 revisions;















/***********************************************************************************************

								03 - RUN GESTATIONAL AGE MACRO

***********************************************************************************************/




*>>>> GA STEP 0 <<<<*;

*** Note: this rolls up info at the GA code and Encounter date. If the GA code is on multiple encounters;
***       on the same date, it is possible that only 1 of those encounters (enc_key) also had a prenatal;
***       encounter code. use MAX fn to indicate any prenatal enc on same date as GA code;
proc sql;
	create table gestagepren as
	select distinct patient_deid, enc_date, parent_code, code, max(prenatal_enc) as Pren_GA_enc,
             preg_outcome,code_hierarchy, gestational_age_days,gest_age_wks,min_gest_age,max_gest_age,
             zhu_test, zhu_hierarchy
    from gestage
    group by patient_deid, parent_code, enc_date, code, preg_outcome, code_hierarchy, 
               gestational_age_days, min_gest_age, max_gest_age 
	;
    quit;


*07.21.24  - add Macro GetLMP(pods) ;
*%let pods = comp;

%Macro GetLMP(pods);

            %GetGA(7, 1);;
            %GetGA(7, 2);;
            %GetGA(7, 3);; 
            %GetGA(7, 4);; 

            %GetGA(14, 1);;
            %GetGA(14, 2);;
            %GetGA(14, 3);; 
            %GetGA(14, 4);; 

            %GetGA(30, 1);; 
            %GetGA(30, 2);; 
            %GetGA(30, 3);; 
            %GetGA(30, 4);; 

        /***********************************************************************************************

        						04 - CREATE FINAL STACKED PREGNANCY DATASETS

        ***********************************************************************************************/


        /*create stacked dataset with all pregnancies and LMPs*/
        /* 06.12.24 - select LMP date (and add label):
            1.	If only one LMP assigned to the pregnancy via gestational age codes, use that LMP. [dt_lmp_alg]
            2.	If more than one LMP assigned to the pregnancy via gestational age codes (>1 code at the same 
                level of a hierarchy), use the earliest LMP estimate. [dt_lmp_min/max/wtavg]
            3.	If no LMP assigned via codes, use the LMP determined via the outcome. [dt_lmp_table]
        */
        data Pregnancy_LMP_&pods._All ;
        length Pregnancy_ID $15. Algorithm $4.;
        set
        	Pregnancy_LMP_&pods._7_1
           	Pregnancy_LMP_&pods._7_2
            Pregnancy_LMP_&pods._7_3 
            Pregnancy_LMP_&pods._7_4 

            Pregnancy_LMP_&pods._14_1
            Pregnancy_LMP_&pods._14_2
            Pregnancy_LMP_&pods._14_3 
            Pregnancy_LMP_&pods._14_4 

            Pregnancy_LMP_&pods._30_1 
            Pregnancy_LMP_&pods._30_2 
            Pregnancy_LMP_&pods._30_3 
            Pregnancy_LMP_&pods._30_4 
            ;

            Pregnancy_ID = compress(Algorithm||'-'||idxpren);

             if missing(dt_lmp_alg) =0 then Dt_LMP = dt_lmp_alg;
             else if missing(dt_lmp_min)=0 then Dt_LMP = dt_lmp_min; *same as min(dt_lmp_min,dt_lmp_max,dt_lmp_wtavg,dt_lmp_complex);
             else dt_lmp = dt_lmp_table;

             label dt_lmp = "Final LMP estiamte"
                   pregnancy_id="Pregnancy identifier (algorithm-idxpren)"
             ;

        run;

        proc copy inlib=work outlib=out;
        	select pregnancy_LMP_&pods._all ;
        quit;

%Mend GetLMP ;

    
option mprint;

       %GetLMP(pods=SIMP) 

        %GetLMP(pods=COMP) 





/***********************************************************************************************

							05 - ADD INFORMATION ON PRENATAL ENCOUNTERS

Added 10.18.2024 by CDL per conversations with MEW on variables needed for stratification.

***********************************************************************************************/


*The dataset with all the prenatal encounter dates according to primary prenatl codes 
		is: int.codeprenatal_meg1_dts;
*Join the prenatal encounter dates onto the pregnancy cohort according to person ID and pregnancy dates;


/***SIMPLE STEP 8 COHORT***/

proc sql;
	create table out.pregnancy_lmp_simp_dedup as
	select distinct a.*, b.num_pnc_start_end /*This step removes duplicate pregnancies*/
	from out.pregnancy_lmp_simp_all as a
	left join (select patient_deid, Pregnancy_ID, sum(enc_date ne .) as num_pnc_start_end
			   from (select a.*, b.enc_date
			         from (select *, min(dt_lmp, dt_prenenc1st) as preg_start format=MMDDYY10., 
						          dt_gapreg as preg_end
		  		           from out.pregnancy_lmp_simp_all) as a
		  	         left join int.codeprenatal_meg1_dts as b
		             on a.patient_deid = b.patient_deid and a.preg_start le b.enc_date le a.preg_end)
	                 group by patient_deid, Pregnancy_ID ) as b
	on a.patient_deid = b.patient_deid and a.pregnancy_ID = b.pregnancy_ID
	;
	quit;


/***COMPLEX STEP 8 COHORT***/

proc sql;
	create table out.pregnancy_lmp_comp_dedup as
	select distinct a.*, b.num_pnc_start_end /*This step removes duplicate pregnancies*/
	from out.pregnancy_lmp_comp_all as a
	left join (select patient_deid, Pregnancy_ID, sum(enc_date ne .) as num_pnc_start_end
			   from (select a.*, b.enc_date
			         from (select *, min(dt_lmp, dt_prenenc1st) as preg_start format=MMDDYY10., 
						          dt_gapreg as preg_end
		  		           from out.pregnancy_lmp_comp_all) as a
		  	         left join int.codeprenatal_meg1_dts as b
		             on a.patient_deid = b.patient_deid and a.preg_start le b.enc_date le a.preg_end)
	                 group by patient_deid, Pregnancy_ID ) as b
	on a.patient_deid = b.patient_deid and a.pregnancy_ID = b.pregnancy_ID
	;
	quit;







/*Through this testing, I discovered that there are duplicate rows in each dataset. COME BACK LATER*/
/*data lmp_comp;*/
/*set out.pregnancy_lmp_comp_all;*/
/*	preg_start = min(dt_lmp, dt_prenenc1st);*/
/*	preg_end = dt_gapreg;*/
/*run;*/

/*proc means data=lmp_comp nmiss;*/
/*	var preg_start preg_end;*/
/*run;*/

proc sql;
	select count(distinct pregnancy_ID) as distinct_preg, /*2874021*/
		   count(pregnancy_ID) as preg, /*2874057 -- This is the number that we want to end with*/
		   count(distinct patient_deid) as distinct_id, /*167404*/
		   count(patient_deid) as id /*2874057*/
	from out.pregnancy_lmp_comp_all;
	quit;
proc sql;
	select count(distinct pregnancy_ID) as distinct_preg, /*2874021*/
		   count(pregnancy_ID) as preg, /*2874057 -- This is the number that we want to end with*/
		   count(distinct patient_deid) as distinct_id, /*167404*/
		   count(patient_deid) as id /*2874057*/
	from out.pregnancy_lmp_simp_all;
	quit;
/*proc sql;*/
/*	select count(distinct pregnancy_ID) as distinct_preg, /*2874021*/*/
/*		   count(pregnancy_ID) as preg, /*2874057 -- This is the number that we want to end with*/*/
/*		   count(distinct idxpren) as distinct_preg_idx, /**/*/
/*		   count(idxpren) as preg_idx, /**/*/
/*		   count(distinct patient_deid) as distinct_id, /*167404*/*/
/*		   count(patient_deid) as id /*2874057*/*/
/*	from out.pregnancy_lmp_comp_all;*/
/*	quit; */
/*	    */
/*proc sort data=out.pregnancy_lmp_comp_all out=test nodup;*/
/*	by algorithm patient_deid;*/
/*run;*/
/*proc sort data=out.pregnancy_lmp_simp_all out=test_simp nodupkey;*/
/*	by _ALL_;*/
/*run;*/
/**/
/**/
/*proc sql;*/
/*	select count(distinct pregnancy_ID) as distinct_preg, /*2874021*/*/
/*		   count(pregnancy_ID) as preg, /*2874057 -- This is the number that we want to end with*/*/
/*		   count(distinct patient_deid) as distinct_id, /*167404*/*/
/*		   count(patient_deid) as id /*2874057*/*/
/*	from test;*/
/*	quit;*/
