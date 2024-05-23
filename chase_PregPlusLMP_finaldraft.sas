/*
************************************************************************************************;          
**** program: Chase_PregnancyPlusLMPs.sas                                                   ****;
**** purpose: Apply gestational age / LMP algorithm steps 1-3 from                             *;
***                                                                                            *;
***     Note: single macro that will pull pregnancy dataset of interest (outcgrp, outc alg)    *;
***           that will run all linked gestational encounters through the algorithm to select  *;
***           ^best^ LMP estimate for those that have eligible GA encounters based on the preg *;
***           outcome and GA encounter timing or table-based timing for outcome (step3)        *;
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
*Sharon;
*%let xdr= %str(\\ad.unc.edu\med\tracs\groups\Research\CDWH\Latour_Chase_22-0689\);
*Chase;
%let xdr=%str(W:/);

*Set up the necessary libraries;

*Point to the old encounter-level data;
*Sharon;
*libname x "&xdr\data\20221220\analysis" access=r;

*Input data;
libname int "&xdr./DATA/20230328_Data_Pull_01/analysis/int_20230516" ;
*Output data;
libname out  "&xdr./DATA\20230328_Data_Pull_01\analysis\int_20230516\20240418\ " ;
libname outint "&xdr./DATA\20230328_Data_Pull_01\analysis\int_20230516\20240418\addl" ;
/*libname out "&xdr./DATA/20230328_Data_Pull_01/analysis/int_20230516/20231102" ;*/
*Old input data;
*libname oldint "&xdr\data\20221220\analysis\int_20230922" access=r;*was20230329;

*Pull in the format statements;
*Sharon;
*%inc "&Xdr.\PROGRAMS\sharon\Preg_Outcomes\chase_Step0_FormatStatements.sas";
*Chase;
%inc "&Xdr./PROGRAMS/sharon/Preg_Outcomes/chase review/chase_Step0_FormatStatements.sas";













/***********************************************************************************************

								01 - PULL IN REFERENCE FILES

***********************************************************************************************/

**Pull data of interest - processed CDWH + Ref files;run;
proc copy inlib=int outlib=work;
	select  GESTAGE; *codeage refage;
quit;
*This all encounter data for gestational age codes;

/*proc copy inlib=out outlib=work;*/
/* select preg: ;*/
/*quit;*/

/*Checks*/
/*proc freq data=pregs;*/
/*	table preg_outcome*preg_outcome_clean/list missing;*/
/*run;*/
/*proc freq data=pregs;*/
/*	table preg_outcome:;*/
/*run;*/


















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
%inc "&Xdr.\PROGRAMS\sharon\Preg_Outcomes\chase_GestationalAge_macro.sas";














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
data Pregnancy_LMP_All;
length Pregnancy_ID $15. Algorithm $4.;
set
	Pregnancy_LMP_7_1
   	Pregnancy_LMP_7_2
    Pregnancy_LMP_7_3 
    Pregnancy_LMP_7_4 

    Pregnancy_LMP_14_1
    Pregnancy_LMP_14_2
    Pregnancy_LMP_14_3 
    Pregnancy_LMP_14_4 

    Pregnancy_LMP_30_1 
    Pregnancy_LMP_30_2 
    Pregnancy_LMP_30_3 
    Pregnancy_LMP_30_4 
    ;

    Pregnancy_ID = compress(Algorithm||'-'||idxpren);

run;

proc copy inlib=work outlib=out;
	select pregnancy_LMP_all ;
quit;
