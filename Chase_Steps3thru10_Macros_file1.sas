/******************************************************************************

Program: WORKING_MACROSTEP3TO10.sas

Programmer: Sharon 

Purpose: Create all of the macros that will be used to execute steps 3 through
10 of the pregnancy identification algorithm from encounter-level data.

Modifications:
	- 05-01-24: CDL reviewed the code, made comments, reformatted.
	- 05.2024: CDL conducted QC. SPH and CDL reviewed. All modifications were
		agreed upon by both.
    - 07.01.24: sph- mods to enable Simple vs Complex prenatal only pregs
        adding macvar POSPW to Step8_LeftoverPren and (07.06) change output
        dataset to Pregnancy_Prenatal_SIMP
        
  Runit(OCW= 7, Alg=1, POW=286, PRENSIMPLE=Y, POSPW=140, PRENCOMPLEX=Y, DAYADJ=60 )

MACROS Created In This Program:
	- PrepOCG
	- Step34_Link_IdxOCG
	- Step5cd_PregTimeline
	- PDO_Updt
	- OGP_Updt
	- Rollup_IdxOutc
	- Step8_LeftoverPren
	- Step10_LeftOverOutc
	- lognote3prep 
	- lognote3 
	- lognote34 
	- lognote5ab
 
    - WrapUpPreg (added 7.9.24)

*******************************************************************************/




*==============================================================================================*;
*  STEP 3 (prep) - Get Outcomes (step1), with grouping using [OCW]days and concordance [ALG]
*==============================================================================================*;

*Prep otucome groups. 
INPUT: None;

%macro PrepOCG;

	/*Create some necessary datasets based upon pregnancy outcome groups*/
	proc sql;

		/*Create a version of the outcomeassigned dataset in the working library.
		These are the final pregnancy outcome group datasets from Steps 1-2*/
     	create table _outcomesassigned_&OCW. as 
		select * 
		from out.outcomeassigned_&OCW.;

		/*Create a dataset with the IDs of patients with at least 1 pregnancy outcome group*/
     	create table outgids as 
		select distinct patient_deid, 1 as AnyOutcomeGrp /*Flag for having at least 1 outcome group*/
		from _outcomesassigned_&OCW. ;

		quit;

	/*Derive some necessary datasets with flags for prenatal care and pregnancy outcomes*/
    proc sql;

		/*Merge the prenatal care dates with a flag to indicate that the person has at least one 
		pregnancy outcome group*/
     	create table _prenataldates_all as
      	select distinct a.patient_deid, a.enc_date , 1 as AnyPrenatal, c.patient_deid ne '' as AnyOutcomeGrp
      	from _codeprenatal_meg1_dts a 
		left join outgids c on 
		a.patient_deid=c.patient_deid
    	;

		/*Create a flag for if a person with a pregnancy outcome group has at least 1 prenatal encounter*/
     	create table _outcomeassigned_pren_all as
      	select distinct a.*, 1 as AnyOutcomeGrp,  b.patient_deid ne '' as AnyPrenatal
      	from _outcomesassigned_&OCW. a 
		left join prenids b 
		on a.patient_deid= b.patient_deid
    	;

		quit;



	/*Create the initial versions of datasets that will be used throught the rest of the pull.

	The algorithm starts by indexing on prenatal care encounters, so we deal with those first.*/
    data Prenataldates_outc_0 Prenataldates_none;
    set _prenataldates_all;
	
	  	/*The number of prenatal encounter dates among everyone in the dataset*/
      	PRENNUM = _n_;

      	/**all prenatal dates are available to link to outcomes at the beginning*/
      	STATUS_0 = "avail";

	  	/*Separate the prenatal encounters into two datasets dependent upon if they could possibly 
	  	link to a pregnancy outcome group - Specifically, if that person has at least 1 pregnancy
	  	outcome group.*/
      	/** link to outcomes to define pregnancy;*/
      	if anyoutcomegrp then output Prenataldates_outc_0 ; /*Person has at least one pregnancy outcome group*/
	      	/** will need these later (step 8 - prenatals define pregnancy;*/
	      	else output Prenataldates_none; /*Person has no pregnancy outcome groups.*/
    run;



	/*Implement similar process. Separate the pregnancy outcome groups according to whether that
	person has at least 1 prenatal encounter.*/
    data outcomeassigned_pren_0 outcomeassigned_none ;
    set _outcomeassigned_pren_all;

     	keep patient_deid outcomegrp dt_outcomegroup_start dt_outcomegroup_end mifepristone misoprostol
          	OutcomeGroup_DelivPRInpatEnc_N DT_OUTCOME_ASSIGNED&alg. outcome_assigned&alg. outcome_assigned_codetype&alg. ;

		/*All of these pregnancy outcome groups need to be linked to a indexing prenatal encounter,
			if available*/
      	RESULT_0 = 'find index'; KEEP RESULT_0; 

		/*Separate the pregnancy outcome groups according to whether the person has at least 1
		prenatal care encounter*/
      	IF anyprenatal then output outcomeassigned_pren_0 ;
      		/** will need these later (step10 - outcomes that define pregnancy;*/
      		else output  outcomeassigned_none ;
    run;


	/*Create the macro variables that we need to output at the end*/
    proc sql noprint;

       	/*indicator of potential prenatal encounter and outcomegrp matches(counts updated later/below)*/
       	select count(distinct patient_deid) into :potmat from prenataldates_outc_0;

       	/*get maximum num of prenatal index dates avail */
       	select max(dts) into :maxenc
       	from (select patient_deid, count(enc_date) as dts 
				from prenataldates_outc_0 
				group by patient_deid);

       	/*get maximum num of outcomegrps avail to attach to prenatal for pregnancy;*/
       	select max(outcomegrp) into :maxocg from outcomeassigned_pren_0;

       	Select nobs into :pdo0 from dictionary.tables where memname='PRENATALDATES_OUTC_0';
       	Select nobs into :oap0 from dictionary.tables where memname='OUTCOMEASSIGNED_PREN_0';
      	quit;

     %lognote3prep

%mend;






















*==============================================================================================*;
*  STEP 3+4 - Find Indexing Prenatal and all OutcomeGrp within [POW] days
*  note - for each iteration, will use prenatal encounters and outcomegroups marked as AVAIL
*  after the previous round (NUM-1) of processing
*==============================================================================================*;

/*Testing:
%let num = 1;*/

%macro Step34_Link_IdxOCG;

	/*Identify first prenatal encounter as indexing encounter*/
    data IndexPre_&num. ;
    set prenataldates_OUTC_%eval(&NUM. - 1) ;  %*prenataldates - prenataldates_updt_#;
    	by patient_deid;
      	WHERE STATUS_%eval(&NUM. - 1) IN ('avail'); /*Only evaluate those needing linkage to pregnancy  outcome group*/

      	if first.patient_deid then do;
       		IDXPREN= _n_ + (&num. * 1000000); /*Create a ID for the indexing prenatal encounter thats depenedent on iteration*/
       		DT_INDEXPRENATAL = ENC_DATE; /*Determine the date of the indexing prenatal encounter*/
          	output IndexPre_&NUM. ; /*Only output the first encounter*/
	    end;

      	Keep Patient_deid enc_date AnyPrenatal AnyOutcomeGrp Prennum
           	idxpren DT_INDEXPRENATAL ;

      	Format dt_indexprenatal date. ;
      	label dt_indexprenatal = "Index Prenatal encounter date"
            	IDXPREN  = "Index prenatal encounter #";
    run;

    *** Step 4 - overlaping index prenatal and outcomes***;
    **Identify all outcome groups that overlap index prenatal bounds (indexdt + POW);
	/*This outputs a dataset of indexing prenatal encounters and pregnancy outcome groups. If more than one pregnancy outcome group
	(with distinct information) links to the prenatal encounter, each pregnancy outcome group will be a row in the dataset.*/
    proc sql;
  		create table IndexPrenatalOutcomes_&Num. as
		select DISTINCT
                 a.patient_deid, a.idxpren, PRENNUM, a.Dt_IndexPrenatal, count(distinct outcomegrp) as OutcomeGroupsFound,
                 b.outcomegrp, b.dt_outcomegroup_start, dt_outcomegroup_end, mifepristone, misoprostol,
                 Case when OutcomeGroup_DelivPRInpatEnc_N > 0 then 1 else 0 end as InpatPRDelivery,
                 DT_outcome_assigned&ALG, result_%eval(&NUM. - 1),
                 b.outcome_assigned&ALG, b.outcome_assigned_codetype&ALG
        from INDEXPRE_&NUM. a 
		left join
               (select sb.* from OUTCOMEASSIGNED_PREN_%eval(&NUM. - 1) sb join INDEXPRE_&NUM. sa on sa.patient_deid=sb.patient_deid
                where (dt_outcomegroup_start between sa.DT_INDEXPRENATAL and sa.DT_INDEXPRENATAL +  &pow.
                   or dt_outcomegroup_end between sa.DT_INDEXPRENATAL and sa.DT_INDEXPRENATAL +  &pow.  )
                     And result_%eval(&NUM. - 1) in ("find index")) b
        on a.patient_deid=b.patient_deid
        group by a.patient_deid;
        quit;


%mend;















/*
*------------------------------------------------------------------------------------------------*;
* STEP 5c - STEP 5d - Find pregnancy timeline based on the pregnancy outcome found (steps 5a-5b)
* setting the DT_PREGNANCYLOOKBACK (table 4) and checking index prenatal date is w/in the lookback
*
*------------------------------------------------------------------------------------------------*;
*/ ;

%macro Step5cd_PregTimeline;

    **continue steps 5c,5d to set lookback period and verify prenatal and outcome timing work;run;
    *now use dsn index prenatal and pregnancy outcome based on the outcomegrps w/in 286d of index w/in table3 gaps;
    proc sort data=s5_index_outcome_fin_&NUM. ;
      	by patient_deid dt_indexprenatal dt_pregnancyoutcome_start dt_pregnancyoutcome_end;
    run;


    Data Index_Outcome_Fin_Result_&NUM. ;
    set s5_index_outcome_fin_&num. ;

     	if PREG_Outcome in ("LBM" "LBS" "MLS" "UDL" "SB") then DT_PregnancyLookback = Dt_Pregnancyoutcome_Start - 286 ; *MAX_LOOKBACK;
	     	else if PREG_Outcome in ("EM" ) then Dt_pregnancyLookback=Dt_Pregnancyoutcome_Start-112 ;
	     	else if PREG_Outcome in ("IAB") then Dt_pregnancylookback=Dt_Pregnancyoutcome_Start-168;
	     	else if PREG_Outcome in ("SAB" "UAB") then Dt_pregnancylookback=Dt_Pregnancyoutcome_Start-133;
	    	%* else if pairedoutcome in ("RX"  'RX-MIS','RX-MIF') then Dt_pregnancylookback= Dt_Pregnancyoutcome_Start-133 ;*RX orders => UAB per CL;

     	length Index_Finding_&NUM.$10;
     	%*flag the index as being within the lookback period of the determined pregnancy outcome or within start/end of pregnancy;
     	%*need the 2nd condition for few cases where the index dt linked to outcomegrp is between outcomegrp start and end dates;
     	%*changed value from PREG to OUTC to indicate pregnancy outcome found and var from index_outcome_num to index_finding_num;

		/*Check if indexing prenatal falls within lookback period for maximum lookback*/
     	if Dt_PregnancyLookback le Dt_IndexPrenatal le Dt_Pregnancyoutcome_Start then Index_Finding_&NUM.="OUTC";*"PREG";
			/*Assume pregnancy from indexing prenatal encounter was LTFU and outcome groups describe different pregnancy.*/
     		else if dt_indexprenatal lt Dt_PregnancyLookback then Index_Finding_&NUM.="LTFU";
			/*Added this because some pregnancy outcome groups started before the indexing prenatal date*/
     		else if dt_pregnancyoutcome_start le dt_indexprenatal le dt_pregnancyoutcome_end then Index_Finding_&NUM.='OUTC';
    run;


    *** 5D - those indexing prenatal encounters without any pregnancy outcome groups in the 286 days ;
    **     - rephrase - pull out index prenatals with no outcome (0 outcomegroups + lostfu ?) and;
    **       label those as missing/unknown;
    **       first by pooling all Dt_IndexPrenatal and then assigning label;
    proc sql;
     	create table Final_Indexpre_&NUM.  as
      	select a.*, 
				/*If not yet evaluated for pregnancy outcome group then index finding is tbd - tbd 
				would occur if index pnc was not linked to a pregnancy outcome group*/
				case when missing(b.idxpren) then "tbd" else Index_Finding_&NUM. end as Index_Finding_&NUM.,
             	dt_pregnancyoutcome_start, dt_pregnancyoutcome_end, DT_PREG_Outcome,

				/*Apply a lookforward date to the pregnancy outcome*/
             	Case /*01.16 add date for dt lookfwd based on table 5 in step 6*/
                	WHEN B.PREG_Outcome IN ('LBS' 'LBM' 'SB' 'MLS' 'UDL') THEN DT_PREGNANCYOUTCOME_END + 42
                	WHEN B.PREG_Outcome IN ('EM' 'SAB' 'IAB' 'UAB' ) THEN DT_PREGNANCYOUTCOME_END + 28
                	when b.PREG_Outcome='missing/unknown' then DT_PREGNANCYOUTCOME_END + 28   /*filler line, will apply step 8*/
             		End as Dt_PregnancyLookFwd label ="Date look fwd from preg outc end (table 5) to id later preg)",

             DT_PREGNANCYLOOKBACK label="Date look back from preg outc beg (table 4) for earliest expected prenatal",

             a.DT_INDEXPRENATAL - DT_PREGNANCYLOOKBACK AS Days_IndexLookback "Days Index prenatal to earliest allowed Indexdt for preg outcome",
             PREG_OutcomeCount, PREG_OutcomesCompared, Preg_outcomeCodetypesCOmpared, PREG_Outcome,PREG_CodeType, PREG_MIFE, PREG_MISO
      	from (select distinct patient_deid, idxpren, PRENNUM, dt_indexprenatal, outcomegroupsfound 
				from indexprenataloutcomes_&NUM.) a
        left join index_outcome_fin_result_&NUM. b 
		on a.patient_deid = b.patient_deid and a.idxpren = b.idxpren
     	;
    	quit;
        /*05.23.24 - added preg_outcomecodetypescompared where preg_outcomescompared is in the code- sph*/

/*		proc freq data=Final_Indexpre_&NUM.;*/
/*			table Index_Finding_&NUM.;*/
/*		run;*/


	%*Reset pregnancy outcome values to missing for those pregnancies LTFU;
    proc sql;
     	update FInal_Indexpre_&NUM. set PREG_OUTCOME = '' where Index_Finding_&NUM.='LTFU';
     	update FInal_Indexpre_&NUM. set DT_PREG_OUTCOME = . where Index_Finding_&NUM.='LTFU';
    	update FInal_Indexpre_&NUM. set PREG_OUTCOMEcount = . where Index_Finding_&NUM.='LTFU';
     	update FInal_Indexpre_&NUM. set preg_outcomescompared = '' where Index_Finding_&NUM.='LTFU';
     	update FInal_Indexpre_&NUM. set preg_outcomeCOdetypescompared = '' where Index_Finding_&NUM.='LTFU';
    	quit;


    proc sql noprint;
     	select count(patient_deid)= count(distinct patient_deid) into :onerec from final_indexpre_&NUM.;
    	quit; *confirm 1rec per id;
    %put --- 1 rec per patient ID (1/0)? &onerec.      ---;

%MEND;









*********************************************************
MACRO: PDO_Updt

The purpose of this program is to update the information
retained on all the prenatal encounters so that we 
can remove those from availability that are now
included in an identified pregnancy.

Any prenatal encounters between the indexing prenatal 
encounter and Dt_PregnancyEnd cannot be a new index.

********************************************************;


%Macro PDO_Updt;

            *Taking index-prenatal-final and linking to all prenatals to mark those visits that are timing-wise with this pregnancy;
            *resultant dsn has 1 row per orig prenatal encounter for patients in index prenatal dataset;

			/*Update the information on the prenatal encounters  that we assessed for linkage to a pregnancy outcome in the 
			previous step.*/
            PROC SQL;

				/*First, deal with those prenatal encounters that were linked to an outcome group
				or they had no outcome groups within 286 days afterwards*/
             	CREATE TABLE PDO_UPDT_&NUM. AS
              	SELECT A.PATIENT_DEID,  A.DT_INDEXPRENATAL,  a.dt_pregnancylookback, a.dt_pregnancylookfwd,
                          A.DT_PREGNANCYOUTCOME_START,A.DT_PREGNANCYOUTCOME_END, A.PREG_Outcome, 
						  a.dt_PREG_Outcome, a.preg_mife,a.preg_miso,
                          a.preg_codetype, A.PREG_OUTCOMECOUNT, PREG_OUTCOMESCOMPARED, preg_outcomecodetypescompared, INDEX_FINDING_&NUM. ,
                          B.PRENNUM, B.ENC_DATE,
                          B.ENC_DATE- A.DT_PREGNANCYOUTCOME_END AS DAYS_ENCOUTC label='Days prenatal encounter to pregnancy outcome end (<0 = before outcome)',
                          CASE WHEN A.PRENNUM = B.PRENNUM AND A.OUTCOMEGROUPSFOUND=0 THEN "NO_OUTC" /*These are those tbd cases*/
                               WHEN A.PRENNUM = B.PRENNUM THEN "PREN_OC_&NUM."  /*this is the index visit*/
                               WHEN A.DT_INDEXPRENATAL LE B.ENC_DATE LE A.DT_PREGNANCYOUTCOME_end THEN "PREN_OC_&NUM."  /*pren enc between idx and outcome*/
                               WHEN 1 le CALCULATED DAYS_ENCOUTC le 42 and A.PREG_Outcome in ("LBS" "LBM" "SB" "MLS" "UDL") THEN "PREN_OC_&NUM. POST" /* POST DAYS ALLOWED*/
                               WHEN 1 le CALCULATED DAYS_ENCOUTC le 28 and A.PREG_Outcome in ("EM" "IAB" "SAB" "UAB") THEN "PREN_OC_&NUM. POST"
                               WHEN B.ENC_DATE LT A.DT_INDEXPRENATAL AND STATUS_%eval(&NUM. - 1)="avail" THEN "weird"  /*prenatal before index - check - should not occur*/
                               else "avail"  /*after index prenatal and not tied to this preg*/
                          END AS STATUS_&NUM. ,
                          Case when calculated Status_&NUM. like "PREN_OC_&NUM.%" then a.idxpren end as IdxPren
                FROM (SELECT * FROM FINAL_INDEXPRE_&NUM. WHERE INDEX_FINDING_&NUM. NE "LTFU") A /*Those TBD or OUTC*/
                JOIN (select * from PRENATALDATES_OUTC_%eval(&NUM. - 1) where status_%eval(&NUM. - 1)='avail') B ON A.PATIENT_DEID=B.PATIENT_DEID /*Default is inner join*/

            	UNION
              	SELECT A.PATIENT_DEID,  A.DT_INDEXPRENATAL,  a.dt_pregnancylookback,a.dt_pregnancylookfwd,
                          A.DT_PREGNANCYOUTCOME_START,A.DT_PREGNANCYOUTCOME_END, A.PREG_Outcome, a.dt_PREG_Outcome, a.preg_mife,a.preg_miso,
                          a.preg_codetype, A.PREG_OUTCOMECOUNT, PREG_OUTCOMESCOMPARED,preg_outcomecodetypescompared, INDEX_FINDING_&NUM. ,
                          B.PRENNUM, B.ENC_DATE,
                          B.ENC_DATE- A.DT_PREGNANCYOUTCOME_END AS DAYS_ENCOUTC label='Days prenatal encounter to pregnancy outcome end (<0 = before outcome)',
                          CASE WHEN A.PRENNUM = B.PRENNUM AND INDEX_FINDING_&NUM="LTFU" THEN "LTFU"
                               else "avail"  /*after index prenatal and not tied to this preg*/
                          END AS STATUS_&NUM. ,
                          Case when calculated Status_&NUM. like "PREN_OC_&NUM.%" then a.idxpren end as IdxPren
                FROM (SELECT * FROM FINAL_INDEXPRE_&NUM. WHERE INDEX_FINDING_&NUM. EQ "LTFU") A
                        JOIN (select * from PRENATALDATES_OUTC_%eval(&NUM. - 1) where status_%eval(&NUM. - 1)='avail') B ON A.PATIENT_DEID=B.PATIENT_DEID

              	;
            	QUIT;

            **check the counts ;
            proc sql noprint;
              select count(case when status_&num. = 'avail' then patient_deid end)
                    +count(case when status_&num. = 'LTFU' then patient_deid end)
                    +count(case when status_&num. = 'NO_OUTC' then patient_deid end) ,
                    count(case when status_&num. contains 'PREN' then patient_deid end),
                    count(case when missing(idxpren) then patient_deid end),
                    count(case when not missing(idxpren) then patient_deid end)
              into :nopoc, :preg, :nmiss, :ncnt
              from pdo_updt_&num. ;

              %put ----- Check: avail+LTFU+NO_OUTC &nopoc = No IdxPren &nmiss. PrenOC &preg. = Has IdxPren &ncnt. ;


			/*This step is particularly important for indexing prenatal enounters after the first --
			  want to make sure that all prenatal encounters are retained.*/
            PROC SQL; %*combine findings for all prenatal dates;
             	CREATE TABLE PRENATALDATES_OUTC_&NUM. AS
              	SELECT case when not missing(sb.idxpren) then sb.idxpren
                         %if &num>1 %then %do; when sa.idxpren>0 then sa.idxpren %end;
                         else . end as Idxpren, SA.*,
                     case when missing(SB.STATUS_&NUM.) then sa.status_%eval(&NUM. - 1) else sb.status_&num. end as STATUS_&NUM.
                     label="prenatal encounter status (index-outcome-pregnancy &num.)"
              	FROM PRENATALDATES_OUTC_%eval(&NUM. - 1) SA 
				LEFT JOIN PDO_UPDT_&NUM. SB
                ON SA.PRENNUM = SB.PRENNUM
              	;
            	QUIT;


%Mend;




*********************************************************
MACRO: OGP_Updt

The purpose of this program is to update the information
retained on all the pregnancy outcome groups so that we 
can remove those from availability that are now
included in an identified pregnancy.

********************************************************;

%MAcro OGP_Updt;


            **NOW need to update outcome groups used in outcomegrp pair evaluation that were marked as LTFU;
            **return to DSN outidsfin because all recs in dsn for patient have been reviewed;
            **need this to find outcomegrp evaluated for index prenatals later marked LTFU;
            proc sql;
             	create table S5_outidsfin_&num._L as
              	select distinct a.*, b.index_finding_&num, dt_pregnancylookfwd, dt_pregnancylookback
              	from s5_outidsfin_&num a 
				join pdo_updt_&num b /*equiv to inner join*/
				on a.patient_deid=b.patient_deid; 
            	quit;


            ****************************************************************************************************;
            **** 6 PREP pt2 - outcome groups linked with the indexing prenatal encounter are sufficiently far from the end date of the prior pregnancy;
            ****            - rephrase - make sure any outcomegrps (not included in pregnancy) are far enough from Dt_pregnancy_end ;
            ****************************************************************************************************;
            **call outcomegrps flagged (from combineresults macro) and update all potential outcome groups status;
            **(result of pairs eval, outcomegrp is paired (preg) Rollup (rolled up into preg (formerly "dropped")) or need index ;


            **ok lets define indicators;
            *START because looking from time preg ends per eval of outcome groups to the
            *start of a subsequent outcome group to see if it is in the time alloted;
            *** DAYS RANGE SEE TABLE 5B1-3;
            **** 01.11.24 - Retain the Pregnancy look forward extension DATE in dsn - will need for step 10;
          	PROC SQL;
             	CREATE TABLE OGP_UPDT_&NUM. AS
              	SELECT A.PATIENT_DEID, A.OUTCOMEGRP, A.DT_OUTCOMEGROUP_START, A.DT_OUTCOMEGROUP_END,
                     	A.OUTCOME_ASSIGNED&ALG., A.Outcome_Assigned_Codetype&ALG., /*all outc groups*/

                     	/*index prenatals pregnancy info to eval against remaining outcgrps*/
                     	a.dt_outcomegroup_START - c.dt_pregnancyoutcome_end as Days_FromPregEnd label="Days from pregnancy End (per same preg pair eval) to start of outcome group",

						/*Create RESULT_NUM for all of the pregnancy outcome groups*/
                     	CASE
                         	/*if the outcomegrp has already been linked to prenatal index keep previous finding/result (only iteration 2+)*/
                          	WHEN A.RESULT_%eval(&NUM. - 1) Not in ("find index") THEN RESULT_%eval(&NUM. - 1)

                         	/*if patient did not have outcgrp eval with the index prenatal then keep previous result*/
                          	when c.outcomegroupsfound=0 then a.RESULT_%eval(&NUM. - 1)

                         	/*if indexprental LTFU then outcomegrp linked to index+286 can be paired with new index (D. pt+outcgrp ltfu)*/
                          	when a.outcomegrp=d.outcomegrp and d.index_finding_&num. in ("LTFU") then "find index"

                          	/*trick here - if idx ltfu then dont evaluate the remaining groups - all should be set to find index*/
                          	when c.index_finding_&num. = "LTFU" And a.RESULT_%eval(&NUM. - 1) ="find index" then "find index"

                         	/*outcomegrps evaluated that were found to be same preg as index prenatal (B. - pt+outcgrp not ltfu)*/
                          	WHEN B.RESULTANT_&NUM. IN ("rollup" "paired") THEN "PREN_OC_&NUM."

                         	/*outcomegrps evaluated that were found NOT to be same preg as index prenatal*/
                         	WHEN B.RESULTANT_&NUM. IN ("need index") THEN "find index"

                         	/*if the outcomegrp dates precede the prenatal index flag as before (mid if started before but ended on/after*/
                          	WHEN .Z< A.DT_OUTCOMEGROUP_END < C.DT_INDEXPRENATAL THEN "BEFORE PREN_OC_&NUM."
                          	WHEN .Z< A.DT_OUTCOMEGROUP_START < C.DT_INDEXPRENATAL THEN "MID PREN_OC_&NUM."  /*should be none -captured above or should be paired/dropped*/

                         	/*check remaining outcomegrps for patients with prenatal index to see if outcgrp should be added to preg (w/in alloted time)*/
                         	/*or is sufficiently far from pregnancy end date to be another/new pregnancy (after alloted time) (step 6 + table 3)*/
							/*CDL: NOTE -- This wasnt how I had originally pictured the algorithm working. Going to chat with Mollie about waht she thinks*/
                            /*             CDL discussed with MW, confirmed this is ok*/
                          	WHEN C.PREG_Outcome in ('LBS' 'LBM' 'MLS'  'SB') And
                               	A.Outcome_assigned&ALG. in ('LBS' 'LBM' 'MLS' 'SB' 'UDL') and .Z< CALCULATED Days_FromPregEnd < 168 then "PREN_OC_&NUM. EXT"
                          	WHEN C.PREG_Outcome in ('LBS' 'LBM' 'MLS' 'SB') And
                               	A.Outcome_assigned&ALG. in ('EM' 'SAB' 'IAB' 'UAB') and .Z< CALCULATED Days_FromPregEnd < 70 then "PREN_OC_&NUM. EXT"

                          	WHEN C.PREG_Outcome in ('EM') And
                               	A.Outcome_assigned&ALG. in ('LBS' 'LBM' 'MLS' 'SB' 'UDL') and .Z< CALCULATED Days_FromPregEnd < 154 then "PREN_OC_&NUM. EXT"
                          	WHEN C.PREG_Outcome in ('EM') And
                               	A.Outcome_assigned&ALG. in ('EM' 'SAB' 'IAB' 'UAB') and .Z< CALCULATED Days_FromPregEnd < 56 then "PREN_OC_&NUM. EXT"

                          	WHEN C.PREG_Outcome in ('SAB' 'IAB' 'UAB' ) And
                               	A.Outcome_assigned&ALG. in ('LBS' 'LBM' 'MLS' 'SB' ) and .Z< CALCULATED Days_FromPregEnd < 140 then "PREN_OC_&NUM. EXT"
                          	WHEN C.PREG_Outcome in ('SAB' 'IAB' 'UAB' ) And
                               	A.Outcome_assigned&ALG. in ('EM' 'SAB' 'IAB' 'UAB') and .Z< CALCULATED Days_FromPregEnd < 42 then "PREN_OC_&NUM. EXT"
                          	WHEN C.PREG_Outcome in ('SAB' 'IAB' 'UAB' ) And
                               	A.Outcome_assigned&ALG. in ('UDL') and .Z< CALCULATED Days_FromPregEnd < 154 then "PREN_OC_&NUM. EXT"

                          	WHEN C.PREG_Outcome in ('UDL') And
                               	A.Outcome_assigned&ALG. in ('LBS' 'LBM' 'MLS' 'SB' 'UDL') and .Z< CALCULATED Days_FromPregEnd < 168 then "PREN_OC_&NUM. EXT"
                          	WHEN C.PREG_Outcome in ('UDL') And
                               	A.Outcome_assigned&ALG. in ('EM' 'SAB' 'IAB' 'UAB') and .Z< CALCULATED Days_FromPregEnd < 70 then "PREN_OC_&NUM. EXT"

                          	/*remaining outcome groups remain available for next index prenatal link (grp not captured in 286d of index or preg outc type ext)*/
                          	ELSE "find index"

                     		END AS RESULT_&NUM. label="outcomegrp status (index-outcome-pregnancy &num.)" ,

                     	/*now based on final result / outcomegrp status, retain info for pregnancy
							Capture ID information for prenatal encounter*/
                     	CASE when Calculated Result_&NUM. in ("PREN_OC_&NUM EXT","BEFORE PREN_OC_&NUM.","MID PREN_OC_&NUM.") then c.idxpren /*capture from indexprenatal preg info*/
                          	when Calculated Result_&NUM.= "PREN_OC_&NUM." then b.idxpren /*from pair evaluation not ltfu*/
                          	when Calculated Result_&NUM. ="find index" then  . /*nada, no prenatal preg info yet*/
                          	%if &num.>1 %then %do;
                            	when calculated Result_&NUM. contains "PREN_OC" then a.idxpren
                          	%end;
                          	else -99999  end as IDXPREN,

                     	CASE when Calculated Result_&NUM. in ("PREN_OC_&NUM EXT","BEFORE PREN_OC_&NUM.","MID PREN_OC_&NUM.") then c.dt_indexprenatal /*capture from indexprenatal preg info*/
                          	when Calculated Result_&NUM.= "PREN_OC_&NUM." then b.dt_indexprenatal /*from pair evaluation not ltfu*/
                          	when Calculated Result_&NUM. ="find index" then  . /*nada, no prenatal preg info yet*/
                          	%if &num.>1 %then %do;
                            	when calculated Result_&NUM. contains "PREN_OC" then a.dt_indexprenatal
                          	%end;
                          	else -99999  end as Dt_IndexPrenatal format=date.,

                     	CASE when Calculated Result_&NUM. in ("PREN_OC_&NUM EXT","BEFORE PREN_OC_&NUM.","MID PREN_OC_&NUM.") then c.dt_pregnancyoutcome_start /*capture from indexprenatal preg info*/
                          	when Calculated Result_&NUM.= "PREN_OC_&NUM." then b.dt_pregnancyoutcome_start /*from pair evaluation not ltfu*/
                          	when Calculated Result_&NUM.= "find index" then .   /*nada, no prenatal preg info yet*/
                          	when calculated Result_&NUM. contains "PREN_OC" then . /*previously found indexpren*/
                          	else -99999  end as Dt_PregnancyOutcome_Start format=date.,

                     	CASE when Calculated Result_&NUM. in ("PREN_OC_&NUM EXT","BEFORE PREN_OC_&NUM.","MID PREN_OC_&NUM.") then c.dt_pregnancyoutcome_end /*capture from indexprenatal preg info*/
                          	when Calculated Result_&NUM. ="PREN_OC_&NUM." then b.dt_pregnancyoutcome_end /*from pair evaluation not ltfu*/
                          	when Calculated Result_&NUM. ="find index" then .   /*nada, no prenatal preg info yet*/
                          	when calculated Result_&NUM. contains "PREN_OC" then . /*previously found indexpren (info in prev file)*/
                          	else -99999  end as Dt_PregnancyOutcome_End format=date.,

                     	CASE when Calculated Result_&NUM. in ("PREN_OC_&NUM EXT","BEFORE PREN_OC_&NUM.","MID PREN_OC_&NUM.") then c.dt_PregnancyLookback /*capture from indexprenatal preg info*/
                          	when Calculated Result_&NUM.= "PREN_OC_&NUM." then b.dt_PregnancyLookback /*from pair evaluation not ltfu*/
                          	when Calculated Result_&NUM.= "find index" then .   /*nada, no prenatal preg info yet*/
                          	when calculated Result_&NUM. contains "PREN_OC" then . /*previously found indexpren*/
                          	else -99999  end as Dt_PregnancyLookback format=date.,

                     	CASE when Calculated Result_&NUM. in ("PREN_OC_&NUM EXT","BEFORE PREN_OC_&NUM.","MID PREN_OC_&NUM.") then c.dt_PregnancyLookFwd /*capture from indexprenatal preg info*/
                          	when Calculated Result_&NUM.= "PREN_OC_&NUM." then b.dt_PregnancyLookFwd /*from pair evaluation not ltfu*/
                          	when Calculated Result_&NUM.= "find index" then .   /*nada, no prenatal preg info yet*/
                          	when calculated Result_&NUM. contains "PREN_OC" then . /*previously found indexpren*/
                          	else -99999  end as Dt_PregnancyLookFwd format=date.,

                     	CASE when  Calculated Result_&NUM. in ("PREN_OC_&NUM EXT" "BEFORE PREN_OC_&NUM." "MID PREN_OC_&NUM.") then c.PREG_Outcome /*capture from indexprenatal preg info*/
                          	when  Calculated Result_&NUM. ="PREN_OC_&NUM." then b.PREG_Outcome /*from pair evaluation not ltfu*/
                          	when  Calculated Result_&NUM. ="find index" then ''   /*nada, no prenatal preg info yet*/
                          	when calculated Result_&NUM. contains "PREN_OC" then '' /*previously found indexpren (info in prev file)*/
                         	else 'weirds'  end as PREG_Outcome,

                     	CASE when  Calculated Result_&NUM. in ("PREN_OC_&NUM EXT" "BEFORE PREN_OC_&NUM." "MID PREN_OC_&NUM.") then c.dt_PREG_Outcome /*capture from indexprenatal preg info*/
                          	when  Calculated Result_&NUM. = "PREN_OC_&NUM." then b.dt_PREG_Outcome /*from pair evaluation not ltfu*/
                          	when  Calculated Result_&NUM. = "find index" then .   /*nada, no prenatal preg info yet*/
                          	when calculated Result_&NUM. contains "PREN_OC" then . /*previously found indexpren*/
                          	else -99999  end as Dt_PREG_Outcome format=date.,

						'x' as enc

              	FROM OUTCOMEASSIGNED_PREN_%eval(&NUM. - 1) A                                                                    /*data for all outcome groups*/
                LEFT JOIN FINAL_INDEXPRE_&NUM. C 
				ON A.PATIENT_DEID = C.PATIENT_DEID                              /*result for all index prenatal eval of outc grps (incl 0 outc)*/
                left join (select * from s5_outidsfin_&num._l
                            where index_finding_&num NE 'LTFU') b
                on a.patient_deid=B.patient_deid and a.outcomegrp=B.outcomegrp              /*data for outc grps w/i 286d of index not ltfu*/
                left join (select patient_deid,outcomegrp,index_finding_&num. from s5_outidsfin_&num._l
                           where index_finding_&num EQ'LTFU') D
                on a.patient_deid=d.patient_deid and a.outcomegrp=d.outcomegrp              /*need to identify those outc groups linked to prenatal that was tagged ltfu*/
            	;
            	quit;

            PROC SQL; %*combine again to get only add latest results info to resuts for all OUTCOME groups;
             	CREATE TABLE outcomeassigned_pren_&NUM. AS
              	SELECT
                    case when sb.Result_&NUM. NE "find index" then sb.idxpren
                         %if &num>1 %then %do; 
							when sa.idxpren>0 then sa.idxpren 
						 %end;
                         else . end as Idxpren,
                    case when sb.Result_&NUM. NE "find index" then sb.dt_indexprenatal
                         %if &num>1 %then %do; 
							when sa.idxpren>0 then sa.dt_indexprenatal 
						 %end;
                         else . end as Dt_IndexPrenatal format=date.,
                     SA.*,
                     case when missing(sb.result_&NUM. ) then sa.result_%eval(&NUM. - 1)
                          else sb.Result_&NUM.    /*keep prior stat as not evaluated*/
                      	  END AS RESULT_&NUM. label="outcomegrp status (index-outcome-pregnancy &num.)"
              	FROM outcomeassigned_pren_%eval(&NUM. - 1) SA 
				LEFT JOIN ogp_UPDT_&NUM. SB
                ON SA.Patient_deid = SB.Patient_deid And SA.outcomegrp = SB.outcomegrp
              	;
            	QUIT;

/*     proc freq data=outcomeassigned_pren_&num; table result_%eval(&num.-1) * result_&num./list missing;*/
/*     run;*/

%Mend;










************************************************
MACRO: Rollup_IndxOutc

PURPOSE: Roll up the results from all the 
comparisons for Steps 5a-5c
***********************************************;

%MAcro Rollup_IdxOutc;

        %**combine ogp_updt and pdo_updt files and retain final status for all prenatal enc_dates, outcomegrps;

		%*Using num-1 throughout because num will be one greater than the final number of rounds at the end of the do loop;
		%*let num = 2;

		%*Roll up the outcomegroup results from all of the rounds. Keep the final values;
        data ogp_result_oc;
        set ogp_updt_%eval(&NUM. - 1) (rename=(result_%eval(&NUM. - 1)=Result_OC_Fin) where=(result_oc_fin not contains "PREN_OC")) /*Final unmatched*/

          	%do i=1 %to %eval(&NUM. - 1);
            	ogp_updt_&i. (rename=(result_&i.=Result_OC_Fin) where=(result_oc_fin contains "PREN_OC_&i.") ) /*Grab the final matches.*/
          	%end;
            	;

          	Prenatal_Outcome = (result_oc_fin =: 'PREN');
          	*PostExtendedTime_Outc = (index(result_oc_fin, "POS")>0);
          	PostExtendedTime_Outc = (index(result_oc_fin, "EXT")>0); 
          	if result_oc_fin =: 'PREN' then result_oc_fin='PREN_OC'; /*Deal with those that end in a number for the round they were linked in*/
          	keep patient_deid outcomegrp dt_outcome: Outcome_assigned&ALG. result_oc_fin prenatal_outcome postextendedtime:
               	idxpren dt_indexprenatal dt_preg: PREG_: PREG_Codetype PREG_MIFE PREG_MISO dt_PREG_Outcome days_frompregend ;
             	label prenatal_outcome="Pregnancy defined by Prenatal encounter and Outcome Group"
                postextendedtime_outc = "OutcomeGrp added after index+outc eval (tbl3) - too close" 
        run;

		%*Roll up the prenatal encounter results from all of the rounds. Keep the final values;
        data pdo_result_oc ;
        set %do i=1 %to %eval(&NUM. - 1);
             pdo_updt_&i. (rename=(status_&i.=Status_OC_Fin index_finding_&i. = Index_Finding) where=(status_oc_fin ne 'avail')) %end; /*Get all the assigned prenatal encounters from earlier rounds*/
             pdo_updt_%eval(&NUM. - 1) (rename=(status_%eval(&NUM. - 1)=Status_OC_Fin) where=(status_oc_fin eq "avail") ) /*Only get available encounters from the final round*/
            ;

          	Prenatal_Outcome = (status_oc_fin =: 'PREN');
          	PostExtendedTime_Pren = (index(status_oc_fin, "POS")>0);

          	%*Step7: Any pregnancies with the Abotion ectopic, or molar come should be recategorized as Unspecified Abortion;
          	PREG_OUTCOME_Clean = preg_outcome;
           	if preg_outcome_clean='AEM' then preg_outcome_clean='UAB';

             keep patient_deid prennum Enc_date Status_oc_fin prenatal_outcome postextendedtime:
                  idxpren dt_indexprenatal dt_preg: PREG_:  dt_PREG_Outcome days_frompregend index_finding;
             label prenatal_outcome="Pregnancy defined by Prenatal encounter and Outcome Group"
                   postextendedtime_pren = "Prenatal encounter added after index+outc eval (tbl5)"
                   preg_outcome_clean = "Pregnancy outcome assigned mod (step7)"
             ;
        run;

        proc sql;

			/*Grab all the distinct pregnancy information from ogp_result_oc*/
         	create table ogp_result_oc_preg as
          	select distinct *
           	from ogp_result_oc (keep=patient_deid idxpren dt_indexprenatal dt_preg: PREG_: dt_PREG_Outcome prenatal_outcome )
            where prenatal_outcome 
			order by patient_deid, idxpren;

			/*Grab all the distinct pregnancy information from pdo_result_oc*/
         	create table pdo_result_oc_preg as
          	select distinct *
           	from pdo_result_oc (keep=patient_deid idxpren dt_indexprenatal dt_preg: PREG_: index_finding prenatal_outcome )
            where prenatal_outcome 
			order by patient_deid, idxpren;

        	quit;

		/*Check*/
        proc compare data=ogp_result_oc_preg compare=pdo_result_oc_preg outnoequal out=oc_preg_ck;
		run;
        *want no unequal values;


       	** pregnancy data now;
		/*Collect information on all the prenatal encounter dates linked to the pregnancy.*/
        data pregs;
        set prenataldates_outc_%eval(&num. -1);
        	where status_%eval(&num. -1) not in ("NO_OUTC", "avail"  "LTFU");

			Status = scan(status_%eval(&num. -1),1,"");
        run;
        proc sort data=pregs;
        	by patient_deid status ;
        run;
        data pregs_pren; 
		set pregs;
        	by patient_deid status ;

			/*Count the pregnancy number for the person*/
            if first.patient_deid then Pregnancy_Counter=0;
            if first.status then pregnancy_Counter+1;

			/*Count the number of prenatal encounters within a pregnancy*/
            if first.status then Prenatal_Counter=0;
            Prenatal_Counter +1;

			/*Grab the first and last prenatal encounter date*/
            if first.status then Dt_PrenEnc1st=enc_date; 
			retain dt_prenenc1st;
            if last.status then Dt_PrenEncLast=enc_date;
            if last.status;

            drop status_0-status_%eval(&num. - 1);
            label Pregnancy_Counter = "Count of pregnancies identified with prenatal+outcome encounters"
            	Prenatal_Counter = "Count of prenatal encounter dates for identified pregnancy"
                dt_prenenc1st = "Date 1st (earliest) prenatal encounter for pregnancy"
                dt_prenenclast = "Date last prenatal encounter for pregnancy"
             ;
             format dt_: date.;
        run;

		/*Collect information on the pregnancy outcome groups linked to a pregnancy.*/
        data opregs;
        set outcomeassigned_pren_%eval(&num. -1);
        	where result_%eval(&num. -1) not in: ("find index" "BEFORE");
	    	Result = scan(result_%eval(&num. -1),1,"");
        run;
        proc sort data=opregs;
        	by patient_deid result ;
        run;
        data pregs_outc; set opregs;
        	by patient_deid result ;

			/*Count the pregnancy number for a person*/
        	if first.patient_deid then Pregnancy_Count=0;
        	if first.result then pregnancy_Count+1;

			/*Count the number of pregnancy outcome groups within the pregnancy*/
        	if first.result then OutcomeGroup_Count=0;
        	Outcomegroup_count+1;

			/*Count the start and end of the pregnancy outcome groups within the pregnancy*/
        	if first.result then Dt_OutcomeGroupStart1st= dt_outcomegroup_start; 
			retain dt_outcomegroupstart1st;
        	if last.result then Dt_OutcomeGroupEndLast=dt_outcomegroup_end;

			/*Output last row for result*/
        	if last.result;

       		keep patient_deid idxpren result  pregnancy_count  outcomegroup_count dt_outcomegroupstart1st dt_outcomegroupendlast;
        	label Pregnancy_Count = "Count of pregnancies identified with prenatal+outcome encounters"
        		Outcomegroup_Count = "Count of outcome groups included in identified pregnancy"
                dt_outcomegroupstart1st = "Date of start of 1st outcome group for pregnancy"
                dt_outcomegroupendlast = "Date of end of last outcome group for pregnancy"
            ;
            format dt_: date.;
        run;
		/*Check*/
/*        proc freq;*/
/*			table result;*/
/*		run;*/

		/*Join the prenatal encounter and pregnancy outcome group information onto the pregnancies.*/
        proc sql;
           	create table Pregnancy_IndexOutc as
           	select * 
			from pregs_pren (drop= prennum status ) a
            full join pregs_outc (drop=result ) b 
			on a.patient_deid=b.patient_deid and a.idxpren=b.idxpren
            full join pdo_result_oc_preg c 
			on a.patient_deid=c.patient_deid and a.idxpren=c.idxpren
            ;
          	quit;
/*          proc compare;var pregnancy_count;with pregnancy_counter;run;*/

		/*Delete datasets that dont need anymore*/
        proc datasets lib=work nolist;
         	delete pdo_updt_: ogp_updt_: ;
         	delete s5_deletedrows: s5_lost: s5_miss: ;
         	delete prenataldates_outc_1-prenataldates_outc_%eval(&num. -2);
         	delete outcomeassigned_pren_1- outcomeassigned_pren_%eval(&num. -2);
        	quit;


%mend Rollup_IdxOutc;









****************************************
MACRO: Step8_LeftoverPren

Purpose: The goal of this program is to 
deal with those prenatal encounters 
that were not linked to a pregnancy
outcome group. Thus, we assume that those
pregnancies are lost to follow-up.

In the simple version of this, we link
together all prenatal encounters within
POWPS days [default 140] of each other.
***************************************;


%macro Step8_LeftoverPren;


/*              *so start by getting all prenatal that did not have outcome-pregnancy;*/
/*              *using prenataldates in last run (since run NUM had no updates);*/
/*              *(technically no Avail if all encounters have been checked - would be NoOutc);*/

%*07.06.24 - move this to main macro as it is the same dataset in both complex and simple step 8;
/*	Data Prenatal_nooutc;*/
/*    set pdo_result_oc (where=(status_oc_fin in ("NO_OUTC", "avail" ,"LTFU")))*/
/*	    prenataldates_none(in=b);*/
/**/
/*    	%*set outcome to missing/unknown step8 - variables match steps 3-6;*/
/*        preg_outcome='UNK';*/
/*        preg_outcome_clean='UNK';*/
/*        prenatal_outcome = 0;*/
/*        Index_finding = "unknown"; *replaces prior values tbd (no outcgp found)/ltfu (outcgp found but timing off);*/
/*        keep patient_deid  prennum enc_date status_oc_fin index_finding*/
/*             prenatal_outcome preg_outcome preg_outcome_clean anyoutcomegrp;*/
/*   	run;*/
/*    proc sort;*/
/*		by patient_deid enc_date prennum;*/
/*	run;*/

/*              Data Prenatal_NoPreg_alt;*/
/*               set prenataldates_outc_%eval(&NUM. - 1) (in=a where=(status_%eval(&NUM. - 1) in ("NO_OUTC", "avail" ,"LTFU")))*/
/*                   prenataldates_none(in=b);*/
/*                   keep patient_deid anyprenatal anyoutcome prennum enc_date ;*/
/*              run;*/
/*                proc sort data=prenatal_nopreg_alt;*/
/*                 by patient_deid enc_date prennum;*/
/*                run;*/
/*                proc compare data=prenatal_nopreg_alt compare=prenatal_nooutc;run;*yep, same;*/


/*  **earliest prenatal date available for woman is now the Index date and all prenatal;*/
/*  **visits within 140d of that Indexing prenatal are treated as the same pregnancy with;*/
/*  **outcome set to missing/unknown and Last prenatal date as dt_LTFU;*/
    data Prenatal_nooutc2;
    set  prenatal_nooutc;  
		by patient_deid enc_date;

        lid = lag1(patient_deid);
        ldt = lag1(enc_date);
        days= enc_date - ldt; %*Calculate days elapsed between subsequent prenatal encounters;

        if first.patient_deid then do;
        	pno=1;
            Dt_IndexPrenatal = enc_date;
            Dt_IndexSPW = enc_date + &POSPW. ;
            Dt_IndexSPW28 = Dt_IndexSPW +28; %*07.01- allow flex in timeframe of same-preg window;
/*            Dt_index140 = enc_date+140;*/
/*            Dt_index168 = enc_date+140+28; %*140 days afterwards plus a 28-day lookforward period;*/
            IdxPren= _n_ + (&num. * 1000000); %*Unique indexing prenatal encounter ID;
            length status $20.;
            status = cat("PREN_NO_",pno);
        end;

        else if patient_deid=lid then do;
        	if dt_indexprenatal le enc_date le dt_indexprenatal+140 then pno+0;
            	else if dt_indexprenatal lt enc_date le dt_indexprenatal+140+28 then status=cat("PREN_NO_",pno," POST");
                else do;
					/*Otherwise create a new indexing prenatal encounter*/
                	pno+1;
                    Dt_IndexPrenatal = enc_date;
                    IdxPren= _n_ + (&num. * 1000000);
                    status = cat("PREN_NO_",pno);
                end;
        end;
        status_final = cat("Pregnancy with no outcome, defined by grouped prenatal encs #",pno);
        retain dt_indexprenatal Dt_IndexSPW pno idxpren Dt_IndexSPW28 status idxpren;
        format dt_: date.;
        drop days lid ldt  ;
	run;

    *last row defines pregnancy;
    data Pregnancy_prenatalonly_SIMP ;
/*    data Pregnancy_prenatalonly ;*/
	set prenatal_noOutc2;
    	by patient_deid pno;

		/*Get first prenatal encounter date*/
        if first.pno then dt_prenenc1st=enc_date; 
		retain dt_prenenc1st;

		/*Count the number of prenatal encounters in a pregnancy*/
		if first.pno then Prenatal_Counter=0;
        Prenatal_Counter +1;

		/*Get last prenatal encounter date*/
		dt_prenencLast=enc_date;
        DT_LTFU = enc_date;

		if last.pno;
        format dt: mmddyy10.;

		/*Count pregnancy order*/
		Pregnancy_counter = pno;
	run;

    **all prenatal dates file updated;
    data PrenatalDates_all_status_&OCW._&ALG. ;
    set pdo_result_oc(in=a) prenatal_nooutc2 (in=b);

		if a then status = status_oc_fin;
		
		PostEnc = indexw(status,"POST")>0;
		
        if (a and status_oc_fin NOT in ("NO_OUTC", "avail" ,"LTFU")) Or b;
        PregNoOutcome = b;
        if a then Status_Final = cat("Pregnancy outcome on index prenatal #",scan(status_oc_fin,3,'_ '));
    run;
    proc sort;
		by patient_deid enc_date;
	run;

  %mend;








****************************************
MACRO: Step10_LeftOverOutc

Purpose: To deal with those 
remaining pregnancy outcome groups that 
are not otherwise linked to a pregnancy.
This will accomplish steps 9-10.
*****************************************;


*Altered 07.10 to include suffix (macvar PODS) to run for both simpla and complex prenatalonly pregs;

%MAcro Step10_LeftOverOutc(PODS=SIMP);


	/*STEP 9*/

	*all known preg dates - ignore leftover outcome groups not between dt preg lookback to dt preg lookfwd;
    *focus first on those where overlap may be an issue then pull in outcomeassigned_none;

    *note: algorithm step 10b says will need to modify for step8 pregs (no lookback and lookfwd dates);
    *      (table5 lookfwd for unknown/missing = 28d), for now use 1st prenatal date as lookback dt);
	*Identify all pregnancy dates.;
    data allpregdates_&PODS.;
	set pregnancy_indexoutc pregnancy_prenatalonly_&PODS. (in=b);
		/*Pregnancy window for pregnancies only defined via prenatal codes is 1st prenatal encounter through 28 + last prenatal encounter*/
     	if b then dt_pregnancylookback = dt_prenenc1st;
     	if b then dt_pregnancylookfwd = dt_prenenclast+28;
     	keep patient_deid idxpren dt_pregnancylookback dt_pregnancylookfwd preg_outcome dt_preg_outcome
          	dt_pregnancyoutcome_start dt_pregnancyoutcome_end ;
    run;

    *use table 5 to determine if remaining outcomegrps overlap known pregnancy, for prenatal only use 28d;
    *Mark outcomegroup with Error on Any (during a pregnancy timeline or too close to known pregnancy timeline);
    proc sql;
    	create table outc_nopreg_&PODS. as
       	select distinct patient_deid, outcomegrp, Outcome_assigned&ALG.,
               dt_outcomegroup_start, dt_outcomegroup_end , max(dataerr) as DataErr,
               max(tooCloseOutc) as TooClose , /*max(tooCloseOutcB) as TooCloseB, */
               max(duringpregnancy) as DuringPreg ,count(distinct idxpren) as idxcnt
			   /*Taking the max here becuase a pregnancy outcome group may appear more than once since linked to pregnancy
			   info on patient ID. We want to know if a pregnancy outcome group overlaps or is too close for any pregnancy*/
		from (

        		/* *leftover outcgrps linked to all possible preg info (1 to many);*/
            	select DISTINCT *,

					/*If outcome group starts or ends between lookback and lookforward, then say that it occurred
						during the pregnancy*/
	             	case when dt_outcomegroup_start between dt_pregnancylookback and dt_pregnancylookfwd then 1
	                  	 when dt_outcomegroup_end between dt_pregnancylookback and dt_pregnancylookfwd then 1
	                  	 else 0 end as DuringPregnancy, /*overlap 10a*/

					/*Determine if the pregnancy outcome group occurred prior to the lookback date*/
	             	case when dt_outcomegroup_start lt dt_pregnancylookback and dt_pregnancylookback >.z then 1
	                  	 when dt_outcomegroup_end lt dt_pregnancylookback and dt_pregnancylookback >.z  then 1
	                  	 else 0 end as BeforeLookback, /*outc becomes 1st - outcgrp then preg */
						 /*This could include overlap but those are also removed.*/

	             	case when dt_outcomegroup_start gt dt_pregnancylookfwd and dt_pregnancylookback>.z then 1
	                  	 when dt_outcomegroup_end gt dt_pregnancylookfwd  and dt_pregnancylookback>.z then 1
	                  	 else 0 end as AfterLookFwd,  /*preg becomse 1st - preg then outcgrp */
						 /*This could include overlap but those are also removed.*/

	             	/*set 1st and 2nd outcomes for comparisons -- ordering matters so identifying the outcome that occurred
					first and the outcome that occurred second.*/
	             	case when Calculated BeforeLookback=1 then Outcome_Assigned&ALG
	                  	 when calculated AfterLookFwd=1 then PREG_OUTCOME 
						 else '' end as CheckOutcome1,
	             	case when Calculated BeforeLookback=1 then PREG_OUTCOME
	                  	when calculated AfterLookFwd=1 then Outcome_Assigned&ALG else '' end as CheckOutcome2,

	             	case when Calculated BeforeLookback=1 then Dt_PregnancyOutcome_Start - Dt_outcomeGroup_End
	                  	 when calculated AfterLookFwd=1 then Dt_OutcomeGroup_Start - Dt_PregnancyOutcome_End
	                  	 end as Days_TooCloseCheck,  /*days to compare table 3, 10b*/

	            	CASE
	             		WHEN calculated CheckOutcome1 in ('LBS' 'LBM' 'MLS' 'SB')
	              			AND calculated CheckOutcome2 in ('LBS' 'LBM' 'MLS' 'SB' 'UDL') and calculated days_TooCloseCheck < 168 then 1
	             		WHEN calculated CheckOutcome1 in ('LBS' 'LBM' 'MLS')
	              			AND calculated CheckOutcome2 in ('EM' 'SAB' 'IAB' 'UAB') and calculated days_TooCloseCheck < 70 then 1
	            		WHEN calculated CheckOutcome1 in ('EM')
	              			AND calculated CheckOutcome2 in ('LBS' 'LBM' 'MLS' 'SB' 'UDL') and calculated days_TooCloseCheck < 154 then 1
	            		WHEN calculated CheckOutcome1 in ('EM')
	              			AND calculated CheckOutcome2 in ('EM' 'SAB' 'IAB' 'UAB') and calculated days_TooCloseCheck < 56 then 1
	            		WHEN calculated CheckOutcome1 in ('SAB' 'IAB' 'UAB' )
	              			AND calculated CheckOutcome2 in ('LBS' 'LBM' 'MLS' 'SB') and calculated days_TooCloseCheck < 140 then 1
	            		WHEN calculated CheckOutcome1 in ('SAB' 'IAB' 'UAB' )
	              			AND calculated CheckOutcome2 in ('EM' 'SAB' 'IAB' 'UAB') and calculated days_TooCloseCheck < 42 then 1
	            		WHEN calculated CheckOutcome1 in ('SAB' 'IAB' 'UAB' )
	              			AND calculated CheckOutcome2 in ('UDL') and calculated days_TooCloseCheck < 154 then 1
	            		WHEN calculated CheckOutcome1 in ('UDL')
	              			AND calculated CheckOutcome2 in ('LBS' 'LBM' 'MLS' 'SB' 'UDL') and calculated days_TooCloseCheck < 168 then 1
	            		WHEN calculated CheckOutcome1 in ('UDL')
	              			AND calculated CheckOutcome2 in ('EM' 'SAB' 'IAB' 'UAB') and calculated days_TooCloseCheck < 70 then 1
	            		ELSE 0 End as TooCloseOutc label="Outcomegrp too close to known preg (10b)" ,

					/*Going to remove those that are too close or occurred during the pregnancy*/
	             	max(calculated toocloseOutc, calculated duringpregnancy) as DataErr

           		from ( 
					/*Join all of the pregnancy date information onto the pregnancy outcome groups that are not linked
					to a prenatal encounter*/
					select patient_deid, outcomegrp, Outcome_assigned&ALG.,
                    	dt_outcomegroup_start,dt_outcomegroup_end
                  	from ogp_result_oc where prenatal_outcome=0) SA  /*leftover outcome groups - those not connected to a prenatal encounter*/
                   	Left Join
                  	AllPregDates_&pods. SB 
					on sa.patient_deid = sb.patient_deid
                )
		group by patient_deid, outcomegrp;
    	;
      	quit;
	/*Check*/
/*	proc freq;*/
/*		table idxcnt;*/
/*	run;*/

	/*Subset to those pregnancy outcome groups that do not overlap and are not too close to an identified pregnancy*/
	proc sql;
		create table outc_nopreg2_&PODS. as 
		select * 
		from outc_nopreg_&PODS.  
		where dataerr=0;
      	quit;
/*    proc freq data=outc_nopreg;table dataerr *tooclose:;run;*/

    /*
    **** STEP 9A - identify preg outcome groups ****;
    **Now run those not tooclose or overlap (dataerr=0) and w/o pren-ids (_none) through modified 5abc macro**;
    */
    data outc_remain0_&PODS.;
    set outcomeassigned_none (in=a) outc_nopreg2_&PODS.(in=b);
    	keep patient_deid outcomegrp dt_outcomegroup_start;
    run;

    proc sort data=outc_remain0_&PODS. nodups;
		by patient_deid outcomegrp;
    run;

    /*
    **** STEP 9B - determine if outcome group too close to outcomegrp already identified in preg ****;
    ** (using pregnancy timeline as it includes the dates of the pregnancy outcomegroups included) **;
    */
    *get base info from original outcomegrp file for all - now have complete outcomegrp information for paireval;
    proc sql;
     	create table RemainingOutcomes_GRP0 as
      	select DISTINCT
             	a.patient_deid, count(distinct b.outcomegrp) as OutcomeGroupsFound,
             	b.outcomegrp, b.dt_outcomegroup_start, b.dt_outcomegroup_end, b.mifepristone, b.misoprostol,
             	Case 
					when b.OutcomeGroup_DelivPRInpatEnc_N > 0 then 1 
					else 0 end as InpatPRDelivery,
             	b.DT_Outcome_assigned&ALG., b.Outcome_assigned&ALG., b.outcome_assigned_codetype&ALG.,
             	'need index' as resultant_&num._0
      	from outc_remain0_&PODS. a 
		left join /*Get the information on the pregnancy outcomes assigned to each of the pregnancy outcome groups*/
        	(select * 
			 from _OUTCOMESASSIGNED_&OCW.   ) b
        on a.patient_deid=b.patient_deid and a.outcomegrp=b.outcomegrp
      	group by a.patient_deid;
		quit; /*CDL: ADDED this line*/

	/*Get the maximum number of outcome groups per person -- This is what we are going to use to loop over, similar to step 5*/
	proc sql;
    	select max(n) into :mxocg
       	from (
				select count(distinct outcomegrp) as n 
				from RemainingOutcomes_GRP0 
				group by patient_deid
			  )
		;
      	quit;   %put --- &mxocg. = max number of outcomegrps to possible be pregnancy;


/*    option notes mprint mlogic;*/
	%let sni=100; %*just to initialize;
    %do stn = 1 %to &mxocg.;
	/*testing: %let stn = 1;*/

    	%*exit step10 if no outcomegrp left to lump into preg;
      	%IF &SNI. = 0 %THEN %DO; %Goto ExitST ; %END;

    	*** treat 1st outcomegrp as the index (basically steps 3,4) - Create the variables that we need to run through the PairEval macro;
        data outc_remain_&stn. ;
        set RemainingOUtcomes_GRP%eval(&stn. -1) ;
        	where resultant_&NUM._%eval(&stn. -1) ='need index';
          	by patient_deid; 
			if first.patient_deid; /*Output the first patient_deid row -- getting the first pregnancy outcome group*/
          	Dt_IndexPrenatal = dt_outcomegroup_start; /*Calling this the first indexing prenatal encounter so that can use the same macro code - treating as indexing outcome group*/
          	IdxPren= _n_ + ((90+&stn.) * 1000000);
        run;
		/*This allows you to get the start date fo the first pregnancy outcome group to comparing with subsequent pregnancy outcome groups.
		Doesnt matter if start or end date, just want some date to link the prgnancy otucome groups together for use in the paireval
		macro.*/

        *get base info from original outcomegrp file for all;
        proc sql;
        	create table RemainingOutcomes_GRP&stn. as
          	select DISTINCT a.patient_deid, a.dt_IndexPrenatal format=date., a.IdxPren ,  b.*  /*added idx info to keep partnerup macro*/
          	from outc_remain_&Stn. a 
			left join RemainingOutcomes_GRP%eval(&stn. -1) B 
			on a.patient_deid=b.patient_deid
          	where b.resultant_&num._%eval(&stn. -1) = 'need index'
          	group by a.patient_deid;
        	quit;
    	*** end step34-like;

    	proc sql noprint;
      		select max(outcomegroupsfound) , count(distinct patient_deid) into :max, :oidc
      		from remainingOutcomes_GRP&stn.;
    		quit;%put &max &oidc  ;

    	%*get current option settings;
    	%let opt_mpr = %SYSFUNC(GETOPTION(mprint));
    	%let opt_mlo = %SYSFUNC(GETOPTION(mlogic));
    	%let opt_nts = %SYSFUNC(GETOPTION(notes));
    	%put &opt_mpr &opt_mlo &opt_nts;

    	%*suspend log info for pair eval steps;;
    	options nomprint nomlogic nonotes;

        %PAIREVAL(RemainingOUTComes_GRP&stn. , S10 );
/*      options  mprint  mlogic  notes;*/

    	%*reset option settings;
     	options &opt_mpr &opt_mlo &opt_nts.;

   		%*Outcome-only pregnancy info;
   		Data Preg_Outconly_&stn. ;
    	set s10_index_outcome_fin_&NUM. ;
    		drop pregnancyoutcomegroupsevaluated;
   		run;
 
   		proc sql noprint;*adding updated results to remaining outcomes file  ;
    		create table REmainingOUtcomes_GRP&Stn. as
     		select a.*, b.resultant_&num as resultant_&num._&stn.
     		from REmainingOUtcomes_GRP&Stn. a 
			left join s10_outidsfin_&NUM. b
       		on a.patient_deid=b.patient_deid and a.outcomegrp=b.outcomegrp;

       		select count(*) into :sni from REmainingOUtcomes_GRP&Stn. where resultant_&num._&stn.='need index';
/*       		select count(*) into :sni from s10_outidsfin_&NUM. where resultant_&num.='need index';*/
       		%put ---- after &stn., &sni. need index;
   			quit;

	%end;


	%exitst:
    Data Pregnancy_OutcOnly_&PODS.;
    set preg_outconly_1-preg_outconly_%eval(&stn. - 1);
    run;

    %*07.09 this dataset isnt used so remove;
/*    Data outcomes_leftoverdates;*/
/*	set remainingoutcomes_grp&stn.;*/
/*    run;*/

    proc datasets nolist; delete outc_remain_1 - outc_remain_&stn.; quit;

%mend;












/*****************************************************************************

MACROS that output log notes

Some of these are important for calculating global macro variables
that are used to run through the code loops.

*****************************************************************************/

/* ***** some notes for log window ******;*/
%macro lognote3prep;

	%put;%put ---------- Steps 3prep ----------;
    %put OutcomeGroups using ** &OCW. days ** And Concordance Algorithm ** &ALG. **;
    %put Available data: ;
    %put -- Prenatal encounters (for women with outcomes) at the start (PrenatalDates_Outc_0) &pdo0.   ;
    	%put ---- &potmat. is the most prenatal encounters possible (-> women with encounters+outcg);
    %put -- Outcome Groups at the start (OutcomeAssigned_Pren_0) &oap0. ;
      	%put ---- &maxocg. is the most outcomegrps per pt available;
%mend;



%macro lognote3; *basically same as in lognote1 (usin macro just to prettify log);
%PUT;%PUT Steps 4-5a  (check this sharon) (index &NUM);
%put        ------  PATIENTS WITH PRENATAL DATES (index prenatal # &NUM): &pdts.  &idc. ;
%put        ------  PATIENTS WITH INDEX PRENATAL DATES &NUM: &ipdts.   ;
%put        ------  PATIENT IDs with 1+ OUTCOMEGROUPS IN  &pow. d: &pog.  &pic. ;
%put        ------  TOTAL OUTCOME GROUPS TO EVALUATE W/IN  &pow. d: &OE.   ;
%put        ------  MEAN OUTCOMEGROUPS PER PATIENT ID IN  &pow. d: &MNOE.   ;
%put        ------  MAX OUTCOMEGROUPS PER PATIENT ID IN  &pow. d: &MXOE.   ;

%put        ------  MAX# OUTCOME GROUPS per PATIENT ID: &max.   ;
%mend;



%macro lognote34;

	proc sql noprint;
		select max(outcomegroupsfound) , /*max - maximum number of outcome groups found per person*/
				count(distinct patient_deid), /*idc - number of patients*/
              	count(case when outcomegroupsfound=0 then patient_deid end), /*oct - number of pncs with no outcome groups found*/
                count(case when outcomegroupsfound>0 then patient_deid end), /*occ - number of outcome groups found to link with pnc*/
                count(distinct case when outcomegroupsfound>0 then prennum end) /*pic - number of prenatal encounters linked to outcome group*/
                into :max,:idc,:oct,:occ, :pic
        from indexprenataloutcomes_&NUM.;
        quit; ;

    %PUT;%PUT Steps 3-4 ;
    %put        ------  PATIENTS WITH INDEX PRENATAL &NUM: &idc.   ;
    %put        ------  PATIENT IDs with 1+ OUTCOMEGROUPS IN  &pow. days: &pic.  (&oct. with 0 outcg) ;
    %put        ------  TOT# OUTCOME GROUPS found w/in &POW. days of index:  &occ.   ;
    %put        ------  MAX# OUTCOME GROUPS per PATIENT ID: &max.   ;

%mend;




%macro lognote5ab;
	proc sql noprint;
     	select nobs into :noic from dictionary.tables where lowcase(memname)=lowcase("&RDP._outcomeneedindex_&NUM.");
     	select nobs into :noif from dictionary.tables where lowcase(memname)=lowcase("&RDP._index_outcome_fin_&NUM.");
     	select nobs into :noid from dictionary.tables where lowcase(memname)=lowcase("&RDP._deletedrows_&NUM.");
       	select count(*) into :noida from &RDP._outcomeneedindex_&NUM. where needpren='a'; *outcomegrps not preg;
       	select count(*) into :noidb from &RDP._outcomeneedindex_&NUM. where needpren='b'; *outcomegrps addl added;
     	select nobs into :noii from dictionary.tables where lowcase(memname)=lowcase("&RDP._outidsfin_&NUM.");
    	quit;


    %PUT Steps 5a-5b (&oct. patient-outcomegrp linked to index prenatal);
    %put ----- outcomes need new prenatal - &noic. (&noida., &noidb.);
    %put ----- outcomes to pregnancy - &noif. ;
    %put ----- outcomes rolled up with 1st outcomegrp into pregnancy - &noid.;
    %put &noii. %eval(&noic. + &noif. + &noid) ;

%mend;


*******************************************************************************;
** 
*******************************************************************************;
%Macro WrapUpPreg(pods=SIMPLE) ;

    *prefer one dataset of all of the pregnancies;
    data preg_&pods._&ocw._&alg. (label="Pregnancy data found prenatal encounters and outcome groups (&OCW.d, &alg.)");
    set pregnancy_indexoutc_&ocw._&alg. 
    	pregnancy_prenatalonly_&PODS._&ocw._&alg. (in=a)
        pregnancy_outconly_&PODS._&ocw._&alg. (in=b);

           prenonly=a; outconly=b;

           *only changes 4.15 - add algorithm variables, mod outconly recs, remove redundant/temp vars, add labels;
           Algorithm="&OCW-&ALG" ;
           Algorithm_Desc="&OCW-&ALG (combined outcomegroups w/in &ocw days for outcome algorithm &alg)" ;

           *04.18 - create distinct pregnancy id using idxpren;
           PregID = compress(idxpren||'-'||algorithm);

          *tech no indexprenatal date for outconly cases (take out filler dt) ;
           if outconly then dt_indexprenatal=.;

    	*STEP 10: use clean and Adjust outc-only pregnancies for AEM to UAB;
           if outconly then prenatal_outcome=0;
           if outconly then preg_outcome_clean=preg_outcome;
        *if preg_outcome='AEM' then preg_outcome='UAB';
        if preg_outcome='AEM' then preg_outcome_clean ='UAB'; *fix 6.18;

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

                preg_outcomecodetypescompared = "Codetypes for outcomes evaluated in defining pregancy outcome"
         ;

    run;

    *04.16 - one more step - sort by dt_pregoutcome and update preg counter;
/*    proc sort data=pregnancy_&ocw._&alg. ; */
    proc sort data=preg_&pods._&ocw._&alg. ; 
    	by patient_deid dt_preg_outcome; 
    run;

/*    data pregnancy_&ocw._&alg. ;*/
/*    data OUT_pregnancy_complex_&ocw._&alg. (label="Pregnancies identified using simple step8 for algorithm parameters &ocw. - &alg.");*/
/**oops 7.18 didnt inc pods macvar;*/
	
     %IF &pods. # SIMPLE, SIMP %then %let podsl = simple;
     %IF &pods. # COMPLEX, COMP %then %let podsl = complex;

    Data Out.Pregnancy_&pods._&ocw._&alg.  (label="Pregnancies identified using &podsl. step8 for algorithm parameters &ocw. - &alg.");
    attrib Pregnancy_Number length=4 label="Patient Pregnancy number (counter)";
    set preg_&pods._&ocw._&alg. ;
          by patient_deid; 
    	if first.patient_deid then Pregnancy_Number=0;
         Pregnancy_number+1;
    run;
%MEnd WrapupPreg;

