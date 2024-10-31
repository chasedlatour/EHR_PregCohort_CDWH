/*********************************************************************************************************
PROGRAM: chase_GestationalAge_macro.sas
PROGRAMER: Sharon

PURPOSE: Create the macro that will estimate LMPs for identified pregnancies.

MODIFICATIONS:
	-	05.2024 - Chase added annotations, reformatted, etc.
	- 	05.2024 - Chase completed QC with minor change. SPH and CDL agreed on these changes and 
		incorporated.
        06.18.24 - sph correct to use preg_outcome_clean from the source pregnancy dsn (lines 46-66)
                   and change the macro to use Preg_outcome_CLEAN instead of Preg_Outcome
    -   07.21.24 - SPH add macvar PODS to account for simple and complex prenatal-only datasets

**********************************************************************************************************/







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
*** Final Step - Put LMP estimates with full pregnancy dataset and output
**********************************************************************************************************;
**********************************************************************************************************;





%Macro GetGA(encw, alg);; 

/*%let encw=7;*/
/*%let alg=1;*/

    *>>>> GA STEP 0 <<<<*;

	data pregs_&pods.  (keep=patient_deid idxpren dt_indexprenatal dt_ltfu dt_preg: prenonly outconly dt_ga: preg_:
                 /*preg_outcome*/ preg_outcome_clean dt_prenenc1st dt_prenenclast algorithm pregnancy_number )
         pregnancy_&pods._&encw._&alg. /*Making working directory version of the dataset*/
         ;
   	set Out.pregnancy_&pods._&encw._&alg. (rename=(preg_outcome = preg_outcome_crude));

		/*Determine dates for looking for gestational age codes*/
    	if prenonly then Dt_GAEncLB = dt_ltfu -7;
        	else dt_GAEncLB = dt_preg_outcome - 7;
       	if prenonly then Dt_GAEncUB = dt_ltfu +7;
        	else dt_GAEncUB = dt_preg_outcome + 7;

		/*For determining gestastional age codes, we want either the preg outcome date or the date ltfu*/
      	DT_GAPreg = coalesce(dt_preg_outcome, dt_ltfu);
      	label dt_gapreg ='Dt preg outcome / dt_ltfu';
      	format dt_: mmddyy10.;

      	*preg_outcome = preg_outcome_clean; *06.18.24;

    run;

	/*Outputs two equivalent datasets*/



    *>>>> GA STEP 1 <<<<*;

    **********************************************************************************************************;
        ** Step 1a - encounters within +/-7 days of date of pregnancy outcome (or ltfu for prenatal-only pregs);

        ** instead of anchoring on specific GA code, calculate LMP at each code/encounter;
        ** do not need the specific GA code but do need to determine which is the most freq;
        ** estimated LMP during the window of interest (7d) for GA outcomes that match preg;
    **********************************************************************************************************;

	/*Prep files so that we can easily apply the gestational age steps*/
    proc sql; *all GA encounters for pregnancy flagged;
     	create table AnyGAEnc as
      	select distinct a.patient_deid, a.idxpren, DT_INDEXPRENATAL, prenonly, outconly, dt_gapreg, a.preg_outcome_Clean,  

              /*count of total GA enc and GA codes across entire pregnancy (idxpren)*/
/*              count(distinct b.enc_date) as Count_PregGADates label="distinct GA enc dates during preg", */
/*              count(distinct b.code) as Count_PregGACodes label="distinct GA enc codes during preg",*/

              /*gestational age info*/
              b.preg_outcome as ga_preg_outcome, code, code_hierarchy, parent_code,
              gestational_age_days, gest_age_wks, min_gest_age, max_gest_age , b.enc_date as dt_GAEnc, 

              /*gestational age and LMP estimates (will use each gestational age value for code)*/
              dt_gapreg - enc_date as Days_PregtoGAEnc label "Days Preg_Outcome - GA Enc",
			  case
               /*if GA-enc after Preg-OUtcome presume it is f/up visit and the GA-days do not need adjustment */
			  	when calculated days_pregtoGAenc < 0 then dt_gapreg - gestational_age_days
				else dt_gapreg - (gestational_age_days + calculated days_pregtoGAenc)
				end as dt_LMP  format=date. label "estimated LMP for code",

              /*identify ^trusted^ GA encounters (match on Outcome w/in +/-7 Days)*/
              Case when  
                     (b.preg_outcome='All' and missing(a.preg_outcome_Clean)=0 )
                  or (b.preg_outcome='Delivery' and a.preg_outcome_Clean in ('LBM' 'LBS' 'MLS' 'UDL' 'SB'  ) )
                  or (b.preg_outcome='Delivery, Missing' and a.preg_outcome_Clean in ('LBM' 'LBS' 'MLS' 'UDL' 'SB' 'UNK') )
                  or (b.preg_outcome='Abortion' and a.preg_outcome_Clean in ('IAB' 'UAB' 'SAB'  ) ) 
                  or (b.preg_outcome="IAB, UAB" and a.preg_outcome_Clean in ('IAB' 'UAB'))
                  or (b.preg_outcome='Missing' and a.preg_outcome_Clean in ('UNK') )
                  then 1 else 0
             	  end as GAOutcMatch label="GA enc outcome match Preg outcome / ltfu",
/*              Case when  */
/*                     (b.preg_outcome='All' and missing(a.preg_outcome)=0 )*/
/*                  or (b.preg_outcome='Delivery' and a.preg_outcome in ('LBM' 'LBS' 'MLS' 'UDL' 'SB'  ) )*/
/*                  or (b.preg_outcome='Delivery, Missing' and a.preg_outcome in ('LBM' 'LBS' 'MLS' 'UDL' 'SB' 'UNK') )*/
/*                  or (b.preg_outcome='Abortion' and a.preg_outcome in ('IAB' 'UAB' 'SAB'  ) ) */
/*                  or (b.preg_outcome="IAB, UAB" and a.preg_outcome in ('IAB' 'UAB'))*/
/*                  or (b.preg_outcome='Missing' and a.preg_outcome in ('UNK') )*/
/*                  then 1 else 0*/
/*             	  end as GAOutcMatch label="GA enc outcome match Preg outcome / ltfu",*/

              Case  when b.preg_outcome ne '' And  enc_date between dt_gaenclb and dt_gaencub then 1 
					else 0
             		end as GADaysMatch label="GA enc w/in +/-7 days of Preg Outcome/ltfu",

            	count(distinct Case when calculated GAOutcMatch and calculated GADaysMatch then b.enc_date end) as Count_PregGADates_OK ,
            	count(distinct Case when calculated GAOutcMatch and calculated GADaysMatch then b.code end) as Count_PregGACodes_OK,

            	/*added for 1c and 2b code hierarchy ranking*/
             	Case 
                 	/* Apply GA hierarchy in 1c to delivery pregnancy outcomes */
                  	when a.preg_outcome_Clean  in ('LBM' 'LBS' 'MLS' 'UDL' 'SB' ) And code_hierarchy='Specific gestational age' then 1  
                  	when a.preg_outcome_Clean  in ('LBM' 'LBS' 'MLS' 'UDL' 'SB' ) And code_hierarchy='Extreme prematurity' then 2 
                  	when a.preg_outcome_Clean  in ('LBM' 'LBS' 'MLS' 'UDL' 'SB' ) And code_hierarchy in ('Other preterm' 'Post-term') then 3 

                  	/* Apply GA hierarchy in 1c to Abortion pregnancy outcomes */
                  	when a.preg_outcome_Clean in ('IAB' 'UAB' 'SAB'  ) And code_hierarchy='Specific gestational age' then 1  
                  	when a.preg_outcome_Clean in ('IAB' 'UAB' 'SAB'  ) And code_hierarchy='Abortion' then 2   

                  	/* Apply GA hierarchy in 1c to EM outcomes */
                  	when a.preg_outcome_Clean  in ('EM' ) And code_hierarchy='Specific gestational age' then 1  
                  	when a.preg_outcome_Clean  in ('EM' ) And code_hierarchy='Extreme prematurity' then 2 
                  	when a.preg_outcome_Clean  in ('EM' ) And code_hierarchy in ('Other preterm') then 3 

                  	/* Apply GA hierarchy in 1c to Missing/Unknown outcomes */
                  	when a.preg_outcome_Clean  in ('UNK') And code_hierarchy='Specific gestational age' then 1  
                  	when a.preg_outcome_Clean  in ('UNK') And code_hierarchy='Extreme prematurity' then 2 
                  	when a.preg_outcome_Clean  in ('UNK') And code_hierarchy in ('Other preterm' 'Post-term') then 3  
                  	when a.preg_outcome_Clean  in ('UNK') And code_hierarchy in ('Missing') then 4  

                	else 99 end as GA_Hierarchy label="Code Hierarchy for outcome (1c)" ,

            	/*added for step 2 (adj for no prenatal encounter pregs)*/
					/*Determine if GA encounter during the prenatal window*/
            	.z lt dt_prenenc1st le b.enc_date le dt_gapreg as GA_PrenatalWindow label="GA enc between 1st prenatal and end preg/ltfu",
            	Pren_GA_enc, zhu_test, zhu_hierarchy, dt_prenenc1st,

           		/*  *>>>> GA STEP 3 <<<<*; */
          	 	/*step 3 - apply gestational age table based on outcome to all pregnancy outcomes*/
            	case a.preg_outcome_Clean  
                	when 'LBS' then 273
                	when 'LBM' then 252
                	when 'SB' then 196
                	when 'MLS' then 273
                	when 'EM' then 56
                	when 'SAB' then 70
                	when 'IAB' then 70
                	when 'UAB' then 70
                	when 'UNK' then 140
                	when 'UDL' then 273
        			end as gest_age_table,
        		dt_gapreg - (calculated gest_age_table) as Dt_LMP_table 

      	from pregs_&pods.  a 
		left join GESTAGEPren b  /*Gestational age encounters rolled up on the encounter date level - created in running file.*/
		on a.patient_deid=b.patient_deid /*Grabbing all GA encounters for anyone with a pregnancy*/
      	where parent_code NE 1   /*exclude GA enc with parent codes (1) - keeps child (0) and no ga-enc recs (.)*/
      	group by a.patient_deid, a.idxpren;
    	quit;

    **for later list of pregids to update;
	proc sql;
     	create table pregs_&pods.List as
      	select distinct pregs_&pods. .*,
             max(case when gaoutcmatch=1 and gadaysmatch=1 then 1 else 0 end) as GA_Match_OutcDays label="Any GA enc matching preg outc and timing",
             max(ga_prenatalwindow) as Any_GA_PrenatalWindow label="Any GA enc between 1st prenatal and end preg/ltfu",
             dt_lmp_table format=date. label="Estimated LMP based on pregnancy outcome table"
      	from pregs_&pods.  (drop=dt_gaencub dt_gaenclb) a 
		join anygaenc b 
		on a.idxpren=b.idxpren
      	group by a.patient_deid, a.idxpren
 		;
 		quit;
 	proc sort ;
		by patient_deid idxpren;
	run;


    ;
    **********************************************************************************************************;
    ** MORE PREP: 
    ** Find the most freq LMP estimate(s) based on GA code hierarchy that match pregnancy outc and timing;
    ** DSN has enc_dates and ga-codes (hcp) counts when both dates and codes Match (1=gaoutcmatch=gadaysmatch);
    ** and counts when either enc_dates or codes do not match, and for pregnancy (idxpren) overall;
    ** Because codes may calculate same est LMP but be different day distance from idxpren preg-outcdt capture 
    ** the closest days_pregtogaenc
    **********************************************************************************************************;

    proc sql stimer noerrorstop;  *prep - get preg-lmpdt dsn ;
     	create table AnyGA_EstLMPs as 
      	select distinct *
      	from 
         	( /*date level totals - want to get counts that meet criteria*/
          		select patient_deid, idxpren, dt_gapreg, preg_outcome_Clean, GA_hierarchy, gaoutcmatch, gadaysmatch, dt_LMP , 
                	min(abs(Days_PregtoGAEnc)) as Days_PregtoGAEnc_ClosestAbs, 
                	count(distinct Case when GAOutcMatch and GADaysMatch then dt_gaenc end) as Count_GADates_OK ,
                	count(distinct Case when GAOutcMatch and GADaysMatch then code end) as Count_GACodes_OK 
/*                	count(distinct Case when not (GAOutcMatch and GADaysMatch) then dt_gaenc end) as Count_GADates_notok ,*/
/*                	count(distinct Case when not (GAOutcMatch and GADaysMatch) then code end) as Count_GACodes_notok*/
         		from anygaenc 
           		group by patient_deid,idxpren, preg_outcome_Clean,gaoutcmatch,gadaysmatch, GA_hierarchy , dt_LMP 
         	) a 
      	full join
         	( /*pregnancy level totals (will repeat across all est lmp dates for idxpren)*/
          		select patient_deid, idxpren, 
	                count(distinct Case when GAOutcMatch and GADaysMatch then dt_LMP end) as Count_PregLMPDates_OK ,
	                count(distinct Case when GAOutcMatch and GADaysMatch then dt_gaenc end) as Count_PregGADates_OK ,
	                count(distinct Case when GAOutcMatch and GADaysMatch then code end) as Count_PregGACodes_OK 
/*	                count(distinct Case when not (GAOutcMatch and GADaysMatch) then dt_gaenc end) as Count_PregGADates_notok ,*/
/*	                count(distinct Case when not (GAOutcMatch and GADaysMatch) then code end) as Count_PregGACodes_notok*/
         		from anygaenc 
           		Group by patient_deid,idxpren 
          	) b 
       	on a.idxpren = b.idxpren
        where GADaysMatch=1 and GAoutcmatch =1 /*Limiting to those within 7 days*/
       /*for remaining step1 steps only interested in dates created from codes that match on date and outc*/
    	;
    	quit;
    proc sort data=anygaenc nodups; 
		by _all_;
	run;
    proc sort data=anyga_estlmps nodups; 
		by _all_;
	run;*good 0s;


    **********************************************************************************************************;
    ** GA-STEP 1b - 1 gestational code with 1 Est LMP date  (excl those with no ga dates - these are in ga-step2);
    ** among est lmp-dts that match pregnancy outcome and date range;
    ** (must have match flags to pick right rec, OK count applied to all pregrecs, match flags to the date) ;
    ** (also must use closest date to preg outc because the 1 code could produce mult est LMPs);
    ** Because 2 pregs have 1 code w/ diff LMP both same absolute days from pregoutcome, take earliest LMP
    **********************************************************************************************************;

    proc sql; *apply 1b.1 - one GA code with 1 LMP date estimate;
     	create table GaEnc_1b1 as
      	select distinct a.patient_deid, a.idxpren, dt_gapreg, a.preg_outcome_Clean, dt_LMP , days_pregtogaenc_ClosestAbs,
            '1b.1' as LMP_AlgStep, 1 as lmp_1gacode ,gaoutcmatch,gadaysmatch,Count_PregGACodes_ok,Count_PregGADates_ok
      	from anyga_EstLmps a 
      	where Count_PregLMPDates_ok=1  /*1 lmp date*/
        		And Count_PregGACodes_ok=1   /*1 ga code*/
      	group by patient_deid,idxpren
     	;
    	quit;


    ** Added 5.2.24 mtg, now get those with 1 LMP date calculated from 2+ codes. same date may have different ;
    ** ga code hierarchy use min to keep ^top^ ;
    proc sql; *apply 1b.1 - one GA code with 1 LMP date estimate;
     	create table GaEnc_1b2 as
      	select distinct a.patient_deid,a.idxpren,dt_gapreg,a.preg_outcome_Clean, dt_LMP, /*days_pregtogaenc_ClosestAbs,*/ /*CDL: COMMENTED OUT this variable - 10.31.2024*/
             	gaoutcmatch,gadaysmatch,Count_PregGACodes_ok,Count_PregGADates_ok,
             	min(ga_hierarchy) as GA_Hierarchy, '1b.2' as LMP_AlgStep, 1 as lmp_1galmp 
      	from anyga_EstLmps a 
      	where Count_PregLMPDates_ok=1     /*1 lmp date*/
        		And Count_PregGACodes_ok>1      /*2+ ga codes*/
      	group by patient_deid,idxpren
     	;
    	quit;

/*	proc sort nodupkey data=anyga_EstLmps out=xx; by idxpren; run;*/
/*    proc sort nodupkey data=gaenc_1b2 out=xx;by idxpren;run;*57 dups now 0 after change to add hier earlier;*/
/*    proc sql; create table xx as select * from gaenc_1b2 group by idxpren having count(*)>1;quit;*/
/*    *because 2 ga_hierarchy - need top rank so take min;*/

	/*Stack these two datasets together*/
    data gaenc_1b;
    merge gaenc_1b1 gaenc_1b2;
		by patient_deid idxpren;
    run;

/*	proc sql;*/
/*		select count(patient_deid) as n_row from gaenc_1b;*/
/*		select count(distinct cats(patient_deid, idxpren)) as n_distinct from gaenc_1b*/
/*		;*/
/*		quit;*/
/**/
/*	proc sql;*/
/*		create table test_1b as */
/*		select *, count(idxpren) as count */
/*		from gaenc_1b*/
/*		group by patient_deid, idxpren*/
/*		having count >1*/
/*		;*/
/*		quit;*/
/**/
/*	proc sql;*/
/*			create table test_1b_pregs as*/
/*			select **/
/*			from gaenc_1b2*/
/*			where patient_deid in ('Z1100787','Z2610093','Z6789839') and*/
/*                	 idxpren in (1005010, 1136776, 1748064)*/
/*			;*/
/*			quit;*/


	/*Check*/
/*	proc sql;*/
/*		select count(patient_deid) as n_1b1 from gaenc_1b1;*/
/*		select count(patient_deid) as n_1b2 from gaenc_1b2;*/
/*		select count(patient_deid) as n_1b from gaenc_1b;*/
/*		quit;*/

    *******************************************************************************************;
    ** Now get pregnancies for step 1c (i.e. those with >1 count_pregaLMPDates_ok 
    ** (note: switch from count_PregGACodes_ok -> ^move from codes to est LMPs^
    ** again limit to those dates derived from GA codes that align with preg outcome for
    ** GA Enc dates within +/- days of pregnancy outcome (GAOutcMatch=1 and GADaysMatch=1)
    ** note: limited to OK matches so LMP will be in dsn for each code hierarchy (most have 1)
    **       also it is possible for a single GA code to produce multiple LMP estimates so to
    **       start only select those with multiple LMPs (code counts accounted for later)
    *******************************************************************************************;


    proc sql; *1c_prep;
     	create table gaenc_1c_pregs_&pods.  as 
      	select * from anyga_estlmps
      	Where Count_PregLMPDates_ok > 1 ; /*those with 2+ LMP estimates*/
    	quit;

/*    proc freq data=anyga_estlmps;*/
/*		table count_preglmpdates_ok;*/
/*	run;*/
    *range 1 to 9, median=1 (1-70%, 2=23%,3=5%);
/*	proc sql;*/
/*		select count(patient_deid) as n_row from gaenc_1c_pregs_&pods.;*/
/*		quit;*/

    *******************************************************************************************;
    ** for 1c need to identify the number of GA encounters within hierarchys limiting pull to ;
    ** pregs that have >1 ga-enc and GA encs within correct days of preg and match preg outcome;
    ** NOTE: applying 1c hierarchies those pregs with only code hierarchies that arent in 1c2 list
    **       (e.g. Delivery pregs with only Missing code hierarchies) CANNOT trigger Top flag
    **       these are set to top_pregga_hierarchy=99, wont be captured in 1c1 but in 1c2 
    *******************************************************************************************;
/*	proc sql;*/
/*		select count(patient_deid) as n_row from GaEnc_1c;*/
/*		quit;*/

    proc sql stimer;*apply 1c hierarchy;
     	create table GaEnc_1c as

      	select *, 
           /*after applying hierarchy in subqueries, check if top hierarchy has only 1 estLMP date*/
            min(ga_hierarchy) as Top_PregGA_Hierarchy  ,
            case when calculated Top_pregga_hierarchy=1 and Count_PregLMPHier1=1 then 1
               when calculated Top_pregga_hierarchy=2 and Count_PregLMPHier2=1 then 1
               when calculated Top_pregga_hierarchy=3 and Count_PregLMPHier3=1 then 1
               when calculated Top_pregga_hierarchy=4 and Count_PregLMPHier4=1 then 1
               else 0 end as TopHier1EstLMP /*preg-level*/
      	From ( 
              
              select *,

                    /*binary var for GA HIERarchy above (carryover from prev versions -deletable?*/
                      case when ga_hierarchy=1 then 1 else 0 end as GA_Hier1,
                      case when ga_hierarchy=2 then 1 else 0 end as GA_Hier2,
                      case when ga_hierarchy=3 then 1 else 0 end as GA_Hier3,
                      case when ga_hierarchy=4 then 1 else 0 end as GA_Hier4,

                      count(distinct case when ga_hierarchy=1 then dt_LMP end) as Count_PregLMPHier1,
                      count(distinct case when ga_hierarchy=2 then dt_LMP end) as Count_PregLMPHier2,
                      count(distinct case when ga_hierarchy=3  then dt_LMP end) as Count_PregLMPHier3,
                      count(distinct case when ga_hierarchy=4 then dt_LMP end) as Count_PregLMPHier4

              from gaenc_1c_pregs_&pods. 
              group by patient_deid,idxpren
        	) 

        /*remove those dates for code_hierarchy that do not align with pregnancy outcome*/
        /*this also removes those with TopGAHierarchy=99 (i.e. women w/ only GA_Hier 99)*/
        Where  GA_Hierarchy ne 99
        group by patient_deid,idxpren
    ;
    quit;

    ***************************************************************************************************
    ** those preg with 1 Date (not code) in top hierarchy - select the record with the GA enc 
    ** for that Hierarchy that is in TopPregHier (much like step 1b) ;
    ** (because only 1 obs per date do not need closest check so the Having line is redundant)
    ******************************************************************************************;
    proc sql; *apply 1c1;
     	create table gaenc_1c1 as
      	select *
      	From 
          (
          select *, '1c.1' as LMP_AlgStep, 1 as lmp_1gacode_hierarchy  
          from gaenc_1c 
           where TopHier1EstLMP=1 and Top_PregGA_Hierarchy = GA_Hierarchy
           GROUP BY IDXPREN
/*              having DAYS_PREGTOGAENC_closestabs  = MIN(DAYS_PREGTOGAENC_closestabs)*/
          ) a

     	GROUP BY PATIENT_DEID, IDXPREN   ;   
       	;
    	quit;
/*	proc sql;*/
/*		select distinct count(patient_deid) from gaenc_1c1;*/
/*		quit;*/

    ***************************************************************************************************
    ** 1c1 has 1 rec for each dt_LMP and code_hierarchy so to get MostFrequent 
    ** most frequent what? each est LMP occurs 1x in dsn when limited to Top-hierarchy
    ** except for top=99 (no match e.g. IAB w/ missing and other preterm hier);
    ** so using COUNT_GACODES_OK (calculated LMP from the Most codes) - Use this as wt (ok-CL 4.25);;
    ** NOTE: Excludes those not in hierarchy (top=99) (because not meet hierarchy crit)  
    ***************************************************************************************************;

	/*Calculate complex LMP -- prep for actually applying*/
    proc sql; *setup 1c2 find pathway;
     	create table gaenc_1c2_prep as
      	Select * , min(complexpath) as PregComplexPath

      	From (
          /*subquery 2 - determine complex pathway*/
          select  distinct *,
                 count(distinct case when PregGaEstLMPMostCodes then dt_LMP end) as Count_Top_PreggaEstLMPs,
                 count(distinct case when PregGAEstLMPCLosestAbsDays then dt_LMP end) as Count_Closest_PregGAEnc ,

                 case when PregGAestLMPMostCodes = 1  /*most frequent GAcode LMP (moved away from enc code)*/ 
                       and calculated count_Top_PreggaEstLMPs = 1  /*only 1 date with highest code freq */
                       then 1   /*LMP for 1c2-complex-1 (most freq recorded and only 1 most freq)*/

                      When PregGAestLMPMostCodes = 1  /*most frequent GA enc code*/
                       and calculated count_Top_PreggaEstLMPs>1   /*more than 1 code with higest freq*/
                       and calculated Count_closest_PregGAEnc=1  /*only 1 code closest to preg outcome*/
                       and PregGAEstLMPCLosestAbsDays =1  /*code rec for closest GA enc date to preg outcome*/
                       then 2   /*age est for 1c2-complex-2 (>1most freq then pick closest to preg outc)*/

                     When PreggaEstLMPMostCodes= 1  /*most frequent GA enc code, no one LMP closest/topfreq*/
                      Then 3  /*age est for 1c2-complex -3 (all top preg codes)*/
                    
                     Else 99   /* "none" top_preggacode=0 leftover*/
                    end  as ComplexPath 

               From
                /*subquery1 - for the pregnancy flag find lmp dts with most GA codes and is closest enc to preg outc*/
               (  select *,
                      count_gaCodes_ok = Max(count_gacodes_ok) as PregGAestLMPMostCodes ,
                        days_pregtoGAEnc_CLosestAbs = min(days_pregtoGAEnc_CLosestAbs) as PregGAestLMPCLosestAbsDays
                 from gaenc_1c 
                 /*note: only using LMP dates where the GA hierarchy is the ^top^ hierarchy for the preg*/
                 where TopHier1EstLMP=0 and Top_PregGA_Hierarchy = GA_Hierarchy
                 group by patient_deid,idxpren  
                ) s1 /*end subquery1*/
       
            group by patient_deid, idxpren

        	) /*end subquery2*/

        Group by patient_deid,idxpren
     	;
    	quit;
/*	proc sql;*/
/*		select count(patient_deid) as n_row from gaenc_1c2_prep;*/
/*		quit;*/

    /*****************************************************************************************
    ** now we have only GA enc with codes that are in the highest hierarchy for the preg outc
    ** and have multiple filed billign GA code(s) within that hierarchy
    ******************************************************************************************/ 

    proc sql;
		
     	/*LMP based on all codes/enc dates for GA codes in hierarchy*/
     	create table gaenc_1c2_ptA as
      	select distinct patient_deid,idxpren, dt_gapreg, preg_outcome_Clean,
             min(dt_LMP) as dt_LMP_Min format=date.,   
             max(dt_LMP) as dt_LMP_Max format=date.,
             mean(dt_LMP) as dt_LMP_Avg format=date.,
             sum(dt_LMP * Count_GACodes_OK) / sum(count_GACodes_ok) as dt_LMP_WtAvg format=date. ,
             1 as lmp_gt1gacode_hierarchy 
      	From gaenc_1c2_prep
      	group by patient_deid, idxpren 
    	;*take away LMP_AlgStep because this is for all 1c2 folks (down below);

     
     	/*LMP based on select codes/enc dates for GA codes in hierarchy-> Complex Pathway, most freq*/
     	/*most freq GA code occurs on separate dates pick the earliest date to calc gest age (CONF)*/
     	create table gaenc_1c2_ptB1 as
      	select distinct patient_deid,idxpren, dt_gapreg, preg_outcome_Clean,
             dt_LMP as Dt_LMP_complex  format=date.,/*LMP for 1c2-complex-1*/

                '1c.2.1' as LMP_AlgStep, 1 as lmp_gt1gacode_hierarchy 
      	From gaenc_1c2_prep 
      	Where PregComplexPath = 1 /*pregnancy with record meeting complexpath 1*/
       		And complexpath = 1  /*the GA enc LMP that fits complex path 1*/
      	group by patient_deid, idxpren 
       	/*note: only 1 LMP date should meet complexpath1 so do not need min fn*/
     	;

     	/*LMP based on select codes/enc dates for GA codes in hierarchy-> Complex Pathway, Closest*/
     	create table gaenc_1c2_ptB2 as
      	select distinct patient_deid,idxpren, dt_gapreg, preg_outcome_Clean,
             dt_LMP   as Dt_LMP_complex  format=date.,/*LMP for 1c2-complex-2*/
                '1c.2.2' as LMP_AlgStep, 1 as lmp_gt1gacode_hierarchy 
      	From gaenc_1c2_prep 
      	Where PregComplexPath = 2 /*pregnancy with record meeting complexpath 2*/
       		And complexpath = 2  /*the GA enc LMP that fits complex path 2*/
      	group by patient_deid, idxpren 
      	;

      	/*Now for those pregs with >1 Topfreq and >1 Closest find LMP Using Mean(gestage days) */
     	create table gaenc_1c2_ptB3 as
      	select distinct patient_deid,idxpren, dt_gapreg, preg_outcome_Clean, pregcomplexpath, 
              sum(dt_LMP * Count_GACodes_OK) / sum(count_GACodes_ok) as dt_LMP_Complex format=date. ,
                '1c.2.3' as LMP_AlgStep, 1 as lmp_gt1gacode_hierarchy 
      	From gaenc_1c2_prep
      	Where pregcomplexpath=3 and complexpath=3
      	group by patient_deid, idxpren 
     	;

    	quit;

    /*combine all 1c2 ptb files*/
    data gaenc_1c2_ptb;
    merge gaenc_1c2_ptb1 gaenc_1c2_ptb2 gaenc_1c2_ptb3; *technically set would work as all Ids only in 1;
      	by patient_deid idxpren; 
	run;
/*    proc freq data=gaenc_1c2_ptb;table LMP_AlgStep;run;*/
    proc sort nodups;
		by patient_deid idxpren;
	run; 

    **see what pregnancies are left after step 1;
    proc sort data=pregs_&pods. ;
		by patient_deid idxpren;
	run;
    proc sort data=gaenc_1b out=x nodupkey;
		by patient_deid idxpren;
	run;*0 dups;

/*	proc sql;*/
/*		create table test as */
/*		select patient_deid, idxpren, count(idxpren) as count*/
/*		from gaenc_1b*/
/*		group by patient_deid, idxpren*/
/*		having count > 1*/
/*		;*/
/*		quit;*/
/*	proc sql;*/
/*		create table test2 as*/
/*		select * */
/*		from gaenc_1b*/
/*		where patient_deid in ('Z1100787','Z2610093','Z6789839') and */
/*				idxpren in (1005010, 1136776, 1748064);*/
/*		quit;*/

    /*proc sort data=gaenc_1c1;by patient_deid idxpren;run; *0;*/
    /*data xx1;set gaenc_1c1;by patient_deid idxpren; if first.idxpren and last.idxpren then delete;run;*0;*/

    /* select count(distinct idxpren) from pregs;*229107;*/


	/*Update the pregnancy list with the new LMP estimates*/
    data pregs_&pods.Updt_1;
    merge pregs_&pods.list (in=a) 
           gaenc_1c2_ptb (in=c2 keep=idxpren dt_: lmp_: patient_deid) /*2+ lmp date top hier - complexpath*/
           gaenc_1c2_pta (in=c2 keep=idxpren dt_: lmp_: patient_deid) /*2+ lmp date top hier -min,max,avg*/
           gaenc_1c1 (in=c1 keep=idxpren dt_: lmp_: patient_deid)     /*1 lmp date top hier*/
           gaenc_1b (in=b keep=patient_deid idxpren dt_: lmp_:)       /*1 lmp date*/
    ;
     	by patient_deid idxpren;
      	pregs=a; step1b=b; step1c1=c1; step1c2=c2;
      	LMP_AlgBased =  (b or c1 or c2);
      	leftpreg=(a and not (b or c1 or c2));
    run;

/*	proc sql;*/
/*		select count(patient_deid) as n_row from pregs_&pods.list;*/
/*		select count(distinct cats(patient_deid,idxpren)) as n_distinct_list from pregs_&pods.list;*/
/*		select count(patient_deid) as n_row_1c2ptb, count(distinct cats(patient_deid,idxpren)) as n_distinct_1c2ptb from gaenc_1c2_ptb;*/
/*		select count(patient_deid) as n_row_1c2pta, count(distinct cats(patient_deid,idxpren)) as n_distinct_1c2pta from gaenc_1c2_pta;*/
/*		select count(patient_deid) as n_row_1c1, count(distinct cats(patient_deid,idxpren)) as n_distinct_1c1 from gaenc_1c1;*/
/*		select count(patient_deid) as n_row_1b, count(distinct cats(patient_deid,idxpren)) as n_distinct_1b from gaenc_1b;*/
/*		quit;*/

/*	proc sort data=pregs_&pods.Updt_1 out=x nodupkey;*/
/*		by patient_deid idxpren;*/
/*	run;*3 dups;*/
/*    proc freq;*/
/*     *table step1b step1c1 step1c2 leftpreg* pregs*prenonly*outconly LMP_AlgBased*step1b*step1c1*step1c2/list;*/
/*     table LMP_AlgBased*LMP_AlgStep / missing;*/
/*    run;*/

    /**ok - for all left now, the preg_outcome and code_hierarchy do not align (step 1c) => will go to step 3;*/


    *>>>> GA STEP 2 <<<<*;


    ***************************************************************************************************;
    ***************************************************************************************************;
    *** 2  Calculate gestational age for pregnancies with missing/unknown outcomes  
    *** and no gestational age codes le 7 days prior to being lost to follow-up.     
    *** Get all the prenatal encounters for preg and check gestational age enc codes;
    ***************************************************************************************************;
    ***************************************************************************************************;

    proc sql;

    	/*** first get the GA encounter records for step 11b;*/
     	create table anygaenc_step2 as 
		select *
        from anygaenc a 
		left join (select idxpren,LMP_AlgBased from pregs_&pods.Updt_1) b 
		on a.idxpren =b.idxpren 
        where b.LMP_AlgBased = 0
           AND a.preg_outcome_Clean='UNK'   /*missing/unknown (prenonly) pregnancies */
           And GA_PrenatalWindow = 1  /*GA encounters during preg date window (1st pren enc to ltfu)*/
           And Pren_GA_enc = 1        /*prenatal encounters with gestational age codes*/
        group by a.idxpren 
		Having max(a.gadaysmatch)= 0  /*no GA enc w/in +/-7d window*/
    	;

    	/*** simliar to step1 simplify dsn by capturing main info on LMP dates; */
      	create table anyga_estlmps_step2 as
          	select distinct *
          	from 
	             ( /*date level totals - want to get counts that meet criteria*/
	              select patient_deid, idxpren,dt_gapreg,preg_outcome_Clean, GA_hierarchy, gaoutcmatch, gadaysmatch, dt_LMP , 
	                    min(abs(Days_PregtoGAEnc)) as Days_PregtoGAEnc_ClosestAbs, 
	                    count(distinct dt_gaenc ) as Count_GADates ,
	                    count(distinct code ) as Count_GACodes
	             from anygaenc_step2
	               group by patient_deid,idxpren, preg_outcome_Clean,gaoutcmatch,gadaysmatch, GA_hierarchy , dt_LMP 
	              ) a 
          	full join
	             ( /*pregnancy level totals (will repeat across all est lmp dates for idxpren)*/
	              select patient_deid, idxpren, 
	                    count(distinct dt_LMP) as Count_PregLMPDates ,
	                    count(distinct dt_gaenc ) as Count_PregGADates ,
	                    count(distinct code ) as Count_PregGACodes
	             from anygaenc_step2
	               Group by patient_deid,idxpren 
	              ) b 
           	on a.idxpren = b.idxpren
    	;
		quit; 

	/*Prep the data for Step 11b, similar to the steps previously.*/
    proc sql;
    	create table gaenc_2_prep as
     	select *, min(ga_hierarchy) as Top_PregGA_Hierarchy  , 
	            case when calculated Top_pregga_hierarchy=1 and Count_PregLMPHier1=1 then 1
	               when calculated Top_pregga_hierarchy=2 and Count_PregLMPHier2=1 then 1
	               when calculated Top_pregga_hierarchy=3 and Count_PregLMPHier3=1 then 1
	               when calculated Top_pregga_hierarchy=4 and Count_PregLMPHier4=1 then 1
	               else 0 end as TopHier1EstLMP /*preg-level*/

      	From (
	              select distinct *,  

	                        /*binary var for GA HIERarchy above (carryover from prev versions -deletable?*/
	                          case when ga_hierarchy=1 then 1 else 0 end as GA_Hier1,
	                          case when ga_hierarchy=2 then 1 else 0 end as GA_Hier2,
	                          case when ga_hierarchy=3 then 1 else 0 end as GA_Hier3,
	                          case when ga_hierarchy=4 then 1 else 0 end as GA_Hier4,

	                          count(distinct case when ga_hierarchy=1 then dt_LMP end) as Count_PregLMPHier1,
	                          count(distinct case when ga_hierarchy=2 then dt_LMP end) as Count_PregLMPHier2,
	                          count(distinct case when ga_hierarchy=3  then dt_LMP end) as Count_PregLMPHier3,
	                          count(distinct case when ga_hierarchy=4 then dt_LMP end) as Count_PregLMPHier4

	                    from anyga_estlmps_step2
	                    group by patient_deid,idxpren
              ) 
        group by patient_deid,idxpren
    	;
		quit;

 
    ************************************************************************************;
    ** copy assignment pathway from step 1:
    **   1 LMP, 1 GA code (2a1)
    **   1 LMP, 2+ GA code (2a2)
    **   2+ LMP date, 1 LMP for top hierarchy (2b1)
    **   2+ LMP date, complex pathway
    **   (remaining get ZHU test treatment)
    ************************************************************************************;

    proc sql;
     	create table gaenc_2_a1  as
      	select *, '2a.1' as LMP_AlgStep, 1 as lmp_2_1gacode 
      	from gaenc_2_prep
      	where count_preggacodes=1 and count_pregLMPdates=1;

     	create table gaenc_2_a2  as
      	select *, '2a.2' as LMP_AlgStep, 1 as lmp_2_1gacode 
      	from gaenc_2_prep
      	where count_preggacodes>1 and count_pregLMPdates=1;

     	create table gaenc_2_2b1 as
      	select *, '2b.0' as LMP_AlgStep, 1 as lmp_2_1gacode_hierarchy , dt_LMP format=date.
      	from gaenc_2_prep
       	where TopHier1EstLMP=1
         	and count_pregLMPdates>1
         	and top_pregga_hierarchy = ga_hierarchy
       	GROUP BY IDXPREN
     	;
    	quit;
    /*data x;merge gaenc_2_a1 gaenc_2_2b1 gaenc_2_a2;by idxpren;run;*/

    proc sql;

     	create table gaenc_2_prep_2b2 as

       	Select * , min(complexpath) as PregComplexPath

      	From (
          /*subquery 2 - determine complex pathway*/
          select  distinct *,
                 count(distinct case when PregGaEstLMPMostCodes then dt_LMP end) as Count_Top_PreggaEstLMPs,
                 count(distinct case when PregGAEstLMPCLosestAbsDays then dt_LMP end) as Count_Closest_PregGAEnc ,

                 case when PregGAestLMPMostCodes = 1  /*most frequent GAcode LMP (moved away from enc code)*/ 
                       and calculated count_Top_PreggaEstLMPs = 1  /*only 1 date with highest code freq */
                       then 1   /*LMP for 1c2-complex-1 (most freq recorded and only 1 most freq)*/

                      When PregGAestLMPMostCodes = 1  /*most frequent GA enc code*/
                       and calculated count_Top_PreggaEstLMPs>1   /*more than 1 code with higest freq*/
                       and calculated Count_closest_PregGAEnc=1  /*only 1 code closest to preg outcome*/
                       and PregGAEstLMPCLosestAbsDays =1  /*code rec for closest GA enc date to preg outcome*/
                       then 2   /*age est for 1c2-complex-2 (>1most freq then pick closest to preg outc)*/

                     When PreggaEstLMPMostCodes= 1  /*most frequent GA enc code, no one LMP closest/topfreq*/
                      Then 3  /*age est for 1c2-complex -3 (all top preg codes)*/
                    
                     Else 99   /* "none" top_preggacode=0 leftover*/
                    end  as ComplexPath 

               From
                /*subquery1 - for the pregnancy flag find lmp dts with most GA codes and is closest enc to preg outc*/
               (  select *,
                      count_gaCodes = Max(count_gacodes) as PregGAestLMPMostCodes ,
                        days_pregtoGAEnc_CLosestAbs = min(days_pregtoGAEnc_CLosestAbs) as PregGAestLMPCLosestAbsDays
                 from gaenc_2_prep
                   where TopHier1EstLMP=0
                     and count_pregLMPdates>1
                     and top_pregga_hierarchy = ga_hierarchy
                 group by patient_deid,idxpren  
                ) s1 /*end subquery1*/
       
            group by patient_deid, idxpren

        ) /*end subquery2*/

        Group by patient_deid,idxpren
     	;
    	quit;

    proc sql;
     	/*LMP based on all codes/enc dates for GA codes in hierarchy*/
     	create table gaenc_2b2_ptA as
      	select distinct patient_deid,idxpren, dt_gapreg, preg_outcome_Clean,
             min(dt_LMP) as dt_LMP_Min format=date.,   
             max(dt_LMP) as dt_LMP_Max format=date.,
              sum(dt_LMP * Count_PREGGACodes ) / sum(count_PREGGACodes) as dt_LMP_WtAvg format=date. ,
                 1 as lmp_2_gt1gacode_hierarchy 
      	From gaenc_2_prep_2b2 
      	group by patient_deid, idxpren 
    	;

     	/*LMP based on select codes/enc dates for GA codes in hierarchy-> Complex Pathway, most freq*/
     	/*most freq GA code occurs on separate dates pick the earliest date to calc gest age (CONF)*/
     	create table gaenc_2b2_ptB1 as
      	select distinct patient_deid,idxpren, dt_gapreg, preg_outcome_Clean,
             dt_LMP  /*LMP for 1c2-complex-1*/
                   as Dt_LMP_complex  format=date.,
                '2b.1' as LMP_AlgStep, 1 as lmp_2_gt1gacode_hierarchy 
      	From gaenc_2_prep_2b2
      	Where PregComplexPath = 1 /*pregnancy with record meeting complexpath 1*/
       		And complexpath = 1  /*the GA enc that fits complex path 1*/
      	group by patient_deid, idxpren 
     	;

     	/*LMP based on select codes/enc dates for GA codes in hierarchy-> Complex Pathway, Closest*/
     	create table gaenc_2b2_ptB2 as
      	select distinct patient_deid,idxpren, dt_gapreg, preg_outcome_Clean,
             dt_LMP   as Dt_LMP_complex  format=date.,/*LMP for 1c2-complex-1*/
                '2b.2' as LMP_AlgStep, 1 as lmp_2_gt1gacode_hierarchy 
      	From gaenc_2_prep_2b2
      	Where PregComplexPath = 2 and Complexpath=2 
      	group by patient_deid, idxpren 
      	;

      	/*Now for those pregs with >1 Topfreq and >1 Closest find LMP Using Mean(gestage days) */
     	create table gaenc_2b2_ptB3 as
      	select distinct patient_deid,idxpren, dt_gapreg, preg_outcome_Clean, pregcomplexpath, 
              sum(dt_LMP * Count_PREGGACodes) / sum(count_PREGGACodes) as dt_LMP_Complex format=date. ,
                '2b.3' as LMP_AlgStep, 1 as lmp_2_gt1gacode_hierarchy 
      	From gaenc_2_prep_2b2
      	Where pregcomplexpath=3 and complexpath=3
      	group by patient_deid, idxpren 
     	;

	/*Combine all of these estimates*/
    data gaenc_2b2_ptb;
	merge gaenc_2b2_ptb1  gaenc_2b2_ptb2 gaenc_2b2_ptb3; 
		by patient_deid idxpren;
    proc sort nodups;
		by patient_deid idxpren;
	run; 


	/*Finally, update the pregnancy information with the new LMP estimates*/
    /* select count(distinct idxpren) from pregs;*229107;*/
    data pregs_&pods.Updt_2b;
    merge pregs_&pods.Updt_1(in=a)
           gaenc_2_a1 (in=c1 keep=idxpren dt_: lmp_: patient_deid) 
           gaenc_2_a2 (in=c1 keep=idxpren dt_: lmp_: patient_deid) 
           gaenc_2_2b1 (in=c2 keep=idxpren dt_: lmp_: patient_deid) 
           gaenc_2b2_pta (in=c3 keep=idxpren dt_: lmp_: patient_deid)
           gaenc_2b2_ptb (in=c3 keep=idxpren dt_: lmp_: patient_deid)
    ;
     	by patient_deid idxpren;
       	step2a=c1; step2b_1=c2; step2b_2= c3; *1 or 2plus;
       	step2b  = c1 or c2 or c3;

       	if c1 or c2 or c3 then LMP_AlgBased =1;

    run;


    **** now check for first ZHU test - 2c - start by getting all related encounters ****;
    ***  2c - get 1st occurence of each screening test - USING TIME FROM INDEX PRENATAL (DT_PRENENC1ST);
    ***      if 1 zhu test then take the 1st instance of that test (2d1);
    ***      if 2+ zhu test use zhu hierarchy (2d2);
    ***;
    proc sql;
     	create table pregs_&pods._2c_zhu_test1st as 
      	select *, count(distinct zhu_test) as Count_PregZhuTests
      	From
	       (select * 
	          from
	           (
	           select distinct a.patient_deid,a.idxpren, dt_gapreg,preg_outcome_Clean,  dt_LMP , gestational_age_days,
	                         dt_gaenc as Dt_ZhuTest format=date., zhu_test,zhu_hierarchy, DT_gaenc - DT_INDEXPRENATAL as days_zhu        
	            from ANYgaenc a join (select idxpren,LMP_AlgBased from pregs_&pods.Updt_2b where LMP_AlgBased=0 and prenonly) a2
	            on a.idxpren=a2.idxpren
	            where not Missing(zhu_test) and GA_PrenatalWindow =1 
	           )
	           group by idxpren, zhu_test
	           having days_zhu = min(days_zhu)  /*take earliest aka 1st enc for each zhu test*/
	/*           HAVING CALCULATED DT_ZHUTEST = MIN(DT_ZHUTEST)*/
	       )
       	group by idxpren
       	order by patient_deid, idxpren, zhu_hierarchy
       	;*note: only a small number found because those w/ tests assigned lmp earlier;
    	quit;

    proc sql;

		create table gaenc_2c1 as 
      	select *, '2c.1' as LMP_AlgStep 
      	from pregs_&pods._2c_zhu_test1st
      	where count_pregzhutests = 1
      	;

     	create table gaenc_2c2 as
      	select *, '2c.2' as LMP_AlgStep 
      	from pregs_&pods._2c_zhu_test1st
      	where count_pregzhutests > 1
      	group by patient_deid, idxpren
      	having zhu_hierarchy= min(zhu_hierarchy)
      	;
    	quit;


    data pregs_&pods.Updt_2cd;
    merge pregs_&pods.Updt_2b (in=a)
           gaenc_2c1  (in=c1 keep=idxpren dt_: lmp_: patient_deid) 
           gaenc_2c2 (in=c2 keep=idxpren dt_: lmp_: patient_deid) 
    ;
     	by patient_deid idxpren;
       	step2c1=c1; step2c2=c2;  
       	step2c  = c1 or c2  ;

       	if c1 or c2 then LMP_AlgBased = 1;

    run;
/*    proc freq;table LMP_AlgBased*( outconly prenonly);run;*/

************************************************;
*** FINAL STEP - Add LMPs to Pregnancy file ****;
************************************************;

    Data Pregnancy_LMP_&pods._&encw._&alg. ;
    set pregs_&pods.updt_2cd;
        drop pregs_&pods. leftpreg step2c1 step2c2 step2b_1 step2b_2 step2c1 step2c2 /*captured elsewhere*/
             lmp_1: lmp_gt: lmp_2:  /*didnt use consistently*/
             dt_lmp_avg/* calc not needed, wt avg more accurate*/
             ;

        *wanted pregvar list includes post-partum f/up date (not otherwise in process steps);
        Dt_PostPartum = dt_gapreg+30; 
        format dt_: mmddyy10.;

        rename dt_lmp = Dt_LMP_Alg;

        label step1b='1 LMP date from 1 GA code (1b.1) or 2+ GA codes (1b.2) using trusted GA encounters'
              step1c1='1 LMP date with top GA hierarchy (1c.1) using trusted GA encounters'
              step1c2='2+ LMP dates selected by 1 with most GA codes (1c.2.1), most adn closest (1c.2.2) or avg of most (1c.2.3) using trusted GA encounters'
              step2a ='1 LMP date from 1 GA code (2a.1) or 2+ GA codes (2a.2) using prenatal+GA encounters'
              step2b= '1 LMP date with top GA hierarchy (2b.0) 2+ LMP dates selected by 1 with most GA codes (2b.1) most and closest(2b.2) or avg of most (2b.3) using prenatal+GA encounters'
              step2c= 'LMP from 1 Zhu test (2c.1) or 2+ Zhu tests (2c.2) using ZHU encounters'
              lmp_algstep = 'Step in gestational age algorithm that defined LMP date estimate'
              lmp_algbased = 'LMP date estimate found using algorithm (1) or not (0 - use dt_lmp_table)'
              dt_lmp_min = 'LMP date estimate using earliest/min date from accepted GA encounters'
              dt_lmp_max = 'LMP date estimate using latest/max date from accepted GA encounters'
              dt_lmp_wtavg = 'LMP date estimate using dates weighted by number of GA codes from accepted GA encounters'
              dt_lmp_complex = 'LMP date estimate using encounter dates selected via complex approach'
                
              dt_postpartum='The post-partum follow-up date: pregnancy outcome(ltfu) date + 30 days'
              ;
     ;
    run;
      
%mend;


