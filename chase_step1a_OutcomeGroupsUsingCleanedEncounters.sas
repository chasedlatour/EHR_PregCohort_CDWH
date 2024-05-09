/******************************************************************************

Program: chase_Step1a_OutcomeGroupsUsingCleanedEncounters.sas

Programmer: Sharon 

Purpose: Preps the input file (codeoutcome to outcomesbytype) and loads macro 
	OutcGrp to create outcome groups. Final dataset is OutcomeGroups_&EncGap 
	(work). This also outputs 
	OutcomeGroups_Within&encgap.Days_%sysfunc(date(),yymmddn8.).rtf (outpath);

Modifications:
	- 04-29-24: Chase (CDL) added comments throughout and modified appearance.
	- 04-29-24: CDL modified how the RX and DXPRRX variables were being rolled up
	 	across pregnancy outcomes (case when instead of max)

*******************************************************************************/







/*****************************************************************************

TABLE OF CONTENTS:

STEP 0 -- CLEAN UP THE ENCOUNTERS
	- 00 - IDENTIFY PREGNANCY OUTCOMES AND SUPPORTING DATA
	- 01 - CLEAN PREGNANCY OUTCOMES
	- 02 - CREATE A MACRO THAT DETERMINES CONCORDANCE PER TABLE 1
	- 03 - APPLY MACRO FOR CONCORDANT OUTCOMES

STEP 1 -- CREATE THE PREGNANCY OUTCOME GROUPS
	- 04 - 04 - MACRO TO CREATE OUTCOME GROUPS WITH ENCOUNTERS +/- [ENCGAP]

******************************************************************************/









****
STEP 0: CLEAN UP THE ENCOUNTERS
****;



/*****************************************************************************

			00 - IDENTIFY PREGNANCY OUTCOMES AND SUPPORTING DATA

******************************************************************************/



*Select distinct encounters, associated outcomes, and indicator variables for
whether they are defined by diagnosis codes, procedure codes, or medication
orders.;
proc sql;
  	create table outcomebytype as
  	select distinct patient_deid, enc_key, enc_base_class, enc_date, put(outcome,$outabbr.) as Outcome, 
		case when codetype in ('dx9' 'dx10') then 'DX'
             when codetype in ('med') and code='misop' then 'RXa' 
             when codetype in ('med') and code='mifep' then 'RXb' 
             else 'PR' end as DXPRRX,  
		1 as types  
  	from  codeoutcome
  	group by patient_deid, enc_key, enc_date, Calculated outcome, calculated dxprRX ;
quit;
*The output dataset has rows defined by distinct combinations of patient_deid,
enc_key, enc_base_class, enc_date, outcome, dxprrx, and types;



/*Look at the distribution of the number of records per person
in the dataset*/
/*proc sql;*/
/*	create table test as*/
/*	select count(patient_deid) as count*/
/*	from outcomebytype*/
/*	group by patient_deid*/
/*	;*/
/*	quit;*/
/*proc freq data=test; table count; run;*/

/*proc freq data=codeoutcome;*/
/*	table code; */
/*	where codetype='med';*/
/*run;*/


*Look at the number of people with mife and miso in an encounter;
/*proc sql;*/
/*	create table multrx as */
/*	select **/
/* 	from outcomebytype where dxprrx like 'RX%' */
/*	group by enc_key having count(*)>1;*/
/*	quit;*/
	*1438;


*Transpose the dataset so that there is an indicator for 
dx, pr, miso (rxa), and mife (rxb) for each encounter;
proc transpose data=outcomebytype out=outcomesbytype;
	by patient_deid enc_key enc_base_class enc_date outcome;
	id dxprrx; 
	var types;
run;










/*****************************************************************************

						01 - CLEAN PREGNANCY OUTCOMES

This provides a dataset with pregnancy outcomes for each encounter which will
then be used in the macro.

******************************************************************************/



*Create dataset with the grouped outcomes within an outcome;
proc sort data=outcomesbytype; 
	by patient_deid enc_key enc_date;
run;
data outcomesbytype2 ;
set outcomesbytype (drop=_name_  );
	by patient_deid enc_key enc_base_class enc_date;

    *If dx or pr indicators are missing values, then  set to 0;
    if dx=. then dx=0;
    if pr=. then pr=0;
    if rxa=1 and rxb=1 then rx=3; *both miso and mife;
		else if rxa=1 then rx=1; *miso;
        else if rxb=1 then rx=2; *mife;
        else rx=0;

	*Create the 3 integer flag for the information contained within
	a pregnancy outcome group that aligns with an outcome;
    length DXPRRX $3.;
    DXPRRX = cat(dx,pr,rx);

	*Create a flag for inpatient delivery procedure codes being contained
	within a pregnancy outcome group;
    DelivPR_Inp = (enc_base_Class='Inpatient' and PR>0 AND Outcome in ('LBM','LBS','MLS','SB','UDL')) ;

	*Create flags for delivery procedure codes in other settings.;
    DelivPR_Out = (enc_base_Class='Outpatient' and PR>0 AND Outcome in ('LBM','LBS','MLS','SB','UDL')) ;
    DelivPR_Em = (enc_base_Class='Emergency' and PR>0 AND Outcome in ('LBM','LBS','MLS','SB','UDL')) ;

run;

*Check - each encounter only has 1 date?;
/*proc sql;*/
/*	create table multdays as */
/*	select enc_key,count(distinct enc_date) as dates */
/*	from outcomesbytype2 */
/*	group by enc_key;*/
/*    proc freq;table dates;run;*/
/*    run; */
*Yes, 1 date per encounter key. Later figure out how this impacts inpatient data;



*Make sure these new flags appear on each outcome-based 
row within an encounter
If one of the rows for an encounter key has a value of 1, then all
rows should. Otherwise, all should have 0.
Further, remove some of the unnecessary variables;
proc sort data=outcomesbytype2; 
	by patient_deid enc_key;
run;
proc sql;
	create table outcomesbytype2b as
    select patient_deid, enc_key, enc_date, enc_base_class, outcome, dxprrx, 
				max(DelivPR_Inp) as DelivPR_Inp,
                max(DelivPR_Out) as DelivPR_Out,
				max(DelivPR_Em) as DelivPR_Em
    from outcomesbytype2 
	group by patient_deid, enc_key, enc_date, enc_base_class;
    quit;


*Transpose the dataset so that there is a single row per ENCOUNTER 
with outcome as column having value of codetype;
proc transpose data=outcomesbytype2b out=EncOut   ;
	by patient_deid ENC_KEY enc_base_class ENC_DATE DelivPR_Inp DelivPR_Out DelivPR_EM;
    var dxprrx;
    id outcome;
run;

*Look at the cross-distribution of the flags;
/*proc freq data=outcomesbytype2b;*/
/*	table delivpr_inp*delivpr_out*delivpr_EM/list missing;*/
/*run;*/
* They do not appear together.;

/*proc freq data=encout noprint;*/
/*	table patient_deid*enc_key / out=x;*/
/*proc freq;*/
/*	table count;*/
/*run;*/
*1 row per encounter;




*Clean up EncOut for Macro
- If someone has missing values for a flag, replace that missing 
	value with 000 to indicate that they do not have any data elements supporting
	that pregnancy outcome.
- Provide a count of the number of pregnancy outcomes identified in that pregnancy 
	outcome group. First ;
data encoutcome;
set EncOut;

	*Set missing to 000;
	array x(*) LBM LBS MLS SB UDL  SAB IAB UAB AEM EM ; 
    array ox(*) $3. o_LBM o_LBS o_MLS o_SB o_UDL o_SAB o_IAB o_UAB  o_AEM o_EM  ; 

   	*start with count = total possible outcomes then -1 if outcome not present;
	*then capture the original outcome code type in new var;
    EncOutcomeCount=dim(x);
    do i=1 to dim(x);
		if missing(x(i)) then do; 
			encoutcomecount=encoutcomecount-1; 
			x(i)='000';
		end;
        ox(i)= x(i) ;
    end;

    *this time count all outcome types with a code found, excl UAB-RX-only;
    EncOutcomes_NoRX=0;
    do i=1 to dim(x); 
    	if x(i)>'000' and vname(x(i)) ne 'UAB' then encoutcomes_norx+1; 
           else if vname(x(i))='UAB' And x(i) not in ('000' '001' '002' '003') then encoutcomes_norx+1;
    end;

    drop i _name_;
run;

*Look at cross check of some variables. These should be the same.;
/*proc freq data=encoutcome;*/
/*	table udl*o_udl uab*o_uab;*/
/*run;*check, looks ok;*/
/**Determine how many UABs only defined via Rx;*/
/*proc freq data=encoutcome;*/
/*	table uab*encoutcomecount*encoutcomes_norx/list missing;*/
/*run;*checks ok;*/









/*****************************************************************************

		02 - CREATE A MACRO THAT DETERMINES CONCORDANCE PER TABLE 1

Per algorithm, some outcomes are considered concordant (i.e., they, together, 
could describe the same outcome). Table 1 provides rules for identifying 
outcomes from these concordant groupings.

******************************************************************************/



*table 1 concordance with step to assign codetype for the concordant pair;
*Ignoring medication orders for concordance so using outcomecounts excl Rx;

* This macro will be used on an array of the pregnancy outcome columns 
within a dataset like the cleaned one.

Inputs:
- o1 is the first outcome that want to test
- o2 is the second outcome that want to test

The ordering matters and is applied as such. o1 should be the outcome
to give if concordant information is identified in o2.
;

%macro encconc(o1,o2); /*table 1 concordance when 2 outcome groups or 3 if 3rd is UAB-001*/

	/*The rest of these steps will only be applied if there is evidence for 2
	concordant outcomes.*/
    if (EncOutcomes_NoRX = 2 And 
			&o1 ne '000' 
			and &o2 ne '000' 
        	%IF &o2 = UAB %then %do; And UAB not in ('001' '002' '003' ) %END;)
			/*Medication orders ignored for the purpose of concordance*/
    then do;
		/*Make a flag for concordance*/
      	Concordant=1; 
		/*The first outcome is the concordant outcome*/
		Encounter_Concordant=upcase("&o1"); 
		/*Now get the max values for the indicator variables.*/
      	Encounter_Concordant_CodeType = 
        	cat(max(substr(&o1,1,1),substr(&o2,1,1)),max(substr(&o1,2,1),substr(&o2,2,1)),max(substr(&o1,3,1),substr(&o2,3,1)));

     	*reset the outcome values based on info from the concordant pair (1=concordant, 2=mising);
       	&o1= encounter_concordant_codetype;
        &o2= '000'; *CDL: QUESTION -- Not sure that its worth overwriting with 000;
    end;
%mend;














/*****************************************************************************

					03 - APPLY MACRO FOR CONCORDANT OUTCOMES

******************************************************************************/

*Set up options;
option mprint symbolgen;

*macro to call in separate algorithm pgms to split data into outcome concordant and discordant;
%MACRO CleanEnc ;

    *for all define concordant outcomes ;;
    data EncOutcomeClean  ;
    set EncOUtcome ;  

         array x(*) LBM LBS MLS SB UDL  SAB IAB UAB  AEM EM ; 

		 *Reset the outcome types for concordant outcomes;
         %EncConc(LBM, UDL) ;
         %EncConc(LBS, UDL) ;
         %EncConc(MLS, UDL) ;
         %EncConc(SB , UDL) ;
         %EncConc(SAB, UAB) ;
         %EncConc(IAB, UAB) ;
         %EncConc(EM , AEM) ;
        ;;;;

      *no longer need the counters (plus they are pre-adjustment counts);
      drop encoutcomecount encoutcomes_norx encounter_concordant encounter_concordant_codetype;
    run;
%MEND;
%CleanEnc ;


*Now look at the distribution of outcomes;
/*proc freq data= EncOUtcomeClean;*/
/*	table concordant lbm lbs mls sb udl sab iab uab em aem;*/
/* 	table udl o_udl lbs o_lbs;*/
/*run;*/
*better - from 70% with UDL (not 000) down to 20% of encounters with UDL;
*now 48% lbs procedure based, originally should be 0%;


**** NOW take this Cleaned Encounter file and make the outcome groups;
**main advantageof this extra step is to make cleaner encounter groups for date assignment;
**final outcome group assessment should be the same;

*put back into original form - mainly to keep adjustments to previous version minimal;
proc sort data= EncOUtcomeClean;by patient_deid enc_key enc_date;
run;
proc transpose data=encoutcomeclean
                out= encoutcomecleanrows (rename=(_name_=OUTCOME col1=DXPRRX));
	by patient_deid enc_key enc_date delivpr: ;
  	var sab--mls; 
run;












****
STEP 1: CREATE THE OUTCOME GROUPS
****;



/*****************************************************************************

		04 - MACRO TO CREATE OUTCOME GROUPS WITH ENCOUNTERS +/- [ENCGAP]

******************************************************************************/

*For testing:;
/*%let encgap = 7;*/

%MACRO OUTCGRP(encgap);

	/*Check*/
/*	proc freq data=encoutcomecleanrows;*/
/*		where outcome in ('SAB' 'IAB');*/
/*		table dxprrx;*/
/*	run;*/

	/*Calculate the number of days elapsed between outcomes using the date of the encounters*/
    proc sort data= EncOutcomeCleanRows;
		by patient_deid enc_date;
    run;
    data outcomesbyGroup_&encgap. ; /*outcomesbytype2_&encgap.*/
    set EncOutcomeCleanRows  ;
    	by patient_deid enc_date;

		/*Create DX, PR , AND RX variables for stacking and later merging*/
        DX=input(substr(dxprrx,1,1), 8.);
        PR=input(substr(dxprrx,2,1), 8.);
        RX=input(substr(dxprrx,3,1), 8.);

/*    	*Identify the enc_date in the previous row - 1st row will have a */
/*    		missing value for this.;*/
    	ldt=lag1(enc_date);
/*    	*Identify the patient_deid in the previous row.;*/
    	lid=lag1(patient_deid);

/*    	*Group all the codes that are within an updating 7 days of each other*/
/*    	into one outcome group;*/
    	if first.patient_deid then do; /*Start at the first patient_deid*/
    	  outcomegrp=1; 
    	  days_prevenc=.; /*Days since previous encounter*/
          StartDt = enc_date;
          EndDt = enc_date;
/*           *EncounterCount=1;*/
/*           *DateCount=1; *cant do here this way because mult rows per date and encounter;*/
    	end;
    	else do;
          days_prevEnc = enc_date - ldt;
          EndDt = enc_date;
        end;

/*    	*Specify a new outcome group if its the same patient_deid and*/
/*    		the new row is >7 days since the rest.;*/
    	if lid=patient_deid and days_prevenc> &ENCGAP. then do;
           outcomegrp+1;
		   /*New start date for the pregnancy outcome group*/
           StartDt = enc_date;
           Enddt = enc_date;
           Days_PrevGroup = days_prevenc; 
        end;

        retain startdt enddt outcomegrp;

        format startdt enddt date.;

    run;

/*	Check*/
/*	proc freq data=outcomesbyGroup_&encgap. ;*/
/*		where outcome in ('SAB' 'IAB');*/
/*		tables rx;*/
/*	run;*/


/*                *checks*;*/
/*                *Look at the number of distinct encounter dates associated with one outcome group;*/
/*                proc sql;*/
/*                	create table dtcnt as */
/*                	select patient_deid,outcomegrp, count(distinct enc_date) as dates*/
/*                	from outcomesbyGroup_&encgap. */
/*                	group by patient_deid,outcomegrp;*/
/*                	quit;*spot check agrees with DateCount byhand;*/
/*                proc freq data=dtcnt ;table dates;run;*/
/*                * still 88% with 1, 9% with 2. Max is 37;*/

/*                *Output in the pregnancy outcomes document.;*/
/*                proc means data=dtcnt min median max mean;*/
/*                var dates; */
/*                run;*/

	/*Summarize information on the pregnancy outcome group level. Previous dataset on outcome level.*/
    proc sql; 
     create table outcomesbyGroup2_&encgap. as
      Select grp.*, types.* 
      from 

		/*Create some new variables in the outcomesbygroup_encgap dataset*/
      	/** first subquery counts at the group level (1+ encounters)**/
       (select distinct patient_deid, outcomegrp, min(startdt) as Dt_OutcomeGroup_Start, 
				max(enddt) as Dt_OutcomeGroup_End, 
             	count(distinct enc_key) as OutcomeGroup_Encounters_N,
             	count(distinct enc_date) as OutcomeGroup_Dates_N, 
             	count(distinct outcome) as OutcomeGroup_Outcomes_N ,  /*CDL: Change. Commented out the next line and retained this var*/
/*             	count(distinct Case when dxprrx ne '001' then Outcome end ) as OutcomeGroup_Outcomes_N ,  */
             	count(distinct Case when dxprrx not in ('000' '003' '002' '001') then Outcome end ) as OutcomeGroup_OutcomesNoRX_N ,  
              	calculated dt_outcomegroup_end - calculated dt_outcomegroup_start +1 as OutcomeGroup_Span ,
/*              COUNT(distinct case when DelivPR_Inp then enc_key end) as OutcomeGroup_InpPrEncounters_N ,*/
              	COUNT(distinct case when DelivPR_Inp then enc_key end) as OutcomeGroup_DelivPRInpatEnc_N ,
              	COUNT(distinct case when DelivPR_Out then enc_key end) as OutcomeGroup_DelivPROutpatEnc_N ,
              	COUNT(distinct case when DelivPR_Em then enc_key end) as OutcomeGroup_DelivPREmergEnc_N ,
             	max(days_prevgroup) as Days_PrevOutcomeGroup
        from outcomesbyGroup_&encgap. 
		group by patient_deid, outcomegrp ) As Grp


      Left join

	  /*Summarize the Dx Pr and Rx information for each outcome within the pregnancy outcome groups*/
      /** 2nd subquery counts at the outcome w/in group (1+ rows for same outcometype combined)**/
       (select distinct patient_deid, outcomegrp, outcome, 
             /*outcome,*/max(dx) as Dx, max(pr) as PR,  /*CDL: Commented out 2nd outcome*/
			 /*CDL: MAX(RX) does not give the desired output. Specifically, if they had both on 
			 different encounters (observed), then we would only see 2 not 3, as would be correct*/
/*			max(rx) as Rx, */
			case
				when count(distinct (case when rx=0 then . else rx end)) > 1 then 3 else max(rx) end as Rx,
/*             cats(max(dx),max(pr),max(rx)) as DxPrRx*/
			 cats(max(dx), max(pr), case when count(distinct (case when rx=0 then . else rx end)) > 1 then 3 else max(rx) end) as DxPrRx
        from outcomesbyGroup_&encgap. group by patient_deid, outcomegrp, outcome) As Types

      On Grp.patient_deid = Types.Patient_deid and Grp.outcomegrp = Types.outcomegrp
    ;
/*    run;*/
    quit;

	/*COUNT (DISTINCT 
            (CASE WHEN C=1 THEN C_ID1 ELSE ' ' END))*/


    *single obs per outcomegrp with outcome as column having value of codetype;
    proc transpose data=outcomesbyGroup2_&encgap. out=OutcomeGroupsRow_&encgap.;
     	by patient_deid outcomegrp dt_outcomegroup: outcomegroup_: Days_PrevOUtcomeGroup;
      	var dxprrx;
       	id outcome;
    run;



    /*  
     1 a.  For each of these pregnancy outcome groups, we will create flags  to indicate:
            1.	A misoprostol order;
            2.	A mifepristone order  ;
            3.	An inpatient encounter during the pregnancy outcome group;
            4.	An outpatient encounter during the pregnancy outcome group; and
            5.	An emergency visit during the pregnancy outcome group.
     */


    *create final base file with outcome groups, type of outcome and code defining outcome;
    proc sql;
     create table OutcomeGroup_addl_&encgap. as
      select distinct a.patient_deid, outcomegrp,  Dt_OutcomeGroup_Start,Dt_OutcomeGroup_End,
      OutcomeGroup_Encounters_N, OutcomeGroup_Dates_N, OutcomeGroup_Outcomes_N ,
      OutcomeGroup_Outcomesnorx_N ,Outcomegroup_span, 
      OutcomeGroup_DelivPRInpatEnc_N ,
      OutcomeGroup_DelivPROutpatEnc_N ,
      OutcomeGroup_DelivPREmergEnc_N ,
       days_Prevoutcomegroup,
             max(case when enc_base_class='Inpatient' then 1 else 0 end) as Inpatient length=3,
             max(case when enc_base_class='Outpatient' then 1 else 0 end) as Outpatient length=3,
             max(case when enc_base_class='Emergency' then 1 else 0 end) as Emergency length=3,
             max(case when code='mifep' then 1 else 0 end) as Mifepristone length=3,
             max(case when code='misop' then 1 else 0 end) as Misoprostol length=3,
             lbm length=3,lbs length=3, mls length=3, sb length=3, udl length=3,
             sab length=3, iab length=3, uab length=3, em length=3 , aem length=3
              
      from outcomegroupsrow_&encgap. a 
	  join codeoutcome b 
	  on a.patient_deid = b.patient_deid
      where b.enc_date between Dt_outcomegroup_start  and Dt_outcomegroup_end
      group by a.patient_deid, a.outcomegrp, Dt_outcomegroup_start, Dt_outcomegroup_end
    ;
    quit;
/*    proc contents data=outcomegroup_addl_&encgap. varnum;*/
/*    run;*/


    ** now make the base outcomeGroups file;;
    data OutcomeGroups_&encgap. ; 
    set outcomegroup_addl_&encgap. ;


	/*START CREATE FLAGS FOR DELIVERY, ABORTION, OR ECTOPIC/MOLAR INFORMATION -- 
	this is overarching information. in other words, not specified at the finer-grained outcome level*/

     array xo(*) SAB  UAB IAB  EM AEM  LBM SB LBS  UDL  MLS;
      
	 /*Indicator variables for delivery dx, pr, rx; abortion, dx, pr, rx; ectopic molar dx, pr, rx*/
     array tmp(*) deld delp delr abod abop abor ecod ecor ecop ; 

      do z=1 to dim(tmp); tmp(z)=0; end; /*Make all start at 0*/

      do i=1 to dim(xo);

	  /*Make sure missing has 000*/
       if missing(xo(i)) then xo(i)='000'; /*outcome not in group*/

       else do;
         if vname(xo(i)) in ('LBM','LBS', 'MLS', 'SB', 'UDL') then do;
		 /*Make indicator variables for delivery diagnosis and procedure codes - overarching*/
           if deld=0 then Deld= put(xo(i),$outcddx.);; 
           if delp=0 then Delp= put(xo(i),$outcdpr.) ;
           *delR not needed - Rx only for UAB or concordant abortion outcomes -- CDL;
         end; 
         else if vname(xo(i)) in ('SAB','IAB','UAB') then do;  
		 /*Make indicator variables for abortion dx, pr, rx - overarching*/
            if aboD=0 then Abod= put(xo(i),$outcddx.); 
            if aboP=0 then Abop= put(xo(i),$outcdpr. ) ; 
            if aboR=0 then Abor= put(xo(i),$outcdrxB.);  *USE ALT FMT TO CAPTURE 1,2,3;
         end;
         else if vname(xo(i)) in ('EM' 'AEM') then do; ;
		 /*Make indicator variables for ecoptic molar dx or pr codes - overarching*/
            if ecod=0 then Ecod= put(xo(i),$outcddx.); 
            if ecoP=0 then Ecop= put(xo(i),$outcdpr.); 
         end; 
       end;
      end; 

      *drop i deld--ecor;
      length Delivery Abortion Ectopic $3.;
         Delivery = cats(deld,delp,delr);
         Abortion = cats(abod,abop,abor); *aha not working with 2,3 for abortion ;
         Ectopic = cats(ecod,ecop,ecor);

	/*STOP CREATE FLAGS FOR DELIVERY, ABORTION, OR ECTOPIC/MOLAR INFORMATION*/



	/*START CREATE COUNT VARIABLES FOR OUTCOME INFORMATION that going to summarize with*/

	/*Counts overall for pr, dx, and rx info*/
      pr_n=0; dx_n=0; rx_n=0;

      *counts by outcome type;
      Delivery_pr_n=0; Delivery_dx_n=0;  
      Abortion_pr_n=0; Abortion_dx_n=0; Abortion_rx_n=0;
      Ectopic_pr_n=0; Ectopic_dx_n=0;  

         *count number outcomes found using procedures/diagnosis (for discordant);
         array x(*) LBM LBS MLS SB UDL  SAB IAB UAB AEM EM ;*06.01 no AEM;
          do i=1 to 10;
           
		  /*Get initial value for LBM (first column in array)*/
            tdx= substr(x(i),1,1);
            tpr= substr(x(i),2,1);
            trx= substr(x(i),3,1);

		  /*Add 1 to counts if first column has that piece of information*/
			*9.20 use substr to fix error of not counting all rx+dx/pr with change in rx structure;
            if tdx='1' then dx_n+1;
            if tpr='1' then pr_n+1;
            if trx in ('1','2','3') then rx_n+1;

			/*Columns 1-5 are delivery outcomes*/
            if 1 le i le 5 then do;
                if tdx='1' then Delivery_dx_n+1;
                if tpr='1' then Delivery_pr_n+1;
                if trx in ('1','2','3') then Delivery_rx_n+1; /*Should be 0*/
             end;
			/*Columns 6-8 are abortion outcomes*/
            else if i le 8 then do; 
                if tdx='1' then Abortion_dx_n+1;
                if tpr='1' then Abortion_pr_n+1;
                if trx in ('1','2','3') then Abortion_rx_n+1;
             end;
			/*Columns 9 and 10 are abortion, ectopic, or molar and ectopic or molar*/
            else do;
                if tdx='1' then Ectopic_dx_n+1;
                if tpr='1' then Ectopic_pr_n+1;
                if trx in ('1','2','3') then Ectopic_rx_n+1;
             end;
          end;

	 /*Only want to retain those count variables that we created*/
     drop i z deld--ecop  tdx tpr trx;

	/*STOP CREATE COUNT VARIABLES FOR OUTCOME INFORMATION*/



	 /*START APPLY FORMATS AND LABELS*/

     format dt_: mmddyy10.;
     label
      outcomegrp ="&encgap.-Day Outcome Group #"
      Dt_OutcomeGroup_Start ="Date 1st encounter in &encgap.-Day outcome group"
      Dt_OutcomeGroup_End= "Date of last encounter in &encgap.-Day outcome group"
      OutcomeGroup_Encounters_N= "Number of encounters (enc_key) in outcome group"
      OutcomeGroup_Dates_N ="Number of dates (enc_date) in outcome group"
      OutcomeGroup_Outcomes_N ="Number of outcome types in outcome group"
	  /*CDL: Modified rx labels*/
/*      SAB= 'Spontaneous Abortion (100=dx, 010=pr, 001=rx)'*/
	  SAB= 'Spontaneous Abortion (100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)'
      LBS='Live Birth (Singleton or Uncategorized) (100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)'
      UDL='Uncategorized Delivery (100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)'
      UAB='Unspecified Abortion (100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)'
      EM='Ectopic/Molar (100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)'
      LBM ='Live Birth (Multiple) (100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)'
      IAB='Induced Abortion (100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)'
      SB='Stillbirth (100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)'
      AEM='Abortion, Ectopic, or Molar (100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)'
      MLS='Mixed Delivery (100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)'
	  /*CDL: Modified labels*/
/*      Inpatient="Inpatient base class for encounters in group"*/
/*      Outpatient="Outpatient base class for encounters in group"*/
/*      Emergency='Emergency base class for encounters in group'*/
/*      Mifepristone="Mifepristone order for encounters in group"*/
/*      Misoprostol="Misoprostol order for encounters in group"*/
	  Inpatient="At least one inpatient encounter in group"
      Outpatient="At least one outpatient encounter in group"
      Emergency='At least one emergency encounter in group'
      Mifepristone="At least one mifepristone order in group"
      Misoprostol="At least one misoprostol order in group"

      /*added to dsn later*/ /*CDL added to label*/
      Delivery = "Delivery outcome (LBM,LBS,SB,MLS,UBL) (100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)"
      Abortion = "Abortion outcome (SAB,IAB,UAB)(100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)"
      Ectopic = "Ectopic/Molar Abortion (EM, AEM) (100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)"

      Delivery_PR_n ="Delivery procedures (n) (LBM,LBS,SB,MLS,UBL)"
      Abortion_PR_n ="Abortion procedures (n)(SAB,IAB,UAB)"
      Ectopic_PR_n ="Ectopic/Molar procedures (n) (EM, AEM)"

      Outcomegroup_span="Total Days in group (incl start and end dates)"
      Days_prevoutcomegroup = "Days from previous outcome group when >1" 
      OutcomeGroup_OutcomesNoRX_N ="Number of outcome types in outcome group excluding RX only (001,002,003)"

      dx_n='(n) outcomes based on diagnosis in group'
      rx_n='(n) outcomes based on med orders in group'
      pr_n='(n) outcomes based on procedures in group'
      abortion_rx_n='(n) abortion outcomes based on med orders in group'
      abortion_dx_n='(n) abortion outcomes based on diagnosis in group'
      abortion_pr_n='(n) abortion outcomes based on procedures in group'
      ectopic_dx_n='(n) ectopic outcomes based on diagnosis in group'
      ectopic_pr_n='(n) ectopic outcomes based on procedures in group'
	  ectopic_rx_n='(n) ectopic outcomes based on med orders in group' /*CDL: Added. Should be 0*/
      delivery_dx_n='(n) delivery outcomes based on diagnosis in group'
      delivery_pr_n='(n) delivery outcomes based on procedures in group'
	  delivery_rx_n='(n) delivery outcomes based on med orders in group' /*CDL: Added. Should be 0*/
      outcomegroup_delivprEmergEnc_n = 'delivery procedure on Emergency encounter in group'
      outcomegroup_delivprInpatEnc_n = 'delivery procedure on Inpatient encounter in group'
      outcomegroup_delivprOutpatEnc_n = '(n) delivery procedure on Outpatient encounter in group'
     ;

	 /*STOP APPLY FORMATS AND LABELS*/

     run;



/*	 Checks*/
/*     proc freq data=outcomegroups_7;table dx_n*delivery_dx_n*abortion_dx_n*ectopic_dx_n/list missing;run;*good;*/
/*     proc freq data=outcomegroups_7;table delivery*delivery_dx_n*delivery_pr_n /list missing;run;*good;*/
/*     proc freq data=outcomegroups_7;table outcomegroup_delivprinpatEnc_n*outcomegroup_delivproutpatEnc_n*outcomegroup_delivpremergEnc_n*delivery_pr_n /list missing;run;*/
/*     data huh;set outcomegroups_7;where dx_n NE sum(delivery_dx_n,abortion_dx_n,ectopic_dx_n) ;*0;*/
/*     data huh;set outcomegroups_7;where pr_n NE sum(delivery_pr_n,abortion_pr_n,ectopic_pr_n) ;*0;*/
/*     run;*/
/*	   proc freq data=outcomegroups_7; table abortion_rx_n delivery_rx_n ectopic_rx_n; run;*/

/*     proc freq data=outcomegroups_&encgap. ;*/
/*      	table delivery*lbm*lbs*sb*mls*udl /list missing;*/
/*      	table abortion*sab*iab*uab/list missing;*/
/*      	table ectopic*em*aem/list missing;*/
/*        table delivery*outcomegroup_deliv: /list;*/
/*     run;*looks ok;*/
/*     proc freq data=outcomegroups_&encgap. ;*/
/*     	table delivery*delivery_pr_n*delivery_dx_n /list missing;*/
/*      	table delivery_pr_n*lbm*lbs*sb*mls*udl /list missing;*/
/*      	table delivery_dx_n*lbm*lbs*sb*mls*udl /list missing;*/
/*     run;*looks good too;*/

%MEND;


