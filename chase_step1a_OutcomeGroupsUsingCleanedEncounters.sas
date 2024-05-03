/******************************************************************************

Program: chase_step1a_OutcomeGroupsUsingCleanedEncounters.sas;

Programmer: Sharon 

Purpose: Preps the input file (codeoutcome to outcomesbytype) and loads macro 
		OutcGrp to create outcome groups. Final output dataset is 
		OutcomeGroups_&EncGap (work).
   		This file also outputs
		OutcomeGroups_Within&encgap.Days_%sysfunc(date(),yymmddn8.).rtf (outpath).

*******************************************************************************/







/*****************************************************************************

TABLE OF CONTENTS:

	- 00 - PREP PREGNANCY OUTCOME DATA
	- 01 - STEP aa - Group all pregnancy outcome encounters +/- [EncGAP] days 

******************************************************************************/







/*****************************************************************************

						00 - PREP PREGNANCY OUTCOME DATA

We want to create a dataset of the pregnancy outcomes with information on
the types of codes/orders used to define that outcome.

******************************************************************************/


**are encounters concordant or not? ;
**copy outcome groups concordant and use on encounters;

        *from 1a;

*Select distinct encounters, associated outcomes, and whether they
are defined by diagnosis codes, medication orders, or procedure codes.

This creates a dataset of outcomes by type of information defining that outcome;
proc sql;
  	create table outcomebytype as
  	select distinct patient_deid,enc_key,enc_base_class, enc_date, put(outcome,$outabbr.) as Outcome, 
		case when codetype in ('dx9' 'dx10') then 'DX'
             when codetype in ('med') and code='misop' then 'RXa' 
             when codetype in ('med') and code='mifep' then 'RXb' 
             else 'PR' end as DXPRRX,  
/*		case when codetype in ('dx9' 'dx10') then 'DX'*/
/*             when codetype in ('med') then 'RX' */
/*             else 'PR' end as DXPRRX,  */
		1 as types  
  	from  codeoutcome
  	group by patient_deid, enc_key, enc_date, calculated outcome, calculated dxprRX ; /*enc_date should be redundant*/
	quit;


/*proc freq data=codeoutcome;table code; where codetype='med';run;*/
/*                             Cumulative    Cumulative
rx    Frequency     Percent     Frequency      Percent
ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ
 0      346992       90.40        346992        90.40
 1       35879        9.35        382871        99.74
 2         267        0.07        383138        99.81
 3         719        0.19        383857       100.00
*/

*Look at who has multiple RXs;
/*proc sql;*/
/*	create table multrx as select **/
/* 	from outcomebytype where dxprrx like 'RX%' group by enc_key having count(*)>1*/
/*	;*/
/*	quit;*/
	*1438 ;

*Transpose this dataset ;
proc transpose data=outcomebytype out=outcomesbytype;
	by patient_deid enc_key enc_base_class enc_date outcome;
	id dxprrx; 
	var types;
run;













/*****************************************************************************

	01 - STEP aa - Group all pregnancy outcome encounters +/- [EncGAP] days 

******************************************************************************/



*Create dataset with the grouped outcomes;
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

	*Concatonate these numeric variable to make the 3-value indicator variable
		from the FORMATS (step 00) file.;
    length DXPRRX $3.;
    DXPRRX = cat(dx,pr,rx);

	*Create a flag for an inpatient delivery procedure code;
    DelivPR_Inp = (enc_base_Class='Inpatient' and PR>0 AND Outcome in ('LBM','LBS','MLS','SB','UDL')) ;

    **09.13 - Created flags for inpatient delivery procedures for all encounter settings;
    DelivPR_Out = (enc_base_Class='Outpatient' and PR>0 AND Outcome in ('LBM','LBS','MLS','SB','UDL')) ;
    DelivPR_Em = (enc_base_Class='Emergency' and PR>0 AND Outcome in ('LBM','LBS','MLS','SB','UDL')) ;

run;

/*proc freq data=codeoutcome;table outcome*codetype/list;where missing(enc_base_class);*/
/*run;*/

*Check - each encounter only has 1 date;
/*proc sql;*/
/*    create table multdays as */
/*	select enc_key,count(distinct enc_date) as dates */
/*	from outcomesbytype2 */
/*	group by enc_key;*/
/*	quit;*/
/*proc freq;*/
/*	table dates;*/
/*run;*/
*Yes, 1 date per encounter key. Later figure out how this impacts inpatient data;
*note looks like all we have is enc_start_instant and enc_date;
    
    proc sort data=outcomesbytype2; by patient_deid enc_key;
    run;
    proc sql;
     create table outcomesbytype2b as
      select patient_deid,enc_key,enc_date,enc_base_class, outcome,dxprrx,max(DelivPR_Inp) as DelivPR_Inp
                ,max(DelivPR_Out) as DelivPR_Out,max(DelivPR_EM) as DelivPR_EM
      from outcomesbytype2 group by patient_deid,enc_key,enc_date,enc_base_class;
    quit;

        *single obs per ENCOUNTER with outcome as column having value of codetype;
    proc transpose data=outcomesbytype2b out=EncOut   ;
     by patient_deid ENC_KEY enc_base_class ENC_DATE DelivPR_Inp DelivPR_Out DelivPR_EM;
      var dxprrx;
       id outcome;
    run;

    proc freq;table delivpr_inp*delivpr_out*delivpr_EM/list missing;
    /*
    
                                                                     Cumulative    Cumulative
DelivPR_Inp    DelivPR_Out    DelivPR_EM    Frequency     Percent     Frequency      Percent
ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ
          0              0             0      113211       49.36        113211        49.36
          0              0             1          77        0.03        113288        49.40
          0              1             0        3791        1.65        117079        51.05
          1              0             0      112267       48.95        229346       100.00

**rerun 9.21 with updated ref files - some codes removed i believe so drop in outcomes ok;
                                                                    Cumulative    Cumulative
DelivPR_Inp    DelivPR_Out    DelivPR_EM    Frequency     Percent     Frequency      Percent
ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ
          0              0             0      107347       48.15        107347        48.15
          0              0             1          45        0.02        107392        48.17
          0              1             0        3347        1.50        110739        49.67
          1              0             0      112208       50.33        222947       100.00
*/
    run;
    proc freq data=encout  noprint;table patient_deid*enc_key/out=x;
    proc freq;table count;run;*yes! 1 row per encounter;

    *clean up for next step;
     data encoutcome;
      set EncOut ;
      
         array x(*) LBM LBS MLS SB UDL  SAB IAB UAB  AEM EM ; 
         array ox(*) $3. o_LBM o_LBS o_MLS o_SB o_UDL  o_SAB o_IAB o_UAB  o_AEM o_EM  ; 
         *set missing to No;

         *start with count = total possible outcomes then -1 if outcome not present;
         *then capture the original outcome code type in new var;
         EncOutcomeCount=dim(x);
          do i=1 to dim(x); 
            if missing(x(i)) then do; encoutcomecount=encoutcomecount-1; x(i)='000';end;
            ox(i)= x(i) ;
          end;

        *this time count all outcome types with a code found, excl UAB-RX-only;
        EncOutcomes_NoRX=0;
          do i=1 to dim(x); 
           if x(i)>'000' and vname(x(i)) ne 'UAB' then encoutcomes_norx+1; 
           Else if vname(x(i))='UAB' And x(i) not in ('000' '001' '002' '003') then encoutcomes_norx+1;
          end;

        drop i _name_;
    run;
    proc freq;table udl*o_udl uab*o_uab ;run;*check, looks ok;
    proc freq;table uab*encoutcomecount*encoutcomes_norx/list missing;run;*checks ok;

    *so now check for concordant outcomes on Encounters - copy from step1a_outcomegroups;

 *table 1 concordance with step to assign codetype for the concordant pair;
 *Ignoring medication orders for concordance so using outcomecounts excl Rx;
  %macro encconc(o1,o2); *table 1 concordance when 2 outcome groups or 3 if 3rd is UAB-001;

    if (EncOutcomes_NORX = 2 And &o1 ne '000' and &o2 ne '000' 
        %IF &o2 = UAB %then %do; And UAB not in ('001' '002' '003' ) %END;)
    then do;

      Concordant=1; Encounter_Concordant=upcase("&o1"); 
      Encounter_Concordant_CodeType = 
        cat(max(substr(&o1,1,1),substr(&o2,1,1)),max(substr(&o1,2,1),substr(&o2,2,1)),max(substr(&o1,3,1),substr(&o2,3,1)));

      *reset the outcome values based on info from the concordant pair (1=concordant, 2=mising);
        &o1= encounter_concordant_codetype;
        &o2= '000'; 
    end;
  %mend;

  option mprint symbolgen;

*macro to call in separate algorithm pgms to split data into outcome concordant and discordant;
%Macro CleanEnc ;

    *for all define concordant outcomes ;;
    data EncOutcomeClean  ;
     set EncOUtcome ;  

         array x(*) LBM LBS MLS SB UDL  SAB IAB UAB  AEM EM ; 

         *actually dont need outcome concordant, just want to reset outcome types;

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
%mend;
%CleanEnc ;

proc freq data= EncOUtcomeClean;
 table concordant lbm lbs mls sb udl sab iab uab em aem;
 table udl o_udl lbs o_lbs;
run;

*better - from 70% with UDL (not 000) down to 20% of encounters with UDL;
*now 48% lbs procedure based, originally should be 0%;

** why o_lbs with proc?;
/*data huh;set conc_encoutcomedisc;where o_lbs=110;run;*/
/*Z1172980 Z2233635 Z5031355 */

/*data huh;set encout;where patient_deid='Z5031355';run;*yep 110;*/
/*data huh;set outcomesbytype2;where patient_deid='Z5031355';run;*yep 110 ;*/
/*data huh; set codeoutcome;where enc_key=135524443;*/
/*keep code: description outcome usage_unclear;*/
/*run;*cpt code for live birth (uncategorized) => lbs;*/
/**so yes, can have procedure without UDL for LbS;*/


**** NOW take this Cleaned Encounter file and make the outcome groups;
**main advantageof this extra step is to make cleaner encounter groups for date assignment;
**final outcome group assessment should be the same;

*put back into original form - mainly to keep adjustmetns to previous version minimal;
proc sort data= EncOUtcomeClean;by patient_deid enc_key enc_date;
run;
proc transpose data=encoutcomeclean
                out= encoutcomecleanrows (rename=(_name_=OUTCOME col1=DXPRRX));
 by patient_deid enc_key enc_date delivpr: ;
  var sab--mls; 
run;


%MACRO OUTCGRP(encgap);

    proc sort data= EncOUtcomeCleanRows ;by patient_deid enc_date;
    run;

    data outcomesbyGroup_&encgap. ; *outcomesbytype2_&encgap.;
     set EncOutcomeCleanRows  ;
    	by patient_deid enc_date;

        DX=input(substr(dxprrx,1,1), 8.);
        PR=input(substr(dxprrx,2,1), 8.);
        RX=input(substr(dxprrx,3,1), 8.);

        *9.20drop dxprrx - dont need anymore as cleaned value now reflected in dx pr rx above;
        *9.21 nope too much trouble; *drop dxprrx;

    	*Identify the enc_date in the previous row - 1st row will have a 
    		missing value for this.;
    	ldt=lag1(enc_date);
    	*Identify the patient_deid in the previous row.;
    	lid=lag1(patient_deid);

    	*Group all the codes that are within an updating 7 days of each other
    	inton one outcome group;
    	if first.patient_deid then do; *Start at the first patient_deid;
    	  outcomegrp=1; 
    	  days_prevenc=.; *Days since previous encounter;
          StartDt = enc_date;
          EndDt = enc_date;
           *EncounterCount=1;
           *DateCount=1; *cant do here this way because mult rows per date and encounter;
    	end;
    	else do;
          days_prevEnc = enc_date - ldt;
          EndDt = enc_date;
        end;

    	*Specify a new outcome groiup if its the same patient_deid and
    		the new row is >7 days since the rest.;
    	if lid=patient_deid and days_prevenc> &ENCGAP. then do;
           outcomegrp+1;
           StartDt = enc_date;
           Enddt = enc_date;
           Days_PrevGroup = days_prevenc;  *6.01 add so can track time between new groups;
        end;

        retain startdt enddt outcomegrp;

        format startdt enddt date.;

    run;
                *checks*;
                *Look at the number of distinct encounter dates associated with one outcome group;
                proc sql;
                	create table dtcnt as 
                	select patient_deid,outcomegrp, count(distinct enc_date) as dates
                	from outcomesbyGroup_&encgap. 
                	group by patient_deid,outcomegrp;
                	quit;*spot check agrees with DateCount byhand;
                proc freq data=dtcnt ;table dates;run;
                * still 87%with 1, 10% with 2;

                *Sharon output this in the pregnancy outcomes document.;
                proc means data=dtcnt min median max mean;
                var dates; 
                run;

     *06.01 add additonal info day count per MW and time between groups;
     *06.01 also get count of outcomes when excluding rx;
                *09.07 add DelivPR_Inp coutn;
                *9.20 add delivpr outp, emergency and remove dxprrx ref - NOPE leave as is;
    proc sql; *find dates for each outcome group and codetype for each outcome;
     create table outcomesbyGroup2_&encgap. as
      Select grp.*, types.* 
      from 

      /*** 09.22 CHANGE - only count encounters with dx/pr/rx (no 000) ***/

      /** first subquery counts at the group level (1+ encounters)**/
       (select distinct patient_deid,outcomegrp,min(startdt) as Dt_OutcomeGroup_Start,max(enddt) as Dt_OutcomeGroup_End, 
             count(distinct enc_key) as OutcomeGroup_Encounters_N,
             count(distinct enc_date) as OutcomeGroup_Dates_N, 
/*             count(distinct outcome) as OutcomeGroup_Outcomes_N ,  */
             count(distinct Case when dxprrx ne '001' then Outcome end ) as OutcomeGroup_Outcomes_N ,  
             count(distinct Case when dxprrx not in ('000' '003' '002' '001') then Outcome end ) as OutcomeGroup_OutcomesNoRX_N ,  
              calculated dt_outcomegroup_end - calculated dt_outcomegroup_start +1 as OutcomeGroup_Span ,
/*              COUNT(distinct case when DelivPR_Inp then enc_key end) as OutcomeGroup_InpPrEncounters_N ,*/
              COUNT(distinct case when DelivPR_Inp then enc_key end) as OutcomeGroup_DelivPRInpatEnc_N ,
              COUNT(distinct case when DelivPR_Out then enc_key end) as OutcomeGroup_DelivPROutpatEnc_N ,
              COUNT(distinct case when DelivPR_Em then enc_key end) as OutcomeGroup_DelivPREmergEnc_N ,
             max(days_prevgroup) as Days_PrevOutcomeGroup
        from outcomesbyGroup_&encgap. group by patient_deid, outcomegrp ) As Grp

      Left join

      /** 2nd subquery counts at the outcome w/in group (1+ rows for same outcometype combined)**/
       (select distinct patient_deid, outcomegrp, outcome, 
             outcome,max(dx) as Dx, max(pr) as PR, max(rx) as Rx, 
             cats(max(dx),max(pr),max(rx)) as DxPrRx
        from outcomesbyGroup_&encgap. group by patient_deid, outcomegrp, outcome) As Types

      On Grp.patient_deid = Types.Patient_deid and Grp.outcomegrp = Types.outcomegrp
    ;
    run;
    quit;

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
    *correction to misop/mifep, 5.31.23;
    *change OutcomeGroup_InpPrEncounters_N to OutcomeGroup_DelivPRInpatEnc_N;
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
              
      from outcomegroupsrow_&encgap. a join codeoutcome b on a.patient_deid = b.patient_deid
      where b.enc_date between Dt_outcomegroup_start  and Dt_outcomegroup_end
      group by a.patient_deid,a.outcomegrp,  Dt_outcomegroup_start, Dt_outcomegroup_end
    ;
    quit;
    proc contents data=outcomegroup_addl_&encgap. varnum;
    run;

    ** now make the base outcomeGroups file;;
    Data OutcomeGroups_&encgap. ; 
     set outcomegroup_addl_&encgap. ;

     array xo(*) SAB  UAB IAB  EM AEM  LBM SB LBS  UDL  MLS;
      
     array tmp(*) deld delp delr abod abop abor ecod ecor ecop ;
      do z=1 to dim(tmp); tmp(z)=0; end; 
      do i=1 to dim(xo);
       if missing(xo(i)) then xo(i)='000'; *outcome not in group;
       else do;
         if vname(xo(i)) in ('LBM','LBS', 'MLS', 'SB', 'UDL') then do;
           if deld=0 then Deld= put(xo(i),$outcddx.);; 
           if delp=0 then Delp= put(xo(i),$outcdpr.) ;
           *delR not needed - Rx only for UAB;
         end; 
         else if vname(xo(i)) in ('SAB','IAB','UAB') then do;  
            if aboD=0 then Abod= put(xo(i),$outcddx.); 
            if aboP=0 then Abop= put(xo(i),$outcdpr. ) ; 
            if aboR=0 then Abor= put(xo(i),$outcdrxB.);  *USE ALT FMT TO CAPTURE 1,2,3;
         end;
         else if vname(xo(i)) in ('EM' 'AEM') then do; ;
            if ecod=0 then Ecod= put(xo(i),$outcddx.); 
            if ecoP=0 then Ecop= put(xo(i),$outcdpr.) ; 
         end; 
       end;
      end; 
      *drop i deld--ecor;
      length Delivery Abortion Ectopic $3.;
         Delivery = cats(deld,delp,delr);
         Abortion = cats(abod,abop,abor); *aha not working with 2,3 for abortion ;
         Ectopic = cats(ecod,ecop,ecor);

      *6.15 moved this from concordant datastep;
      pr_n=0; dx_n=0; rx_n=0;

      *add 06.14 counts by outcome type per MW - moved from pgm 1b 6.15;
      Delivery_pr_n=0; Delivery_dx_n=0;  
      Abortion_pr_n=0; Abortion_dx_n=0; Abortion_rx_n=0;
      Ectopic_pr_n=0; Ectopic_dx_n=0;  

         *count number outcomes found using procedures/diagnosis (for discordant);
         array x(*) LBM LBS MLS SB UDL  SAB IAB UAB  AEM EM ;*06.01 no AEM;
          do i=1 to 10;
/*           if x(i) in ( '010','011','110','111') then pr_n+1;*/
/*           if x(i) in ('100','110','101','111') then dx_n+1;*/
/*           if x(i) in ('001','101','011','111') then rx_n+1;*/
           
            tdx= substr(x(i),1,1);
            tpr= substr(x(i),2,1);
            trx= substr(x(i),3,1);

            if tdx='1' then dx_n+1;
            if tpr='1' then pr_n+1;
            if trx in ('1','2','3') then rx_n+1;

             *9.20 use substr to fix error of not counting all rx+dx/pr with change in rx structure;
            if 1 le i le 5 then do;
                if tdx='1' then Delivery_dx_n+1;
                if tpr='1' then Delivery_pr_n+1;
                if trx in ('1','2','3') then Delivery_rx_n+1;
/*               if x(i) in ( '010','011','110','111') then Delivery_pr_n+1;*/
/*               if x(i) in ( '012','112') then Delivery_pr_n+1;*/
/*               if x(i) in ( '013','113') then Delivery_pr_n+1;*/
/**/
/*               if x(i) in ('100','110','101','111') then Delivery_dx_n+1;*/
/*               if x(i) in ('102','112') then Delivery_dx_n+1;*/
/*               if x(i) in ('103','113') then Delivery_dx_n+1;*/
             end;
            else if i le 8 then do; 
                if tdx='1' then Abortion_dx_n+1;
                if tpr='1' then Abortion_pr_n+1;
                if trx in ('1','2','3') then Abortion_rx_n+1;
               
/*               if x(i) in ( '010','011','110','111') then Abortion_pr_n+1;*ugh also 012,013...;*/
/*               if x(i) in ('100','110','101','111') then Abortion_dx_n+1;*/
/*               if x(i) in ('001','101','011','111') then Abortion_rx_n+1;*/
/*               if x(i) in ('002','102','012','112') then Abortion_rx_n+1;*/
/*               if x(i) in ('003','103','013','113') then Abortion_rx_n+1;*/
             end;
            else do;
                if tdx='1' then Ectopic_dx_n+1;
                if tpr='1' then Ectopic_pr_n+1;
                if trx in ('1','2','3') then Ectopic_rx_n+1;
/*               if x(i) in ( '010','011','110','111') then Ectopic_pr_n+1;*/
/*               if x(i) in ( '012','112') then Ectopic_pr_n+1;*/
/*               if x(i) in ( '013','113') then Ectopic_pr_n+1;*/
/*               if x(i) in ('100','110','101','111') then Ectopic_dx_n+1;*/
/*               if x(i) in ('102','112') then Ectopic_dx_n+1;*/
/*               if x(i) in ('103','113') then Ectopic_dx_n+1;*/
             end;
          end;
     drop i z deld--ecop  tdx tpr trx;
     format dt_: mmddyy10.;
     label
      outcomegrp ="&encgap.-Day Outcome Group #"
      Dt_OutcomeGroup_Start ="Date 1st encounter in &encgap.-Day outcome group"
      Dt_OutcomeGroup_End= "Date of last encounter in &encgap.-Day outcome group"

      OutcomeGroup_Encounters_N= "Number of encounters (enc_key) in outcome group"
      OutcomeGroup_Dates_N ="Number of dates (enc_date) in outcome group"
      OutcomeGroup_Outcomes_N ="Number of outcome types in outcome group"
      SAB= 'Spontaneous Abortion (100=dx, 010=pr, 001=rx)'
      LBS='Live Birth (Singleton or Uncategorized) (100=dx, 010=pr, 001=rx)'
      UDL='Uncategorized Delivery (100=dx, 010=pr, 001=rx)'
      UAB='Unspecified Abortion (100=dx, 010=pr, 001=rx)'
      EM='Ectopic/Molar (100=dx, 010=pr, 001=rx)'
      LBM ='Live Birth (Multiple) (100=dx, 010=pr, 001=rx)'
      IAB='Induced Abortion (100=dx, 010=pr, 001=rx)'
      SB='Stillbirth (100=dx, 010=pr, 001=rx)'
      AEM='Abortion, Ectopic, or Molar (100=dx, 010=pr, 001=rx)'
      MLS='Mixed Delivery (100=dx, 010=pr, 001=rx)'
      Inpatient="Inpatient base class for encounters in group"
      Outpatient="Outpatient base class for encounters in group"
      Emergency='Emergency base class for encounters in group'
      Mifepristone="Mifepristone order for encounters in group"
      Misoprostol="Misoprostol order for encounters in group"

      /*added to dsn later*/
      Delivery ="Delivery outcome (LBM,LBS,SB,MLS,UBL)"
      Abortion ="Abortion outcome (SAB,IAB,UAB)"
      Ectopic ="Ectopic/Molar Abortion (EM, AEM)"

      Delivery_PR_n ="Delivery outcome (LBM,LBS,SB,MLS,UBL)"
      Abortion_PR_n ="Abortion outcome (SAB,IAB,UAB)"
      Ectopic_PR_n ="Ectopic/Molar Abortion procedures (n) (EM, AEM)"

      Outcomegroup_span="Total Days in group (incl start and end dates)"
      Days_prevoutcomegroup = "Days from previous outcome group when >1"
      OutcomeGroup_OutcomesNoRX_N ="Number of outcome types in outcome group excluding RX only (001)"

      dx_n='outcomes based on diagnosis in group'
      rx_n='outcomes based on med orders in group'
      pr_n='outcomes based on procedures in group'
      abortion_rx_n='abortion outcomes based on med orders in group'
      abortion_dx_n='abortion outcomes based on diagnosis in group'
      abortion_pr_n='abortion outcomes based on procedures in group'
      ectopic_dx_n='ectopic outcomes based on diagnosis in group'
      ectopic_pr_n='ectopic outcomes based on procedures in group'
      delivery_dx_n='delivery outcomes based on diagnosis in group'
      delivery_pr_n='delivery outcomes based on procedures in group'
      outcomegroup_delivprEmergEnc_n = 'delivery procedure on Emergency encounter in group'
      outcomegroup_delivprInpatEnc_n = 'delivery procedure on Inpatient encounter in group'
      outcomegroup_delivprOutpatEnc_n = 'delivery procedure on Outpatient encounter in group'
     ;

     run;

/*     proc freq data=outcomegroups_7;table dx_n*delivery_dx_n*abortion_dx_n*ectopic_dx_n/list missing;run;*good;*/
/*     proc freq data=outcomegroups_7;table delivery*delivery_dx_n*delivery_pr_n /list missing;run;*good;*/
/*     proc freq data=outcomegroups_7;table outcomegroup_delivprinpatEnc_n*outcomegroup_delivproutpatEnc_n*outcomegroup_delivpremergEnc_n*delivery_pr_n /list missing;run;*/
/*     data huh;set outcomegroups_7;where dx_n NE sum(delivery_dx_n,abortion_dx_n,ectopic_dx_n) ;*0;*/
/*     data huh;set outcomegroups_7;where pr_n NE sum(delivery_pr_n,abortion_pr_n,ectopic_pr_n) ;*0;*/
/*     run;*/


     proc freq data=outcomegroups_&encgap. ;
      table delivery*lbm*lbs*sb*mls*udl /list missing;
      table abortion*sab*iab*uab/list missing;
      table ectopic*em*aem/list missing;
        table delivery*outcomegroup_deliv: /list;
    run;*looks ok;
    proc freq data=outcomegroups_&encgap. ;
     table delivery*delivery_pr_n*delivery_dx_n /list missing;
      table delivery_pr_n*lbm*lbs*sb*mls*udl /list missing;
      table delivery_dx_n*lbm*lbs*sb*mls*udl /list missing;
    run;*looks good too;

%MEND;


