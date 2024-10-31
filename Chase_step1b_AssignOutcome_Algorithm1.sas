/******************************************************************************

Program: chase_Step1b_AssignOutcome_Algorithm1.sas

Programmer: Sharon 

Purpose: Assigns pregnancy outcomes to pregnancy outcome groups using the 
proposed methodology in Algorithm 1.

Modifications:
	- 04-29-24: Chase (CDL) added comments throughout and modified appearance.
		Suggested changes to how we are collecting the codetype information 
		for outcome1.
	- 05.2024: CDL conducted QC. SPH and CDL reviewed. All modifications were 
		reviewed and agreed upon by both.

*******************************************************************************/




*Output a new dataset for algorithm 1;

data &NewDsn._Alg1 ; *outcome solely based on hierarchy;
 set &concdsn (in=a)
     &discdsn (in=b)
	 /*Combine the pregnancy outcome groups that are concordant and discordant*/
 ;

 	*Create a flag for the discordant pregnancy outcome groups.;
	 *if a then discordant1=0;
	 Discordant1 = b;

	 *order using hierarchy - this is the same as table 1 for concordant outcomes so dont
	 	explicitly incorporate the concordance here;
	 array hier(*) LBM LBS MLS SB UDL SAB IAB UAB EM AEM;

	 *outcome assigned based on alg1 hierarchy ;
	 *table 1 concordance is IGNORED (no conc/disc) - per step 1b.3;
	 *so use the Delivery/Abortion/Ectopic for codetype 08.09;
	 Outcome_Assigned1='unk';

	 do i=1 to dim(hier) until (Outcome_assigned1 ne 'unk');
	   if hier(i) ne '000' then do; 
	     	Outcome_Assigned1= vname(hier(i));
	      	*Outcome_Assigned_Codetype1 = put(hier(i), $outcd.);

			*Now get all the information concordant with the final pregnancy outcome class;
	      	IF outcome_assigned1 in ('LBM' 'LBS' 'UDL' 'SB' 'MLS') then Outcome_Assigned_Codetype1= Delivery;
	      		ELSE IF outcome_assigned1 in ('SAB' 'IAB' 'UAB') then Outcome_Assigned_Codetype1= Abortion;
	      		ELSE IF outcome_assigned1 in ('EM' 'AEM') then Outcome_Assigned_Codetype1= Ectopic;
	   end;
	 end;

	 *Output the type of pregnancy outcome;
	 Outcome_Class_Assigned1 = put(outcome_assigned1,$outclass.);

	 drop i;

	 label
	   	outcome_assigned1 = "Outcome Assigned (Algorithm1)"
	   	outcome_assigned_codetype1="Outcome codetype (100=dx, 010=pr, 001=miso, 002=mife, 003=miso+mife)"
	   	discordant1 = "Discordant outcome flag (algorithm1)"
	    outcome_class_assigned1="Class of outcome (deliv/abort/ectopic)"
	 ;

run;


*Check if getting different output;
/*proc sort data=&NewDsn._Alg1; by outcome_assigned1; run;*/
/*proc freq data=&NewDsn._Alg1;*/
/*	by outcome_assigned1;*/
/*	tables outcome_assigned_codetype1*outcome_assigned_codetype1_mod;*/
/*run;*/
/*proc print data=&NewDsn._Alg1;*/
/*	where outcome_assigned1 = 'IAB' and outcome_assigned_codetype1 = '111' and */
/*		(outcome_assigned_codetype1_mod = '112' or outcome_assigned_codetype1_mod = '113');*/
/*run;*/
