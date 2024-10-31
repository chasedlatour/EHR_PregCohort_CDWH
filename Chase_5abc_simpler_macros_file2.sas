/***********************************************************************************************
PROGRAM: macro_5abc_simpler.sas		
PROGRAMMER: Sharon
 
PURPOSE: Apply Section 5 macros that cover 5a, 5b.1 to 5b.2d                           
         Check outcomes groups attached to index prenatal encounter episode (step 5) or   
         outcomegrps remaining after linking pregnancy with prenatal encounters (step 10) 
          -are they for same pregnancy? (using table 3 Time between outcome groups)        
          if same pregnancy, evaluate paird outcomegrps to assign Pregnancy Outcome       
           for each iteration use the results from previous pairing for comparison         
  
NOTES:
Note: 5a - 1 outcomegrp, 5b.1 - 2 outcomegrps, 5b.2c - 3+ outcomegrps                  
      method of comparing outcomegrp pair is the same.                                 
Note: mode pgm from chase_step5abc... - changes include
              revising varnames: PAIRcount to OUTCOMECOUNT PairsCompared to OUTCOMESCOMPARED
                      PAIREDoutcome to PREG_OUTCOME, DT_PAIREDoutcome to DT_PREGNANCYOUTCOME

MACROS coded in program:
	- CombineResultsFaux 
	- CombineResults 
	- PairEval

MODIFICATIONS:
	- 05.2024 - Chase modified formatting, etc.
	- 05.2024 - Chase modified PAIREVAL macro
	- 05.2025 - CDL conducted QC. SPH and CDL reviewed. All modifications were agreed upon
		by both.

************************************************************************************************/



*Specify options*;

 OPTIONS MINOPERATOR MINDELIMITER=',';           





*MACRO: CombeResultsFaux
 INPUT: rdp = start of the dataset name;

*setup datasets for PairEval results;
%Macro CombineResultsFaux (rdp=);
	
	/*Create a 0 row dataset of outcomes that need an index*/
    data &RDP._OutcomeNeedIndex_&NUM.;
    set &RDP._OutcomeNeedIndex_1 (obs=0);
    run;

	/*Create a 0 row dataset of deleted rows*/
    Data &RDP._deletedrows_&NUM.; *outcome groups included in preg outcome pair;
    set &RDP._deletedrows_1(obs=0);
    run;
 
	/*Merge the 0 row dataset of finished pregnancies with the indexprenataloutcomes dataset that we have 
	determined had no prenatal encounters link to a pregnancy outcome*/
    Data &RDP._Index_Outcome_Fin_&NUM.;
    set &RDP._Index_outcome_Fin_1 (obs=0) /*0 row dataset*/
          indexprenataloutcomes_&NUM. (keep = patient_deid idxpren prennum dt_indexprenatal outcomegroupsfound);

    	keep patient_deid idxpren dt_indexprenatal dt_pregnancyoutcome_start dt_pregnancyoutcome_end 
           PREG_Outcome PREG_CodeType /*pregnancyoutcomegroupsevaluated*/ outcomegrp OUTCOMEGROUPSFOUND
           PREG_OutcomeCount PREG_OutcomesCompared DT_PREGNANCYOUTCOME_start DT_PREGNANCYOUTCOME_end  DT_PREG_OUTCOME
           PRENNUM PREG_MISO PREG_MIFE  preg_outcomecodetypescompared
      	;
      	Format dt_: date. ;
    run;
    proc sort nodups Data= &RDP._index_outcome_fin_&NUM.; 
      by patient_deid dt_indexprenatal ;
    run;

	/*All of the pregnancy outcome groups in this stage are not assigned to any pregnancy */
    data &RDP._outidsfin_&NUM.;
    set &RDP._outidsfin_1; 
    	resultant_&NUM. = 'tbd';
			;
        Format dt_: date. ;
    run;
    proc sort nodups data=&RDP._outidsfin_&NUM.;
		by patient_deid outcomegrp;
	run;
       

/*    proc datasets lib=work nolist;*/
/*     delete %do i=1 %to &max.; r&i._:  %end; ipo_: ;*/
/*    quit;*/
%Mend;




/*MACRO: CombineResults()

PURPOSE: This is the last step of PAIREVAL prior to the lognote.
The goal of this is to combine the results from all of the rounds
analyses.

INPUT: None

**/
*instead of runnig separately including as last step in pair eval;;

%macro CombineResults;

	/*Identify all the pregnancy outcome groups that still need an indexing prenatal encounter*/
	Data &RDP._OutcomeNeedIndex_&NUM.;
  	set %do i=1 %to &max.; 
			r&i._D_NewPren (in=a) r&i._D_NewPren2 (in=b) 
		%end; ;
  		Format dt_: date. ;

  		%*only retain info re outcomegroup - other info irrelevant since not linked to this prenatal pregnancy;
  		if a then NeedPren='a'; 
		if b then Needpren='b'; *for tracking only;

		KEEP patient_deid outcomegrp dt_outcomegroup_start dt_outcomegroup_end prennum dt_indexprenatal needpren;
	run;
	proc sort Data=&RDP._OutcomeNeedIndex_&NUM.;
 		by patient_deid dt_indexprenatal dt_outcomegroup_start ;
	run;

	/*Identify all of the pregnancy outcome groups that were deleted becuase determind to be part of the same pregnancy as earlier outcome group(s)
	Not necessary - helpful for tracking*/
	Data &RDP._deletedrows_&NUM._base; %*outcome groups included in preg outcome pair;
  		set %do i=1 %to &max.; 
				r&i._Delete 
			%end; ;
  		Format dt_: date. ;
  		*since deleted row info re preg not captured on this outcomegrp rec (can find on Paired row);
  		KEEP patient_deid outcomegrp dt_outcomegroupstart dt_outcomegroupend idxpren dt_indexprenatal prennum;
	run;
	proc sort Data=&RDP._deletedrows_&NUM._base ;
 		by patient_deid dt_indexprenatal;
	run;

	/*Identify all the pregnancy outcome groups that are done being evaluated (i.e., all have pregnancies assigned)*/
	Data &RDP._Index_Outcome_Fin_&NUM.;
  	set %do i=1 %to &max.; r&i._F_Fin  r&i._I_Fin %end; ;
	/*  set %do i=1 %to &max.; r&i._F_Fin r&i._G_Fin %end; ;*/
  	*drop pairedcodetypex count;

	  	keep patient_deid idxpren dt_indexprenatal dt_pregnancyoutcome_start dt_pregnancyoutcome_end 
	       	PREG_Outcome PREG_CodeType pregnancyoutcomegroupsevaluated outcomegrp OUTCOMEGROUPSFOUND
	       	PREG_OutcomeCount PREG_OutcomesCompared DT_PREGNANCYOUTCOME_start DT_PREGNANCYOUTCOME_end  DT_PREG_OUTCOME
	       	PRENNUM PREG_MISO PREG_MIFE  preg_outcomecodetypescompared
	  	;
  		Format dt_: date. ;
	run;
	proc sort nodups Data= &RDP._index_outcome_fin_&NUM.; 
  		by patient_deid dt_indexprenatal ;
	run;

	%*add preg info to deletedrows (i.e. outcomegrps rolled up, accounted for in pairedoutcome);
	proc sql;
 		create table &RDP._deletedrows_&num. as
  		select a.*, b.*
  		from &RDP._deletedrows_&NUM._base a 
		join &RDP._index_outcome_fin_&num. b 
    	on a.patient_deid=b.patient_deid and a.dt_indexprenatal=b.dt_indexprenatal;
		quit;

	/*Combine together all the pregnancy outcome group information*/
    data &RDP._outidsfin_&NUM.;
    set &RDP._outcomeneedindex_&NUM.(in=a) &RDP._index_outcome_fin_&NUM.(in=b) &RDP._deletedrows_&NUM.(in=c);
     	%*keep patient_deid outcomegrp resultant_&NUM.  IDXPREN;

        if a then resultant_&NUM. ='need index';
        if b then resultant_&NUM.='paired'; 
        if c then resultant_&NUM.='rollup'; %*change from dropped to rollup 01.23;
/*  		keep patient_deid outcomegrp idxpren dt_indexprenatal outcomegroupsfound */
/*           	paircount pairscompared pairedoutcome dt_pairedoutcome pairedcodetype*/
/*           	dt_pregnancyoutcome_start dt_pregnancyoutcome_end resultant_&num.; */
      	;
      	Format dt_: date. ;
    run;
    proc sort nodups data=&RDP._outidsfin_&NUM.;
		by patient_deid outcomegrp;
	run;
   
    proc sql;

/*     create table &RDP._missed_index_&NUM. as select a.* from indexprenataloutcomes_&NUM. a left join &RDP._index_outcome_fin_&NUM. b*/
     	create table &RDP._missed_index_&NUM. as 
		select a.* from &PODSN. a 
		left join &RDP._index_outcome_fin_&NUM. b
       	on a.idxpren=b.idxpren 
		where b.idxpren=. and a.outcomegroupsfound>0;

/*     create table &RDP._lost_outgrp_&NUM. as select a.* from indexprenataloutcomes_&NUM. a left join &RDP._outidsfin_&NUM. b*/
     	create table &RDP._lost_outgrp_&NUM. as 
		select a.* from &PODSN. a 
		left join &RDP._outidsfin_&NUM. b
       	on a.patient_deid=b.patient_deid and a.outcomegrp=b.outcomegrp
        where b.outcomegrp=.  and a.outcomegrp ne .;

		quit;

	/*Delete the datasets that no longer need - keep the work library less cluttered.*/
    proc datasets lib=work nolist;
     *	delete %do i=1 %to &max.; r&i._:  %end; ipo_: ;
    quit;

%mend;












/***************************************************************************
MACRO: PAIREVAL
Purpose: The purpose of this macro is to sort through the pregnancy outcome
groups within the outcome lookforward window for a prenatal encounter and
determine if they describe the same or different pregnancies.

INPUT:
*podsn - prenatal+outcomegrp or remaining outcomegrps datasets dataset
*rdp - results dataset prefix (using S5 (step 5) and S10 (step 10)
***************************************************************************/

/*Testing:
%let podsn = INDEXPRENATALOUTCOMES_&num.; */

%macro PairEval(podsn, rdp);

	/*ocw = outcome window*/
	%if &ocw= %then &ocw=1;  

	/*Sort the prenatal+outcomegrp dataset according to the start date
	of the pregnancy outcome group (both outcomegrp and dt_outcomegroup_start
	accomplish this*/
    proc sort data= &podsn. ; %* %let podsn=indexprenataloutcomes_1; 
    	by patient_deid outcomegrp dt_outcomegroup_start;
    run; 

	/*Max is the maximum number of outcome groups found per person*/
  	%do round= 1 %to &Max. ;

  		/*%put ------------- &ROUND. / &MAX. ----------------;*/
		/*Testing: %let round = 1; */
        %if  &Round = 1 %then %do; /*For the first round*/

			/*Create the datasets for subsequent rounds*/
			data ipo_5abc_&Round.    
                 ipo_5abc_next_&round. (drop=PREG_Outcome dt_pregnancy: count) ;
            set &podsn.;

             	by patient_deid;
              	where outcomegroupsfound>0; 
				/*Consider only those prenatal encounters that linked to at least 1 pregnancy outcome group*/

              	PREG_OutcomeCount=1; /*By definition, this is the first pregnancy outcome compared*/
              	length PREG_OutcomesCompared $100.; /*enough room to capture 25 pairs*/
                PREG_OutcomesCompared=Outcome_Assigned&ALG.; /*start with 1st outcome, add more in macro*/

              	length PREG_OutcomeCodetypesCompared $100.; /*05.23.24 - CDL request to capture codetypes compared (000,001...)*/
                PREG_OutcomeCodetypesCompared=Outcome_Assigned_Codetype&ALG.; /*start with 1st outcome, add more in macro*/

                /*select the outcome for the paired outcomegrps - 5b.2;*moved from 5abc*/
                attrib PREG_Outcome length=$10 label="Outcome for paired prenatal on same pregnancy";
                PREG_Outcome=''; /*Creating the column*/

                /*05.23.24 - Need to initialize PREG_MIFE and PREG_MISO*/
                PREG_MIFE = mifepristone;
                PREG_MISO = misoprostol;

              	/*8.17 add initial date values*/
                Dt_PregnancyOutcome_Start = dt_outcomegroup_start;
                Dt_PregnancyOutcome_End = dt_outcomegroup_end;

              	if first.patient_deid then count=0;

			  	count+1; /*Counting the number of prenatal+outcome group combinations - makes it start at 1 for everyone*/

              	if count le 2 then output ipo_5abc_1 ;/*keep 1st set of outcome pairs*/
                             else output ipo_5abc_next_1 ; /*rest of the pregnancy outcome groups that need to be evaluated in the next round.*/
			run;

        %end;


	    %*prep the set of outcomegrps to be evaluated for same pregnancy;
	    proc sort data= ipo_5abc_&Round.  ; 
	        by patient_deid descending dt_outcomegroup_start;
	    run; %*reversing the order so can attach Next outcome info to first pregnancy outcome record;

		/*Pull the information from the subsequent pregnancy outcome group onto the first
		pregnancy outcome group for a person. -- This will be used to check timing of the 
		pregnancy outcome groups (i.e., if same pregnancy).*/
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

	      	%*for preg w/ prev outcome group this will be >0;
	      	%*IF missing(PregnancyOutcomeGroupsEvaluated) then PregnancyOutcomeGroupsEvaluated = 1; 

	      	if patient_deid = lptid then do;
	          	dt_OutNext_start = dt_lds;
	          	dt_OutNext_end = dt_lde ;
	          	Days_OutEndNextStart = dt_lds - dt_PregnancyOUtcome_end ;

	          	OutNext_Assigned = lot ;
	          	OutNext_CodeType = loc ;
				outnext_codetype_cd = put(outnext_codetype, $outcdrxoverdx.);
	          	OutNext_Mifepristone = lmf;
	          	OutNext_Misoprostol = lms;
	          	OutNext_InpatPRDelivery= lid;

	          	OUTNEXT_OUTCOMEDT = LDA;

	          	PREG_OutcomesCompared = cats(PREG_OutcomesCompared,'-', outnext_assigned);
	          	PREG_OutcomeCodetypesCompared = cats(PREG_OutcomeCodetypesCompared,'-', outnext_CODEtype);  %*added 05.23.24;
	          	PREG_OutcomeCount+1 ;
	      	end;

			drop lot loc lptid dt_lds dt_lde  lmf lms;
	      	format dt_: mmddyy10.;
	      	label
	        	dt_outnext_start = 'Date of subsequent outcome start'
	        	dt_outnext_end = 'Date of subsequent outcome group end'
	        	days_outendnextstart = 'Days from Outcome group end to start of subsequent outcome'
	        	outnext_assigned = 'Subsequent outcome assigned'
				outnext_codetype = 'Subsequent outcome code type' 
	        	outnext_codetype_cd = 'Subsequent outcome code (PR/DX/RX)' 
	        	outnext_mifepristone= 'Mifrepristone RX on subsequent outcome'
	        	outnext_misoprostol = 'Misoprostol RX on subsequent outcome'
	        	PREG_OutcomeCount = "Number of outcome groups included in pregnancy"
	      	;
	    run;

		/*Check*/
/*		proc freq data=R&round._A_prep;*/
/*			tables OutNext_Assigned*outnext_codetype_cd;*/
/*		run;*/
		/*Confirmed that only getting RX for abortion outcomes*/

		/*put back in order;*/
	    proc sort data= R&ROUND._A_prep ;
	    	by patient_deid dt_outcomegroup_start;
	    run;


	    /*** Now for Index with multiple groups check for time between 1st and 2nd to;
	    *** determine if outcomes represent possibly 2 pregnancies and should be ;
	    *** attached to different Index prenatal + outcome groups or if the same pregnancy;*/
	    data R&Round._B_sameck R&round._delete ; 
	    set R&round._A_prep ; 
	     	by patient_deid; 

	     	/** Time between outcomes within bounds in 5b.1 table 3 to check if outcomes can be tied to same index;
	     	* if first/last then only 1 record to be evaluated in the round, otherwise check pair for timing;
	     	* note first/last always true for those with one outcomegroup;*/
	     	if first.patient_deid and last.patient_deid then OnlyRec=1; /*If only one pregnancy outcomegroup then we are set*/
	     	else do;
	       		SamePregnancy = 0; /*Initially we assume that the pregnancy outcome groups represent different pregnancies*/
	        
	       		/**flag for pregnancy outcome fields (set to 1st outcomegrp values round 1 / if not prev set);
	       		*after round1 the Preg/Paired values are set from evaluation of previous outcomegrp pairs;*02.2024;*/
	        	%if &round. = 1 %then %do;
	         		Preg_PRDelivery = inpatPRdelivery; *true if 1st round;
	         		Preg_MIFE = mifepristone;
	         		Preg_MISO = misoprostol;
	         		Preg_Outcome = outcome_assigned&ALG.;
					Preg_CodeType =  put(outcome_assigned_codetype&ALG. , $outcdrxoverdx.);
	         		Dt_Preg_Outcome = dt_outcome_assigned&ALG.;
	        	%end;      

	       		%*****01.25 - NEED TO COMPARE THE NEXT OUTCOMEGROUP TO THE PAIREDOUTCOME FOR THE PREG AT THIS STAGE****;
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

	         	else if PREG_Outcome in: ('SAB' 'IAB' 'UAB') then do; 
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

	      	%*Reset pregnancy end dates - end date will change if outgrps represent same pregnancy;
	        if samepregnancy=1 then dt_PregnancyOutcome_End = dt_outnext_end;

			/*Using the PROC SORT these should be ordered such that the first pregnancy outcome group (dtstart) is the row
			before the second pregnancy outcome group for a person. So, if you look back for the same person and their prior
			group was determined to be part of the same pregnancy, you drop the second group's record. The second groups
			information has been retained on the first record.*/
	      	%*drop rows accounted for already - only works for 2 - can drop 2nd row as its captured as Next in 1st otucomegrp;
	      	lsi=lag1(samepregnancy);
	       	if lsi=1 then output r&round._delete; %*not needed but will help track counts, now all pair for same preg on 1 obs ;
	       		%*else output R&round._B_sameck ;
	        else do;
	          output R&round._B_sameck ;
	        end;
	       drop lsi;
	    run;
		

	    %*** now separate the pregnancy outcome groups according to how they should be handled for next evaluation;
		proc sort data=R&Round._B_sameck;
			by patient_deid Dt_OutcomeGroup_Start;
		run;
	    data R&Round._C_eval (label="index w/ identified outcomes, clear (outgrp marked as same pregnancy for evaluation)") %*Need to evaluate next pregnancy outcome group*;
	         R&Round._D_newpren (label="outcomes that need a new index prenatal")
	         R&Round._E_done (label="outcomes groups where Only 1 for index Or most recent pairing was not for same pregnancy")
	     ;
	    set R&Round._B_sameck ; 
	      	by patient_deid Dt_OutcomeGroup_Start;

			/*Deal with individuals who only have one prenatal+preg outcome group row in this dataset. These individuals either:
			(1) only had 1 pregnancy outcome group link to their first indexing prenatal encounter or (2) had at least 2 
			pregnancy outcome groups link but the first pair evaluated were determined to be part of the same pregnancy
			per timing criteria.*/
	       	if first.patient_deid and last.patient_deid then do;
	          	if OnlyRec then  OUT_E=1; %*no need to evaluate if there was only 1 remaining rec for pt;  
	          	else if samepregnancy then OUT_C=1; %*evaluate if paired outcomes are same pregnancy - we will look for a subsequent pregnancy outcome group;
	       	end;
	
			/*Now deal with those individuals who have 2 prenatal+preg outcome group rows in this dataset. This would occur because
			there were 2 pregnancy outcome groups linked to the same prenatal encounter but they were determined, via timing criteria above, 
			to represent different pregnancies.*/
	       	else if  samepregnancy=0 then do;          %*2 potential outcomes too far apart to count as same preg;
	          	if first.patient_deid then do;   %*the first rec is the final prenatal-outcome pair;
	            	Out_E=1; 
	            	PREG_OutcomesCompared = substr(PREG_OutcomesCompared,1, (length(PREG_OutcomesCompared)-(length(scan(PREG_OutcomesCompared,-1,'-'))+1)) );  %*need to take off last outcome (-ABC);
	            	PREG_OutcomeCodetypesCompared = substr(PREG_OutcomeCodetypesCompared,1, 
                                                           (length(PREG_OutcomeCodetypesCompared)-(length(scan(PREG_OutcomeCodetypesCompared,-1,'-'))+1)) );  %*need to take off last outcome (-ABC);
		            PREG_OutcomeCount =PREG_OutcomeCount-1; %*Subtract 1 from the number of pregnancy outcome groups evaluated.;
	          	end; 
	          	else out_d=1; %*the subsequent should be paired with new indexing prenatal encounter ;
	       	end;
	
			/*CDL: Conceptually, no pregnancy outcome groups should make it to these lines of code.
			Holdover from previous versions. Retained to capture issues.*/
			else if pregnancyoutcomegroupsevaluated >0 then OUT_E=1; %*this pair not the same preg so revert to prev pairing;
	       	else  OUT_D=1; %*all others are not the same pregnancy so link to new prenatal;

			/*Output each dataset accordingly.*/
	        if out_c then output R&Round._C_eval;  
	        if out_d then output R&Round._D_newpren; 
	        if out_e then output R&Round._E_Done ; 
	        DROP out_c out_d out_e;

	    run;



	    /***** get previous record for cases where last outcome group was in previous iteration;
	    **** for 1st pass of a grp where there are 2 pairs but not for same pregnancy just use the rec as is (next outgrp leave for now);
	    ***  postmtg 12.14 - ADJUST RX-MIS to use original abortion type and change the pairedcodetype to RS (MIS) and RF (MIF);*/
	    %IF &Round. # 1  %THEN %DO;  /*Pound sign is equivalent to IN -- macro in delimitor.*/
			/*Pregnancies that end up in this final dataset in the 1st round only do so becuase (1) they only had one pregnancy
			outcome group linked to their prenatal encounter, or (2) they were evaluated for same_preg and the 2nd outcome
			group was too long after the first for the outcomes identified.*/
	      	data R&round._F_Fin; 
	       	set R&Round._E_done;

					PREG_CodeType = Put(outcome_assigned_codetype&ALG., $outcdrxoverdx.);

	        	PREG_Outcome = outcome_assigned&ALG.;
	        	DT_PREG_Outcome = DT_OUTCOME_ASSIGNED&ALG. ;
	      	run;
	    %END;

	    %ELSE %DO; /*If they are done, then it means that what was determined as the pregnancy outcome
						in the last round is correct.*/
	    	proc sql;
	       		create table R&round._F_Fin as
	        	select b.* 
				from r&Round._E_done a 
				join R%eval(&round.-1)_G_OUTC b
	        	on a.patient_deid=b.patient_deid and a.dt_indexprenatal = b.dt_indexprenatal ;
	      		quit;
	    %END;

		/*Deal with those pregnancy outcome groups that still need to be evaluated.*/
    	%*** find pregnancy outcome based on paired outcome groups  ;
    	data R&round._G_Outc  (label="pairs to pregnancy outcome"); 
     	set R&Round._C_eval ;  
			by patient_deid;

      		Paired=1; %*indicate that pregnancy outcome based on paired results;

	        %*Determine pregnancy outcome based on paired outcome groups selecting based on priority;
	        %*order: inptdel-pr, pr-pr (hier3), rx-rx (mife, miso), dx-dx (hier4),  any pr, any rx-mif, any rx-mis, any dx;
	        Array prord(*) LBM LBS MLS SB UDL SAB IAB UAB EM AEM;  %*this is hierarchy 3 5b.2b (1st 5 are hierarcy #2);
	        Array dxord(*) SAB IAB UAB EM AEM SB LBM LBS MLS UDL;  %*this is hiearchy 4 5b.2b;

	        %*select the outcome for the paired outcomegrps - 5b.2;
			PREG_CodeTypex = cats(PREG_CodeType,'-',OutNext_CodeType_cd /*OutNext_Codetype*/);

	        tmp_found=0;

	        %*if EITHER outcome proc-based inpatient delivery take the highest priorty outcome in the pair;
	        if  PREG_PRDELIVERY=1 And OUTNEXT_INPATPRDELIVERY = 1 THEN DO i=1 to dim(prord) Until (tmp_found=1);
	        	if PREG_Outcome = vname(prord(i)) or outnext_assigned = vname(prord(i)) then  DO;
					/*Get the earliest date aligning with the pregnancy outcome*/
					if PREG_Outcome = vname(prord(i)) then Dt_PREG_Outcome = Dt_PREG_Outcome;  *can use 1st as that is earlier date;
	                	else Dt_Preg_Outcome = outnext_outcomedt;
	            	PREG_Outcome= vname(prord(i));
	               	Preg_PRDelivery = 1;
	               	PREG_CodeType = "PR";
	               tmp_found=1;
	            end;
			end;
	
			/*If the current pregnancy outcome group is defined by inpt delivery procedure and next outcome group not*/
	        %IF &ROUND # 1 %then %do;
	        	else if PREG_PRDELIVERY=1 THEN DO i=1 to dim(prord) Until (tmp_found=1); %*unnecessary after round 1, resetting to itself;
	              	if PREG_Outcome = vname(prord(i)) THEN DO;
	               		PREG_Outcome= vname(prord(i));
	               		Preg_PRDelivery =1;
	               		PREG_CodeType = "PR";
	               		tmp_found=1;
	                	%*no need to update date since code type did not change;
	              	end;
	          	end;
	         %END;

			 /*If the subsequent outcome group defined by an inpt delivery procedure and the current pregnancy outcome groups are not*/
	         else if Outnext_InpatPRDELIVERY=1 THEN DO i=1 to dim(prord) Until (tmp_found=1); 
	         	if outnext_assigned = vname(prord(i)) THEN DO; 
					PREG_Outcome= vname(prord(i));
	               	Preg_PRDelivery =1;
	               	PREG_CodeType = "PR";
	                Dt_PREG_Outcome = outnext_outcomedt ; %*Reset preg outcome date to best guess given pairs;
	               	tmp_found=1;
	             end;
			 end;

	        %*if paired outcomes are both defined pr codes then go to hierarchy 5b.2b;
	                %*only update pregnancy date if changing type (if same type keeping earliest);
	        ELSE if PREG_CodeTypex in ( "PR-PR" ) then do i=1 to dim(prord) Until (tmp_found=1);
	        	if PREG_Outcome = vname(prord(i)) or outnext_assigned = vname(prord(i)) then do;
					%*IF 2ND DATE IS HIGHER PRIORITY OUTCOME (IN DXORD) THEN RESET DATE;
					if PREG_Outcome = vname(prord(i)) then Dt_PREG_Outcome = Dt_PREG_Outcome;  %*can use 1st as that is earlier date;
	                	else Dt_Preg_Outcome = outnext_outcomedt;
	               	PREG_Outcome= vname(prord(i));
	               	PREG_CodeType ="PR";
	                tmp_found=1;
	            end;
	        end;

	        %*Reassign outcomes for abortions based upon mife and miso orders -- pregmife pregmiso;
	        else if PREG_CodeTypex in ( "RX-RX" ) then do; 
				/*CDL: MODIFIED -- More inclusive of all abortion types now.*/
				if PREG_MIFE = 0 & outnext_mifepristone=1 then do;
					PREG_MIFE = 1;
					Dt_PREG_Outcome = outnext_outcomedt; /*Update the pregnancy outcome date*/
					/*CDL: ADDED*/
					/*Update the outcome type assigned to the subsequent pregnancy outcome group*/
					PREG_Outcome = OutNext_Assigned;
				end;
				%*else if both equal preg_miso (only option for this else if statement, then keep everything the same;
	        end;  

	        else if PREG_CodeTypex in ( "DX-DX" ) then do i=1 to dim(dxord) until (tmp_found=1);
	            if PREG_Outcome = vname(dxord(i)) or outnext_assigned = vname(dxord(i)) then do;
					/*CDL: MODIFIED - Updated date to match the above approaches.*/
					if PREG_Outcome = vname(dxord(i)) then Dt_PREG_Outcome = Dt_PREG_Outcome;  %*can use 1st as that is earlier date;
	                	else Dt_Preg_Outcome = outnext_outcomedt;
	                PREG_Outcome= vname(dxord(i));
	                PREG_CodeType="DX";
	                tmp_found=1;
	            end;
	        end;
 

			/*Now deal with those situations where they are not both defined via the same code type (dx, pr, rx)*/
	        %*order: inptdel-pr, pr-pr (hier3), dx-dx (hier4), rx-rx (mife, miso), any pr, any rx-mif, any rx-mis, any dx;

			/*First outcome is a procedure code and the second isnt -- keep the info from the first outcome*/
	        else if PREG_CodeType='PR' then do; 
	         *no change needed, resultant pregnancy outcome same;
	        end;

			/*Second outcome is a procedure code and the second isnt -- keep the info from the second outcome*/
	        else if Outnext_codetype_cd = 'PR' then do;
	        	PREG_Outcome = outnext_assigned;
	          	PREG_CodeType='PR';
	          	dt_PREG_Outcome = outnext_outcomedt;
	        end;

			/*Deals with RX-DX cases for UABs in the first one*/
	        else if PREG_CodeType='RX' and PREG_MIFE=1 then do;
	         	%*no change - already mife defined preg outcome;
	        end;

			/*CDL: MODIFIED -- Deals with DX-RX cases for any abortions in the second one -- Previously only focused on UABs*/
	        else if Outnext_codetype_cd = 'RX' And Outnext_mifepristone=1 then do;
	          	PREG_MIFE=1;
			  	/*CDL: ADDED*/
			  	PREG_MISO = max(PREG_MISO, outnext_misoprostol);
				/*CDL: MODIFIED -- Should be applied for any abortion outcome*/
				PREG_Outcome = OutNext_Assigned;
/*	          	PREG_Outcome = 'UAB';*/
	          	PREG_CodeType='RX';
	          	dt_PREG_Outcome = outnext_outcomedt;
	        end;
	        
			/*Deals with RX-DX cases for MISO in first*/
	        else if PREG_CodeType='RX' and PREG_MISO=1 then do;; %*Would only get RX code type for abortion outcomes;
	         	%*no change - already mife defined preg outcome;
	        end;

			/*Deals with DX-RX cases for MISO in second*/
	        else if Outnext_codetype_cd = 'RX' And Outnext_misoprostol=1 then do;
	          	PREG_MISO=1;
				PREG_Outcome = OutNext_Assigned;
	          	PREG_CodeType='RX';
	          	dt_PREG_Outcome = outnext_outcomedt;
	        end;

			/*If first is a DX code at this point, then just need to roll up mife and miso information.*/
	        else if PREG_CodeType='DX' then do; ;
	         	*no change needed, resultant pregnancy outcome same;
	        end;

			/*Finishing the hierarchy but I believe we should not get to this situation*/
	        else if Outnext_codetype_cd = 'DX' then do;
	        	PREG_Outcome = outnext_assigned;
	          	PREG_CodeType='DX';
	          	dt_PREG_Outcome = outnext_outcomedt;
	        end;

			/*Roll up the medication information across all pregnancy outcome groups evaluated.*/
			PREG_MIFE = max(PREG_MIFE, outnext_mifepristone);
			PREG_MISO = max(PREG_MISO, outnext_misoprostol);

	        drop i tmp_found LBM LBS MLS SB UDL SAB IAB UAB EM AEM;
	        format dt_: mmddyy10.;
	        label dt_pregnancyoutcome_start = "Date start of 1st outcomegrp for pregnancy"
	              dt_pregnancyoutcome_end = "Date end of last outcomegrp for pregnancy"
	              dt_preg_outcome = "Date of pregnancy outcome"
	              preg_outcome = "Pregnancy outcome assigned"
	              preg_codetype = "Pregnancy outcome assigned codetype (DX/PR/RX)"
	              preg_miso = "Misoprostol RX among pregn outcome groups"
	              preg_mife = "Mifepristone RX among pregn outcome groups"
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
	    merge r&round._D_NewPren(keep=patient_deid in=a) /*Pregnancy outcome groups that need to be evaluated for a new indexing prenatal encounter*/
	            ipo_5abc_next_&ROUND. (in=b); /*Pregnancy outcome gorups that were set aside earlier for subsequent pair eval rounds.*/
	    	by patient_deid;

			if a and b then output R&round._D_NewPren2; /*Goal is to identify ALL pregnancy outcome groups for that person that need a new indexing prenatal encoutner*/
	        	else if b then output ipo_5abc_next_&ROUND._adj ;
	    run;

		/*Stack all of the pregnancy outcome groups that still need to be evaluated for the next round with those
		prenatal+pregnancy outcome groups that were determined to describe the same pregnancy.*/
	    data R&Round._H_nextrnd (label="Info needed for next pair comparison i.e. next round"); 
	    set  ipo_5abc_next_&Round._adj
	         r&round._G_Outc  (in=a drop= dt_outnext: outnext: samepregnancy onlyrec) ;

	     	drop count; *gonna reset in prep below;
	    run;
	    proc sort data=R&Round._H_Nextrnd; 
			by patient_deid dt_outcomegroup_start;
	    run;

		/*Create the new datasets with pregnancy outcome group pairs and the rest of the 
		pregnancy outcome groups that still need to be evaluated*/
	    data ipo_5abc_%eval(1+&round.) (label="outcome pair to evaulate in next round")
	         ipo_5abc_next_%eval(1+&round.)  (label="outcome groups to pair in successive rounds") 
	         R&round._I_Fin (label="done - only rec left was pair just evaluated")
	         ;
	    set R&ROUND._H_nextrnd;  ;
	    	by patient_deid;

	      	if first.patient_deid then count=0;
	      	count+1;
	       	IF first.patient_deid and last.patient_deid then output R&Round._I_Fin ;
	       		else do;
	         		if count le 2 then output ipo_5abc_%eval(1+&round.);  /*first two pregnancy outcome groups that need to be evaluated*/
	                	else output ipo_5abc_next_%eval(1+&round.);  /*the rest of the pregnancy outcome gropus to be evaluated in a subsequent round*/
	       		end;
	    run;

	    Data R&Round._Final ; 
		set r&round._F_fin /*Those where pregnancy otucome determined to be correct in an earlier set of steps.*/
			r&round._I_fin ; 
		run;

	%end;

    %CombineResults 
    %lognote5ab

%mend;

