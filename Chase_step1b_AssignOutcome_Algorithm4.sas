/******************************************************************************

Program: chase_Step1b_AssignOutcome_Algorithm4.sas

Programmer: Sharon 

Purpose: Assigns pregnancy outcomes to pregnancy outcome groups using the 
proposed methodology in Algorithm 4.

Modifications:
	- 04-29-24: Chase (CDL) added comments throughout and modified appearance.
		Suggested changes to how we are collecting the codetype information 
		for outcome1.

*******************************************************************************/



*modified from chase_1b4_assignoutcome_algorithm4.sas;

*algorithm4 (complex) - checkng for delivery procedure (copied alg3) then non-delivery procedure;
*set outcomes to PRD - Procedure, delivery -- PRO - Procedure, Other  ;

*First create the discordance flags and assign outcomes for some -- Subset to only those pregnancy
outcome groups with discordant pregnancy outcomes;
data &discdsn._int_Alg4 ;
set &discdsn (in=b);
	
	*Create the discordance flag and set the length of an outcome indicator;
  	Discordant4=0;
  	length Disc_OutPot4 $8.;
  
	*First, determine if there is a delivery procedure code present;
  	if put(delivery,$outcdpr.) = 1 then do; *any delivery proc? check for concordant combos;

		*CDL: MODIFIED -- Cleaner than original code but provided the same result among those with discordant4 = 1
		-- Same modification as Algorithm 3;
		**step 1b1 part 2;
	    *1 - ...and concordant delivery codes (redo table 1 check);
		if lbm NE ('000') /*and udl NE ('000')*/ And SB='000' And LBS='000' and MLS='000' 
	         then do; discordant4=1; Disc_OutPot4='PRD-LBM';end;
		else if lbs NE ('000') /*and udl NE ('000')*/ And SB='000' And LBM='000' and MLS='000' 
	         then do; discordant4=1; Disc_OutPot4='PRD-LBS';end;
		else if mls NE ('000') /*and udl NE ('000')*/ And SB='000' And LBM='000' and LBS='000'
	         then do; discordant4=1; Disc_OutPot4='PRD-MLS';end;
	    else if sb NE ('000') /*and udl NE ('000')*/ And MLS='000' And LBM='000' and LBS='000' 
	        then do; discordant4=1; Disc_OutPot4='PRD-SB';end;
		else if UDL NE ('000') and SB In ('000') And MLS='000' And LBM='000' and LBS='000' 
	        then do; discordant4=1; Disc_OutPot4='PRD-UDL';end;

/*   		*1 - ...concordant delivery procedures (redo table 1 check);*/
/*    	if lbm Not In ('000') and udl Not In ('000') And SB='000' And LBS='000' and MLS='000' */
/*         	then do; Discordant4=1; Disc_OutPot4='PRD-LBM';end;*/
/*    	else if lbs Not In ('000') and udl Not In ('000') And SB='000' And LBM='000' and MLS='000' */
/*        	then do; Discordant4=1; Disc_OutPot4='PRD-LBS';end;*/
/*	    else if mls Not In ('000') and udl Not In ('000') And SB='000' And LBM='000' and LBS='000'*/
/*	         then do; Discordant4=1; Disc_OutPot4='PRD-MLS';end;*/
/*	    else if sb Not In ('000') and udl Not In ('000') And MLS='000' And LBM='000' and LBS='000' */
/*	        then do; Discordant4=1; Disc_OutPot4='PRD-SB';end;*/
/**/
/*	    *if ignoring non-delivery then single delivery outcome types are concurrent;*/
/*	    else if lbm Not In ('000') and udl In ('000') And SB='000' And LBS='000' and MLS='000' */
/*	         then do; Discordant4=1; Disc_OutPot4='PRD-LBM';end;*/
/*	    else if lbs Not In ('000') and udl In ('000') And SB='000' And LBM='000' and MLS='000' */
/*	        then do; Discordant4=1; Disc_OutPot4='PRD-LBS';end;*/
/*	    else if mls Not In ('000') and udl In ('000') And SB='000' And LBM='000' and LBS='000'*/
/*	         then do; Discordant4=1; Disc_OutPot4='PRD-MLS';end;*/
/*	    else if sb Not In ('000') and udl In ('000') And MLS='000' And LBM='000' and LBS='000' */
/*	        then do; Discordant4=1; Disc_OutPot4='PRD-SB';end;*/
/*	    else if UDL Not In ('000') and SB In ('000') And MLS='000' And LBM='000' and LBS='000' */
/*	        then do; Discordant4=1; Disc_OutPot4='PRD-UDL';end;*/

  	*2 - ...and discordant delivery codes;
   		else do;
     		Discordant4=2; Disc_OutPot4="PRD-Dsc"; *has deliv proc but combo of outcomes discordant;
   		end;

  	end;
  
  	*now for those without a delivery procedure code ;
  	Else if put(delivery,$outcdpr.) = 0 then do; *redundant line but easier to read pgm;

		*3 - ...but non-delivery (abortion/ectopic) procedures;
   		if put(abortion,$outcdpr.) = 1 Or put(ectopic,$outcdpr.) = 1 then do; *non-delivey procedures;
    		Discordant4=3;  Disc_OutPot4 = "PRO-Any";  *procedure-other--any;
   		end;

		*4 - no procedures;
   		else do; 
    		Discordant4=4; Disc_OutPot4="PRO-None"; *no procedure code for outcome group;
   		end;
  	End;

run;

 
**steps 4-8 for algorithm 4;run;
data &NewDsn._alg4;
set &ConcDsn. (in=a) 
    &DiscDsn._int_Alg4 (in=b) ;

	*Create concordant value;
 	if a then discordant4 =0;

 	array hier(*) EM SAB IAB UAB AEM ; *step 1b.5 hierarchy;
 	array hier6(*) SAB IAB UAB SB LBM LBS MLS UDL EM AEM ; *hierarchy for pt2 step 1b.6 (same as alg3);
 	*array hiert1(*)  LBM LBS MLS SB UDL SAB IAB UAB EM AEM; *3b.1 - table 1; *CDL: Commented out because not referenced again;

	*Assign outcomes;
 	outassgn_pt1 ='unk';

	*Concordant outcomes;
 	if discordant4 = 0 /*concordant*/ then outassgn_pt1 = outcome_concordant; * CDL: Modified the concordant call to be more explicit;

 	else if discordant4 ne 0 /*concordant=0*/ then do; *CDL: MODIFIED the concordant to match the prior line;

    	*step 1b.3 discordant but with concordant delivery procedure using table 1 (Disc_OutPot3= PRD-[x])
		-- This was determined in the step above in the last 3 characters of the value disc_outpot4;
    	if discordant4=1 then outassgn_pt1 = substr(disc_outpot4,5); 

    	*step 1b.4 discordant with delivery procedure but discordant delivery codes;
    	else if discordant4=2 then outassgn_pt1 = 'UDL' ;*substr(disc_outpot4,5);

    	*step 1b.5 discordant, no delivery but procedure codes for other outcomes;
    	else if discordant4=3 then do i=1 to 10 until (outassgn_pt1 ne 'unk');
        	if put(hier(i),$outcd.)='PR' then outassgn_pt1 = vname(hier(i));
    	end;

    	*step 1b.6 discordant, other - 1st check code type (dx then rx);
    	if discordant4 = 4 then do i=1 to 10 until (outassgn_pt1 ne 'unk');

        	if dx_n>=1 then do i=1 to 10 until (outassgn_pt1 ne 'unk'); *1proc just use that one;

            	if hier6(i) in ('100','101','110','111'  '102','112','103','113') then do; 
                 	outassgn_pt1= vname(hier6(i));
            	end;

            	else if rx_n>=1 then do; 
             		outassgn_pt1  = 'UAB';
            	end; 

        	end;
    	end; 

	end;


  	*next step7- 1b.7 - discordant abortion clean up;
 	If outassgn_pt1  in ('SAB' 'IAB' 'UAB' ) then do; 
	*CDL: ADDED UDL to be consistent with alg 3. Might be some edge cases captured. - Chaned documentation;

		*CDL: MODIFIED to be more straightforward;
		if SAB ne ('000') and IAB ne ('000') then outassgn_pt2='SAB'; *Deal w discordant abortion outcomes first;
				else if SAB ne ('000') then outassign_pt2='SAB';
				else if IAB ne ('000') then outassign_pt2 = 'IAB';
		*NOTE: Some may have meds for IAB or SAB because they had concordant codes on an encounter whihc were 
				rolled up to the group level;

/*  		*if both (SAB+IAB +/- UAB);*/
/*  		if sab ne '000' and iab ne '000' then outassgn_pt2='SAB'; *3b.7.1;*/
/*   */
/*   		*if just one;*/
/*   		else do ;*1b.7.2 - go back to table 1 for abortion;*/
/*        	if sab NE '000' then outassgn_pt2='SAB'; */
/*    		else if iab NE '000' then outassgn_pt2='IAB';*/
/*    		else if sab='000' and iab='000' and uab not in ('000') then Outcome_assigned4_clean='UAB';*/
/*  		end;*/
 	end;

 	*1b8 - final outcome assignment;
 	If outassgn_pt2 ne '' then Outcome_Assigned4 = outassgn_pt2;
 		else outcome_assigned4 = outassgn_pt1;

	*Assign code type according to the pregnancy outcome class.;
    IF Outcome_Assigned4 in ('LBM' 'LBS' 'UDL' 'SB' 'MLS') then Outcome_Assigned_Codetype4= Delivery;
      ELSE IF Outcome_Assigned4 in ('SAB' 'IAB' 'UAB') then Outcome_Assigned_Codetype4= Abortion;
      ELSE IF Outcome_Assigned4 in ('EM' 'AEM') then Outcome_Assigned_Codetype4= Ectopic;

      Outcome_Class_Assigned4 = put(outcome_assigned4,$outclass.);*09.07;

 	drop i;
   	label
   		outcome_assigned4 = "Outcome Assigned (Algorithm4)"
   		outcome_assigned_codetype4="Outcome codetype (100=dx, 010=pr, 0001=rx)"
   		discordant4 = "Discordant outcome (algorithm4)"
   		outcome_class_assigned4="Class of outcome (deliv/abort/ectopic)"
  	;

run;


/*Checks*/
/*proc freq data= &newdsn._alg4;*where outcome_assigned3='unk';*/
/*where outassgn_pt1 in ('SAB' 'IAB') And SAB ne '000' and iab ne '000';*/
/*table discordant4*outcome_assigned4* SAB* IAB* UAB* EM* AEM /list missing ;*/
/**table delivery* SAB* IAB* UAB* EM* AEM  * discordant4 *outcome_assigned4 /list missing;*uab-pr other ab not pr;*/
/*format pr_n onepl.;*/
/*run;*/

/*Output a RTF file with descriptive information*/

ods rtf file = "&outpath.\Step1b_ReviewAlgorithm4_&Newdsn._%sysfunc(date(),yymmddn8.).rtf" style=minimal;

        title1 "Step 1b - Algorithm 4 outcomes" ;
        proc freq data= &newdsn._alg4 ;
         table concordant discordant4 OutAssgn_pt1 * OutAssgn_pt2 *outcome_assigned4 /list missing;
         format discordant4  discord4alg.;
        run;


        title3 ' review discordant';
        proc sql;
         select LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned4,Outcome_Assigned_Codetype4, count(*) as cnt
         from &newdsn._Alg4 where discordant4>0 And Outcome_assigned4 in ('LBM', 'LBS', 'MLS', 'SB', 'UDL')
          group by LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned4,Outcome_Assigned_Codetype4 
            order by calculated cnt desc;

         select LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned4,Outcome_Assigned_Codetype4, count(*) as cnt
         from &newdsn._Alg4 where discordant4>0 And Outcome_assigned4 in ('SAB','IAB','UAB')
          group by LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned4 ,Outcome_Assigned_Codetype4 
            order by calculated cnt desc;

         select LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned4,Outcome_Assigned_Codetype4, count(*) as cnt
         from &newdsn._Alg4 where discordant4>0 And Outcome_assigned4 in ('EM','AEM')
          group by LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned4 ,Outcome_Assigned_Codetype4 
            order by calculated cnt desc;
        quit;


ods rtf close;
