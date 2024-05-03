
********************************************************************;
**** STEP 1b - Assigns outcome to each pregnancy outcome group  ****;
**** Outcome Algorithm 2 Applied                                ****;
********************************************************************;

*modified from chase_1b2_assignoutcome_algorithm2.sas;

*** NOW step 1b.1 part 2 concordance flags (for alg 2 All are discordant, nothing to add); run;

*** STEP 1b.2 - this is included in the concordance assignment in pgm chase_1b ***; run;

*** STEP 1b.3 assign outcome to those discordant ***;run;

/*
*08.09 changed Not In to NE and remove Clean variables - instead apply Clean to outcome var;
*      for CodeType capture type of code(s) present for outcome type (delivery/abortion/ectopic);
*      (note alg directions separates rslt/cleaned+delivery/abortion outcome vars (outcome_b[2,4,5]);
*/

data &NewDsn._Alg2 ; *outcome solely based on hierarchy;
 set &concdsn (in=a)
     &discdsn (in=b)
 ;

 *summarize approach: ;
 * For concordant groups just use outcome from concordant datastep;
 * But if discordant assign based on stepwise approach: ;
 *  if an outcome in group is defined by procedure then assign that outcome based on the priority of 1b.3 hier#2;
 *  if no outcome defined by proc but defined by diagnosis then assign that outcome again using same hier#2;
 *  if no outcome defined by proc or diag then using Rx again by hier#2;

 *if a then discordant2=0;
 Discordant2 = b;
 OutAssgn_pt1 ='unk';

 if concordant then OutAssgn_pt1 = outcome_concordant;
 *else do;
  *check for presence of each code type (alg2 hierarchy 1);
   if put(delivery,$outcd.) = 'PR' or put(abortion,$outcd.)='PR' or put(ectopic,$outcd.)='PR'
    then Hier2_1 = 'PR';
   else if put(delivery,$outcd.) = 'DX' or put(abortion,$outcd.)='DX' or put(ectopic,$outcd.)='DX'
    then Hier2_1 = 'DX' ;
   else Hier2_1 = 'RX';
 *end;

 *using hierarchy order same as in Alg 1 (step 1b.3 was3b.3);
 array hier(*) LBM LBS MLS SB UDL SAB IAB UAB EM AEM; *labeled as hierarchy2 in 1b.3;

 OutAssgn_pt1='unk';
 if concordant then OutAssgn_pt1=outcome_concordant;
 else do;
   *Outcome_Assigned (simpler - pick outc based on codetype and 3b.3-hier2);
     if hier2_1 = 'PR' then do i=1 to 10 until (OutAssgn_pt1 ne 'unk');
       if hier(i) in ( '010','011' ,'110', '111' '012','112', '013','113' ) then do; 
         OutAssgn_pt1 = vname(hier(i)); *3b.2;
       end;
     end;
     else if Hier2_1='DX' then do i=1 to 10 until (OutAssgn_pt1 ne 'unk');
       if hier(i) in ('100','110','101','111' '102','112', '103','113') then do; 
         OutAssgn_pt1 = vname(hier(i));
       end;
     end;
     else if Hier2_1='RX' then OutAssgn_pt1 = 'UAB';
 end;
 *note - same result as more detailed/long code (both were in chase_1b2_assignoutcome_algorithm2);

 *Data cleaning steps 1b.4-1b.5;
 *step 1b.4 - if delivery reassign outcome if removing non-delivery results in concordance  ;
 if OutAssgn_pt1 in ('LBM','LBS','MLS','SB','UDL') then do;
         if lbm NE ('000')  And SB='000' And LBS='000' and MLS='000' 
         then do; OutAssgn_pt2='LBM';end;
    else if lbs NE ('000')  And SB='000' And LBM='000' and MLS='000' 
        then do; OutAssgn_pt2='LBS';end;
    else if mls NE ('000')  And SB='000' And LBM='000' and LBS='000'
         then do; OutAssgn_pt2='MLS';end;
    else if sb NE ('000')  And MLS='000' And LBM='000' and LBS='000' 
        then do; OutAssgn_pt2='SB';end;
    else OutAssgn_pt2='UDL'; *mix of single,multi,mixed OR just UDL;
 end;

 *Step 1b.5 - if abortion again use table 1 for concordant outcomes ignoring AEM/EM  ;
    *07.05 per CL and MW - not adjusting for AEM at this point in alg;
  else if OutAssgn_pt1 in ('SAB' 'IAB' 'UAB') then do; *follow 3b. table 1;
         if sab NE ('000') And IAB='000' then OutAssgn_pt2='SAB'; *3b.5.1;
    else if iab NE ('000') And SAB='000' then OutAssgn_pt2='IAB';
    else if SAB='000' And IAB='000' then OutAssgn_pt2='UAB';  
    else if SAB NE ('000') and IAB NE ('000') then OutAssgn_pt2='UAB'; *3b.5.2 discordant;

/*         if sab NE ('000') and uab NE ('000') And IAB='000' then OutAssgn_pt2='SAB'; *3b.5.1;*/
/*    else if iab NE ('000') and uab NE ('000') And SAB='000' then OutAssgn_pt2='IAB';*/
/*    else if UAB NE ('000') And SAB='000' And IAB='000' then OutAssgn_pt2='UAB';  */
/*    else if SAB NE ('000') and IAB NE ('000') then OutAssgn_pt2='UAB'; *3b.5.2 discordant;*/

    *if ignoring ectopic then single abortion outcome types are concordant;
/*    else if SAB NE ('000') and IAB In ('000') And UAB='000' then OutAssgn_pt2='SAB'; */
/*    else if IAB NE ('000') and SAB In ('000') And UAB='000' then OutAssgn_pt2='IAB'; */
/*    else if UAB NE ('000') and SAB In ('000') And IAB='000' then OutAssgn_pt2='UAB'; */

 end;

 else OutAssgn_pt2 = OutAssgn_pt1; 
 *not in table explicit (3b.4 only refers to delivery) - confirmed ok per CL 6.7 mtg;
 *Drop i hier2_1;

 *step 6 - outcome code type added here for assigned2_clean (already done for assigned2 above);
 Outcome_Assigned2 = outAssgn_pt2;

 *use the highest codetype for outcome type assigned to group;
    *only UDL has procedures, all other delivery types are diagnosis code based;
    *AEM is only dx based but all other outcomes have multiple possible code types;
    *for abortion if both SAB and IAB then otucome would be UAB so ok to use Abortion value (covers all);
      IF Outcome_Assigned2 in ('LBM' 'LBS' 'UDL' 'SB' 'MLS') then Outcome_Assigned_Codetype2= Delivery;
      ELSE IF Outcome_Assigned2 in ('SAB' 'IAB' 'UAB') then Outcome_Assigned_Codetype2= Abortion;
      ELSE IF Outcome_Assigned2 in ('EM' 'AEM') then Outcome_Assigned_Codetype2= Ectopic;

       Outcome_Class_Assigned2 = put(outcome_assigned2,$outclass.);*09.07;

  label
   outcome_assigned2 = "Outcome Assigned (Algorithm2)"
   outcome_assigned_codetype2="Outcome codetype (100=dx, 010=pr, 0001=rx)"
   discordant2 = "Discordant outcome (algorithm2)"
   outcome_class_assigned2="Class of outcome (deliv/abort/ectopic)"
  ;*added 9.22.23;

run;    



    ods rtf file = "&outpath.\Step1b_ReviewAlgorithm2_&Newdsn._%sysfunc(date(),yymmddn8.).rtf" style=minimal;

        title1 "Step 1b - Algorithm 2 outcomes" ;
        proc freq data= &newdsn._alg2 ;
         table concordant discordant2 OutAssgn_pt1 * OutAssgn_pt2 *outcome_assigned2 /list missing;
        run;


        title3 ' review discordant';
        proc sql;
         select LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned2,Outcome_Assigned_Codetype2, count(*)
         from &newdsn._Alg2 where discordant2 And OutAssgn_pt2 in ('LBM', 'LBS', 'MLS', 'SB', 'UDL')
          group by LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned2,Outcome_Assigned_Codetype2 ;

         select LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned2,Outcome_Assigned_Codetype2, count(*)
         from &newdsn._Alg2 where discordant2 And OutAssgn_pt2 in ('SAB','IAB','UAB')
          group by LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned2 ,Outcome_Assigned_Codetype2;

         select LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned2,Outcome_Assigned_Codetype2, count(*)
         from &newdsn._Alg2 where discordant2 And OutAssgn_pt2 in ('EM','AEM')
          group by LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned2 ,Outcome_Assigned_Codetype2;
        quit;

        proc sql;
         select LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, OutAssgn_pt1, OutAssgn_pt2, count(*)
         from &newdsn._Alg2 where discordant2 And AEM ne '000'
          group by LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, OutAssgn_pt1, OutAssgn_pt2;

        quit;

    ods rtf close;
