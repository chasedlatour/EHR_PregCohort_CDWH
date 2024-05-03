

********************************************************************;
**** STEP 1b - Assigns outcome to each pregnancy outcome group  ****;
**** Outcome Algorithm 3 Applied                                ****;
********************************************************************;

*modified from chase_1b3_assignoutcome_algorithm3.sas;
/*
*08.09 changed Not In to NE and remove Clean variables - instead apply Clean to outcome var;
*      for CodeType capture type of code(s) present for outcome type (delivery/abortion/ectopic);
*      (note alg directions separates rslt/cleaned+delivery/abortion outcome vars (outcome_b[2,4,5]);
*/

*algorithm3 (complex) - checkng for delivery procedure, ignoring other non-delivery outcomes; 

*** step 1b.2 discordnat flags ***;run;

data &discdsn._int_Alg3 ;
 set &discdsn (in=b);

  Discordant3=0;

  **summarize: ;
  ** outcomes are delivery procedures or not;
  **  if there is a delivery procedure then check for delivery concordance using 1b table1 (ignoring non-delivery outcomes);
  **  if no deliver procedure flag as such and stop;

  length Disc_OutPot3 $8.; 
  *any delivery proc? check for concordant combos;;
  if put(delivery,$outcdpr.) = 1 then do;;

   **step 1b1 part 2;
   *1 - ...and concordant delivery codes (redo table 1 check);
    if lbm NE ('000') and udl NE ('000') And SB='000' And LBS='000' and MLS='000' 
         then do; discordant3=1; Disc_OutPot3='PRD-LBM';end;
    else if lbs NE ('000') and udl NE ('000') And SB='000' And LBM='000' and MLS='000' 
        then do; discordant3=1; Disc_OutPot3='PRD-LBS';end;
    else if mls NE ('000') and udl NE ('000') And SB='000' And LBM='000' and LBS='000'
         then do; discordant3=1; Disc_OutPot3='PRD-MLS';end;
    else if sb NE ('000') and udl NE ('000') And MLS='000' And LBM='000' and LBS='000' 
        then do; discordant3=1; Disc_OutPot3='PRD-SB';end;

    *note: this is somewhat redundant - only dif from above is UDL;
    *if ignoring non-delivery outcomes (per note) then single delivery outcome types are concordent;
    else if lbm NE ('000') and udl In ('000') And SB='000' And LBS='000' and MLS='000' 
         then do; discordant3=1; Disc_OutPot3='PRD-LBM';end; *only delivery is lbm though may also have SAB etc;
    else if lbs NE ('000') and udl In ('000') And SB='000' And LBM='000' and MLS='000' 
        then do; discordant3=1; Disc_OutPot3='PRD-LBS';end;
    else if mls NE ('000') and udl In ('000') And SB='000' And LBM='000' and LBS='000'
         then do; discordant3=1; Disc_OutPot3='PRD-MLS';end;
    else if sb NE ('000') and udl In ('000') And MLS='000' And LBM='000' and LBS='000' 
        then do; discordant3=1; Disc_OutPot3='PRD-SB';end;
    else if UDL NE ('000') and SB In ('000') And MLS='000' And LBM='000' and LBS='000' 
        then do; discordant3=1; Disc_OutPot3='PRD-UDL';end;
  
    *2 - ...and discordant delivery codes;
    else do;
      Discordant3=2; Disc_OutPot3="PRD-Dsc"; *has deliv proc but combo of outcomes discordant;
    end;
  end;

   *3 no delivery procedure;
  else if put(delivery,$outcdpr.) = 0 then do;
    Discordant3=3;  Disc_OutPot3 = "PRD-Not";
  end;
  *PRD-LBM thru PRD_UDL are Delivery procedure, Delivery outcome concordant;
  *PRD-Dsc - Delivery procedure, Delivery outcome discordant;
  *PRD-Not - Delivery procedure not found;

run;


data &NewDSn._alg3;
 set &ConcDsn (in=a) 
     &discdsn._int_alg3 (in=b);

 if a then discordant3 =0;
 outassgn_pt1='unk';

 array hier(*) SAB IAB UAB SB LBM LBS MLS UDL EM AEM ; *hierarchy for pt2 Step 1b.5 (was step3b.5;
 array hiert1(*)  LBM LBS MLS SB UDL SAB IAB UAB EM AEM; *1b1 (was 3b.1 - table 1);

**Check for code type (proc/diag/rx) and assign outcome based on Hierarchy for algorithm hier(*);

*Assign outcomes;
 if concordant then outassgn_pt1= outcome_concordant; *3b.2;

 else do;
  *step 1b.3 discordant but with concordant delivery procedure using table 1 (Disc_OutPot3= PRD-[x]);
  if discordant3=1 then outassgn_pt1= substr(disc_outpot3,5); 
  *step 1b.4 discordant with delivery discordant;
  if discordant3=2 then outassgn_pt1= 'UDL'; *discordant=udl per instruction substr(disc_outpot3,5);

  *step 1b.5 discordant, no delivery;
  if discordant3=3 then do ;

     *using hierarchy for 1b.5;
     if pr_n>=1 then do i=1 to 10 until (outassgn_pt1 ne 'unk'); *1proc just use that one;
       if hier(i) in ('010','011','110','111' '012','112' '013','113') then do; 
         Outassgn_pt1= vname(hier(i));
         *OUtcome_Assigned3_Codetype='PR';
       end;
     end;

     else if dx_n>=1 then do i=1 to 10 until (outassgn_pt1 ne 'unk');
       if hier(i) in ('100','110','101','111' '102','112', '103','113') then do; 
         outassgn_pt1= vname(hier(i));
         *OUtcome_Assigned3_Codetype = 'DX'; 
       end;
     end;

     else if rx_n>=1 then do; *no need to loop since only uab has rx;
         outassgn_pt1= 'UAB';
         *OUtcome_Assigned3_Codetype = 'RX';  
     end;
  end;
 end;

  *now step6 - 3b.6 - discordant abortion clean up;
 If outassgn_pt1 in ('SAB' 'IAB' 'UAB' ) then do;
  if sab ne '000' and iab ne '000' then outassgn_pt2='UAB'; *3b.6.1;

  else do ;*3b.6.2 - go back to table 1 for abortion;
         if sab NE ('000') and uab NE ('000') then outassgn_pt2='SAB'; 
    else if iab NE ('000') and uab NE ('000') then outassgn_pt2='IAB';
    else if sab='000' and iab='000' and uab NE ('000') then outassgn_pt2='UAB';

    *else if AEM ne '000' and sab='000' and iab='000' and uab='000' then  outassgn_pt2='UAB';*3b.6.3;*MOVED to step 7;
  end;
 end;

 *step 1b.7 assign final outcome (clean if present else 1st pass which includes concordant);
 If outassgn_pt2 ne '' then Outcome_Assigned3 = outassgn_pt2;
 else outcome_assigned3 = outassgn_pt1;

 *use the highest codetype for outcome type assigned to group;
    *only UDL has procedures, all other delivery types are diagnosis code based;
    *AEM is only dx based but all other outcomes have multiple possible code types;
    *for abortion if both SAB and IAB then otucome would be UAB so ok to use Abortion value (covers all);
      IF Outcome_Assigned3 in ('LBM' 'LBS' 'UDL' 'SB' 'MLS') then Outcome_Assigned_Codetype3= Delivery;
      ELSE IF Outcome_Assigned3 in ('SAB' 'IAB' 'UAB') then Outcome_Assigned_Codetype3= Abortion;
      ELSE IF Outcome_Assigned3 in ('EM' 'AEM') then Outcome_Assigned_Codetype3= Ectopic;

      Outcome_Class_Assigned3 = put(outcome_assigned3,$outclass.);*09.07;
 drop i;
  label
   outcome_assigned3 = "Outcome Assigned (Algorithm3)"
   outcome_assigned_codetype3="Outcome codetype (100=dx, 010=pr, 0001=rx)"
   discordant3 = "Discordant outcome (algorithm3)"
      outcome_class_assigned3="Class of outcome (deliv/abort/ectopic)"
  ;
run;
proc format ;value onepl 1-high='1+';run;
proc freq data= &Newdsn._alg3;table outcome_assigned3 outcome_assigned_codetype3 ;run;

proc freq data= &Newdsn._alg3;table outcome_assigned3 outcome_assigned_codetype3 ;run;


proc freq data= &newdsn._alg3;*where outcome_assigned3='unk';
where outassgn_pt1='UAB' and outcome_assigned3 ne 'UAB';
*table LBM* LBS* MLS* SB* UDL* SAB* IAB* UAB* EM* AEM /list missing;
*table delivery* SAB* IAB* UAB* EM* AEM *pr_n*outassgn_pt1* discordant3 /list missing;
table delivery* SAB* IAB* UAB* EM* AEM  * discordant3 *outcome_assigned3 /list missing;*uab-pr other ab not pr;
format pr_n onepl.;
run;



    ods rtf file = "&outpath.\Step1b_ReviewAlgorithm3_&Newdsn._%sysfunc(date(),yymmddn8.).rtf" style=minimal;

        title1 "Step 1b - Algorithm 3 outcomes" ;
        proc freq data= &newdsn._alg3 ;
         table concordant discordant3 OutAssgn_pt1 * OutAssgn_pt2 *outcome_assigned3 /list missing;
         format discordant3  discord3alg.;
        run;


        title3 ' review discordant';
        proc sql;
         select LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned3,Outcome_Assigned_Codetype3, count(*) as cnt
         from &newdsn._Alg3 where discordant3>0 And Outcome_assigned3 in ('LBM', 'LBS', 'MLS', 'SB', 'UDL')
          group by LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned3,Outcome_Assigned_Codetype3 
            order by calculated cnt desc;

         select LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned3,Outcome_Assigned_Codetype3, count(*) as cnt
         from &newdsn._Alg3 where discordant3>0 And Outcome_assigned3 in ('SAB','IAB','UAB')
          group by LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned3 ,Outcome_Assigned_Codetype3 
            order by calculated cnt desc;

         select LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned3,Outcome_Assigned_Codetype3, count(*) as cnt
         from &newdsn._Alg3 where discordant3>0 And Outcome_assigned3 in ('EM','AEM')
          group by LBM, LBS, MLS, SB, UDL, SAB, IAB, UAB, EM, AEM, outcome_assigned3 ,Outcome_Assigned_Codetype3 
            order by calculated cnt desc;
        quit;


    ods rtf close;
