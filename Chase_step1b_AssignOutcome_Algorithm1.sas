
********************************************************************;
**** STEP 1b - Assigns outcome to each pregnancy outcome group  ****;
**** Outcome Algorithm 1 Applied                                ****;
********************************************************************;
*9.20 add labels;


***step 1b.1 (pt 2) -  discordance defined separately for each algorithm ***;run;;;
/*data og_disc1; *algorithm1 - if not table 1 then discordant;*/
/* set og_disc;*/
/* Discordant1=1;*/
/*run;*/

  
***step 1b.2 (was 3b.2) assign outcomes ; run;

data &NewDsn._Alg1 ; *outcome solely based on hierarchy;
 set &concdsn (in=a)
     &discdsn (in=b)
 ;

 *if a then discordant1=0;
 Discordant1 = b;

 *order using hierarchy defined in step 2 (technically same as outcome_concordant);
 array hier(*) LBM LBS MLS SB UDL SAB IAB UAB EM AEM;

 *outcome assigned based on alg1 hierarchy ;
 *table 1 concordance is IGNORED (no conc/disc) - per step 1b.3;
 *so use the Delivery/Abortion/Ectopic for codetype 08.09;
 Outcome_Assigned1='unk';
 do i=1 to dim(hier) until (Outcome_assigned1 ne 'unk');
   if hier(i) ne '000' then do; 
     Outcome_Assigned1= vname(hier(i));
      *Outcome_Assigned_Codetype1 = put(hier(i), $outcd.);
      IF outcome_assigned1 in ('LBM' 'LBS' 'UDL' 'SB' 'MLS') then Outcome_Assigned_Codetype1= Delivery;
      ELSE IF outcome_assigned1 in ('SAB' 'IAB' 'UAB') then Outcome_Assigned_Codetype1= Abortion;
      ELSE IF outcome_assigned1 in ('EM' 'AEM') then Outcome_Assigned_Codetype1= Ectopic;
   end;
 end;

 Outcome_Class_Assigned1 = put(outcome_assigned1,$outclass.);
 drop i;
  label
   outcome_assigned1 = "Outcome Assigned (Algorithm1)"
   outcome_assigned_codetype1="Outcome codetype (100=dx, 010=pr, 0001=rx)"
   discordant1 = "Discordant outcome (algorithm1)"
      outcome_class_assigned1="Class of outcome (deliv/abort/ectopic)"

  ;
run;


