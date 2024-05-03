
**was Chase_1b1_AssignOutcome_Algorithm1.sas
** changed to keep highest code level for outcome;

********************************************************************;
**** STEP 1b - Assigns outcome to each pregnancy outcome group  ****;
********************************************************************;

*** Step 1b.1 (pt1) - concordant outcomes based on table 1 ***;
*** also Step 1b.2 - assign outcome to concordant for alg 2-4;

 *dont use outcomes based only on Rx (001- these are all UAB) ;run;

 *table 1 concordance with step to assign codetype for the concordant pair;
 *Ignoring medication orders for concordance so using outcomecounts excl Rx;
  %macro conctbl(o1,o2); *table 1 concordance when 2 outcome groups or 3 if 3rd is UAB-001;
    if (outcomegroup_OutcomesNORX_N = 2 And &o1 ne '000' and &o2 ne '000' 
   %IF &o2 = UAB %then %do; And UAB not in  ('001' '002' '003') %END;)
/*    Or (outcomegroup_OutcomesNoRX_N = 3 And &o1 ne '000' and &o2 ne '000' And UAB in ('000','001'))*/
    then do;
      Concordant=1; Outcome_Concordant=upcase("&o1"); 
      Outcome_Concordant_CodeType = 
        cat(max(substr(&o1,1,1),substr(&o2,1,1)),max(substr(&o1,2,1),substr(&o2,2,1)),max(substr(&o1,3,1),substr(&o2,3,1)));
    end;
  %mend;

*macro to call in separate algorithm pgms to split data into outcome concordant and discordant;
%Macro SplitConcordant(grpdsn);

    *for all define concordant outcomes ;;
    data Conc_&GrpDsn. Disc_&GrpDsn.  
          Conc_&GrpDsn.disc (keep=patient_deid outcomegrp concordant delivery: abortion: ectopic: outcome_concordant: pr_n dx_n rx_n)
          ;
     set &grpdsn. ; *OutcomeGroups;

     Concordant=0;  
     Outcome_Concordant='not';
     array x(*) LBM LBS MLS SB UDL  SAB IAB UAB  AEM EM ;*06.01 no AEM;

      If outcomegroup_OutcomesNORX_N in (0, 1) then do; *1 outcome or 2 with uab=rx (0=> only outcome is Rx);
      *If outcomegroup_Outcomes_N = 1 then do;
        concordant=1; *only 1 outcome for group = cannot be discordant;
        do i=1 to dim(x);
          if x(i) ne '000' then do;
           Outcome_Concordant = vname(x(i));  
           Outcome_Concordant_Codetype= x(i);
          end;
        end;
      end;
      *else if outcomegroup_outcomes_N = 2 OR (outcomegroup_outcomes_n= 3 And UAB ne '000') then do;
        *yes if outcome is present, no matter the codetypes found;
        %conctbl(LBM, UDL)
        %ConcTbl(LBS, UDL)
        %ConcTbl(MLS, UDL)
        %ConcTbl(SB , UDL)
        %ConcTbl(SAB, UAB)
        %ConcTbl(IAB, UAB)
        %ConcTbl(EM , AEM)
        ;;;;
      *end;
      drop i;
 
      if concordant=1 then output Conc_&GrpDsn.; *OutcomeGroup_Concordance; 
                      else output Disc_&GrpDsn.; *will use for algorithms 1-4;
      output Conc_&GrpDsn.disc; *just for stats if needed;
    run;


    ods rtf file = "&outpath.\Step1b1_Concordance_&grpdsn._%sysfunc(date(),yymmddn8.).rtf" style=minimal;

        title1 "Step 1b1 - Concordant outcomes";
        proc freq data=conc_&grpdsn.disc ;
         table concordant outcome_concordant outcome_concordant_codetype;
        run;

    ods rtf close;

%mend;
