/******************************************************************************

Program: chase_Step1and2_PregnancyOutcomeGroups.sas
    (used chase_1a_ConcordanceAndSharedFormatStatements.sas as base and added
     call to chase_1a_createoutcomegroups, kept calls to separate algorithms);

Programmer: Sharon 

Purpose: Progra pregnancy outcomes table steps 1a-1b8 of pregnancy algorithm, 
         calling programs that apply different steps
         Formats shared by all outcome algorithms and macros for shared
         datasteps, including Concordant Outcomes

*******************************************************************************/

*Specify options;

ods listing;
options nocenter  nodate dlcreatedir;

%let xdr= %str(\\ad.unc.edu\med\tracs\groups\Research\CDWH\Latour_Chase_22-0689\);
libname int "&xdr./DATA/20230328_Data_Pull_01/analysis/int_20230516" access=r;

libname out "&xdr.DATA/20230328_Data_Pull_01/analysis/int_20230516/%sysfunc(date(),yymmddn8.)/";
    
libname oldint "&xdr\data\20221220\analysis\int_20230329" access=r;
libname base "&xdr.\DATA\20230328_Data_Pull_01\analysis" access=r;

*algorithm path;
%let lalgpath=C:\Users\peacocks\OneDrive - University of North Carolina at Chapel Hill\_projects\mollie\;
%let algpath=&xdr.\programs\sharon\preg_outcomes;

%let outpath = &algpath.;


*** Call formats needed for algorithm ***;
%inc "&algpath.\chase_Step0_FormatStatements.sas";

*** Get the outcomes source datafile(s) ***;

data codeoutcome;
 set int.codeoutcome int.codemed (in=a);
  if a then codetype='med';
run;


********************************************************************;
**** STEP 1a - Group outcome encounters w/in +/- [EncGap] days  ****;
********************************************************************;
/*%inc "&algpath./chase_Step1a_CreateOutcomeGroups.sas";*/
%inc "&algpath./chase_Step1a_OutcomeGroupsUsingCleanedEncounters.sas";
   *preps the input file (codeoutcome to outcomesbytype);
   *and loads macro OutcGrp to create outcome groups;
   *final dsn OutcomeGroups_&EncGap (work);
   *output OutcomeGroups_Within&encgap.Days_%sysfunc(date(),yymmddn8.).rtf (outpath);

%OutcGrp(EncGap = 7)
%OutcGrp(EncGap = 30)

%OUTCGRP(ENCGAP = 14)
;

    ***NOTE sasve encoutcomecleanrows;
    data out.EncOutcomeCleanRows;set encoutcomecleanrows;run;

********************************************************************;
**** STEP 1b - Assigns outcome to each pregnancy outcome group  ****;
********************************************************************;

*** Step 1b.1 (pt1) - concordant outcomes based on table 1 ***;run;
/*%inc "&algpath.\chase_Step1b1_ConcordanceFlagMacro.sas";*/
%inc "&algpath.\chase_Step1b_AssignAllConcordanceFlagMacro.sas";  *name change only;
   *takes the outcomegroups (1a) and identifies concordant outcomes;
   *call macro SplitConcordant(GrpDsn) to split outcomegroups into;
   *concordant (conc_grpdsn) and discordant (disc_GrpDsn) groups;

%MACRO RUNALG(gap);

    %SplitConcordant(OutcomeGroups_&gap.);


    *** Step 1b.1 (pt2) - discordant outcomes algorithms 1-4 ***;run;
       *each algorithm uses same concordant (concdsn) and discordant data;
       *and creates dataset newdsn with suffix alg1 - alg4;
        %let concdsn= CONC_OUTCOMEGROUPS_&gap. ;
        %let discdsn= DISC_OUTCOMEGROUPS_&gap. ;
         %let newdsn= %substr(&concdsn,6);

        **Outcome Algorithm 1 Applied**;
         *creates newdsn_alg1;
            %inc "&algpath.\chase_step1b_AssignOutcome_Algorithm1.sas";

        **Outcome Algorithm 2 Applied**;
         *creates newdsn_alg2;
            %inc "&algpath.\chase_step1b_AssignOutcome_Algorithm2.sas";

        **Outcome Algorithm 3 Applied**;
         *creates newdsn_alg3;
            %inc "&Algpath.\chase_step1b_AssignOutcome_Algorithm3.sas";

        **Outcome Algorithm 4 Applied**;
         *creates newdsn_alg4;
            %inc "&Algpath.\chase_step1b_AssignOutcome_Algorithm4.sas";
%mend;
  %Runalg(7)
  %Runalg(14)
  %Runalg(30)

********************************************************************;
**** STEP 2 - Assigns outcome dates based on findings of Algs  ****;
********************************************************************;

**** merge the outcome algorithms - process macro OutDts ****;
    %inc "&Algpath.\chase_step2_OutcomeDatesAssignedMacro.sas";;;;

    %outdts(7);
    %outdts(14);
    %outdts(30);


********************************************************************;
**** STEPS 1+2 Results combined - save OutcomeAssigned Datasets ****;
********************************************************************;

*MOD 11.02 - save outcomedateassigned as descriptive (inpt-proc, etc) using format created in _step2 pgm;
%MACRO SAVERPTIT(gap);
    proc sql;
     create table out.OutcomeAssigned_&gap. as
      select a.* , g1.outcome_concordant label="Concordant outcome (table1)",
             g1.outcome_concordant_codetype label="Concordant outcome codetype (100=dx, 010=pr, 001=rx)", 
             g1.discordant1, g1.outcome_assigned1, g1.outcome_assigned_codetype1,g1.outcome_class_assigned1,
               gd1.Dt_Outcome_Assigned1,OutcomeDateAssigned1, 
             g2.discordant2, hier2_1, g2.outcome_assigned2, g2.outcome_assigned_codetype2, g2.outcome_class_assigned2,
               gd2.Dt_Outcome_Assigned2,OutcomeDateAssigned2,
             g3.discordant3,  g3.outcome_assigned3, g3.outcome_assigned_codetype3, g3.outcome_class_assigned3,
                Dt_Outcome_Assigned3,OutcomeDateAssigned3 ,
             g4.discordant4,  g4.outcome_assigned4, g4.outcome_assigned_codetype4, g4.outcome_class_assigned4,
                Dt_Outcome_Assigned4,OutcomeDateAssigned4 

      from outcomegroups_&gap. A 
       left join OUTCOMEGROUPS_&gap._alg1 g1 on a.patient_deid = g1.patient_deid and a.outcomegrp= g1.outcomegrp
        left join getdate_&gap._1 gd1 on a.patient_deid= gd1.patient_deid and a.outcomegrp= gd1.outcomegrp

       left join OUTCOMEGROUPS_&gap._alg2 g2 on a.patient_deid = g2.patient_deid and a.outcomegrp= g2.outcomegrp
        left join getdate_&gap._2 gd2 on a.patient_deid= gd2.patient_deid and a.outcomegrp= gd2.outcomegrp
       left join OUTCOMEGROUPS_&gap._alg3 g3 on a.patient_deid = g3.patient_deid and a.outcomegrp= g3.outcomegrp
        left join getdate_&gap._3 gd3 on a.patient_deid= gd3.patient_deid and a.outcomegrp= gd3.outcomegrp
       left join OUTCOMEGROUPS_&gap._alg4 g4 on a.patient_deid = g4.patient_deid and a.outcomegrp= g4.outcomegrp
        left join getdate_&gap._4 gd4 on a.patient_deid= gd4.patient_deid and a.outcomegrp= gd4.outcomegrp
    ;
    quit;


    ods rtf file = "&outpath.\Step1_AlgorithmAssignments._Gap&gap.d_%sysfunc(date(),yymmddn8.).rtf" style=minimal;

    title1 "Step 1 - Algorithm Assignments";
    proc tabulate data=out.outcomeassigned_&gap. missing format=8.1;
     class outcome_assigned:;
      table outcome_assigned1 all, n colpctn /nocellmerge;
      table outcome_assigned2 all, n colpctn/nocellmerge;;
      table outcome_assigned3 all, n colpctn/nocellmerge;;
      table outcome_assigned4 all, n colpctn/nocellmerge;;
      
      table outcome_assigned1*outcome_assigned_codetype1, n pctn<outcome_assigned_codetype1>/  nocellmerge;
      table outcome_assigned2*outcome_assigned_codetype2, n pctn<outcome_assigned_codetype2> / nocellmerge;
      table outcome_assigned3*outcome_assigned_codetype3, n pctn<outcome_assigned_codetype3> / nocellmerge;
      table outcome_assigned4*outcome_assigned_codetype4, n pctn<outcome_assigned_codetype4>/ nocellmerge;
    format outcome_assigned_codetype: $outcd.;
    run;

    proc sql;
     create table assignedcounts_&gap. as
      select outcome_assigned1 as outcome, 1 as algorithm,put(outcome_assigned_codetype1,$outcd.) as Codetype, count(*) as cnt
      from out.outcomeassigned_&gap. group by outcome_assigned1,calculated codetype
      Union
      select outcome_assigned2 as outcome, 2 as algorithm,put(outcome_assigned_codetype2,$outcd.) as Codetype, count(*) as cnt
      from out.outcomeassigned_&gap. group by outcome_assigned2,calculated codetype
      Union
      select outcome_assigned3 as outcome, 3 as algorithm,put(outcome_assigned_codetype3,$outcd.) as Codetype, count(*) as cnt
      from out.outcomeassigned_&gap. group by outcome_assigned3,calculated codetype
      Union
      select outcome_assigned4 as outcome, 4 as algorithm,put(outcome_assigned_codetype4,$outcd.) as Codetype, count(*) as cnt
      from out.outcomeassigned_&gap. group by outcome_assigned4,calculated codetype
      Union
      select outcome_assigned1 as outcome, 1 as algorithm,'all' as Codetype, count(*) as cnt
      from out.outcomeassigned_&gap. group by outcome_assigned1,calculated codetype
      Union
      select outcome_assigned2 as outcome, 2 as algorithm,'all' as Codetype, count(*) as cnt
      from out.outcomeassigned_&gap. group by outcome_assigned2,calculated codetype
      Union
      select outcome_assigned3 as outcome, 3 as algorithm,'all' as Codetype, count(*) as cnt
      from out.outcomeassigned_&gap. group by outcome_assigned3,calculated codetype
      Union
      select outcome_assigned4 as outcome, 4 as algorithm,'all' as Codetype, count(*) as cnt
      from out.outcomeassigned_&gap. group by outcome_assigned4,calculated codetype
    ;
    proc tabulate data=assignedcounts_&gap.;
     class outcome algorithm codetype;
     var cnt;
      table outcome*codetype , algorithm*cnt*(sum);
    run;

    proc sql;
     create table assignedcountpts_&gap. as
      select distinct outcomegrp,outcome_assigned1 as outcome, 1 as algorithm,put(outcome_assigned_codetype1,$outcd.) as Codetype, patient_deid
      from out.outcomeassigned_&gap. group by outcome_assigned1,calculated codetype
      Union
      select outcomegrp,outcome_assigned2 as outcome, 2 as algorithm,put(outcome_assigned_codetype2,$outcd.) as Codetype, patient_deid
      from out.outcomeassigned_&gap. group by outcome_assigned2,calculated codetype
      Union
      select outcomegrp,outcome_assigned3 as outcome, 3 as algorithm,put(outcome_assigned_codetype3,$outcd.) as Codetype, patient_deid
      from out.outcomeassigned_&gap. group by outcome_assigned3,calculated codetype
      Union
      select outcomegrp,outcome_assigned4 as outcome, 4 as algorithm,put(outcome_assigned_codetype4,$outcd.) as Codetype, patient_deid
      from out.outcomeassigned_&gap. group by outcome_assigned4,calculated codetype
    ;
    proc tabulate missing;
     class outcome algorithm codetype;
      table outcome*codetype all, algorithm*(n colpctn);
    run;

   ods rtf close;


%mend;
    %saveRptit(7)
    %saveRptit(14)
    %saveRptit(30)


/* data x;set encoutcomecleanrows;where patient_deid='Z100181';run;*/
/* proc freq data=getdate;table outcome_assigned1*outcome_assigned_codetype1;where outcome='';run;*/





