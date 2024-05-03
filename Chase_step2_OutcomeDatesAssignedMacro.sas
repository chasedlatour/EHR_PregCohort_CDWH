
/*%outdts(7)*/

/**** addition 9.2023 -- Assign date based on outcome assigned (technically STEP 2);*/

** 10.31 revision - using Outcome_Assigned# (LBM,...) Outcome_Assigned_COdetype# (110,...) Outcome_Class_Assigned# (Delivery,Abortion,Ectopic);
** change approach to use table 2 as written (long!) that in the end per CL uses concordance of outcomes;
              /*from 9.22 CHANGE NEEDED - MUST ALSO CHECK THAT DATE IS FOR A CONCORDANT OUTCOME (TABLE 1)*/
              /*                   if group has LBM, LBS, UDL and Assign=LBM then date uses LBM, UDL*/
              /*                   (try setup as macro loop eg dx code lookups)*/
** the concordance pairs from conctbl mac - (LBM, UDL) (LBS, UDL) (MLS, UDL) (SB , UDL) (SAB, UAB) (IAB, UAB) (EM , AEM) ;


%macro outdts(gap);
/**/
/*%let gap=7;*/
/*%let i=1;*/

 proc sql stimer;  
  %do i=1 %to 4;

  
      create table getdate0_&gap._&i as
       select distinct a.patient_deid,dt_outcomegroup_start,dt_outcomegroup_end, outcomegrp, 
              a.outcome_assigned&i.,outcome_assigned_codetype&i., outcome_class_assigned&i, 
              min(case

              /*delivery assigned - pick encounter inpt proc [DelivPR_Inp], then any proc (conc pair), then diag-cd(pair)*/
                       when outcome_class_assigned&i ='Delivery'  and DelivPR_Inp=1 then Enc_Date

                       when Outcome_Assigned&i. in ("LBM") And b.outcome in ("LBM" "UDL") and put(dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("LBS") And b.outcome in ("LBS" "UDL") and put(dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("MLS") And b.outcome in ("MLS" "UDL") and put(dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("SB") And b.outcome in ("SB" "UDL") and put(dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("UDL") And b.outcome in ( "UDL") and put(dxprrx ,$outcdpr.)='1' then Enc_Date
                      
                       when Outcome_Assigned&i. in ("LBM") And b.outcome in ("LBM" "UDL") and put(dxprrx ,$outcddx.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("LBS") And b.outcome in ("LBS" "UDL") and put(dxprrx ,$outcddx.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("MLS") And b.outcome in ("MLS" "UDL") and put(dxprrx ,$outcddx.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("SB") And b.outcome in ("SB" "UDL") and put(dxprrx ,$outcddx.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("UDL") And b.outcome in ( "UDL") and put(dxprrx ,$outcddx.)='1' then Enc_Date

                       /*if assigned outcome is UDL then pick earliest delivery proc date in outcome group or deliv dx date if no proc*/
                       when Outcome_Assigned&i. in ("UDL") And put(b.outcome,$outclass.)='Delivery' and put(dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("UDL") And put(b.outcome,$outclass.)='Delivery' and put(dxprrx ,$outcddx.)='1' then Enc_Date


              /*abortion assigned - pic encounter any [conc] proc, mife/miso, diag*/
                       when Outcome_Assigned&i. in ("SAB") And b.outcome in ("SAB" "UAB") and put(b.dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("IAB") And b.outcome in ("IAB" "UAB") and put(b.dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("UAB") And b.outcome in ("UAB") and put(b.dxprrx ,$outcdpr.)='1' then Enc_Date

                       when Outcome_Assigned&i. in ("SAB" "IAB" "UAB") And b.outcome in ("SAB" "IAB" "UAB") And substr(b.dxprrx,3) in ('2','3') then Enc_Date
                       when Outcome_Assigned&i. in ("SAB" "IAB" "UAB") And b.outcome in ("SAB" "IAB" "UAB") And substr(b.dxprrx,3) in ('1') then Enc_Date

                       when Outcome_Assigned&i. in ("SAB") And b.outcome in ("SAB" "UAB") and put(b.dxprrx ,$outcddx.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("IAB") And b.outcome in ("IAB" "UAB") and put(b.dxprrx ,$outcddx.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("UAB") And b.outcome in ("UAB") and put(b.dxprrx ,$outcddx.)='1' then Enc_Date

                       /*if assigned outcome is UAB then pick earliest abortion proc date in outcome group or abortion dx date if no proc*/
                       when Outcome_Assigned&i. in ("UAB") And put(b.outcome,$outclass.)='Abortion' and put(dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("UAB") And put(b.outcome,$outclass.)='Abortion' and put(dxprrx ,$outcddx.)='1' then Enc_Date


              /*Ectopic - pick encounter any [conc] proc, diag*/
                       when Outcome_Assigned&i. in ("EM", "AEM") And b.outcome in ("EM" "AEM") and put(b.dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("EM", "AEM") And b.outcome in ("EM" "AEM") and put(b.dxprrx ,$outcddx.)='1' then Enc_Date

                   End) as Dt_Outcome_Assigned&i.  format=mmddyy10.,

 1 as eof
       from outcomegroups_&gap._alg&i a left join 
            (select * from encoutcomecleanrows Where DxPrRx ne '000') B on a.patient_deid=b.patient_deid
       where enc_date between dt_outcomegroup_start and dt_outcomegroup_end  
       group by a.patient_deid,outcome_class_assigned&i,dt_outcomegroup_start,dt_outcomegroup_end  ;
;


 create table  getdate_&gap._&i as
  select distinct a.*, 
        min( Case  
              /*delivery assigned - pick encounter inpt proc [DelivPR_Inp], then any proc (conc pair), then diag-cd(pair)*/
                       when outcome_class_assigned&i ='Delivery'  and DelivPR_Inp>0 then 1 /*"INPT-PR"*/

                       when Outcome_Assigned&i. in ("LBM") And b.outcome in ("LBM" "UDL") and put(dxprrx ,$outcdpr.)='1' then 2 /*"DEL-PR"*/
                       when Outcome_Assigned&i. in ("LBS") And b.outcome in ("LBS" "UDL") and put(dxprrx ,$outcdpr.)='1' then 2 /*"DEL-PR"*/
                       when Outcome_Assigned&i. in ("MLS") And b.outcome in ("MLS" "UDL") and put(dxprrx ,$outcdpr.)='1' then 2 /*"DEL-PR"*/
                       when Outcome_Assigned&i. in ("SB") And b.outcome in ("SB" "UDL") and put(dxprrx ,$outcdpr.)='1' then 2 /*"DEL-PR"*/
                       when Outcome_Assigned&i. in ("UDL") And b.outcome in ( "UDL") and put(dxprrx ,$outcdpr.)='1' then 2 /*"DEL-PR"*/
                      
                       when Outcome_Assigned&i. in ("LBM") And b.outcome in ("LBM" "UDL") and put(dxprrx ,$outcddx.)='1' then 3 /*"DEL-DX"*/
                       when Outcome_Assigned&i. in ("LBS") And b.outcome in ("LBS" "UDL") and put(dxprrx ,$outcddx.)='1' then 3 /*"DEL-DX"*/
                       when Outcome_Assigned&i. in ("MLS") And b.outcome in ("MLS" "UDL") and put(dxprrx ,$outcddx.)='1' then 3 /*"DEL-DX"*/
                       when Outcome_Assigned&i. in ("SB") And b.outcome in ("SB" "UDL") and put(dxprrx ,$outcddx.)='1' then 3 /*"DEL-DX"*/
                       when Outcome_Assigned&i. in ("UDL") And b.outcome in ( "UDL") and put(dxprrx ,$outcddx.)='1' then 3 /*"DEL-DX"*/

              /*abortion assigned - pic encounter any [conc] proc, mife/miso, diag*/
                       when Outcome_Assigned&i. in ("SAB") And b.outcome in ("SAB" "UAB") and put(b.dxprrx ,$outcdpr.)='1' then 2 /*"AB-PR"*/
                       when Outcome_Assigned&i. in ("IAB") And b.outcome in ("IAB" "UAB") and put(b.dxprrx ,$outcdpr.)='1' then 2 /*"AB-PR"*/
                       when Outcome_Assigned&i. in ("UAB") And b.outcome in ("UAB") and put(b.dxprrx ,$outcdpr.)='1' then 2 /*"AB-PR"*/

                       when Outcome_Assigned&i. in ("SAB" "IAB" "UAB") And b.outcome in ("SAB" "IAB" "UAB") And substr(b.dxprrx,3) in ('2','3') then 4 /*"AB-MIFE"*/
                       when Outcome_Assigned&i. in ("SAB" "IAB" "UAB") And b.outcome in ("SAB" "IAB" "UAB") And substr(b.dxprrx,3) in ('1') then 5 /* "AB-MISO"*/

                       when Outcome_Assigned&i. in ("SAB") And b.outcome in ("SAB" "UAB") and put(b.dxprrx ,$outcddx.)='1' then 3 /*"AB-DX"*/
                       when Outcome_Assigned&i. in ("IAB") And b.outcome in ("IAB" "UAB") and put(b.dxprrx ,$outcddx.)='1' then 3 /*"AB-DX"*/
                       when Outcome_Assigned&i. in ("UAB") And b.outcome in ("UAB") and put(b.dxprrx ,$outcddx.)='1' then 3 /*"AB-DX"*/

                       when Outcome_Assigned&i. in ("EM", "AEM") And b.outcome in ("EM" "AEM") and put(b.dxprrx ,$outcdpr.)='1' then 2 /*"EM-PR"*/
                       when Outcome_Assigned&i. in ("EM", "AEM") And b.outcome in ("EM" "AEM") and put(b.dxprrx ,$outcddx.)='1' then 3 /*"EM-DX"*/

                   End ) as OutcomeDateAssigned&i.  ,

           1 as eof2
        from  getdate0_&gap._&i A left join    
              (select * from encoutcomecleanrows Where DxPrRx ne '000') B on a.patient_deid=b.patient_deid And Dt_Outcome_Assigned&i. =b.Enc_Date
        group by a.patient_deid,outcome_class_assigned&i,dt_outcomegroup_start,dt_outcomegroup_end  ;
        ;

 **dataset of groups not assigned final date - data cleaning result in UAB/UDL assignments when discordant outcome types;
 create table nodt_&gap.&i. as  select a.patient_deid,a.dt_outcomegroup_start,a.dt_outcomegroup_end, a.outcomegrp,  
              a.outcome_assigned&i.,a.outcome_assigned_codetype&i., a.outcome_class_assigned&i, b.*       
 from outcomegroups_&gap._alg&i a left join getdate0_&gap._&i. x on a.patient_deid=x.patient_deid and a.outcomegrp=x.outcomegrp
      left join (select * from encoutcomecleanrows Where DxPrRx ne '000') B on a.patient_deid=b.patient_deid
       where x.dt_outcome_assigned&i=. and b.enc_date between a.dt_outcomegroup_start and a.dt_outcomegroup_end
 ;
 
    select outcome_assigned&i., count(distinct cat(patient_deid,outcomegrp)) as NoDt_Alg&i._&gap.days from nodt_&gap.&i. group by outcome_assigned&i
 ;

  %end;
  ; 
 quit;

%mend; 
proc format;
 *outcome assigned date assignment;
 value outdtasgn 1='INPT-PR' 2='PR' 3='DX' 5='MISO' 4='MIFE';
run;

/*%outdts(30)*/


