/******************************************************************************

Program: chase_step2_OutcomeDatesAssignedMacro.sas

Programmer: Sharon 

Purpose: This program creates a macro that assigned pregnancy outcome dates
to the pregnancy outcome groups (i.e., Step 2 of the cohort derivation).

Modifications:
	- 04-29-24: CDL made comments and applied standardized set up.

*******************************************************************************/


%macro outdts(gap);

/*Testing*/
/*%let gap=7;*/
/*%let i=1;*/

 	proc sql stimer;  

		%do i=1 %to 4; /*Do this once for each algorithm*/

		/*Create an initial dataset with the pregnancy outcome dates*/
      	create table getdate0_&gap._&i as
       	select distinct a.patient_deid, dt_outcomegroup_start, dt_outcomegroup_end, outcomegrp, 
              a.outcome_assigned&i., outcome_assigned_codetype&i., outcome_class_assigned&i, /*Maybe dont need the a. for outcome_assigned?*/
              min(case

					   /**DELIVERY outcome dates**/

              		   /*delivery assigned - pick encounter inpt proc [DelivPR_Inp], then any proc (conc pair), then diag-cd(pair)*/
			  		   /*If inpatient delivery w procedure code, then outcome date is the encounter date*/
                       when outcome_class_assigned&i ='Delivery'  and DelivPR_Inp=1 then Enc_Date

					   /*Grab the first concordant procedure code -- deliveries (order doesnt matter from abortions since based on outcome_assigned*/
                       when Outcome_Assigned&i. in ("LBM") And b.outcome in ("LBM" "UDL") and put(dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("LBS") And b.outcome in ("LBS" "UDL") and put(dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("MLS") And b.outcome in ("MLS" "UDL") and put(dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("SB")  And b.outcome in ("SB" "UDL") and put(dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("UDL") And b.outcome in ( "UDL") and put(dxprrx ,$outcdpr.)='1' then Enc_Date

					   /*CDL: MODIFICATION -- Prioritize non-UDL delivery pr codes over UDL dx codes -- DISCUSS*/
					   /*NOTE: Some UDLs dont have any UDL pr codes because they had discordant delivery codes. Deal with that here.
                       If assigned outcome is UDL then pick earliest delivery proc code in outcome group*/
                       when Outcome_Assigned&i. in ("UDL") And put(b.outcome,$outclass.)='Delivery' and put(dxprrx ,$outcdpr.)='1' then Enc_Date
                      
                       when Outcome_Assigned&i. in ("LBM") And b.outcome in ("LBM" "UDL") and put(dxprrx ,$outcddx.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("LBS") And b.outcome in ("LBS" "UDL") and put(dxprrx ,$outcddx.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("MLS") And b.outcome in ("MLS" "UDL") and put(dxprrx ,$outcddx.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("SB") And b.outcome in ("SB" "UDL") and put(dxprrx ,$outcddx.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("UDL") And b.outcome in ( "UDL") and put(dxprrx ,$outcddx.)='1' then Enc_Date

					   /*NOTE: Some UDLs dont have any UDL dx codes because they had discordant delivery codes. Deal with that here.
                       If assigned outcome is UDL then pick earliest delivery dx code in outcome group*/
                       when Outcome_Assigned&i. in ("UDL") And put(b.outcome,$outclass.)='Delivery' and put(dxprrx ,$outcddx.)='1' then Enc_Date


					   /**ABORTION outcome dates**/
              		   /*abortion assigned - pic encounter any [conc] proc, mife/miso, diag*/

					   /*proc codes*/
                       when Outcome_Assigned&i. in ("SAB") And b.outcome in ("SAB" "UAB") and put(b.dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("IAB") And b.outcome in ("IAB" "UAB") and put(b.dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("UAB") And b.outcome in ("UAB") and put(b.dxprrx ,$outcdpr.)='1' then Enc_Date

					   /*CDL: MODIFICATION -- Same as done for deliveries*/
					   /*NOTE: Some UABs dont have UAB procedure codes but may have procedure codes for a SAB or IAB. We deal with those 
					   UABs here. If a UAB doesnt have concordant proc codes, then first abortion proc code*/
					   when Outcome_Assigned&i. in ("UAB") And put(b.outcome,$outclass.)='Abortion' and put(dxprrx ,$outcdpr.)='1' then Enc_Date

					   /*med orders*/
                       when Outcome_Assigned&i. in ("SAB" "IAB" "UAB") And b.outcome in ("SAB" "IAB" "UAB") And substr(b.dxprrx,3) in ('2','3') then Enc_Date
                       when Outcome_Assigned&i. in ("SAB" "IAB" "UAB") And b.outcome in ("SAB" "IAB" "UAB") And substr(b.dxprrx,3) in ('1') then Enc_Date

					   /*dx codes*/
                       when Outcome_Assigned&i. in ("SAB") And b.outcome in ("SAB" "UAB") and put(b.dxprrx ,$outcddx.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("IAB") And b.outcome in ("IAB" "UAB") and put(b.dxprrx ,$outcddx.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("UAB") And b.outcome in ("UAB") and put(b.dxprrx ,$outcddx.)='1' then Enc_Date

                       /*if assigned outcome is UAB then pick earliest abortion dx date if no other information*/
                       when Outcome_Assigned&i. in ("UAB") And put(b.outcome,$outclass.)='Abortion' and put(dxprrx ,$outcddx.)='1' then Enc_Date

					   /**ECTOPIC/MOLAR OUTCOMES**/

              		   /*Ectopic - pick encounter any [conc] proc, diag*/
                       when Outcome_Assigned&i. in ("EM", "AEM") And b.outcome in ("EM" "AEM") and put(b.dxprrx ,$outcdpr.)='1' then Enc_Date
                       when Outcome_Assigned&i. in ("EM", "AEM") And b.outcome in ("EM" "AEM") and put(b.dxprrx ,$outcddx.)='1' then Enc_Date

                   End) as Dt_Outcome_Assigned&i.  format=mmddyy10.,
				   1 as eof
       from outcomegroups_&gap._alg&i a 
	   left join (select * from encoutcomecleanrows Where DxPrRx ne '000') B 
	   on a.patient_deid=b.patient_deid
       where enc_date between dt_outcomegroup_start and dt_outcomegroup_end  
       group by a.patient_deid, outcome_class_assigned&i, dt_outcomegroup_start, dt_outcomegroup_end;
	   ;

		/*Create an indicator variable for how the pregnancy outcome dates were assigned*/
 		create table  getdate_&gap._&i as
  		select distinct a.*, 
        	min( Case  
              		   /*DELIVERY assigned - pick encounter inpt proc [DelivPR_Inp], then any proc (conc pair), then diag-cd(pair)*/
                       when outcome_class_assigned&i ='Delivery'  and DelivPR_Inp>0 then 1 /*"INPT-PR"*/

                       when Outcome_Assigned&i. in ("LBM") And b.outcome in ("LBM" "UDL") and put(dxprrx ,$outcdpr.)='1' then 2 /*"DEL-PR"*/
                       when Outcome_Assigned&i. in ("LBS") And b.outcome in ("LBS" "UDL") and put(dxprrx ,$outcdpr.)='1' then 2 /*"DEL-PR"*/
                       when Outcome_Assigned&i. in ("MLS") And b.outcome in ("MLS" "UDL") and put(dxprrx ,$outcdpr.)='1' then 2 /*"DEL-PR"*/
                       when Outcome_Assigned&i. in ("SB")  And b.outcome in ("SB" "UDL") and put(dxprrx ,$outcdpr.)='1' then 2 /*"DEL-PR"*/
                       when Outcome_Assigned&i. in ("UDL") And b.outcome in ( "UDL") and put(dxprrx ,$outcdpr.)='1' then 2 /*"DEL-PR"*/

					   /*CDL: ADDED -- I think we still need to deal with those UDLs that did not have a non-UDL code*/
					   when Outcome_Assigned&i. in ("UDL") And put(b.outcome,$outclass.)='Delivery' and put(dxprrx ,$outcddx.)='1' then 2 /*"DEL-PR"*/
                      
                       when Outcome_Assigned&i. in ("LBM") And b.outcome in ("LBM" "UDL") and put(dxprrx ,$outcddx.)='1' then 3 /*"DEL-DX"*/
                       when Outcome_Assigned&i. in ("LBS") And b.outcome in ("LBS" "UDL") and put(dxprrx ,$outcddx.)='1' then 3 /*"DEL-DX"*/
                       when Outcome_Assigned&i. in ("MLS") And b.outcome in ("MLS" "UDL") and put(dxprrx ,$outcddx.)='1' then 3 /*"DEL-DX"*/
                       when Outcome_Assigned&i. in ("SB") And b.outcome in ("SB" "UDL") and put(dxprrx ,$outcddx.)='1' then 3 /*"DEL-DX"*/
                       when Outcome_Assigned&i. in ("UDL") And b.outcome in ("UDL") and put(dxprrx ,$outcddx.)='1' then 3 /*"DEL-DX"*/

					   /*CDL: ADDED -- Same as above*/
					   when Outcome_Assigned&i. in ("UDL") And put(b.outcome,$outclass.)='Delivery' and put(dxprrx ,$outcddx.)='1' then 3 /*"DEL-DX"*/

             		   /*ABORTION assigned - pic encounter any [conc] proc, mife/miso, diag*/
                       when Outcome_Assigned&i. in ("SAB") And b.outcome in ("SAB" "UAB") and put(b.dxprrx ,$outcdpr.)='1' then 2 /*"AB-PR"*/
                       when Outcome_Assigned&i. in ("IAB") And b.outcome in ("IAB" "UAB") and put(b.dxprrx ,$outcdpr.)='1' then 2 /*"AB-PR"*/
                       when Outcome_Assigned&i. in ("UAB") And b.outcome in ("UAB") and put(b.dxprrx ,$outcdpr.)='1' then 2 /*"AB-PR"*/

					   /*CDL: ADDED -- Same as above.*/
					   when Outcome_Assigned&i. in ("UAB") And put(b.outcome,$outclass.)='Abortion' and put(dxprrx ,$outcdpr.)='1' then 2 /*"AB-PR"*/

                       when Outcome_Assigned&i. in ("SAB" "IAB" "UAB") And b.outcome in ("SAB" "IAB" "UAB") And substr(b.dxprrx,3) in ('2','3') then 4 /*"AB-MIFE"*/
                       when Outcome_Assigned&i. in ("SAB" "IAB" "UAB") And b.outcome in ("SAB" "IAB" "UAB") And substr(b.dxprrx,3) in ('1') then 5 /* "AB-MISO"*/

                       when Outcome_Assigned&i. in ("SAB") And b.outcome in ("SAB" "UAB") and put(b.dxprrx ,$outcddx.)='1' then 3 /*"AB-DX"*/
                       when Outcome_Assigned&i. in ("IAB") And b.outcome in ("IAB" "UAB") and put(b.dxprrx ,$outcddx.)='1' then 3 /*"AB-DX"*/
                       when Outcome_Assigned&i. in ("UAB") And b.outcome in ("UAB") and put(b.dxprrx ,$outcddx.)='1' then 3 /*"AB-DX"*/

					   /*CDL: ADDED -- Same as above*/
					   when Outcome_Assigned&i. in ("UAB") And put(b.outcome,$outclass.)='Abortion' and put(dxprrx ,$outcddx.)='1' then 3 /*"AB-DX"*/

                       when Outcome_Assigned&i. in ("EM", "AEM") And b.outcome in ("EM" "AEM") and put(b.dxprrx ,$outcdpr.)='1' then 2 /*"EM-PR"*/
                       when Outcome_Assigned&i. in ("EM", "AEM") And b.outcome in ("EM" "AEM") and put(b.dxprrx ,$outcddx.)='1' then 3 /*"EM-DX"*/

                   End ) as OutcomeDateAssigned&i. ,
				   1 as eof2
        from  getdate0_&gap._&i A 
		left join (select * from encoutcomecleanrows Where DxPrRx ne '000') B 
		on a.patient_deid=b.patient_deid And Dt_Outcome_Assigned&i.=b.Enc_Date
        group by a.patient_deid,outcome_class_assigned&i,dt_outcomegroup_start,dt_outcomegroup_end  ;
        ;

		/*Check: Output data with groups not assigned a final pregnancy outcome date.*/
		create table nodt_&gap.&i. as  
		select a.patient_deid, a.dt_outcomegroup_start, a.dt_outcomegroup_end, a.outcomegrp,  
               a.outcome_assigned&i., a.outcome_assigned_codetype&i., a.outcome_class_assigned&i, b.*       
 		from outcomegroups_&gap._alg&i a 
		left join getdate0_&gap._&i. x 
		on a.patient_deid=x.patient_deid and a.outcomegrp=x.outcomegrp
      	left join (select * from encoutcomecleanrows Where DxPrRx ne '000') B 
		on a.patient_deid=b.patient_deid
       	where x.dt_outcome_assigned&i=. and b.enc_date between a.dt_outcomegroup_start and a.dt_outcomegroup_end
 		;
 
    	select outcome_assigned&i., count(distinct cat(patient_deid,outcomegrp)) as NoDt_Alg&i._&gap.days 
		from nodt_&gap.&i. group by outcome_assigned&i
 		;

  	%end;
  	; 
 	quit;

%mend; 
proc format;
 	*outcome assigned date assignment;
 	value outdtasgn 1='INPT-PR' 2='PR' 3='DX' 4='MIFE' 5='MISO';
run;





/*OLD NOTES*/


/**** addition 9.2023 -- Assign date based on outcome assigned (technically STEP 2);*/

** 10.31 revision - using Outcome_Assigned# (LBM,...) Outcome_Assigned_COdetype# (110,...) Outcome_Class_Assigned# (Delivery,Abortion,Ectopic);
** change approach to use table 2 as written (long!) that in the end per CL uses concordance of outcomes;
              /*from 9.22 CHANGE NEEDED - MUST ALSO CHECK THAT DATE IS FOR A CONCORDANT OUTCOME (TABLE 1)*/
              /*                   if group has LBM, LBS, UDL and Assign=LBM then date uses LBM, UDL*/
              /*                   (try setup as macro loop eg dx code lookups)*/
** the concordance pairs from conctbl mac - (LBM, UDL) (LBS, UDL) (MLS, UDL) (SB , UDL) (SAB, UAB) (IAB, UAB) (EM , AEM) ;
