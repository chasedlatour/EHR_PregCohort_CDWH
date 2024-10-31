/*******************************************************************************************
Program: Chase_Step8complex.sas  
Programmer: Sharon

Purpose: Complex method for identify ing pregnancies defined by Prenatal encounters ONLY 
(i.e., pregnancies without pregnancy outcome groups)

Modifications:
	- 07-27-24: CDL reviewed the code, made comments, reformatted.
    - 07-31-24: sph/cdl review and modification including altering PG group window from
                286 days to whatever is set for macrovar POW in Step3thru10 master pgm
    -08.05.24: sph removed libnames and some commented code and correction to output dsn
	-10.17.2024: CDL reviewed the code and corrected two bugs, documented by date
*******************************************************************************************/






 *** steps summary ***
 *** 1. find ^provisional^ GA values for remainig prenatal encounters not linked to a pregancy;
 *** 2. for ;


        /*Macro vars:*/
/*         %let AlgDsn= 7_1;*/
/*         %let Dayadj = 60 ;     *needed for 8d.4a;*/
/*         %let Num = 50;  *this will be set in steps3thru10;*/
/* %let pospw = 140;*/



*************************************************************;
*************************************************************;
** PREP1 - get gestational age prenatal encounters  and the  ;
**        prenatal info for algorithm of interest ;
*************************************************************;
*************************************************************;

data gestagepren;
set int.gestagepren_step8;
run;



/*%macro Step8_complex(dayadj=60) ;*/

 %Macro Step8_Complex ;  *07.06 remove the dayadj macro as it overwrites macro call from main pgm;



*************************************************************;
*************************************************************;
**
** Start finding prenatal-only pregnancies the Complex way  ;
**
*************************************************************;
*************************************************************;


*************************************************************;
** 8a - Assign provisional GA to prenatal encounters       ;
*************************************************************;

%global leftover SUBS;
/*%let p=1;*/

    %DO p=1 %to 110;  *large number - dont expect to repeat that many times;


     %if &p = 1 %then %do;
        %goto FindPreg;
     %end;
     %else %if &leftover =0 %then %do;
        %goto WrapUp;
     %end;
     %else %do;
        %goto FindPreg;
     %end;

     %FindPreg:
       
	 
        *** identify prenatal groups - those w/in 286d window (will use later);
        *** (keep all encounters as each pren-enc are assigned a gest-age value);

	 	/*For the first round of evaluation, grab all of the prenatal encounters that 
	 	are not linked to at least one pregnancy outcome group.*/

        *07.31 - Focus on 1 286d group at a time, those not w/in window will be saved for next round;
        *        i.e. move to Leftover dsn instead of adding PG+1 groups. Add Leftovers dsn to NewInd dsns (below);

        data rpgroups_&p. (label="prenatal encounters that fall w/in &POW.d window")
             leftover_&p. (label="prenatals not in &POW.d")
             ;
        %if &p. = 1 %then %do;
         set Prenatal_nooutc ;
        %end;
        %else %do; 
         set remaining_%eval(&p. -1)         ;
        %end;

		  /*Group together all those prenatal encounters that are within 286 days of each other,
			indexing on the first prenatal encounter to set this time frame.*/
          by patient_deid dt_pren_enc; 
          if first.patient_deid then do;
            PG=1;
            PGCnt=0;
            Dt_PG_Index = dt_pren_enc; /*Indexing prenatal encounter date*/
            Dt_POWd = dt_pren_enc + &POW. ; /*Date 286 days after the indexing prenatal encounter*/
            Dt_PG_Last = dt_pren_enc; /*Currently, only the first prenatal encounter included in the pregnancy*/
          end;
           if Dt_PG_Index le dt_pren_enc le dt_POWd then do; /*Grab all those encounters 286 days after the indexing prenatal encounter*/
            PG = pg+0; /*Potentially part of the same pregnancy*/
            PgCnt+1 ;
            Dt_PG_Last = dt_pren_enc;
             Output rpgroups_&p. ;
           end;
           ELSE output leftover_&p. ;*07.31;


           retain pg pgcnt Dt_PG_Index dt_POWd;
           Format Dt_PG_Index Dt_PG_last dt_POWd date.;
           label pg="Prenatal Grouping" 
				 pgcnt="Number of prenatal dates in grouping"
                 Dt_PG_Index = "Index Prenatal encounter date (1st in &POW.d group)"
                 dt_PG_Last = "Last Prenatal encounter date (&POW.d group)"
             ;
         run;


/*CDL: COMMENTED OUT -- 10.17.2024 - Not used elsewhere*/
/*        /*Join together the information for the prenatal encounters with the information about gestational ages at those */
/*		 encounters. -- This is done on the date level*/*/
/*        proc sql;*/
/*        	create table rpgroups_gax_&p. as*/
/*          	select distinct a.*, b.*, */
/*                 missing(b.dt_gaenc) = 0 as AnyGACode */
/*          	from rpgroups_&p. a */
/*            left join   gestagepren   b*/
/**/
/*            on a.patient_deid=b.patient_deid and dt_pren_enc = dt_gaenc*/
/*			;*/
/*        	quit;*/


       *** link prenatals to ga encs with Ok codes on same date. ;
       *** if gaenc date links 2+ times there is different gest-age info ;
       *** must find the GA enc date with best age info for that prenatal encounter date;
            **if just 1 GA encounter then use that to assign ^provisional age^ values;
            **if 2+ choose highest hierarchy;
            **if 2+ and no highest then smallest range;
            **if 2+ and no smallest range then highest age_days;
            **actual GA enc date not important now (same as prenatal date) so could drop that field;

        proc sql;
        	create table rpgroups_enc_&p.   as 
         	select *, 

                case when countAgeRows = 0 then 9                                   /*encounter date did not have GA enc*/
                      when countAgeRows = 1 then 2                                  /*only 1 GA enc for pren-enc*/
                      when Top_PrenGAHier= gahier and tophier1agerow = 1 then 3    /*use enc with top hierarchy in 8a*/
                      when smallestagerowsrange =1 then 4                           /*smallest range for top hier*/
                      when HighestAgeRowsDays =1 then 5                             /*highest age days*/
                end  as AgeRowPick                                                  
                /*using 9 for 0 GA so not picked when PG-Index has 2+ Pren-enc dates and 1 has no GA*/

         	From ( 
            	/*again for the prenatal encounter date, flag for whether 1 rec for highest/best hier level*/
                SELECT * ,  min(GAHier) as Top_PrenGAHier ,
                        case when calculated Top_PrenGAHier=1 and Count_PrenAgeRows1=1 then 1
                           when calculated Top_PrenGAHier=2 and Count_PrenAgeRows2=1 then 1
                           when calculated Top_PrenGAHier=3 and Count_PrenAgeRows3=1 then 1
                           when calculated Top_PrenGAHier=4 and Count_PrenAgeRows4=1 then 1
                           else 0 end as TopHier1AgeRow /*1 agerow for top hierarchy*/ ,

                        case when calculated Top_PrenGAHier= gahier 
                                  And minmax_ga_range=min(minmax_ga_range) then 1 
                            else 0 end as SmallestAgeRowsRange /*smallest range for top hierarchy row*/,
                        case when calculated Top_PrenGAHier= gahier 
                                  And gestational_age_days=max(gestational_age_days) then 1
                            else 0 end  as HighestAgeRowsDays  /*highest GA days for top hierarchy row*/ 

              	From ( 
                	/*for the prenatal encounter, create binary vars for hierarchy level met*/
                    select *, count(dt_gaenc) as CountAgeRows ,
                              count( case when GAHier=1 then dt_gaenc end) as Count_PrenAgeRows1,
                              count( case when GAHier=2 then dt_gaenc end) as Count_PrenAgeRows2,
                              count( case when GAHier=3  then dt_gaenc end) as Count_PrenAgeRows3,
                              count( case when GAHier=4 then dt_Gaenc end) as Count_PrenAgeRows4 

                    from ( /*add GA info to prenatal encounter dates, 1 row added for each GA data rec*/
                            select distinct a.*, b.*, missing(b.dt_gaenc) = 0 as AnyGACode  
                            from rpgroups_&p. a 
							left join gestagepren b
                            on a.patient_deid=b.patient_deid and dt_pren_enc = dt_gaenc
                           )
                    group by patient_deid, dt_pren_enc
                 	) 
              	group by patient_deid,dt_pren_enc
              	)
          	group by patient_deid, dt_pren_enc

          	/*limit table to GA enc with best provisional age data for the prenatal encounter*/
          	having calculated agerowpick = min(agerowpick)
     		;

/*		CDL: COMMENTED OUT 10.17.2024*/
	    /*Check for duplicate rows.*/
/*      	create table dupsck as */
/*	  		select * */
/*			from rpgroups_enc_&p. */
/*			group by patient_deid, dt_pren_enc */
/*			having count(*)>1*/
/*			;*/
      	*should be 0 - only 1 age row selected for each prenatal encounter;

      QUIT; *ends 8a;


    **************************************************************************;
    **************************************************************************;
    ** Now focus on prenatal groups (PG), anchoring on Index Prenatal date    ;
    ** moving forward will need PG info, max GA and latest GA
    **************************************************************************;
    **************************************************************************;

    ***a little prep - get separate prenatal datasets ;
    proc sql;

		 /*Get those prenatal groups that have no GA encounters*/
	     create table rpgroups_ind_0_&p. as
	     select patient_deid, pg, pgcnt, dt_pg_index, dt_pg_last, dt_pren_enc, anygacode, countagerows,
	             SUM(ANYGACODE) AS CountPGRows
	     from rpgroups_enc_&p. 
	     group by patient_deid, pg /*Group by the prenatal group*/
		 having sum(ANYGACODE) = 0
	     order by patient_deid, dt_pg_index, dt_pren_enc
	     ;*for 8b;

		 /*Get those prenatal groups that have only 1 GA encounter*/
	     create table rpgroups_ind_1_&p. as
	     select patient_deid, pg, pgcnt, dt_pg_index, dt_pg_last, dt_pren_enc, countagerows, max_gest_age,
				latest_preg_outcome, SUM(ANYGACODE) AS CountPGRows
	     from rpgroups_enc_&p. 
	     group by patient_deid, pg 
		 having sum(AnyGACode) = 1
	     order by patient_deid, dt_pg_index, dt_pren_enc
	     ;*for 8c;

		 /*Get the prenatal groups that have more than 1 gestational age encounter*/
	     create table rpgroups_ind_2_&p. as
	     select patient_deid, pg, pgcnt, dt_pg_index, dt_pg_last, dt_pren_enc, countagerows, max_gest_age, latest_preg_outcome,
	             gestational_age_days, min_gest_age , max_gest_age,
	             SUM(ANYGACODE) AS CountPGRows
	     from rpgroups_enc_&p. 
	     group by patient_deid, pg 
		 having sum(AnyGaCode) > 1
	     order by patient_deid, dt_pg_index, dt_pren_enc
	     ;*for 8d;

	    /* *rowcount check - works for round1;*/
	    /*  select **/
	    /*     from (select memname,nobs,sum(nobs) from dictionary.tables where libname='WORK' and memname like 'RPGROUPS_IND_%') a */
	    /*       , (select nobs from dictionary.tables where libname='WORK' and memname ="RPGROUPS_ENC_&p.") b;*/
	    quit;


/*		*Count the number of IDs in each dataset;*/
/*		proc sql;*/
/*			select count(distinct patient_deid) as orig_id from rpgroups_enc_&p.;*/
/*			select count(distinct patient_deid) as group0 from rpgroups_ind_0_&p.;*/
/*			select count(distinct patient_deid) as group1 from rpgroups_ind_1_&p.;*/
/*			select count(distinct patient_deid) as group2 from rpgroups_ind_2_&p.;*/
/*			;*/
/*			quit;*/


     

    **************************************************************************;
    ** 8b - PG with 0 gestational age encounters   ;
    **************************************************************************;

    %* replace 140d with POSPW macro var (07.06);
    %*07.31 - for those not in 1st 140d send back for New Index - create dsn ;
    data rpg_8b1a_&p. (label="grouped enc to form [pospw/140d] group preg")
         NewInd_8b1a_&p. (label="enc outside 140d group get new index")
       ;
    set rpgroups_ind_0_&p.;
    	by patient_deid pg dt_pren_enc ;

        if first.patient_deid then do;
	        *same vars as in simplified step 8;
	        pno=1;
	        Dt_IndexPrenatal = dt_pren_enc; 
	        Dt_index140 = dt_pren_enc + &POSPW. ; /*If the first prenatal encounter, identify date pospw days after that encounter*/
	        IdxPren= _n_ + (&num. * 1000000);
            length status $20.;
            status = cat("PREN_NO_",pno);
        end;
        	else if dt_indexprenatal le dt_pren_enc le dt_indexprenatal+ &POSPW. then pno+0; /*part of the same pregnancy*/

		if dt_indexprenatal le dt_pren_enc le dt_indexprenatal+ &POSPW. then output rpg_8b1a_&p.; /*CDL: ADDED 10.16.2024 -- previously not outputting into that dataset*/
            ELSE If dt_pren_enc GT dt_indexprenatal+ &pospw. then OUTPUT  NewInd_8b1a_&p.; /*07.31 (deleted pno+1 datasteps);*/
           	
       	retain pno dt_indexprenatal idxpren status dt_index140 ;
        Format Dt_: date.;
    run;


/*CDL testing*/
/*	data test1;*/
/*	input value1;*/
/*	datalines;*/
/*	3*/
/*	4*/
/*	5*/
/*	; run;*/
/*	data test2 test3;*/
/*	set test1;*/
/*	if value1 = 3 then value2 = 1;*/
/*	else if value1 >3 then output test3;*/
/*	run;*/
/*	data test2 ;*/
/*	set test1;*/
/*	if value1 = 3;*/
/*	run;*/

    *last row defines pregnancy;
    data preg_8b1a_&p. ;
    set rpg_8b1a_&p.;
      	by patient_deid pno;
       	if first.pno then dt_prenenc1st=dt_pren_enc ; 
		retain dt_prenenc1st;
        dt_prenencLast= dt_pren_enc;
       	DT_LTFU = dt_pren_enc;
       	LTFU_Step='8.b';
       	LTFU_Assignment="&POSPW.d look forward, no GA";
       	if last.pno then output; /*10.16.24 -- CDL ADDED: then output*/
        format dt: mmddyy10.;

        keep patient_deid dt_indexprenatal dt_ltfu ltfu_step ltfu_assignment idxpren 
             status dt_prenenc1st dt_prenenclast  ;
     run;

/*	 proc sql;*/
/*	 	select count(distinct patient_deid) as old from rpg_8b1a_&p.;*/
/*		select count(distinct patient_deid) as new_id from preg_8b1a_&p.;*/
/*		quit;*/

    **************************************************************************;
    ** 8c - PG with 1 gestational age encounter   ;
    **************************************************************************;

    /*    *separate those recs before earliest and after earliest expected pren-enc ;*/
    data rpg_8c2a_&p. rpg_8c2b_&p.; 
    set rpgroups_ind_1_&p. ;
/*    	where countagerows = 1;   */
		where countagerows > 0;*need this because the PG may have 1+ pren-enc w/o GA code - subset to only GA encounter;
        by patient_deid pg dt_pren_enc ;
    /*  *8c.1 set date bounds; */
        Dt_Earliest_PNC = dt_pren_enc - max_gest_age;
        Dt_Last_Outcome = dt_pren_enc + latest_preg_outcome; 
        format dt_: date.;
         
    /*  *8c.2 split obs based on timing; */
        if dt_pg_index lt dt_earliest_pnc then output rpg_8c2a_&p. ; /*Indexing prenatal encounter before earliest PNC encounter -- subsequent encounters shoudl be evaluated for a new preg*/
        	else if dt_pg_index ge dt_earliest_pnc then output rpg_8c2b_&p.; /*Indexing prenatal encounter on or after earliest PNC encounter date*/
    run;

/*	proc sql;*/
/*		select count(distinct patient_deid) as orig_id from rpgroups_ind_1_&p.;*/
/*		select count(distinct patient_deid) as id_2a from rpg_8c2a_&p.;*/
/*		select count(distinct patient_deid) as id_2b from rpg_8c2b_&p.;*/
/*		quit;*/


    /*    **add back in any of the coutagerows0 and do 8c.2a: check if all part of same pregnancy;*/
    data rpg_8c2a_1_&p. 
         newind_8c2a_&p. ;
         merge rpg_8c2a_&p.  (in=a)  rpgroups_ind_1_&p. ;
         by patient_deid pg; if a;

         *pren enc on/after index but before earliest pnc is same preg;
         if dt_pg_index le dt_pren_enc lt dt_earliest_pnc then  samepreg=1;
         	else samepreg=0;
         
         if samepreg=1 then output rpg_8c2a_1_&p. ; *enc same pregnancy;
         	else output  newind_8c2a_&p. ; *not the same pregnancy - assign new indexprenatal;
     run;

    /*    **8c2a - last row defines pregnancy;*/

	 /*Output the pregnancy level information*/
     data preg_2ca_&p. ;
     set rpg_8c2a_1_&p. ;
     	by patient_deid pg;
        if first.pg then dt_prenenc1st=dt_pren_enc ; 
		retain dt_prenenc1st;
        dt_prenencLast= dt_pren_enc;
        DT_LTFU = dt_pren_enc;
        status = cat("PREN_NO_",pg);*note will change this later;
        LTFU_Step='8.c2a';
        LTFU_Assignment="1 ga code, too far from indexing pnc";
        if last.pg then output; /*10.16.2024- CDL ADDED then output*/
     run;


    /*    **now 8c2b;*/

    /*   **add back in any of the coutagerows0 for those headed to 8c.2b;*/
     data rpg_8c2b_1_&p. 
          newind_8c2b_&p. ;
     	merge rpg_8c2b_&p.  (in=a)  rpgroups_ind_1_&p. ;
        by patient_deid pg; if a;

        *same preg if pren-enc between index and last outcome;
        if dt_pg_index le dt_pren_enc lt dt_last_outcome then  samepreg=1;
        	else samepreg=0;
         
        if samepreg=1 then output rpg_8c2b_1_&p. ; *enc same pregnancy;
        	else output  newind_8c2b_&p. ; *not the same pregnancy - assign new indexprenatal;
     run;

    /*    **again last row defines pregnancy;*/

	proc sort data = rpg_8c2b_1_&p.; 
		by patient_deid pg dt_pren_enc;
	run;
    data preg_2cb_&p. ;
    set rpg_8c2b_1_&p.  ;
    	by patient_deid pg;
        if first.pg then dt_prenenc1st=dt_pren_enc ; 
		retain dt_prenenc1st;
        dt_prenencLast= dt_pren_enc;
        DT_LTFU = dt_pren_enc;
        status = cat("PREN_NO_",pg); 
        LTFU_Step='8.c2b';
        LTFU_Assignment="1 ga code, including indexing pnc";
        if last.pg;
        *rename pg=pno;
    run;



    **************************************************************************;
    *** 8d - PG with 2+ gestational age encounters   ;
    ***      (paths split - 8d2b and 8d3+8d4);
    **************************************************************************;

	proc sort data= rpgroups_ind_2_&p. ; 
		by patient_deid pg dt_pren_enc; 
	run; 

    **note: the PG may have 1+ pren-enc w/o GA code, and the 1st/Index pren-enc date may be the one w/o GA;
    **      so anchoring on first pren-enc with GA info to determine if group goes to 8d2 or 8d3 pathway;
	** GOAL: Determine if the first encounter with GA information is sufficiently close to indexing PNC encounter;
    data rpg_8d2b_&p. 
         rpg_8d3_&p.; 
    set rpgroups_ind_2_&p. ;
    	by patient_deid pg dt_pren_enc ;
        where countagerows >=1;

    /*       *8d.1 - set enc1 values;*/
        if first.pg then do;  
        	dt_1 = dt_pren_enc;
            ga1 = gestational_age_days;
            min_ga1 = min_gest_age;
            max_ga1 = max_gest_age;
            last_preg_out1 = latest_preg_outcome;
              dt_1max=dt_1-max_ga1;
    /*          *8d.2a - pathway ;*/
                if dt_pg_index >= dt_1-max_ga1 then path='d3'; /*indexing PNC sufficiently close*/
                else if dt_pg_index < dt_1-max_ga1 then path='d2' ; /*indexing PNC too far from first GA encounter*/
		end;

		retain dt_1--path;
        format dt_: date.;
        if path = 'd3' then output rpg_8d3_&p.;
        	else if path='d2' then output rpg_8d2b_&p.;
       run;
                                                                                        


      **----------------------------------;                                
      ** Continue 8d.2b **;
      **----------------------------------;   
 
	*capture the 0 GA rec rows - add back the PNC enc with no GA information;
	proc sql;
    	create table rpg_8d2b_pl_&p. as
        select * from rpg_8d2b_&p. 
        outer UNION  corr /*keep all vars*/
        select distinct a.*, b.dt_1, b.ga1, min_ga1, max_ga1, dt_1max, last_preg_out1, path 
        from rpgroups_ind_2_&p. a 
		join rpg_8d2b_&p. b
        on a.patient_deid=b.patient_deid and a.pg=b.pg
        where a.countagerows = 0
        order by patient_deid, pg, dt_pren_enc
		;
        quit;

	*8d2b - pren-encs between index and max GA adj;
    data rpg_8d2b_1_&p.
         newind_8d2b_&p.;
    set rpg_8d2b_pl_&p. ;
    	by patient_deid pg;
          
    	if dt_pren_enc >= dt_pg_index And dt_pren_Enc < dt_1-max_ga1 then samepreg=1;
    		else samepreg=0;

    	if samepreg=1 then output rpg_8d2b_1_&p.; /*CDL: ADDED =1 10.17.2024 -- making explicit*/
    		else output newind_8d2b_&p.; /*Send back for new indexing prenatal encounter*/
	run;



	**Pregnancy recs 8db2 - last record contains the pregnancys information;
	data preg_d2b_&p. ;
	set rpg_8d2b_1_&p.  ;
		by patient_deid pg;
		if first.pg then dt_prenenc1st=dt_pren_enc ; 
		retain dt_prenenc1st;
        dt_prenencLast= dt_pren_enc;
        DT_LTFU = dt_pren_enc;
        status = cat("PREN_NO_",pg);
        LTFU_Step='8.d2b';
        length LTFU_Assignment $100.;
        LTFU_Assignment="multiple GA code, did not include indexing pnc";
        if last.pg;
        *rename pg=pno;
	run;   
     
        ************************************************;
        ** Now 8d3 - 2nd date, and 8d4 - check closeness;
        ************************************************;
      
	%do DD = 1 %to 100  ; *again high filler number;
	/*%let dd = 1;*/

		%if &dd = 1 %then %do;
            %goto CheckSubsequent;
		%end;
			%else %if &subs = 0 %then %do;
            	%goto Done2pl ;
         	%end;
         	%else %do;
            	%goto CheckSubsequent;
         	%end;

		/*Check the subsequent GA encounter to determine if sufficiently close*/
        %CheckSubsequent:

		************************************************;
		** Now 8d3 - 2nd date, and 8d4 - check closeness;
		************************************************;

		data rpg_8d4_A2_&p._&dd. rpg_8d4_A3_&p._&dd.
			 rpg_8d4_B2_&p._&dd. rpg_8d4_B3_&p._&dd. ;

		%if &dd. = 1 %then %do ;
			set rpg_8d3_&p. (drop=path);
		%end;
		%else %do;
			set rpg_4a2_Next_&p._%eval(&dd. -1) 
			rpg_4b3_Next_&p._%eval(&dd. -1)  
           	;
		%end;

			by patient_deid pg dt_pren_enc;
			if first.pg then do; 
				dt_2=.; ga2=.; min_ga2=.; max_ga2=.; last_preg_out2=.; path='  ';
               	cnt=0;
            end;
            cnt+1;
            if cnt=2 then do;
        /*       *8d.3 - set enc2 values;*/
				dt_2 = dt_pren_enc;
                ga2 = gestational_age_days;
                min_ga2 = min_gest_age;
                max_ga2 = max_gest_age;
                last_preg_out2 = latest_preg_outcome;
            end;

        /*          *8.d4 pathway;*/
			if ga2 >= ga1 then do; *path = 'd4a'; /*retains those ga enc where cnt > 1*/
				*8d.4.a.1;
				if dt_2 <= ( dt_1 + &dayadj. + max_ga2 ) then output rpg_8d4_A2_&p._&dd.; *path='4a2'; 
					else output rpg_8d4_A3_&p._&dd.; *path='4a3';
			end;

        /*            *8d.4b pathway;*/
				/*CDL: ADDED . < -- 10.17.2024 -- Think that without, it was including the first enc from earlier pregnancies, now all datasets have cnt >1*/
				else if . < ga2 < ga1 then do; *path='d4b';*ga2 < ga1; /*retains all ga enc since ga2 is . for cnt = 1*/ 
                        *8d.4b.1;
					if dt_2 >= ( dt_1 + &dayadj. + min_ga2 ) then output rpg_8d4_B2_&p._&dd. ;*path = '4b2';
						else output rpg_8d4_b3_&p._&dd.; *path='4b3';
				end;
			retain dt_2--path;
            format dt_: date.;    
		run;


		***-------------------------------------------------------------------------;
		*** 4.a.2 - cnt=2 is same preg, check subsequent date with GA against dt1;
		***-------------------------------------------------------------------------;

		data rpg_preg_4a2_&p._&dd. 
			 rpg_4a2_Next_&p._&dd. (keep= patient_deid--last_preg_out1);
		set  rpg_8d4_A2_&p._&dd.;
			by patient_deid pg ;
			if  cnt=2 then output rpg_preg_4a2_&p._&dd. ;
                /*This next prenatal encounter needs to be evaluated, assuming that there are additional encounters w GA information*/
				else output rpg_4a2_next_&p._&dd.; 
		run;

		***-------------------------------------------------------------------------;
		*** 4.a.3 - get all encounters, if w/in date range >=pg_idx and < dt_2- (adj+max2) as preg;
		***-------------------------------------------------------------------------;

		/*Grab all of the prenatal encounters that were originally linked to the indexing prenatal encounter*/
		proc sql;
			create table  rpg_8d4_A3_2_&p._&dd. as
            select * from rpg_8d4_A3_&p._&dd. where cnt>1
                 
            outer UNION  corr /*1st pren GA enc*/
            select  a.*, b.dt_2,b.ga2,b.min_ga2,b.max_ga2, b.last_preg_out2,b.path 
            from (select * 
				  from rpg_8d4_a3_&p._&dd.(keep=patient_deid--last_preg_out1 cnt) 
				  where cnt=1) a 
			join (select * 
				  from rpg_8d4_a3_&p._&dd. 
				  where cnt=2) b
            on a.patient_deid=b.patient_deid and a.pg=b.pg
			outer UNION  corr /*0 GA enc*/
            select distinct a.*, 
                   b.dt_1, b.ga1, b.min_ga1, b.max_ga1, b.last_preg_out1, 
                   b.dt_2, b.ga2, b.min_ga2, b.max_ga2, b.last_preg_out2, b.path 
            from rpgroups_ind_2_&p.  a 
			join (select * 
				  from rpg_8d4_a3_&p._&dd. 
				  where cnt=2) b
            on a.patient_deid=b.patient_deid and a.pg=b.pg
            where a.countagerows = 0
            order by patient_deid, pg, dt_pren_enc;
            ;      
            quit; 

		/*Output those that need new indexing prenatal encounters*/
		data rpg_8d4_A3_preg_&p._&dd.  
			 newind_d4A3_&p._&dd. ;
		set rpg_8d4_A3_2_&p._&dd. ;
			by patient_deid pg dt_pren_enc;

			IF dt_pren_enc >= dt_pg_index And dt_pren_enc < (dt_2 - (&dayadj. + max_ga2))
				then output rpg_8d4_A3_preg_&p._&dd.; *SamePreg4A3=1;

            IF dt_pren_enc > dt_2 - (&dayadj. + max_ga2) then
                output newind_d4a3_&p._&dd. ; *NewInd4A3=1 ;
		run;
	
		/*Output the pregnancy level information for those prenatal encounters
		Last row contains the pregnancy information*/
        data preg_rpg_4a3_&p._&dd. ;
		set rpg_8d4_A3_preg_&p._&dd.;
			by patient_deid pg ;
			if first.pg then dt_prenenc1st=dt_pren_enc ; 
			retain dt_prenenc1st;
            dt_prenencLast= dt_pren_enc;
            DT_LTFU = dt_pren_enc;
            LTFU_Step='8d.4a3';
            length LTFU_Assignment $100.;
            LTFU_Assignment="ga2>=ga1 but too far";
            if last.pg;
        run;

		***-------------------------------------------------------------------------;         
		*** 4.b.2 - get all encounters, if w/in date range >=pg_idx and < dt_2- (adj+min2) set as preg;
		***-------------------------------------------------------------------------;
		proc sql;
			create table  rpg_8d4_B2_2_&p._&dd. as
			select * 
			from rpg_8d4_b2_&p._&dd. 
			where cnt>1
                 
            Outer UNION  corr /*1st pren GA enc*/
            select  a.*, b.dt_2,b.ga2,b.min_ga2,b.max_ga2, b.last_preg_out2,b.path 
            from (select * 
				  from rpg_8d4_b2_&p._&dd.(keep=patient_deid--last_preg_out1 cnt) 
				  where cnt=1) a 
			join (select * 
				  from rpg_8d4_b2_&p._&dd. 
				  where cnt=2) b
			on a.patient_deid=b.patient_deid and a.pg=b.pg
			Outer UNION  corr /*0 GA enc*/
            select distinct a.*, 
                        b.dt_1,b.ga1,b.min_ga1,b.max_ga1, b.last_preg_out1, 
                        b.dt_2,b.ga2,b.min_ga2,b.max_ga2, b.last_preg_out2,b.path 
            from rpgroups_ind_2_&p.  a 
			join (select * 
				  from rpg_8d4_b2_&p._&dd. 
				  where cnt=2) b
            on a.patient_deid=b.patient_deid and a.pg=b.pg
            where a.countagerows = 0
            order by patient_deid, pg, dt_pren_enc;
            ;      
            quit; 

		Data  rpg_8d4_B2_preg_&p._&dd.  newind_d4B2_&p._&dd. ;
		set  rpg_8d4_b2_2_&p._&dd. ;
			by patient_deid pg dt_pren_enc;

            IF dt_pren_enc >= dt_pg_index And dt_pren_enc < (dt_2 - (&dayadj. + min_ga2))
            	then output rpg_8d4_B2_preg_&p._&dd. ;* SamePreg4b2=1;

			IF dt_pren_enc > dt_2 - (&dayadj. + min_ga2) 
				then output newind_d4B2_&p._&dd. ; *NewInd4b2=1 ; 

		run;

		/*Output the pregnancy-level information*/
		data preg_rpg_4b2_&p._&dd.;
		set  rpg_8d4_b2_preg_&p._&dd.;
			by patient_deid pg ;
            if first.pg then dt_prenenc1st=dt_pren_enc ; 
			retain dt_prenenc1st;
            dt_prenencLast= dt_pren_enc;
            if dt_pren_enc <= dt_2 - (&dayadj. + min_ga2) then DT_LTFU = dt_pren_enc; 
			retain dt_ltfu;  *will update for each encounter that fits;
            LTFU_Step='8d.4b2';
            length LTFU_Assignment $100.;
            LTFU_Assignment="ga2<ga1, far enough apart";
            if last.pg; /*Last row contains the final pregnancy information -- updating prior rows with new info til last row*/
		run;


		***-------------------------------------------------------------------------;
		*** 4.b.3 - cnt=2 is same preg, check subsequent date with GA against dt1;
		*** (technicaly could keep with 4.a.2, difference comes with ltfu assignment);
		***-------------------------------------------------------------------------;
		data rpg_preg_4b3_&p._&dd. 
             rpg_4b3_Next_&p._&dd. (keep= patient_deid--last_preg_out1);
		set  rpg_8d4_b3_&p._&dd.;
			by patient_deid pg ;
            if  cnt=2 then output rpg_preg_4b3_&p._&dd. ;
            	else output rpg_4b3_next_&p._&dd.;
		run;

		proc sql;
			select sum(nobs), &dd. into :subs from dictionary.tables
			where lowcase(memname) in ("rpg_4a2_next_&p._&dd." "rpg_4b3_next_&p._&dd.")
			;
            quit; 
		%put &subs for next subsequent;

	%end;

	%Done2pl:

	***-------------------------------------------------------------------------;
	** wrap up pregnancy from 4.d.A2;
	***-------------------------------------------------------------------------;
	Data rpg_preg_4a2_&p._ ;
    set rpg_preg_4a2_&p._:  ;
    run;
	/*Stack all of these pregnancy records generated above.*/

    **get 0 GA dates too;
    proc sql;
    	create table rpg_Preg_4a2_&p. as
        select distinct * 
		from rpg_preg_4a2_&p._ 
     	Outer UNION corr /*0 GA enc*/
        select distinct a.*, 
                    b.dt_1,b.ga1,b.min_ga1,b.max_ga1, b.last_preg_out1, 
                    b.dt_2,b.ga2,b.min_ga2,b.max_ga2, b.last_preg_out2, b.path 
        from rpgroups_ind_2_&p.  a 
		join (select * 
			  from rpg_preg_4a2_&p._ ) b
        on a.patient_deid=b.patient_deid and a.pg=b.pg
        where a.countagerows = 0
        order by patient_deid, pg, dt_pren_enc;
        ;      
        quit; 

		/*Get all the information to one row per pregnancy*/
	data Preg_rpg_4a2_&p. ;
	set rpg_Preg_4a2_&p.  ; 
		by patient_deid pg ;
		if first.pg then dt_prenenc1st=dt_pren_enc ; 
		retain dt_prenenc1st;
        dt_prenencLast= dt_pren_enc;
		if dt_pren_enc <= dt_2 + last_preg_out2 then
		DT_LTFU = dt_pren_enc; 
		retain dt_ltfu;  *will update for each encounter that fits;
		LTFU_Step='8d.4a2';
		length LTFU_Assignment $100.;
		LTFU_Assignment="ga2>ga1, no disagreement"; 
		if last.pg;  
	run;

	***-------------------------------------------------------------------------;
	*** wrap up pregnancy from 4.d.B3;
	***-------------------------------------------------------------------------;

	Data rpg_Preg_4b3_&p._ ;
	set rpg_preg_4b3_&p._:  ;
	run;

	**get 0 GA dates too;
	proc sql;
		create table rpg_Preg_4B3_&p. as
        select distinct * from rpg_preg_4b3_&p._ 
        Outer UNION  corr /*0 GA enc*/
        select distinct a.*, 
                    b.dt_1,b.ga1,b.min_ga1,b.max_ga1, b.last_preg_out1, 
                    b.dt_2,b.ga2,b.min_ga2,b.max_ga2, b.last_preg_out2,b.path 
        from rpgroups_ind_2_&p.  a 
		join (select * 
			  from rpg_preg_4b3_&p._ ) b
		on a.patient_deid=b.patient_deid and a.pg=b.pg
		where a.countagerows = 0
		order by patient_deid, pg, dt_pren_enc;
        ;      
        quit; 

		/*Roll up the pregnancy information*/
	data Preg_rpg_4b3_&p. ;
	set rpg_Preg_4b3_&p. ; 
		by patient_deid pg ;
		if first.pg then dt_prenenc1st=dt_pren_enc ; 
		retain dt_prenenc1st;
        dt_prenencLast= dt_pren_enc;
		if dt_pren_enc <= dt_1 + last_preg_out1 then
		DT_LTFU = dt_pren_enc; retain dt_ltfu;  *will update for each encounter that fits;
		LTFU_Step='8d.4b3';
		length LTFU_Assignment $100.;
		LTFU_Assignment="ga2<ga1, too close, no subsequent";
		if last.pg;
	run;
        
	**combine all that need new index;
	Data NewInd_4d_&p. ;
	set newind_d4b2_&p._:
		newind_d4a3_&p.: ;
		keep patient_deid dt_pren_enc;
	run;

    **combine all 4D pregnancies found;
    Data Preg_4D_&p. ;
    set preg_rpg_4a2_&p. 
		preg_rpg_4a3_&p._: 
		preg_rpg_4b2_&p._:
		preg_rpg_4b3_&p.
		;
	run;
 
	************************************************************;
    *** Now put together all pregs for this round (8a - 8d);
    *** and compile all prenatal ^New Index^ encounters;
    ************************************************************;
     
    Data PrenPregnancies_&p.;
    set preg_4d_&p. 
    	preg_d2b_&p. 
        preg_2cb_&p.
        preg_2ca_&p.
        preg_8b1a_&p.;
       	;
    run;

    *07.31 - Adjust to include those Prenatal encounters Not included in 1st 286d group;
    *        and pren encounters not included in no-GA [140d] pregnancy (8b1a);
    *        i.e. add files Leftover from FindPreg 1st datastep and NewInd_8b1a;
    Data allrem_&p.;
    set newind_4d_&p. 
		newind_8d2b_&p. /*8b2b??*/
        newind_8c2b_&p.
        newind_8c2a_&p.

        leftover_&p.
         NewInd_8b1a_&p.
       	;
	keep patient_deid dt_pren_enc;
    run;

	proc sql;
		create table Remaining_&p.  as
       	select a.*
       	from Prenatal_nooutc A 
		join allrem_&p. B 
		on a.patient_deid=b.patient_deid and a.dt_pren_enc=b.dt_pren_enc
        order by patient_deid,dt_pren_enc
		;

       	select nobs, &p. as left_after_round into :leftover  
		From dictionary.tables 
		where memname= "REMAINING_&p."
		;

     	quit;
/**/
/*     proc datasets nolist lib=work;*/
/*      delete rpg_: preg_: ;*/
/*     quit;*/

	%ENd; 

  	%WrapUp: 

    *08.05 - correct dsn name - should be _COMP not _Complex;
  	data pregnancy_prenatalonly_COMP; *need to modify Step10 if use dsn other than Pregnancy_PrenatalOnly (done 07.01);
   	set %do i = 1 %to &p.-1 ; Prenpregnancies_&i. %end; ;
	    if dt_indexprenatal=. then Dt_IndexPrenatal = dt_pg_index ;
	    IdxPren= _n_ + (&num. * 1000000);
	    status = cat("PREN_NO_",ltfu_step) ;

	    keep patient_deid dt_indexprenatal idxpren status dt_ltfu ltfu_assignment ltfu_step
	          dt_prenenc1st dt_prenenclast ;
  	run;
  	proc freq;table ltfu_: dt_: ; format dt_: year.;;run;


%Mend;


/*options nomprint nomlogic;*/
/*%Step8Complex */
;


/**Count the number of unique patient_deids in original;*/
/*proc sql;*/
/*	select count(distinct patient_deid) as distinct_original from prenatal_nooutc;*/
/*	select count(distinct patient_deid) as distinct_output from pregnancy_prenatalonly_comp;*/
/*	quit;*/
