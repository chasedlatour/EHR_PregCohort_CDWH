/*************************************************************************************

PROGRAM: chase_0_prep_importmarchdata.sas
PROGRAMMER: Sharon
DATE: 05-2024

PURPOSE: Create code reference files and attach relevant codes 
to encounters and medication orders.

MODIFICATIONS:
	- 05.2024 - Chase added additional comments and formatting.
    - 08.2024 - sph - add correction to capture all Med orders, create additional 
                datasets used in applying algorithm (e.g gestational age )
**************************************************************************************/












/*************************************************************************************

TABLE OF CONTENTS:

	- 00 - SET UP LIBRARIES, ETC.
	- 01 - IMPORT EHR DATA FILES
	- 02 - MAKE CODE LEVEL DATASETS
	- 03 - CREATE SUBSEQUENT NECESSARY DATASETS FOR THE ALGORITHM

**************************************************************************************/






/*************************************************************************************

							00 - SET UP LIBRARIES, ETC.

**************************************************************************************/

/*Specify the location of files on the server*/
/*Point to location of files - string/text will be added to libname statements; */

%let xdr= %str(\\ad.unc.edu\med\tracs\groups\Research\CDWH\Latour_Chase_22-0689);
/*CDL version:
%let xdr= %str(W:\);
*/


%inc "&xdr.\PROGRAMS\preg_encounter_algorithm\chase_All_LibrariesAndOptions.sas";

/*Alternative to running %inc above, use the following libname statements*/

/**Data received - most ^raw^ code files, manipulations but no linkage to the reference files;*/
/*libname Raw "&xdr\data\20230328_Data_Pull_01\analysis"  ;*/
/**/
/**/
/**Data received and divided according to reference files as of date run;*/
/**note: add datestamp to filename (option DLCREATEDIR above will create folder if doesnt exist);*/
/**/
/*libname int "&xdr.\data\20230328_Data_Pull_01\analysis\int_202408" ;                *<====== CHECK PATH;*/
/**/
/**/
/**Data received and divided according to reference files as of date run;*/
/**note: add datestamp to filename (option DLCREATEDIR above will create folder if doesnt exist);*/
/**/
/*libname Out "&xdr.\data\20230328_Data_Pull_01\analysis\int_202408\preg_alg" ;       *<====== CHECK PATH;*/
/*libname Outd "&xdr.\data\20230328_Data_Pull_01\analysis\int_202408\preg_alg\details" ;       *<====== CHECK PATH;*/

*/
;;;;

libname x(raw);  *alt libname (used below);











/*************************************************************************************

							01 - IMPORTANT EHR DATA FILES

**************************************************************************************/


*Macro to import the encounter data and medication order data;
%Macro getdat(ft);                                      
	proc import out=&ft. datafile="&xdr.\data\20230328_Data_Pull_01\_dr103_01_d_pop_enc_data_&ft..xlsx" dbms=excel ;
 		getnames=yes;
	quit;
%mend;
 

*Run the macro;
%getdat(icd09)
%getdat(icd10_a)
%getdat(icd10_b)
%getdat(med)
;;;;















/*************************************************************************************

							02 - MAKE CODE LEVEL DATASETS

The datasets provided to us had an encounter on each row of the dataset. The 
codes recorded at an encounter were saved as a long list with a | separating each 
code (e.g., |Z3A.08|O03.1). These codes had to be separated out so that we could
work with them.

**************************************************************************************/

%let wc= %sysfunc(countw(&codevar));;
%let src=%scan(&codevar.,4,"_");
%let ver=%scan(&codevar.,3,"_");
%let typ=%scan(&codevar.,2,"_");
%put &codevar &src &ver &typ;

*This macro splits up a character variable with the codes separated by vertical bars;
%macro Split(codevar,src,ver,typ, n);

 	if &codevar. ne '' then do;
   		Source="&src."; Version="&ver."; Codetype="&typ.";
   		do i=1 to 40 until (stop&n.=1);
    		code = scan(&codevar.,i,"|");
    		if code ne '' then output CD&Fil. ;
/*    		if code ne '' then output ; *replaced 08.24*/
    			else stop&n.=1; 
   		end;
 	end;

%mend; 

/*Run this on each of the encounter-level datasets so that each row is a code.*/
/*08.5.2024 - add file with all encounter data for Meds;*/
%macro Runit(fil);
proc freq data=&fil; table mom_med_flag;
run;

	data cd&fil.
         MedEnc&fil. (keep= patient_deid enc_date enc_deid enc_base_class enc_type
                            hospital location_name department_specialty mom_med_flag)
    ;
 	set &fil (in=file_&fil.)    ;
		length COde $10.;

 		PullReas_diag= ( mom_diag_icd09_hb_flag=1 or mom_diag_icd10_hb_flag=1 or mom_diag_icd09_pb_flag=1 or mom_diag_icd10_pb_flag=1);
 		PullReas_anyproc= (mom_proc_icd10_flag=1 or mom_proc_icd09_flag=1 or mom_hcpcs_flag or mom_cpt_flag);
 		PullReas_med= mom_med_flag;
          IF mom_med_flag = 1 then output MedEnc&fil.; *08.24;

		%split(mom_diag_icd10_hb_all,hb,icd10,dx10,1); /*HB = hospital billing codes*/
		%split(mom_diag_icd09_hb_all,hb,icd09,dx9,2);
		%split(mom_diag_icd10_pb_all,pb,icd10,dx10,3); /*PB = physician billing codes*/
		%split(mom_diag_icd09_pb_all,pb,icd09,dx9,4);

		%split(mom_proc_icd10_all,hb,icd10,pr10,5);
		%split(mom_proc_icd09_all,hb,icd09,pr9,6); %*technically hospitals bill icd;

		%split(mom_cpt_all,hpb,cpt,cpt,9);
		%split(mom_hcpcs_all,hpb,cpt,cpt,0); %*technically outpt hospital and phys bill cpt for care in hospital;
  
 		keep patient_deid enc_date enc_key enc_start_instant enc_deid enc_base_class enc_type hospital
      		Code source version codetype i pullreas:
      		enc_visit_type location_name department_specialty
      		mom_age_at_Enc mom_marital_status mom_race mom_raceth insurance
       	;
		
 		format enc_start_instant datetime.;
 		rename i=position;
		/* drop stop: mom_diag_icd: mom_proc: mom_hcpcs_all mom_cpt_all mom_diag;*/
	run;
%mend;

*Check;
/*proc freq data=icd10;*/
/*	table enc_base_class enc_type;*/
/*	where mom_proc_icd09_flag=1;*/
/*run;*/

/*Run this on each of the files (note that ICD10-era encounters were provided in
two separate datasets in the exact same format - This was due to Excel file size 
constraints*/
%runit(ICD09)
%runit(ICD10_a)
%runit(ICD10_b)
;;;;



















/*************************************************************************************

				03 - CREATE SUBSEQUENT NECESSARY DATASETS FOR THE ALGORITHM

**************************************************************************************/

*Create a dataset with all diagnosis and procedure codes -- stacking all of the datasets
on top of each other.;
data allicd;
set cdicd10_A cdicd10_B cdicd09 ;

	label
    	Code = 'Diagnosis/Procedure code'
    	codetype="Code type: dx9, dx10, pr, cpt"
	    pullreas_diag = 'any icd_diag Flag set to 1'
	    pullreas_anyproc = 'any icd-proc/cpt/hcpcs Flag set to 1'
	    pullreas_med = 'med Flag set to 1'
	    position='placement in original code-string'
	    source='HB (dx,pr), PB (dx), HP (hcpc/cpt)'
	    version='ICD-9 or ICD-10'
   	;
run;

*08.5.2024 - combine medencounter dsns;
data MedEncounters;
 set medencicd10_A medencicd10_b medencicd09;
run;
proc sort data=medencounters nodups; by _all_;
run;*should be 0 dups as each diagsource file is 1rec per enc (base info doesnt repeat);

*Checks;
/*proc freq data=cdicd10;*/
/*	table pull:*source/missing;*/
/*run;*/

/*Remove any duplicates - some encounters may have a code recorded more than once.
This removes those instances.*/
proc sort data=allicd nodups;
	by _all_;
run;

/*Create datasets that we will subsequently use*/
proc sql;

	/*Grab all the de-identified IDs of the potential pregnant people*/
	create table x.women_ids (label="unique patient_deid") as 
  		select distinct patient_deid 
		from allicd;

	/*Grab demographic information on the potential pregnant people*/
	create table x.women_demog (label="Women data (may have multiple ages, insurance types)") as 
		/*In CDW-H (i.e., provided data), martial status, race, and ethnicity are NOT collected in a time-varying way.
		We receive the most recently updated value for that variable.*/
  		select distinct patient_deid, mom_age_at_Enc, mom_marital_status, mom_raceth, insurance 
		from allicd;*319803;

		/*Get a count of all the distinct IDs in teh pregnant person dataset*/
 		select count(distinct patient_deid) as PatientIDs into :womencnt from allicd;*188491;

	/*Make a dataset with all the diagnosis and procedure codes - each code is a row*/
	create table x.women_diag_proc (label="All encounters, 1 obs per Diag/Proc code") as
 		select * 
		from allicd (drop=mom_age_at_Enc mom_marital_status mom_race mom_raceth insurance);

	quit;
%put &womencnt;



/***pull reference files - need this to assign code lists, constantly link to most recent version online;*/
 *connect to ref file (synced to onedrive/sharepoint);
/*libname ref excel */
/*  'C:\Users\peacocks\University of North Carolina at Chapel Hill\EPI.Safe2Treat - Cohort Derivation\Variable Identification.xlsx'*/
/*  access=r;*/

libname ref excel
	'W:\PROGRAMS\preg_encounter_algorithm\Variable Identification.xlsx'
	access=r;




/*Malformations code list -- not used*/
data refmal;set ref.'malformations$'n;
run;*removed from prenatl list;


/*Prenatal encounters code list*/
data refpre;
*set ref.'prenatal_care$'n; 
set ref.'prenatal$'n;
     if code_type in ('ICD-10 DX' 'ICD10 ZCode') then code_typeb='dx10';
     	else if code_type in ('ICD-9 DX') then code_typeb='dx9';
     	else if code_type in ('ICD-10 PX') then code_typeb='pr10';
     	else if code_type in ('ICD-9 PX') then code_typeb='pr9';
     	else if code_type in ('CPT' 'HCPCS' 'Proc CPT' 'Proc HCPCS') then code_typeb='cpt';
run;*6851; *2.8.23 counts; *2.9 count drop to 5181 after 1st run this AM?;
    /*proc freq;table code_type*code_type;run;*/



/*Pregnancy outcomes code list*/
data refout;
set ref.'preg_outcomes$'n;
     if code_type in ('ICD-10 DX' 'ICD10 ZCode') then code_type='dx10';
     else if code_type in ('ICD-9 DX') then code_typeb='dx9';
     else if code_type in ('ICD-10 PX') then code_typeb='pr10';
     else if code_type in ('ICD-9 PX') then code_typeb='pr9';
     else if code_type in ('CPT' 'HCPCS' 'Proc CPT' 'Proc HCPCS') then code_typeb='cpt';
    run;*1507;
    /*proc freq;table code_type*code_type/missing;run;*/



/*Gestational age code list*/
data refage;set ref.'gest_age$'n;
     if code_type in ('ICD-10 Dx' 'ICD10 ZCode') then code_typeb='dx10';
     else if code_type in ('ICD-9 Dx') then code_typeb='dx9';
     else if code_type in ('CPT' 'HCPCS' 'Proc CPT' 'Proc HCPCS') then code_typeb='cpt';
    run;*177;


/*Misoprostol and mifepristone order names*/
data refmeds;
	set ref.'meds_abortion$'n;
run;


/*Rho-D order names*/
data refrho;
	set ref.'rho_d$'n;
run;
libname ref;


*Different Checks;

*Look at insurance status, see values and if changing over time;
/*proc sort data=women_demog nodupkey out=wdi;*/
/*	by patient_deid insurance;*/
/*run;*/
/*proc transpose data=wdi out=dem1 prefix=ins;*/
/*    by patient_deid;*/
/*	var insurance;*/
/*run;*/
/*proc freq data=dem1;*/
/*	table ins1*ins2*ins3/list missing;*/
/*run;*/

*Look at age changes over the encounters included in the dataset;
/*proc transpose data=women_demog out=dem2 prefix=age;*/
/*    by patient_deid ;var mom_age_at_enc;*/
/*run;*/
/*proc freq data=dem2;*/
/*	table age15;*/
/*run;*/
/*data dem2b;*/
/*set dem2;*/
/**/
/*    array age(15);*/
/*      	do i=2 to 15 while (age(i) ne .);*/
/*        	lastage=age(i);*/
/*      	end;*/
/*    FirstLastAgeDif = lastage-age1;*/
/*    MultipleAges = age2 ne .;*/
/**/
/*run;*/
/*proc sql;*/
/*    create table women_demog2  as */
/*	select * */
/*    from (select distinct * */
/*		  from women_demog(drop=insurance mom_age_at_enc)) a*/
/*    join dem1 */
/*	on a.patient_deid=dem1.patient_deid */
/*	join dem2b */
/*	on a.patient_deid=dem2b.patient_deid*/
/*    ;*/
/*    quit;*now 1row per pt;*/


/***now apply codes from reference files;*/

proc sql stimer noerrorstop;

	/*Grab all gestational age codes from the allicd file - this will provide info
	on which encounters have a gestational age code, though an encounter may appear more
	than once if more than one gestational age code is recorded at that encounter.*/
	create table codeage as
    select a.*, b.* /*b.description,b.gest_age_wks,b.gestational_age_days*/
    from allicd a 
	join refage b 
	on a.code = b.code  /*And a.codetype=b.code_typeb*/;

	/*Get counts*/
	select count(distinct patient_deid) 
	as PatientIDs, count(distinct enc_deid) as Encounters, count(*) as rows  
	from codeage
    ;
	      /*
	PatientIDs  Encounters      rows
	ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ
	     11511       32475     43079
	*/ 

	/*Grab all prenatal codes from the allicd file -- Same set up as gestational age codes,
	just different code reference list.*/
    create table codePrenatal as
    select a.*, b.*
    from allicd a 
	join refPre b 
	on a.code = b.code /*And a.codetype=b.code_typeb*/;

	/*Get counts to evaluate*/
    select count(distinct patient_deid) as PatientIDs, count(distinct enc_deid) as Encounters, count(*) as rows 
	from codePrenatal
    ;
	/*
	PatientIDs  Encounters      rows
	ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ
	    163400     1185200   3622256

	*/

	/*Get pregnancy outcome codes from allicd -- Same set up as the gestational age codes and prenatal codes*/
    create table codeOutcome as
    select a.*, b.*
    from allicd a 
	join refOut b 
	on a.code = b.code /*And a.codetype=b.code_typeb*/;

	/*Get counts for evaluating*/
    select count(distinct patient_deid) as PatientIDs, count(distinct enc_deid) as Encounters, count(*) as rows 
	from codeOutcome
    ;
	/*PatientIDs  Encounters      rows
	ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ
	    105935      225366    990528

	*/

	/*Get malformation codes - same as gestational age codes (these arent used in the algorithm)*/
    create table codeMal as
    select a.*, b.*
    from allicd a 
	join refMal b 
	on a.code = b.code /*And a.codetype=b.code_typeb*/;

	/*Get counts for evaluating*/
    select count(distinct patient_deid) as PatientIDs, count(distinct enc_deid) as Encounters, count(*) as rows 
	from codeMal
    ;
	/*
	PatientIDs  Encounters      rows
	ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ
	     31985      115740    152921

	*/

	/*Create a dataset of all distinct encounter information (not code information) from allicd.
	This will be important for the medication orders because the medication orders Excel file 
	only has the encounter key and no other encounter information (e.g., no encounter date)*/
    /*08.5.2024 this was replaced with addl steps in macro Runit above*/
 	*create table encounters as
  	select distinct patient_deid,enc_date,enc_deid, enc_base_class,enc_type,hospital,location_name,department_specialty
  	from allicd;

	/*Grab all medication orders for mifepristone or misoprostol (according to identified order names)*/
   	create table CodeMed  /*medslist, codeoutmed*/ as
    select * 
	from refmeds a 
	join Med b 
	on a.order_name=b.order_name;

	/*Merge encounter information onto the mife and miso med order information*/
    /*08.5.2024 - correction capture encounter-level data with MedEncounters (not encounters)*/
    create table codemed as
    select distinct a.*, b.*
    from MEDencounters a 
	join (select distinct patient_deid,enc_key, substr(generic_med,1,5) as Code, outcome 
			from refmeds a 
			join Med b 
			on a.order_name=b.order_name) b
    on a.patient_deid=b.patient_deid and a.enc_deid=b.enc_key ;

	/*Get counts for evalating*/
    select count(distinct patient_deid) as PatientIDs, count(distinct enc_deid) as Encounters, count(*) as rows from codeMed
    ;
	/*
	PatientIDs  Encounters      rows
	ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ
	     32922       36865     37584

	*/

    quit;



/*Output the files that we want to keep*/
proc copy inlib=work outlib=x; *import files;
 	select med icd09 icd10_a icd10_b;
quit;

proc copy inlib=work outlib=int; *created files;
 	select ref: codeage codemal codemed codeoutcome codeprenatal;
quit;


************************** add more datasets ************************;

/*08.2024 - get date-level prenatal enc file */
**(formerly a step in Steps3thru10 program);

proc sql stimer; 

 create table _codeprenatal_meg1 as 
  select distinct patient_deid,enc_date, megan_primary_prenatal,
         case enc_base_class when '' then "unknown" else enc_base_class end as BASE_CLASS
   from codeprenatal where megan_primary_prenatal order by patient_deid, enc_date;

 *create table prenids as select distinct patient_deid,1 as AnyPrenatal 
   from _codeprenatal_meg1; 
quit;

proc transpose data= _codeprenatal_meg1  prefix=pren_
      out= _codeprenatal_meg1_dts (drop= _name_ _label_);
 by patient_deid enc_date; id base_class; var megan_primary_prenatal;
run; 
proc datasets nolist lib=work; change _codeprenatal_meg1_dts = codeprenatal_meg1_dts;
quit;*08.24 added rename to keep dsn name consistent w/ other datafiles in folder;


/*----------------------------------------------------------------------------------------------------*/

/*08.2024  - add gest-age datasets that were included previous versions of gestage/lmp macro (step11)*/

** creates gestage dsn - adding prenatal encounter flag to those with gestational age codes;
**(formerly chase_gestagedsn.sas);
proc sql;
 create table gestage as
  select distinct a.* ,b.enc_key ne . as prenatal_enc  
  from codeage a left join 
         (select enc_key from codeprenatal where megan_primary_prenatal) b on a.enc_key=b.enc_key
;
quit;


/*----------------------------------------------------------------------------------------------------*/

/*08.2024  - and add alt gestage dsn needed for complex prenatal-only pregnancies (step8-complex)*/

 **(formerly chase_createGestAgePrenforStep8Complex.sas);
*** Note: this rolls up info at the GA code and Encounter date. If the GA code is on multiple encounters;
***       on the same date, it is possible that only 1 of those encounters (enc_key) also had a prenatal;
***       encounter code. use MAX fn to indicate any prenatal enc on same date as GA code;

*** Note: this rolls up info at the GA code and Encounter date. If the GA code is on multiple encounters;
***       on the same date, it is possible that only 1 of those encounters (enc_key) also had a prenatal;
***       encounter code. use MAX fn to indicate any prenatal enc on same date as any GA code;
*** Note: dont care about individual encounter billing code, just the age values assigned on that date;
***       so exclude GA enc billing code from output dsn;
***       06.18-move here limit to OK GA encounters (8a ignore prenatal care, weight only);
***             also limit to those GA encounters that are also prenatal encounters;
    proc sql;
     create table gestageprenx as
      select *
      from (
              select distinct patient_deid,enc_date , enc_date as Dt_GAEnc, max(prenatal_enc) as Pren_GA_enc,
                     code_hierarchy, gestational_age_days,gest_age_wks,min_gest_age,max_gest_age,
                     max_gest_age - min_gest_age as MinMax_GA_Range, LATEST_PREG_OUTCOME,
                     case code_hierarchy 
                       when "Specific gestational age" then 1
                       when "Extreme prematurity" then 2
                       when "Other preterm" then 3
                       when "Post-term" then 3
                       when "Missing" then 4
                       else 99 end as GAHier
              from gestage
              where  lowcase(code_hierarchy) not in ("prenatal care" "weight only - preterm")
              group by patient_deid,parent_code,enc_date, code,preg_outcome,code_hierarchy, 
                       gestational_age_days,min_gest_age,max_gest_age 
           )
      where pren_ga_enc =1 
    ;
    quit;
    
    *if date has different hierarchy but same age estimates then can keep the best hierarchy match row;
    *now if mult rows for GA enc Date its because provisional age est for underlying GA-codes differ;
    proc sql;
     create table gestagepren_step8 as 
     select DISTINCT patient_deid,dt_gaenc,pren_ga_enc,gestational_age_days,gest_age_wks,min_gest_age,max_gest_age,
            MinMax_GA_Range,latest_preg_outcome ,gahier
       from gestageprenx 
     group by patient_deid,dt_gaenc,gestational_age_days,gest_age_wks,min_gest_age,max_gest_age 
     having gahier = min(gahier);
     quit;


/*----------------------------------------------------------------------------------------------------*/
 *save to permanent INT folder;
     proc copy inlib=work outlib=int; 
      select codeprenatal_meg1_dts gestagepren_step8  gestage;
     quit;


