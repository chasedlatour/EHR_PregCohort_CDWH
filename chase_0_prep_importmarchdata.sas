/*************************************************************************************

PROGRAM: chase_0_prep_importmarchdata.sas
PROGRAMMER: Sharon
DATE: 05-2024

PURPOSE: Create code reference files and attach relevant codes 
to encounters and medication orders.

MODIFICATIONS:
	- 05.2024 - Chase added additional comments and formatting.

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
*Sharon version;
%let xdr= %str(\\ad.unc.edu\med\tracs\groups\Research\CDWH\Latour_Chase_22-0689);
*Chase version;
*%let xdr= %str(W:/);

options nocenter noreplace nodate dlcreatedir;

/*Specify the libraries where the data are stored*/
libname x "&xdr\data\20230328_Data_Pull_01\analysis"  ;
libname int "&xdr.\data\20230328_Data_Pull_01\analysis\int_20230516" ;*access=r;

ods listing;

options mprint;














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
    		if code ne '' then output ;
    			else stop&n.=1; 
   		end;
 	end;

%mend; 

/*Run this on each of the encounter-level datasets so that each row is a code.*/
%macro Runit(fil);
	data cd&fil.;
 	set &fil (in=file_&fil.)    ;
		length COde $10.;

 		PullReas_diag= ( mom_diag_icd09_hb_flag=1 or mom_diag_icd10_hb_flag=1 or mom_diag_icd09_pb_flag=1 or mom_diag_icd10_pb_flag=1);
 		PullReas_anyproc= (mom_proc_icd10_flag=1 or mom_proc_icd09_flag=1 or mom_hcpcs_flag or mom_cpt_flag);
 		PullReas_med= mom_med_flag;

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
libname ref excel 
  'C:\Users\peacocks\University of North Carolina at Chapel Hill\EPI.Safe2Treat - Cohort Derivation\Variable Identification.xlsx'
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
 	create table encounters as
  	select distinct patient_deid,enc_date,enc_deid, enc_base_class,enc_type,hospital,location_name,department_specialty
  	from allicd;

	/*Grab all medication orders for mifepristone or misoprostol (according to identified order names)*/
   	create table CodeMed  /*medslist, codeoutmed*/ as
    select * 
	from refmeds a 
	join Med b 
	on a.order_name=b.order_name;

	/*Merge encounter information onto the mife and miso med order information*/
    create table codemed as
    select distinct a.*, b.*
    from encounters a 
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
