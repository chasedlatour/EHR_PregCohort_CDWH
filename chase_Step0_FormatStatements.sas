/******************************************************************************

Program: Chase_Step0_FormatStatements.sas                                                                                                                          chase_0b_FormatStatements.sas
         (was chase_ob_formatstatements.sas)
Programmer: Sharon 

Purpose: formats / blocks of code needed for pregnancy algorithm steps.

Updates/Modifications:
- 04-26-24: CDL review with comments added.
- 05.2024: CDL and SPH reviewed CDL QC. CDL added format. All changes were 
	agreed upon.
*******************************************************************************/




*Set the directory;
*%let xdr= %str(\\ad.unc.edu\med\tracs\groups\Research\CDWH\Latour_Chase_22-0689\);






/******************************************************************************
					Formats for outcome group assignments 
*******************************************************************************/

*Use a format to change long outcome descriptive to abbreviated version 
that will be used in hierarchy assignment;
proc format; 

	value $outabbr
	'Live Birth (Multiple)' = 'LBM'
	'Live Birth (Singleton)' = 'LBS'
	'Live Birth (Uncategorized)'= 'LBS'
	'Mixed Delivery' ='MLS'
	'Stillbirth' ='SB'
	'Uncategorized Delivery' ='UDL'
	'Spontaneous Abortion'='SAB'
	'Induced Abortion'= 'IAB' 
	'Unspecified Abortion'='UAB'
	'Ectopic/Molar' ='EM'
	'Abortion, Ectopic, or Molar'='AEM'
	;

	/*Assign each outcome type to an outcome class;*/
	value $outclass
	 'LBM','LBS','MLS','SB','UDL' = 'Delivery'
	 'SAB', 'IAB' , 'UAB' = 'Abortion'
	 'EM','AEM' = 'Ectopic'
	 ;

	/*Created a 3-value indicator variable for the code-based and medication 
	 order based information available for a pregnancy outcome. 

	 ---

	 First digit refers to the presence of diagnosis codes (0/1 -- n/y)
	 Second digit refers to the presence of procedure codes (0/1 -- n/y)
	 Third digit refers to the presence of medication orders for mifepristone and misoprostol 
	 	(0/1/2/3 -- n/miso only/mife only/both)*/



	/*Indicator variable (y/n) for if there is any code or order based*/
	/* evidence for that pregnancy outcome*/
	value $outcyn
	 '000'='No' 
	 '100','110','101','111', '010','011' ,'001'='Yes'
	;

	/*Numeric indicator variable (y=1/n=0) of whether there is any*/
	/*code or order based evidence for that pregnancy outcome.*/
	value $outcynn
	 '000'=0 
	 '100','110','101','111', '010','011' ,'001'=1
	  '102','112','012' ,'002'=1
	 '103','113', '013' ,'003'=1
	;


	/*Create a letter-based indicator variable for the information
	available to support the outcome assignment*/
	value $outcdm(multilabel)  /*PR 1st should make this priority for freq*/
	 '000'='None' 
	 '010','011' ,'110', '111'= 'PR'
	 '100','110','101','111' ='DX'
	 '001', '101','011','111'='RX'
	 '002', '102','012','112'='RX'
	 '003', '103','013','113'='RX'
	;

	value $outcd  /*PR 1st should make this priority for freq*/
	 '000'='None' 
	 '010','011' ,'110', '111', '012','112', '013','113'= 'PR'
	 '100', '101' ,'102','103' ='DX'
	 '001', '002' ,'003'='RX'
	;


	/*Create an indictor that prioritizes the presence of a med (only recorded on abortion outcomes)
	over the presence of a diagnosis code*/
	value $outcdrxoverdx
		'000' = 'None'
		'010','011' ,'110', '111', '012','112', '013','113'= 'PR'
		'101' ,'102','103', '001', '002' ,'003' = 'RX' /*only abortion outcomes should have RXs*/
		'100' = 'DX'
	;


	/*Create an indicator format for if someone has a diagnosis code for an outcome
	-- 0/1 -- n/y */
	value $outcddx
	 '100','110','101','111' ='1' other='0'
	 '102','112', '103','113' = '1'
	;

	/*Create an indicator format for if someone has a procedure code for an outcome
	-- 0/1 -- n/y */
	value $outcdpr
	 '010','011' ,'110', '111'= 1 other=0
	       '012'       , '112' = 1  
	       '013'       , '113' = 1
	;

	/*Create an indicator format for if someone has a medication order for an outcome
	-- 0/1 -- n/y */
	value $outcdrx
	 '001', '101', '011','111'=  1 other=0
	 '002', '102', '012','112'= 1
	 '003', '103', '013','113'= 1
	;

	/*Create an indicator format for if someone has a medication order for an outcome, 
	but stratify by the type of medication
	(0/1/2/3 -- n/miso only/mife only/both)
	*/
	value $outcdrxb
	 '001', '101', '011','111'=  1 other=0
	 '002', '102', '012','112'= 2
	 '003', '103', '013','113'= 3
	;*for abortion want to know diff between mife/miso;

run;



/*Create formats that we are going to use when applying the pregnancy outcome
assignment algorithms*/

proc format;

	/*Indicator for type of discordant outcome in algorithm 3*/
	 value discord3alg 1='Delivery procedure, Delivery outcome concordant'
	                   2='Delivery procedure, Delivery outcome discordant'
	                   3='Delivery procedure, not found'
	     ;

	/*Indicator for type of discordant outcome in algorithm 4*/
	 value discord4alg 1='Delivery procedure, Delivery outcome concordant'
	                   2='Delivery procedure, Delivery outcome discordant'
	                   3='Non-Delivery procedure'
	                   4='No procedure'
	     ;
run;
