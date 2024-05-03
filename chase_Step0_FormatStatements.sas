/******************************************************************************

Program: Chase_Step0_FormatStatements.sas                                                                                                                          chase_0b_FormatStatements.sas
         (was chase_ob_formatstatements.sas)
Programmer: Sharon 

Purpose: formats / blocks of code needed for pregnancy algorithm steps
*******************************************************************************/

%let xdr= %str(\\ad.unc.edu\med\tracs\groups\Research\CDWH\Latour_Chase_22-0689\);


/* formats for outcome group assignments */
*use a format to change long outcome desc to abbr that will be used in hierarchy assign;
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
;*06.01 table 1 changed - EM now includes AEM or EM;
/*nope read table wrong 'Abortion, Ectopic, or Molar'='EM'*/

*90.7 - outcome type to outcome class;
value $outclass
 'LBM','LBS','MLS','SB','UDL' = 'Delivery'
 'SAB', 'IAB' , 'UAB' = 'Abortion'
 'EM','AEM' = 'Ectopic'
 ;

value $outcyn
 '000'='No' 
 '100','110','101','111', '010','011' ,'001'='Yes'
;
value $outcynn
 '000'=0 
 '100','110','101','111', '010','011' ,'001'=1
  '102','112','012' ,'002'=1
 '103','113', '013' ,'003'=1
;
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

value $outcddx
 '100','110','101','111' ='1' other='0'
 '102','112', '103','113' = '1'
;
value $outcdpr
 '010','011' ,'110', '111'= 1 other=0
       '012'       , '112' = 1  
       '013'       , '113' = 1
;
value $outcdrx
 '001', '101', '011','111'=  1 other=0
 '002', '102', '012','112'= 1
 '003', '103', '013','113'= 1
;

value $outcdrxb
 '001', '101', '011','111'=  1 other=0
 '002', '102', '012','112'= 2
 '003', '103', '013','113'= 3
;*for abortion want to know diff between mife/miso;

run;


proc format;
 value discord3alg 1='Delivery procedure, Delivery outcome concordant'
                   2='Delivery procedure, Delivery outcome discordant'
                   3='Delivery procedure, not found'
     ;

 value discord4alg 1='Delivery procedure, Delivery outcome concordant'
                   2='Delivery procedure, Delivery outcome discordant'
                   3='Non-Delivery procedure'
                   4='No procedure'
     ;
run;
