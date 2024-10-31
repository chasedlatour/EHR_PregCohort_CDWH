************************************************************************************************;          
**** program: Chase_All_LibrariesAndOptions.sas                                                *;
**** purpose: setup libraries needed by all steps in encounter defined pregnancy alg           *;
***                                                                                            *;
***     note: IMPORTANT - check that the INT, OUT libnames point to the correct folder         *;
***                                                                                            *;
***     note: By default NoReplace option is turned on. Alternatively set ACCESS=r (or read)   *;
***           on the libname statement,  if mult libnames point to same directory files may    *;
***           still be overwritten.
***                                                                                            *;
***     Libnames                                                                               *;
***        RAW - files received converted to SAS transformed to code-level datasets            *;
***        INT - files created from raw incorporating billing code reference files             *;
***        OUT - primary output datasets created when applying steps in the algorithm          *;
***        OUTd - subdirectory under out for detail-level pren-outcome pregnancy (steps3-10)   *;
************************************************************************************************;          
*/run;;;;


/*Turn on system options */

options nodate dlcreatedir nocenter;

options noreplace; *Note: this will prevent writing over existing files;

ods listing;

/*Specify the location of files on the server*/
/*Point to location of files - string/text will be added to libname statements; */

%let xdr= %str(\\ad.unc.edu\med\tracs\groups\Research\CDWH\Latour_Chase_22-0689);
/*CDL version:
%let xdr= %str(W:\);
*/

/*Point to location of algorithm program files;*/

%let algpath=&xdr.\programs\preg_encounter_algorithm;				*<====== CHECK PATH;


/*Specify the libraries where the data are stored*/

*Data received - most ^raw^ code files, manipulations but no linkage to the reference files;
libname Raw "&xdr\data\20230328_Data_Pull_01\analysis"  ;


*Data received and divided according to reference files as of date run;
*note: add datestamp to filename (option DLCREATEDIR above will create folder if doesnt exist);

libname int "&xdr.\data\20230328_Data_Pull_01\analysis\int_202410" ;                *<====== CHECK PATH;

/*libname int "&xdr.\data\20230328_Data_Pull_01\analysis\int_202408" ;                *<====== CHECK PATH;*/


*Data received and divided according to reference files as of date run;
*note: add datestamp to filename (option DLCREATEDIR above will create folder if doesnt exist);

libname Out "&xdr.\data\20230328_Data_Pull_01\analysis\int_202410\preg_alg" ;       *<====== CHECK PATH;
libname Outd "&xdr.\data\20230328_Data_Pull_01\analysis\int_202410\preg_alg\details" ;       *<====== CHECK PATH;

/*libname Out "&xdr.\data\20230328_Data_Pull_01\analysis\int_202408\preg_alg" ;       *<====== CHECK PATH;*/
/*libname Outd "&xdr.\data\20230328_Data_Pull_01\analysis\int_202408\preg_alg\details" ;       *<====== CHECK PATH;*/




