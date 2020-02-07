
%LET index = %SCAN(&SYSPARM, 1);

DATA d;
INFILE "data/data_&index..csv" DELIMITER = "," FIRSTOBS=2;
INPUT id $10. blk trt y;
RUN;

PROC SORT DATA = d;
BY id;
RUN;

ODS SELECT NONE;

PROC GLIMMIX DATA = d;
  BY id;
  CLASS blk trt;
  MODEL y = trt /DIST = POIS;
  RANDOM INT trt /SUBJECT = blk;
  COVTEST /CL(TYPE = WALD);
  ODS OUTPUT 
    covparms = wald_normal
    convergenceStatus = conv_normal;
RUN;

PROC GLIMMIX DATA = d;
  BY id;
  CLASS blk trt;
  MODEL y = trt /DIST = NEGBIN;
  RANDOM INT /SUBJECT = blk;
  COVTEST /CL(TYPE = WALD);
  ODS OUTPUT 
    covparms = wald_nb
    convergenceStatus = conv_nb;
RUN;

ODS SELECT ALL;

DATA wald_normal;
  MERGE 
    wald_normal (WHERE = (CovParm = 'Intercept'))
    conv_normal;
  BY id;

DATA wald_nb;
  MERGE 
    wald_nb (WHERE = (CovParm = 'Intercept'))
    conv_nb;
  BY id;
RUN;

PROC EXPORT DATA = wald_normal
DBMS = CSV
OUTFILE = "res/wald_normal_&index..csv"
REPLACE;

PROC EXPORT DATA = wald_nb
DBMS = CSV
OUTFILE = "res/wald_nb_&index..csv"
REPLACE;

RUN;
