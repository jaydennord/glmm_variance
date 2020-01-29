
%LET index = %SCAN(&SYSPARM, 1);

DATA d;
INFILE "data/data_&index..csv" DELIMITER = "," FIRSTOBS=2;
INPUT id $ blk trt y;
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
  ODS OUTPUT covparms = wald_normal;
RUN;

PROC GLIMMIX DATA = d;
  BY id;
  CLASS blk trt;
  MODEL y = trt /DIST = NEGBIN;
  RANDOM INT /SUBJECT = blk;
  COVTEST /CL(TYPE = WALD);
  ODS OUTPUT covparms = wald_nb;
RUN;

ODS SELECT ALL;

PROC EXPORT DATA = wald_normal
DBMS = CSV
OUTFILE = "res/wald_normal_&index..csv"
REPLACE;

PROC EXPORT DATA = wald_nb
DBMS = CSV
OUTFILE = "res/wald_nb_&index..csv"
REPLACE;

RUN;
