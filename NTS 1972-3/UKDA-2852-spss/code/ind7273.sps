TITLE        'NATIONAL TRAVEL SURVEY 1972-1973 - SN:2852'.
FILE HANDLE  NTS/NAME = 'h:/kdenn/2852/o2852.dat'.
FILE TYPE    NESTED FILE=NTS RECORD=RECID 1 WILD=NOWARN.
RECORD TYPE 3.
DATA LIST   /RECID 1 HHOLDID 2-9 NOJOUR 10-11 I1 12-13 I2 TO I6 14-18
             I7 TO I10 19-26 I11 TO I15 27-31 I16 32-33
             I17 34 I18 35 I19 36-37 I20 TO I22 38-40 I23 41-42
             I24 43 I25 44 I27 45 I28 46 I30 47 I31 TO I35 48-57.
END FILE TYPE.
VAR LABELS   RECID   RECORD INDICATOR
             HHOLDID HOUSEHOLD SERIAL NUMBER/
             NOJOUR  NUMBER OF JOURNEYS/
             I1  PERSON NUMBER/
             I2  TRAVEL RECORD COMPLETED/
             I3  IND SCHEDULE COMPLETED/
             I4  HOUSEHOLD STATUS/
             I5  MARITAL STATUS/
             I6  WORKING STATUS/
             I7  SEG OF INDIVIDUAL/
             I8  GROSS INCOME/
             I9  AGE OF PERSON/
             I10 AGE-SEX OF PERSON/
             I11 SEX/
             I12 HOUSEWIFE/
             I13 CAR LICENCE/
             I14 MOTORCYCLE LICENCE/
             I15 OTHER LICENCE/
             I16 DRIVING EXPERIENCE/
             I17 EVER HELD LICENCE/
             I18 WHEN LICENCE GIVEN UP/
             I19 YEARS LAPSE SINCE LICENCE GIVEN UP/
             I20 TYPE OF LICENCE/
             I21 ABLE TO USE PUBLIC TRANSPORT/
             I22 ACCESS OF BUS TRANSPORT/
             I23 ACCESS OF RAIL TRANSPORT/
             I24 ACCESS OF ALL PUBLIC/
             I25 QUALITY OF LOCAL BUS/
             I27 NUMBER OF CRITICISMS OF LOCAL BUS SERVICE/
             I28 QUALITY OF LOCAL TRAIN/
             I30 NUMBER OF CRITICISMS OF LOCAL TRAIN/
             I31 TOTAL INDIVIDUAL JOURNEYS/
             I32 TOTAL INDIVIDUAL STAGES/
             I33 TOTAL INDIVIDUAL MILEAGE/
             I34 TOTAL INDIVIDUAL EXP/
             I35 TOTAL INDIVIDUAL TRANSPORT (7TH DAY)/.
EXPORT OUTFILE='h:\kdenn\2852\ind7273.por'.
EXIT.