C     SUBROUTINE        B  C  T  I  M  E           SUTRA VERSION 4.0     BCTIME.........100
C                                                                        BCTIME.........200
C *** PURPOSE :                                                          BCTIME.........300
C ***  USER-PROGRAMMED SUBROUTINE WHICH ALLOWS THE USER TO SPECIFY:      BCTIME.........400
C ***   (1) TIME-DEPENDENT SPECIFIED PRESSURES AND TIME-DEPENDENT        BCTIME.........500
C ***       CONCENTRATIONS OR TEMPERATURES OF INFLOWS AT THESE POINTS    BCTIME.........600
C ***   (2) TIME-DEPENDENT SPECIFIED CONCENTRATIONS OR TEMPERATURES      BCTIME.........700
C ***   (3) TIME-DEPENDENT FLUID SOURCES AND CONCENTRATIONS              BCTIME.........800
C ***       OR TEMPERATURES OF INFLOWS AT THESE POINTS                   BCTIME.........900
C ***   (4) TIME-DEPENDENT ENERGY OR SOLUTE MASS SOURCES                 BCTIME........1000
C                                                                        BCTIME........1100
      SUBROUTINE BCTIME(IPBC,PBC,IUBC,UBC,QIN,UIN,QUIN,IQSOP,IQSOU,      BCTIME........1200
     1   IPBG,PBG1,QPBG1,PBG2,QPBG2,CPQL1,CPQL2,UPBGI,UPBGO,CUPBGO,      BCTIME........1300
     2   IUBG,UBG1,QUBG1,UBG2,QUBG2,IPBCT,IUBCT,IQSOPT,IQSOUT,           BCTIME........1400
     3   IPBGT,IUBGT,X,Y,Z,IBCPBC,IBCUBC,IBCSOP,IBCSOU,IBCPBG,IBCUBG,    BCTIME........1500
     4   IBCSF,IBCSS,IBCSP,IBCSU,IBCSPG,IBCSUG)                          BCTIME........1600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                BCTIME........1700
      LOGICAL OPND                                                       BCTIME........1800
      CHARACTER*1 CPQL1(NPBG),CPQL2(NPBG)                                BCTIME........1900
      CHARACTER*3 CUPBGO(NPBG)                                           BCTIME........2000
      DIMENSION IPBC(NBCN),PBC(NBCN),IUBC(NBCN),UBC(NBCN),               BCTIME........2100
     1   QIN(NN),UIN(NN),QUIN(NN),IQSOP(NSOP),IQSOU(NSOU),               BCTIME........2200
     2   X(NN),Y(NN),Z(NN)                                               BCTIME........2300
      DIMENSION PBG1(NPBG),QPBG1(NPBG),PBG2(NPBG),QPBG2(NPBG),           BCTIME........2400
     1   UPBGI(NPBG),UPBGO(NPBG),UBG1(NUBG),QUBG1(NUBG),                 BCTIME........2500
     2   UBG2(NUBG),QUBG2(NUBG)                                          BCTIME........2600
      DIMENSION IPBG(NPBG),IUBG(NUBG)                                    BCTIME........2700
      INTEGER(1) IBCPBC(NBCN),IBCUBC(NBCN),IBCSOP(NSOP),IBCSOU(NSOU),    BCTIME........2800
     1   IBCPBG(NPBG),IBCUBG(NUBG)                                       BCTIME........2900
      INTEGER IBCSP(NBCN),IBCSU(NBCN),IBCSF(NSOP),IBCSS(NSOU),           BCTIME........3000
     1   IBCSPG(NPBG),IBCSUG(NUBG)                                       BCTIME........3100
      COMMON /DIMS/ NN,NE,NIN,NBI,NCBI,NB,NBHALF,NPBC,NUBC,              BCTIME........3200
     1   NSOP,NSOU,NBCN,NPBG,NUBG,NCIDB                                  BCTIME........3300
      COMMON /FUNITS/ K00,K0,K1,K2,K3,K4,K5,K6,K7,K8,K9,                 BCTIME........3400
     1   K10,K11,K12,K13,K14,K15,K16,K17,K18,K19,K20,K21,K22,K23         BCTIME........3500
      COMMON /GRAVEC/ GRAVX,GRAVY,GRAVZ                                  BCTIME........3600
      COMMON /TIMES/ DELT,TSEC,TMIN,THOUR,TDAY,TWEEK,TMONTH,TYEAR,       BCTIME........3700
     1   TMAX,DELTP,DELTU,DLTPM1,DLTUM1,IT,ITBCS,ITRST,ITMAX,TSTART      BCTIME........3800
C                                                                        BCTIME........3900
C.....DEFINITION OF REQUIRED VARIABLES                                   BCTIME........4000
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME........4100
C     NN = EXACT NUMBER OF NODES IN MESH                                 BCTIME........4200
C     NPBC = EXACT NUMBER OF SPECIFIED PRESSURE NODES                    BCTIME........4300
C     NUBC = EXACT NUMBER OF SPECIFIED CONCENTRATION                     BCTIME........4400
C            OR TEMPERATURE NODES                                        BCTIME........4500
C     NPBG = EXACT NUMBER OF SPECIFIED GENERALIZED-FLOW NODES            BCTIME........4600
C     NUBG = EXACT NUMBER OF SPECIFIED GENERALIZED-TRANSPORT NODES       BCTIME........4700
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME........4800
C     IT = NUMBER OF CURRENT TIME STEP                                   BCTIME........4900
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME........5000
C     TSEC = TIME AT END OF CURRENT TIME STEP IN SECONDS                 BCTIME........5100
C     TMIN = TIME AT END OF CURRENT TIME STEP IN MINUTES                 BCTIME........5200
C     THOUR = TIME AT END OF CURRENT TIME STEP IN HOURS                  BCTIME........5300
C     TDAY = TIME AT END OF CURRENT TIME STEP IN DAYS                    BCTIME........5400
C     TWEEK = TIME AT END OF CURRENT TIME STEP IN WEEKS                  BCTIME........5500
C     TMONTH = TIME AT END OF CURRENT TIME STEP IN MONTHS                BCTIME........5600
C     TYEAR = TIME AT END OF CURRENT TIME STEP IN YEARS                  BCTIME........5700
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME........5800
C     PBC(IP) = SPECIFIED PRESSURE VALUE AT IP(TH) SPECIFIED             BCTIME........5900
C               PRESSURE NODE                                            BCTIME........6000
C     UBC(IP) = SPECIFIED CONCENTRATION OR TEMPERATURE VALUE OF ANY      BCTIME........6100
C               INFLOW OCCURRING AT IP(TH) SPECIFIED PRESSURE NODE       BCTIME........6200
C     IPBC(IP) = ACTUAL NODE NUMBER OF IP(TH) SPECIFIED PRESSURE NODE    BCTIME........6300
C                {WHEN NODE NUMBER I=IPBC(IP) IS NEGATIVE (I<0),         BCTIME........6400
C                VALUES MUST BE SPECIFIED FOR PBC AND UBC.}              BCTIME........6500
C     IBCPBC(IP) = INDICATOR OF WHERE THIS PRESSURE SPECIFICATION        BCTIME........6600
C                  WAS MADE. MUST BE SET TO -1 TO INDICATE THAT THIS     BCTIME........6700
C                  SPECIFICATION WAS MADE IN SUBROUTINE BCTIME.          BCTIME........6800
C     IBCSP(IP) = INDICATOR OF WHERE THIS PRESSURE SPECIFICATION         BCTIME........6900
C                 WAS MADE. MUST BE SET TO 0 TO INDICATE THAT THIS       BCTIME........7000
C                 SPECIFICATION WAS MADE IN SUBROUTINE BCTIME.           BCTIME........7100
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME........7200
C     UBC(IUP) = SPECIFIED CONCENTRATION OR TEMPERATURE VALUE AT         BCTIME........7300
C                IU(TH) SPECIFIED CONCENTRATION OR TEMPERATURE NODE      BCTIME........7400
C                (WHERE IUP=IU+NPBC)                                     BCTIME........7500
C     IUBC(IUP) = ACTUAL NODE NUMBER OF IU(TH) SPECIFIED CONCENTRATION   BCTIME........7600
C                 OR TEMPERATURE NODE (WHERE IUP=IU+NPBC)                BCTIME........7700
C                 {WHEN NODE NUMBER I=IUBC(IU) IS NEGATIVE (I<0),        BCTIME........7800
C                 A VALUE MUST BE SPECIFIED FOR UBC.}                    BCTIME........7900
C     IBCUBC(IUP) = INDICATOR OF WHERE THIS CONCENTRATION OR TEMPERATURE BCTIME........8000
C                  SPECIFICATION WAS MADE. MUST BE SET TO -1 TO INDICATE BCTIME........8100
C                  THAT THIS SPECIFICATION WAS MADE IN SUBROUTINE        BCTIME........8200
C                  BCTIME.                                               BCTIME........8300
C     IBCSU(IUP) = INDICATOR OF WHERE THIS CONCENTRATION OR TEMPERATURE  BCTIME........8400
C                  SPECIFICATION WAS MADE. MUST BE SET TO 0 TO INDICATE  BCTIME........8500
C                  THAT THIS SPECIFICATION WAS MADE IN SUBROUTINE        BCTIME........8600
C                  BCTIME.                                               BCTIME........8700
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME........8800
C     IQSOP(IQP) = NODE NUMBER OF IQP(TH) FLUID SOURCE NODE.             BCTIME........8900
C                  {WHEN NODE NUMBER I=IQSOP(IQP) IS NEGATIVE (I<0),     BCTIME........9000
C                  VALUES MUST BE SPECIFIED FOR QIN AND UIN.}            BCTIME........9100
C     QIN(-I) = SPECIFIED FLUID SOURCE VALUE AT NODE (-I)                BCTIME........9200
C     UIN(-I) = SPECIFIED CONCENTRATION OR TEMPERATURE VALUE OF ANY      BCTIME........9300
C               INFLOW OCCURRING AT FLUID SOURCE NODE (-I)               BCTIME........9400
C     IBCSOP(IQP) = INDICATOR OF WHERE THIS FLUID SOURCE SPECIFICATION   BCTIME........9500
C                   WAS MADE. MUST BE SET TO -1 TO INDICATE THAT THIS    BCTIME........9600
C                   SPECIFICATION WAS MADE IN SUBROUTINE BCTIME.         BCTIME........9700
C     IBCSF(IQP) = INDICATOR OF WHERE THIS FLUID SOURCE SPECIFICATION    BCTIME........9800
C                  WAS MADE. MUST BE SET TO 0 TO INDICATE THAT THIS      BCTIME........9900
C                  SPECIFICATION WAS MADE IN SUBROUTINE BCTIME.          BCTIME.......10000
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME.......10100
C     IQSOU(IQU) = NODE NUMBER OF IQU(TH) ENERGY OR                      BCTIME.......10200
C                  SOLUTE MASS SOURCE NODE                               BCTIME.......10300
C                  {WHEN NODE NUMBER I=IQSOU(IQU) IS NEGATIVE (I<0),     BCTIME.......10400
C                  A VALUE MUST BE SPECIFIED FOR QUIN.}                  BCTIME.......10500
C     QUIN(-I) = SPECIFIED ENERGY OR SOLUTE MASS SOURCE VALUE            BCTIME.......10600
C                AT NODE (-I)                                            BCTIME.......10700
C     IBCSOU(IQU) = INDICATOR OF WHERE THIS ENERGY OR SOLUTE MASS        BCTIME.......10800
C                   SOURCE SPECIFICATION WAS MADE. MUST BE SET TO -1     BCTIME.......10900
C                   TO INDICATE THAT THIS SPECIFICATION WAS MADE IN      BCTIME.......11000
C                   SUBROUTINE BCTIME.                                   BCTIME.......11100
C     IBCSS(IQU) = INDICATOR OF WHERE THIS ENERGY OR SOLUTE MASS         BCTIME.......11200
C                  SOURCE SPECIFICATION WAS MADE. MUST BE SET TO 0       BCTIME.......11300
C                  TO INDICATE THAT THIS SPECIFICATION WAS MADE IN       BCTIME.......11400
C                  SUBROUTINE BCTIME.                                    BCTIME.......11500
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME.......11600
C     PBG1(IPG) = SPECIFIED PRESSURE VALUE AT THE FIRST OF TWO POINTS    BCTIME.......11700
C                 THAT DEFINE THE LINEAR RELATION BETWEEN FLOW AND       BCTIME.......11800
C                 PRESSURE AT IPG(TH) GENERALIZED-FLOW NODE              BCTIME.......11900
C     QPBG1(IPG) = SPECIFIED FLOW VALUE AT THE FIRST OF TWO POINTS       BCTIME.......12000
C                  THAT DEFINE THE LINEAR RELATION BETWEEN FLOW AND      BCTIME.......12100
C                  PRESSURE AT IPG(TH) GENERALIZED-FLOW NODE             BCTIME.......12200
C     PBG2(IPG) = SPECIFIED PRESSURE VALUE AT THE SECOND OF TWO POINTS   BCTIME.......12300
C                 THAT DEFINE THE LINEAR RELATION BETWEEN FLOW AND       BCTIME.......12400
C                 PRESSURE AT IPG(TH) GENERALIZED-FLOW NODE              BCTIME.......12500
C     QPBG2(IPG) = SPECIFIED FLOW VALUE AT THE SECOND OF TWO POINTS      BCTIME.......12600
C                  THAT DEFINE THE LINEAR RELATION BETWEEN FLOW AND      BCTIME.......12700
C                  PRESSURE AT IPG(TH) GENERALIZED-FLOW NODE             BCTIME.......12800
C     CPQL1(IPG) = TYPE OF LIMIT ON PRESSURE OR FLOW SPECIFIED AT        BCTIME.......12900
C                  THE FIRST OF TWO POINTS THAT DEFINE THE LINEAR        BCTIME.......13000
C                  RELATION BETWEEN FLOW AND PRESSURE AT IPG(TH)         BCTIME.......13100
C                  GENERALIZED-FLOW NODE                                 BCTIME.......13200
C     CPQL2(IPG) = TYPE OF LIMIT ON PRESSURE OR FLOW SPECIFIED AT        BCTIME.......13300
C                  THE SECOND OF TWO POINTS THAT DEFINE THE LINEAR       BCTIME.......13400
C                  RELATION BETWEEN FLOW AND PRESSURE AT IPG(TH)         BCTIME.......13500
C                  GENERALIZED-FLOW NODE                                 BCTIME.......13600
C     UPBGI(IPG) = SPECIFIED CONCENTRATION OR TEMPERATURE VALUE OF       BCTIME.......13700
C                  ANY INFLOW OCCURRING AT IPG(TH) GENERALIZED-FLOW      BCTIME.......13800
C                  NODE                                                  BCTIME.......13900
C     CUPBGO(IPG) = SPECIFIED INDICATOR OF WHETHER THE VALUE OF          BCTIME.......14000
C                   OUTFLOW CONCENTRATION OR TEMPERATURE, UPBGO(IPG),    BCTIME.......14100
C                   IS SPECIFIED DIRECTLY OR RELATIVE TO THE COMPUTED    BCTIME.......14200
C                   VALUE OF CONCENTRATION OR TEMPERATURE AT IPG(TH)     BCTIME.......14300
C                   GENERALIZED-FLOW NODE                                BCTIME.......14400
C     UPBGO(IPG) = SPECIFIED CONCENTRATION OR TEMPERATURE VALUE OF       BCTIME.......14500
C                  ANY OUTFLOW OCCURRING AT IPG(TH) GENERALIZED-FLOW     BCTIME.......14600
C                  NODE                                                  BCTIME.......14700
C     IPBG(IPG) = ACTUAL NODE NUMBER OF IPG(TH) GENERALIZED-FLOW NODE    BCTIME.......14800
C                 {WHEN NODE NUMBER I=IPBG(IPG) IS NEGATIVE (I<0),       BCTIME.......14900
C                 VALUES MUST BE SPECIFIED FOR PBG1, QPBG1, PBG2,        BCTIME.......15000
C                 QPBG2, CPQL1, CPQL2, UPBGI, CUPBGO, AND UPBGO.}        BCTIME.......15100
C     IBCPBG(IPG) = INDICATOR OF WHERE THIS GENERALIZED-FLOW             BCTIME.......15200
C                   SPECIFICATION WAS MADE. MUST BE SET TO -1            BCTIME.......15300
C                   TO INDICATE THAT THIS SPECIFICATION WAS MADE         BCTIME.......15400
C                   IN SUBROUTINE BCTIME.                                BCTIME.......15500
C     IBCSPG(IPG) = INDICATOR OF WHERE THIS GENERALIZED-FLOW             BCTIME.......15600
C                   SPECIFICATION WAS MADE. MUST BE SET TO 0             BCTIME.......15700
C                   TO INDICATE THAT THIS SPECIFICATION WAS MADE         BCTIME.......15800
C                   IN SUBROUTINE BCTIME.                                BCTIME.......15900
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME.......16000
C     UBG1(IUG) = SPECIFIED CONCENTRATION OR TEMPERATURE VALUE AT THE    BCTIME.......16100
C                 FIRST OF TWO POINTS THAT DEFINE THE LINEAR RELATION    BCTIME.......16200
C                 BETWEEN SOLUTE-MASS OR ENERGY FLOW AND CONCENTRATION   BCTIME.......16300
C                 OR TEMPERATURE AT IUG(TH) GENERALIZED-TRANSPORT NODE   BCTIME.......16400
C     QUBG1(IUG) = SPECIFIED SOLUTE-MASS OR ENERGY FLOW VALUE AT THE     BCTIME.......16500
C                  FIRST OF TWO POINTS THAT DEFINE THE LINEAR RELATION   BCTIME.......16600
C                  BETWEEN SOLUTE-MASS OR ENERGY FLOW AND CONCENTRATION  BCTIME.......16700
C                  OR TEMPERATURE AT IUG(TH) GENERALIZED-TRANSPORT NODE  BCTIME.......16800
C     UBG2(IUG) = SPECIFIED CONCENTRATION OR TEMPERATURE VALUE AT THE    BCTIME.......16900
C                 SECOND OF TWO POINTS THAT DEFINE THE LINEAR RELATION   BCTIME.......17000
C                 BETWEEN SOLUTE-MASS OR ENERGY FLOW AND CONCENTRATION   BCTIME.......17100
C                 OR TEMPERATURE AT IUG(TH) GENERALIZED-TRANSPORT NODE   BCTIME.......17200
C     QUBG2(IUG) = SPECIFIED SOLUTE-MASS OR ENERGY FLOW VALUE AT THE     BCTIME.......17300
C                  SECOND OF TWO POINTS THAT DEFINE THE LINEAR RELATION  BCTIME.......17400
C                  BETWEEN SOLUTE-MASS OR ENERGY FLOW AND CONCENTRATION  BCTIME.......17500
C                  OR TEMPERATURE AT IUG(TH) GENERALIZED-TRANSPORT NODE  BCTIME.......17600
C     IUBG(IUG) = ACTUAL NODE NUMBER OF IUG(TH) GENERALIZED-TRANSPORT    BCTIME.......17700
C                 NODE {WHEN NODE NUMBER I=IUBG(IUG) IS NEGATIVE (I<0),  BCTIME.......17800
C                 VALUES MUST BE SPECIFIED FOR UBG1, QUBG1, UBG2, AND    BCTIME.......17900
C                 QUBG2.}                                                BCTIME.......18000
C     IBCUBG(IUG) = INDICATOR OF WHERE THIS GENERALIZED-TRANSPORT        BCTIME.......18100
C                   SPECIFICATION WAS MADE. MUST BE SET TO -1            BCTIME.......18200
C                   TO INDICATE THAT THIS SPECIFICATION WAS MADE         BCTIME.......18300
C                   IN SUBROUTINE BCTIME.                                BCTIME.......18400
C     IBCSUG(IUG) = INDICATOR OF WHERE THIS GENERALIZED-TRANSPORT        BCTIME.......18500
C                   SPECIFICATION WAS MADE. MUST BE SET TO 0             BCTIME.......18600
C                   TO INDICATE THAT THIS SPECIFICATION WAS MADE         BCTIME.......18700
C                   IN SUBROUTINE BCTIME.                                BCTIME.......18800
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME.......18900
C                                                                        BCTIME.......19000
C.....ADDITIONAL USEFUL VARIABLES                                        BCTIME.......19100
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME.......19200
C     "FUNITS" ARE UNIT NUMBERS FOR INPUT AND OUTPUT FILES               BCTIME.......19300
C         AS ASSIGNED IN THE INPUT FILE "SUTRA.FIL"                      BCTIME.......19400
C                                                                        BCTIME.......19500
C     X(I), Y(I), AND Z(I) ARE THE X-, Y-, AND Z-COORDINATES OF NODE I   BCTIME.......19600
C     (FOR 2-D PROBLEMS, Z(I) IS THE MESH THICKNESS AT NODE I)           BCTIME.......19700
C                                                                        BCTIME.......19800
C     GRAVX, GRAVY AND GRAVZ ARE THE X-, Y-, AND Z-COMPONENTS OF THE     BCTIME.......19900
C     GRAVITY VECTOR (FOR 2-D PROBLEMS, GRAVZ = 0)                       BCTIME.......20000
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  BCTIME.......20100
C                                                                        BCTIME.......20200
C                                                                        BCTIME.......20300
C.....NSOPI IS ACTUAL NUMBER OF FLUID SOURCE NODES                       BCTIME.......20400
      NSOPI=NSOP-1                                                       BCTIME.......20500
C.....NSOUI IS ACTUAL NUMBER OF ENERGY OR SOLUTE MASS SOURCE NODES       BCTIME.......20600
      NSOUI=NSOU-1                                                       BCTIME.......20700
C                                                                        BCTIME.......20800
C                                                                        BCTIME.......20900
C                                                                        BCTIME.......21000
C                                                                        BCTIME.......21100
C                                                                        BCTIME.......21200
C                                                                        BCTIME.......21300
      IF(IPBCT) 50,240,240                                               BCTIME.......21400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......21500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......21600
C.....SECTION (1):  SET TIME-DEPENDENT SPECIFIED PRESSURES OR            BCTIME.......21700
C     CONCENTRATIONS (TEMPERATURES) OF INFLOWS AT SPECIFIED              BCTIME.......21800
C     PRESSURE NODES                                                     BCTIME.......21900
C                                                                        BCTIME.......22000
   50 CONTINUE                                                           BCTIME.......22100
      DO 200 IP=1,NPBC                                                   BCTIME.......22200
      I=IPBC(IP)                                                         BCTIME.......22300
      IF(I) 100,200,200                                                  BCTIME.......22400
  100 CONTINUE                                                           BCTIME.......22500
C     NOTE: A FLOW AND TRANSPORT SOLUTION MUST OCCUR FOR ANY             BCTIME.......22600
C           TIME STEP IN WHICH PBC( ) CHANGES.                           BCTIME.......22700
C     PBC(IP) =  ((          ))                                          BCTIME.......22800
C     UBC(IP) =  ((          ))                                          BCTIME.......22900
C.....IBCPBC(IP) MUST BE SET TO -1 AND IBCSP(IP) MUST BE SET TO 0        BCTIME.......23000
C        TO INDICATE THAT PBC(IP) AND/OR UBC(IP) HAVE BEEN SET BY        BCTIME.......23100
C        SUBROUTINE BCTIME.                                              BCTIME.......23200
      IBCPBC(IP) = -1                                                    BCTIME.......23300
      IBCSP(IP) = 0                                                      BCTIME.......23400
  200 CONTINUE                                                           BCTIME.......23500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......23600
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......23700
C                                                                        BCTIME.......23800
C                                                                        BCTIME.......23900
C                                                                        BCTIME.......24000
C                                                                        BCTIME.......24100
C                                                                        BCTIME.......24200
C                                                                        BCTIME.......24300
  240 IF(IUBCT) 250,440,440                                              BCTIME.......24400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......24500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......24600
C.....SECTION (2):  SET TIME-DEPENDENT SPECIFIED                         BCTIME.......24700
C     CONCENTRATIONS (TEMPERATURES)                                      BCTIME.......24800
C                                                                        BCTIME.......24900
  250 CONTINUE                                                           BCTIME.......25000
      DO 400 IU=1,NUBC                                                   BCTIME.......25100
      IUP=IU+NPBC                                                        BCTIME.......25200
      I=IUBC(IUP)                                                        BCTIME.......25300
      IF(I) 300,400,400                                                  BCTIME.......25400
  300 CONTINUE                                                           BCTIME.......25500
C     NOTE: A TRANSPORT SOLUTION MUST OCCUR FOR ANY TIME STEP IN WHICH   BCTIME.......25600
C           UBC( ) CHANGES.  IN ADDITION, IF FLUID PROPERTIES ARE        BCTIME.......25700
C           SENSITIVE TO 'U', THEN A FLOW SOLUTION MUST OCCUR AS WELL.   BCTIME.......25800
C     UBC(IUP) =   ((          ))                                        BCTIME.......25900
C.....IBCUBC(IUP) MUST BE SET TO -1 AND IBCSU(IUP) MUST BE SET TO 0      BCTIME.......26000
C        TO INDICATE THAT UBC(IUP) HAS BEEN SET BY SUBROUTINE BCTIME.    BCTIME.......26100
      IBCUBC(IUP) = -1                                                   BCTIME.......26200
      IBCSU(IUP) = 0                                                     BCTIME.......26300
  400 CONTINUE                                                           BCTIME.......26400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......26500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......26600
C                                                                        BCTIME.......26700
C                                                                        BCTIME.......26800
C                                                                        BCTIME.......26900
C                                                                        BCTIME.......27000
C                                                                        BCTIME.......27100
C                                                                        BCTIME.......27200
  440 IF(IQSOPT) 450,640,640                                             BCTIME.......27300
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......27400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......27500
C.....SECTION (3):  SET TIME-DEPENDENT FLUID SOURCES/SINKS,              BCTIME.......27600
C      OR CONCENTRATIONS (TEMPERATURES) OF SOURCE FLUID                  BCTIME.......27700
C                                                                        BCTIME.......27800
  450 CONTINUE                                                           BCTIME.......27900
      DO 600 IQP=1,NSOPI                                                 BCTIME.......28000
      I=IQSOP(IQP)                                                       BCTIME.......28100
      IF(I) 500,600,600                                                  BCTIME.......28200
  500 CONTINUE                                                           BCTIME.......28300
C     NOTE: A FLOW AND TRANSPORT SOLUTION MUST OCCUR FOR ANY             BCTIME.......28400
C           TIME STEP IN WHICH QIN( ) CHANGES.                           BCTIME.......28500
C     QIN(-I) =   ((           ))                                        BCTIME.......28600
C     NOTE: A TRANSPORT SOLUTION MUST OCCUR FOR ANY                      BCTIME.......28700
C           TIME STEP IN WHICH UIN( ) CHANGES.                           BCTIME.......28800
C     UIN(-I) =   ((           ))                                        BCTIME.......28900
C.....IBCSOP(IQP) MUST BE SET TO -1 AND IBCSF(IQP) MUST BE SET TO 0      BCTIME.......29000
C        TO INDICATE THAT QIN(-I) AND/OR UIN(-I) HAVE BEEN SET BY        BCTIME.......29100
C        SUBROUTINE BCTIME.                                              BCTIME.......29200
      IBCSOP(IQP) = -1                                                   BCTIME.......29300
      IBCSF(IQP) = 0                                                     BCTIME.......29400
  600 CONTINUE                                                           BCTIME.......29500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......29600
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......29700
C                                                                        BCTIME.......29800
C                                                                        BCTIME.......29900
C                                                                        BCTIME.......30000
C                                                                        BCTIME.......30100
C                                                                        BCTIME.......30200
C                                                                        BCTIME.......30300
  640 IF(IQSOUT) 650,840,840                                             BCTIME.......30400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......30500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......30600
C.....SECTION (4):  SET TIME-DEPENDENT SOURCES/SINKS                     BCTIME.......30700
C     OF SOLUTE MASS OR ENERGY                                           BCTIME.......30800
C                                                                        BCTIME.......30900
  650 CONTINUE                                                           BCTIME.......31000
      DO 800 IQU=1,NSOUI                                                 BCTIME.......31100
      I=IQSOU(IQU)                                                       BCTIME.......31200
      IF(I) 700,800,800                                                  BCTIME.......31300
  700 CONTINUE                                                           BCTIME.......31400
C     NOTE: A TRANSPORT SOLUTION MUST OCCUR FOR ANY                      BCTIME.......31500
C           TIME STEP IN WHICH QUIN( ) CHANGES.                          BCTIME.......31600
C     QUIN(-I) =   ((           ))                                       BCTIME.......31700
C.....IBCSOU(IQU) MUST BE SET TO -1 AND IBCSS(IQU) MUST BE SET TO 0      BCTIME.......31800
C        TO INDICATE THAT QUIN(-I) HAS BEEN SET BY SUBROUTINE BCTIME.    BCTIME.......31900
      IBCSOU(IQU) = -1                                                   BCTIME.......32000
      IBCSS(IQU) = 0                                                     BCTIME.......32100
  800 CONTINUE                                                           BCTIME.......32200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......32300
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......32400
C                                                                        BCTIME.......32500
C                                                                        BCTIME.......32600
C                                                                        BCTIME.......32700
C                                                                        BCTIME.......32800
C                                                                        BCTIME.......32900
C                                                                        BCTIME.......33000
  840 IF(IPBGT) 850,1040,1040                                            BCTIME.......33100
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......33200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......33300
C.....SECTION (5):  SET TIME-DEPENDENT GENERALIZED-FLOW CONDITIONS       BCTIME.......33400
C                                                                        BCTIME.......33500
  850 CONTINUE                                                           BCTIME.......33600
      DO 1000 IPG=1,NPBG                                                 BCTIME.......33700
      I=IPBG(IPG)                                                        BCTIME.......33800
      IF(I) 900,1000,1000                                                BCTIME.......33900
  900 CONTINUE                                                           BCTIME.......34000
C     NOTE: A TRANSPORT SOLUTION MUST OCCUR FOR ANY                      BCTIME.......34100
C           TIME STEP IN WHICH CHANGES IN GENERALIZED-FLOW               BCTIME.......34200
C           SPECIFICATIONS CHANGE THE RESULTANT FLOW OF FLUID            BCTIME.......34300
C           IN OR OUT OF THE MODEL.                                      BCTIME.......34400
C     PBG1(IPG) =    ((           ))                                     BCTIME.......34500
C     QPBG1(IPG) =   ((           ))                                     BCTIME.......34600
C     PBG2(IPG) =    ((           ))                                     BCTIME.......34700
C     QPBG2(IPG) =   ((           ))                                     BCTIME.......34800
C     CPQL1(IPG) =   ((           ))                                     BCTIME.......34900
C     CPQL2(IPG) =   ((           ))                                     BCTIME.......35000
C     UPBGI(IPG) =   ((           ))                                     BCTIME.......35100
C     CUPBGO(IPG) =  ((           ))                                     BCTIME.......35200
C     UPBGO(IPG) =   ((           ))                                     BCTIME.......35300
C.....IBCPBG(IPG) MUST BE SET TO -1 AND IBCSPG(IPG) MUST BE SET TO 0     BCTIME.......35400
C        TO INDICATE THAT THE GENERALIZED-FLOW BOUNDARY CONDITION        BCTIME.......35500
C        HAS BEEN SET BY SUBROUTINE BCTIME.                              BCTIME.......35600
      IBCPBG(IPG) = -1                                                   BCTIME.......35700
      IBCSPG(IPG) = 0                                                    BCTIME.......35800
 1000 CONTINUE                                                           BCTIME.......35900
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......36000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......36100
C                                                                        BCTIME.......36200
C                                                                        BCTIME.......36300
C                                                                        BCTIME.......36400
C                                                                        BCTIME.......36500
C                                                                        BCTIME.......36600
C                                                                        BCTIME.......36700
 1040 IF(IUBGT) 1050,1240,1240                                           BCTIME.......36800
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......36900
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......37000
C.....SECTION (6):  SET TIME-DEPENDENT GENERALIZED-TRANSPORT CONDITIONS  BCTIME.......37100
C                                                                        BCTIME.......37200
 1050 CONTINUE                                                           BCTIME.......37300
      DO 1200 IUG=1,NUBG                                                 BCTIME.......37400
      I=IUBG(IUG)                                                        BCTIME.......37500
      IF(I) 1100,1200,1200                                               BCTIME.......37600
 1100 CONTINUE                                                           BCTIME.......37700
C     NOTE: A TRANSPORT SOLUTION MUST OCCUR FOR ANY                      BCTIME.......37800
C           TIME STEP IN WHICH CHANGES IN GENERALIZED-TRANSPORT          BCTIME.......37900
C           SPECIFICATIONS CHANGE THE RESULTANT FLOW OF SOLUTE OR        BCTIME.......38000
C           ENERGY IN OR OUT OF THE MODEL.                               BCTIME.......38100
C     UBG1(IUG) =    ((           ))                                     BCTIME.......38200
C     QUBG1(IUG) =   ((           ))                                     BCTIME.......38300
C     UBG2(IUG) =    ((           ))                                     BCTIME.......38400
C     QUBG2(IUG) =   ((           ))                                     BCTIME.......38500
C.....IBCUBG(IUG) MUST BE SET TO -1 AND IBCSUG(IUG) MUST BE SET TO 0     BCTIME.......38600
C        TO INDICATE THAT THE GENERALIZED-FLOW BOUNDARY CONDITION        BCTIME.......38700
C        HAS BEEN SET BY SUBROUTINE BCTIME.                              BCTIME.......38800
      IBCUBG(IUG) = -1                                                   BCTIME.......38900
      IBCSUG(IUG) = 0                                                    BCTIME.......39000
 1200 CONTINUE                                                           BCTIME.......39100
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......39200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  BCTIME.......39300
C                                                                        BCTIME.......39400
C                                                                        BCTIME.......39500
C                                                                        BCTIME.......39600
C                                                                        BCTIME.......39700
C                                                                        BCTIME.......39800
C                                                                        BCTIME.......39900
 1240 CONTINUE                                                           BCTIME.......40000
C                                                                        BCTIME.......40100
      RETURN                                                             BCTIME.......40200
      END                                                                BCTIME.......40300
C                                                                        BCTIME.......40400
C     SUBROUTINE        U  N  S  A  T              SUTRA VERSION 4.0     UNSAT..........100
C                                                                        UNSAT..........200
C *** PURPOSE :                                                          UNSAT..........300
C *** SUBROUTINE PROVIDES TOTAL SATURATION RELATIONS                     UNSAT..........400
C ***  FOR UNSATURATED CONDITIONS.                                       UNSAT..........500
C ***  -- THREE PRE-PROGRAMMED FUNCTIONS ARE PROVIDED                    UNSAT..........600
C ***  -- USER MAY ADD CODING FOR A NEW FUNCTION.                        UNSAT..........700
C ***                                                                    UNSAT..........800
C *** (TOTAL-WATER SATURATION IS DEFINED AS A FUNCTION OF ONLY PRESSURE, UNSAT..........900
C ***   AND IS INDEPENDENT OF FREEZING.)                                 UNSAT.........1000
C ***                                                                    UNSAT.........1100
C *** SUBROUTINE GIVES:                                                  UNSAT.........1200
C ***  (1)  TOTAL-WATER SATURATION AS A FUNCTION OF PRESSURE ( SW(PRES)  UNSAT.........1300
C ***  (2)  DERIVATIVE OF TOTAL SATURATION WITH RESPECT TO PRESSURE      UNSAT.........1400
C ***       AS A FUNCTION OF EITHER PRESSURE OR SATURATION               UNSAT.........1500
C ***       ( DSWDP(PRES), OR DSWDP(SW) )                                UNSAT.........1600
C ***                                                                    UNSAT.........1700
C ***  USER MAY ADD CODE BETWEEN THESE LINES "====" TO GIVE THE          UNSAT.........1800
C ***  PARTICULAR UNSATURATED RELATIONSHIPS DESIRED.                     UNSAT.........1900
C ***                                                                    UNSAT.........2000
C ***  DIFFERENT FUNCTIONS (3 PRE-PROGRAMMED PLUS USER-PROGRAMMED)       UNSAT.........2100
C ***  MAY BE SELECTED IN THE INPUT DATA IN EACH REGION OF THE MESH,     UNSAT.........2200
C ***  AND THEIR PARAMETER VALUES MAY VARY AMONG REGIONS.                UNSAT.........2300
C ***     REGIONS ARE SPECIFIED BY BOTH NODE NUMBER AND ELEMENT NUMBER   UNSAT.........2400
C ***     IN INPUT DATA FILE FOR UNIT K1 (INP).                          UNSAT.........2500
C ***                                                                    UNSAT.........2600
C                                                                        UNSAT.........2700
      SUBROUTINE UNSAT(SW,DSWDP,PRES,KREG)                               UNSAT.........2800
      USE ALLARR, ONLY : SWMOD,SWPAR,NSWPAR,SWPNM                        UNSAT.........2900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                UNSAT.........3000
      DIMENSION KTYPE(2)                                                 UNSAT.........3100
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  UNSAT.........3200
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    UNSAT.........3300
     2   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IFREEZ,IALSAT,KTYPE          UNSAT.........3400
C                                                                        UNSAT.........3500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  UNSAT.........3600
C     SELECT TOTAL-WATER SATURATION RELATION:                            UNSAT.........3700
C        NONE = NO FUNCTION SPECIFIED                                    UNSAT.........3800
C        VGEN = VAN GENUCHTEN                                            UNSAT.........3900
C        BCOR = BROOKS-COREY                                             UNSAT.........4000
C        PLIN = PIECEWISE-LINEAR                                         UNSAT.........4100
C        UDEF = USER-DEFINED                                             UNSAT.........4200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  UNSAT.........4300
      IF (SWMOD(KREG).EQ."NONE") THEN                                    UNSAT.........4400
C                                                                        UNSAT.........4500
C        -----------------------------------------                       UNSAT.........4600
C        NO UNSATURATED FUNCTION SPECIFIED BY USER                       UNSAT.........4700
C        -----------------------------------------                       UNSAT.........4800
C                                                                        UNSAT.........4900
         SW = 1.D0                                                       UNSAT.........5000
         IF (IALSAT.EQ.1) DSWDP = 0.D0                                   UNSAT.........5100
C                                                                        UNSAT.........5200
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  UNSAT.........5300
      ELSE IF (SWMOD(KREG).EQ."VGEN") THEN                               UNSAT.........5400
C                                                                        UNSAT.........5500
C        -------------------------------------------------               UNSAT.........5600
C        VAN GENUCHTEN (1980) TOTAL-WATER SATURATION MODEL               UNSAT.........5700
C        -------------------------------------------------               UNSAT.........5800
C........SET PARAMETERS FOR THE CURRENT REGION                           UNSAT.........5900
         SWRES = SWPAR(KREG,1)                                           UNSAT.........6000
         AA = SWPAR(KREG,2)                                              UNSAT.........6100
         VN = SWPAR(KREG,3)                                              UNSAT.........6200
C                                                                        UNSAT.........6300
C........TOTAL-WATER SATURATION AS A FUNCTION OF PRESSURE (SW VS. PRES)  UNSAT.........6400
C           (VALUE CALCULATED ON EACH CALL TO UNSAT)                     UNSAT.........6500
         SWRM1 = 1.D0 - SWRES                                            UNSAT.........6600
         AAPVN = 1.D0 + (AA*(-PRES))**VN                                 UNSAT.........6700
         VNF = (VN - 1.D0)/VN                                            UNSAT.........6800
         AAPVNN = AAPVN**VNF                                             UNSAT.........6900
         SW = SWRES + SWRM1/AAPVNN                                       UNSAT.........7000
C                                                                        UNSAT.........7100
         IF (IALSAT.EQ.1) THEN                                           UNSAT.........7200
C...........DERIVATIVE OF TOTAL-WATER SATURATION WITH RESPECT TO PRESSUR UNSAT.........7300
C              AS A FUNCTION OF PRESSURE (DSWDP VS. PRES)                UNSAT.........7400
C              (CALCULATED ONLY WHEN IALSAT=1)                           UNSAT.........7500
            DNUM = AA*(VN - 1.D0)*SWRM1*(AA*(-PRES))**(VN - 1.D0)        UNSAT.........7600
            DNOM = AAPVN*AAPVNN                                          UNSAT.........7700
            DSWDP = DNUM/DNOM                                            UNSAT.........7800
         END IF                                                          UNSAT.........7900
C                                                                        UNSAT.........8000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  UNSAT.........8100
      ELSE IF (SWMOD(KREG).EQ."BCOR") THEN                               UNSAT.........8200
C                                                                        UNSAT.........8300
C        --------------------------------------------------              UNSAT.........8400
C        BROOKS & COREY (1964) TOTAL-WATER SATURATION MODEL              UNSAT.........8500
C        --------------------------------------------------              UNSAT.........8600
C........SET PARAMETERS FOR THE CURRENT REGION                           UNSAT.........8700
         SWRES = SWPAR(KREG,1)                                           UNSAT.........8800
         PENT = SWPAR(KREG,2)                                            UNSAT.........8900
         RLAMB = SWPAR(KREG,3)                                           UNSAT.........9000
C                                                                        UNSAT.........9100
C........TOTAL-WATER SATURATION AS A FUNCTION OF PRESSURE (SW VS. PRES)  UNSAT.........9200
C           (VALUE CALCULATED ON EACH CALL TO UNSAT)                     UNSAT.........9300
         PEFF = MIN(PRES, PENT)                                          UNSAT.........9400
         PRATIO = PENT/PEFF                                              UNSAT.........9500
         SWXTRA = (1.D0 - SWRES)*(PRATIO)**(-RLAMB)                      UNSAT.........9600
         SW = SWRES + SWXTRA                                             UNSAT.........9700
C                                                                        UNSAT.........9800
         IF (IALSAT.EQ.1) THEN                                           UNSAT.........9900
C...........DERIVATIVE OF TOTAL-WATER SATURATION WITH RESPECT TO PRESSUR UNSAT........10000
C              AS A FUNCTION OF PRESSURE (DSWDP VS. PRES)                UNSAT........10100
C              (CALCULATED ONLY WHEN IALSAT=1)                           UNSAT........10200
            IF (PEFF.LT.PENT) THEN                                       UNSAT........10300
               DSWDP = -RLAMB*SWXTRA/PEFF                                UNSAT........10400
            ELSE                                                         UNSAT........10500
               DSWDP = 0.D0                                              UNSAT........10600
            END IF                                                       UNSAT........10700
         END IF                                                          UNSAT........10800
C                                                                        UNSAT........10900
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  UNSAT........11000
      ELSE IF (SWMOD(KREG).EQ."PLIN") THEN                               UNSAT........11100
C                                                                        UNSAT........11200
C        ---------------------------------------                         UNSAT........11300
C        PIECEWISE LINEAR WATER SATURATION MODEL                         UNSAT........11400
C        ---------------------------------------                         UNSAT........11500
C........SET PARAMETERS FOR THE CURRENT REGION                           UNSAT........11600
         SWRES = SWPAR(KREG,1)                                           UNSAT........11700
         PENT = SWPAR(KREG,2)                                            UNSAT........11800
         PSWRES = SWPAR(KREG,3)                                          UNSAT........11900
C........TOTAL-WATER SATURATION AS A FUNCTION OF PRESSURE (SW VS. PRES)  UNSAT........12000
C           (VALUE CALCULATED ON EACH CALL TO UNSAT)                     UNSAT........12100
         SLOPES = (1.D0 - SWRES)/(PSWRES - PENT)                         UNSAT........12200
         IF (PRES.LE.PSWRES) THEN                                        UNSAT........12300
            SW = SWRES                                                   UNSAT........12400
         ELSE IF (PRES.GE.PENT) THEN                                     UNSAT........12500
            SW = 1.D0                                                    UNSAT........12600
         ELSE                                                            UNSAT........12700
            SW = 1.D0 + SLOPES*(PENT - PRES)                             UNSAT........12800
         END IF                                                          UNSAT........12900
C                                                                        UNSAT........13000
         IF (IALSAT.EQ.1) THEN                                           UNSAT........13100
C...........DERIVATIVE OF TOTAL-WATER SATURATION WITH RESPECT TO         UNSAT........13200
C               PRESSURE, AS A FUNCTION OF PRESSURE (DSWDP VS. PRES)     UNSAT........13300
C              (CALCULATED ONLY WHEN IALSAT=1)                           UNSAT........13400
            IF (PRES.LE.PSWRES) THEN                                     UNSAT........13500
               DSWDP = 0.D0                                              UNSAT........13600
            ELSE IF (PRES .GE. PENT) THEN                                UNSAT........13700
               DSWDP = 0.D0                                              UNSAT........13800
            ELSE                                                         UNSAT........13900
               DSWDP = -SLOPES                                           UNSAT........14000
            END IF                                                       UNSAT........14100
         END IF                                                          UNSAT........14200
C                                                                        UNSAT........14300
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  UNSAT........14400
      ELSE IF (SWMOD(KREG).EQ."UDEF") THEN                               UNSAT........14500
C                                                                        UNSAT........14600
C        ------------------                                              UNSAT........14700
C        USER-DEFINED MODEL                                              UNSAT........14800
C        ------------------                                              UNSAT........14900
C                                                                        UNSAT........15000
C........TOTAL-WATER SATURATION AS A FUNCTION OF PRESSURE(SW VS. PRES)   UNSAT........15100
C           (VALUE CALCULATED ON EACH CALL TO UNSAT) AND                 UNSAT........15200
C        DERIVATIVE OF SATURATION WITH RESPECT TO PRESSURE, AS A         UNSAT........15300
C           FUNCTION OF PRESSURE (DSWDP VS. PRES)                        UNSAT........15400
C           (VALUE CALCULATED ONLY WHEN IALSAT=1)                        UNSAT........15500
C                                                                        UNSAT........15600
C                                                                        UNSAT........15700
C        ************************************************************    UNSAT........15800
C        * CODING MUST GIVE A VALUE TO SATURATION, SW, AND TO       *    UNSAT........15900
C        * DERIVATIVE OF SATURATION WITH RESPECT TO PRESSURE, DSWDP *    UNSAT........16000
C        ************************************************************    UNSAT........16100
C                                                                        UNSAT........16200
C        *****************************************************           UNSAT........16300
C        *  EXTRACT MODEL PARAMETERS FROM ARRAY SWPAR,       *           UNSAT........16400
C        *  WHERE SWPAR(KREG,NP) IS THE NP-th PARAMETER IN   *           UNSAT........16500
C        *  UNSATURATED/FREEZING REGION KREG.                *           UNSAT........16600
C        *                                                   *           UNSAT........16700
C        *  NOTE THAT THE FIRST PARAMETER (NP=1) **MUST**    *           UNSAT........16800
C        *  BE THE RESIDUAL TOTAL-WATER SATURATION, SWRES.   *           UNSAT........16900
C        *****************************************************           UNSAT........17000
C                                                                        UNSAT........17100
C======================================================================= UNSAT........17200
C======================================================================= UNSAT........17300
C======================================================================= UNSAT........17400
C                                                                        UNSAT........17500
C     E X A M P L E   C O D I N G   FOR                                  UNSAT........17600
C     MESH WITH ONE OR MORE REGIONS OF UNSATURATED PROPERTIES USING      UNSAT........17700
C     TWO-PARAMETER RELATIVE-PERMEABILITY RELATIONSHIP OF                UNSAT........17800
C     VAN GENUCHTEN(1980)                                                UNSAT........17900
C        (1) VG RESIDUAL SATURATION PARAMETER, SWRES, IN UNITS {L**0}    UNSAT........18000
C        (2) VG PARAMETER, AA, GIVEN IN UNITS [(L s)/M ]                 UNSAT........18100
C        (2) VG PARAMETER, VN, GIVEN IN UNITS [L**0]                     UNSAT........18200
C        SUTRA PARAMETER, RKRES, GIVEN IN UNITS [L**0]                   UNSAT........18300
C                                                                        UNSAT........18400
C        *************************************************************** UNSAT........18500
C        * GET FUNCTION PARAMETERS FROM ARRAY SWPAR FOR CURRENT REGION * UNSAT........18600
C        *                                                             * UNSAT........18700
C        * NOTE THAT THE FIRST PARAMETER, SWPAR(KREG,1), **MUST** BE   * UNSAT........18800
C        * THE RESIDUAL TOTAL-WATER SATURATION, SWRES                  * UNSAT........18900
C        *************************************************************** UNSAT........19000
         SWRES = SWPAR(KREG,1)                                           UNSAT........19100
         AA = SWPAR(KREG,2)                                              UNSAT........19200
         VN = SWPAR(KREG,3)                                              UNSAT........19300
C                                                                        UNSAT........19400
C        ************************                                        UNSAT........19500
C        * ASSIGN A VALUE TO SW *                                        UNSAT........19600
C        ************************                                        UNSAT........19700
C........VALUE IS CALCULATED ON EACH CALL TO UNSAT                       UNSAT........19800
         SWRM1 = 1.D0 - SWRES                                            UNSAT........19900
         AAPVN = 1.D0 + (AA*(-PRES))**VN                                 UNSAT........20000
         VNF = (VN - 1.D0)/VN                                            UNSAT........20100
         AAPVNN = AAPVN**VNF                                             UNSAT........20200
         SW = SWRES + SWRM1/AAPVNN                                       UNSAT........20300
C                                                                        UNSAT........20400
C        ***************************                                     UNSAT........20500
C        * ASSIGN A VALUE TO DSWDP *                                     UNSAT........20600
C        ***************************                                     UNSAT........20700
C........CALCULATED ONLY WHEN IALSAT=1 (WHEN SUTRA NEEDS DSWDP VALUE)    UNSAT........20800
         IF (IALSAT.EQ.1) THEN                                           UNSAT........20900
            DNUM = AA*(VN - 1.D0)*SWRM1*(AA*(-PRES))**(VN - 1.D0)        UNSAT........21000
            DNOM = AAPVN*AAPVNN                                          UNSAT........21100
            DSWDP = DNUM/DNOM                                            UNSAT........21200
         END IF                                                          UNSAT........21300
C                                                                        UNSAT........21400
C     USER SHOULD NOTE THAT THIS SUBROUTINE IS NOT CALLED FOR            UNSAT........21500
C     CALCULATIONS AT POINTS THAT ARE FULLY SATURATED (PRES>0).          UNSAT........21600
C     IN THIS CASE, VALUES FOR SW AND DSWDP, ARE AUTOMATICALLY SET,      UNSAT........21700
C     IN SUBROUTINE ALLSAT, TO SW=1.0 AND DSWDP=0.0                      UNSAT........21800
C                                                                        UNSAT........21900
C        -------------------------                                       UNSAT........22000
C        END OF USER-DEFINED MODEL                                       UNSAT........22100
C        -------------------------                                       UNSAT........22200
C                                                                        UNSAT........22300
C======================================================================= UNSAT........22400
C======================================================================= UNSAT........22500
C======================================================================= UNSAT........22600
C                                                                        UNSAT........22700
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  UNSAT........22800
      END IF                                                             UNSAT........22900
C                                                                        UNSAT........23000
 1800 RETURN                                                             UNSAT........23100
C                                                                        UNSAT........23200
      END                                                                UNSAT........23300
C                                                                        UNSAT........23400
C     SUBROUTINE        L  I  Q  S  A  T           SUTRA VERSION 4.0     LIQSAT.........100
C                                                                        LIQSAT.........200
C *** PURPOSE :                                                          LIQSAT.........300
C *** SUBROUTINE PROVIDES LIQUID-WATER SATURATION RELATIONS              LIQSAT.........400
C ***  FOR SATURATED CONDITIONS.                                         LIQSAT.........500
C ***  -- THREE PRE-PROGRAMMED FUNCTIONS ARE PROVIDED                    LIQSAT.........600
C ***  -- USER MAY ADD CODING FOR A NEW FUNCTION.                        LIQSAT.........700
C ***                                                                    LIQSAT.........800
C *** (LIQUID-WATER SATURATION IS DEFINED AS A FUNCTION OF ONLY          LIQSAT.........900
C ***   TEMPERATURE AND IS INDEPENDENT OF PRESSURE DESATURATION.)        LIQSAT........1000
C ***                                                                    LIQSAT........1100
C *** SUBROUTINE GIVES:                                                  LIQSAT........1200
C ***  (1)  LIQUID-WATER SATURATION AS A FUNCTION OF TEMPERATURE         LIQSAT........1300
C ***       ( SLSAT(TEMP) )                                              LIQSAT........1400
C ***  (2)  DERIVATIVE OF LIQUID-WATER SATURATION WITH RESPECT TO        LIQSAT........1500
C ***       TEMPERATURE AS A FUNCTION OF TEMPERATURE ( DSLSATDT(TEMP) )  LIQSAT........1600
C ***                                                                    LIQSAT........1700
C ***  USER MAY ADD CODE BETWEEN THESE LINES "====" TO GIVE THE          LIQSAT........1800
C ***  PARTICULAR UNSATURATED RELATIONSHIPS DESIRED.                     LIQSAT........1900
C ***                                                                    LIQSAT........2000
C ***  DIFFERENT FUNCTIONS (3 PRE-PROGRAMMED PLUS USER-PROGRAMMED)       LIQSAT........2100
C ***  MAY BE SELECTED IN THE INPUT DATA IN EACH REGION OF THE MESH,     LIQSAT........2200
C ***  AND THEIR PARAMETER VALUES MAY VARY AMONG REGIONS.                LIQSAT........2300
C ***     REGIONS ARE SPECIFIED BY BOTH NODE NUMBER AND ELEMENT NUMBER   LIQSAT........2400
C ***     IN INPUT DATA FILE FOR UNIT K1 (INP).                          LIQSAT........2500
C ***                                                                    LIQSAT........2600
C                                                                        LIQSAT........2700
      SUBROUTINE LIQSAT(SLSAT,DSLSATDT,TEMP,KREG)                        LIQSAT........2800
      USE ALLARR, ONLY : SLMOD,SLPAR,NSLPAR,SLPNM,TFREEZ                 LIQSAT........2900
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                LIQSAT........3000
C.....DECLARE USER-DEFINED MODEL PARAMETERS AS REAL HERE IF DESIRED      LIQSAT........3100
      DIMENSION KTYPE(2)                                                 LIQSAT........3200
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  LIQSAT........3300
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    LIQSAT........3400
     2   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IFREEZ,IALSAT,KTYPE          LIQSAT........3500
      COMMON /PARAMS/COMPL,COMPI,CL,CI,SIGMAL,SIGMAI,                    LIQSAT........3600
     1   RHOL0,URHOL0,DRLDU,RHOI,VISC0                                   LIQSAT........3700
C                                                                        LIQSAT........3800
C.....SET VALUE OF LIQUID-WATER SATURATION UNDER FULLY SATURATED         LIQSAT........3900
C        CONDITIONS AND ITS DERIVATIVE WITH RESPECT TO TEMPERATURE       LIQSAT........4000
C                                                                        LIQSAT........4100
C.....CALCULATE RELATIVE TEMPERATURE                                     LIQSAT........4200
      TEMPREL = TEMP - TFREEZ(KREG)                                      LIQSAT........4300
C                                                                        LIQSAT........4400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  LIQSAT........4500
C     SELECT LIQUID-WATER SATURATION RELATION:                           LIQSAT........4600
C        NONE = NO FUNCTION SPECIFIED                                    LIQSAT........4700
C        EXPO = EXPONENTIAL                                              LIQSAT........4800
C        BCOR = MODIFIED POWER-LAW                                       LIQSAT........4900
C        PLIN = PIECEWISE-LINEAR                                         LIQSAT........5000
C        UDEF = USER-DEFINED                                             LIQSAT........5100
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  LIQSAT........5200
      IF (SLMOD(KREG).EQ."NONE") THEN                                    LIQSAT........5300
C        ---------------------------------------------                   LIQSAT........5400
C        NO FREEZING                                                     LIQSAT........5500
C        ---------------------------------------------                   LIQSAT........5600
         SLSAT = 1.D0                                                    LIQSAT........5700
         IF (IALSAT.EQ.1) DSLSATDT = 0.D0                                LIQSAT........5800
C                                                                        LIQSAT........5900
      ELSE IF (SLMOD(KREG).EQ."EXPO") THEN                               LIQSAT........6000
C        -------------------------------------------------               LIQSAT........6100
C        EXPONENTIAL MODEL BASED ON MOTTAGHY & RATH (2006)               LIQSAT........6200
C        -------------------------------------------------               LIQSAT........6300
C........SET PARAMETERS FOR THE CURRENT REGION                           LIQSAT........6400
         SLSATRES = SLPAR(KREG,1)                                        LIQSAT........6500
         W = SLPAR(KREG,2)                                               LIQSAT........6600
C........VALUE OF LIQUID-WATER SATURATION AS A FUNCTION OF               LIQSAT........6700
C           TEMPERATURE (SL VS. TEMP) ASSUMING SATURATED CONDITIONS      LIQSAT........6800
C           (SLSAT, CALCULATED ON EACH CALL TO LIQSAT)                   LIQSAT........6900
         SLEXP=DEXP(-(TEMPREL/W)**2)                                     LIQSAT........7000
         SLSAT=(1.D0-SLSATRES)*SLEXP+SLSATRES                            LIQSAT........7100
         IF (IALSAT.EQ.1) THEN                                           LIQSAT........7200
C...........DERIVATIVE OF LIQUID-WATER SATURATION WITH RESPECT           LIQSAT........7300
C              TO TEMPERATURE, AS A FUNCTION OF TEMPERATURE              LIQSAT........7400
C              (DSLDT VS. TEMP) ASSUMING SATURATED CONDITIONS            LIQSAT........7500
C              (DSLSATDT, CALCULATED ONLY WHEN IALSAT=1)                 LIQSAT........7600
            DSLSATDT=-(1.D0-SLSATRES)*(2.D0*(TEMPREL)/(W*W))*SLEXP       LIQSAT........7700
         END IF                                                          LIQSAT........7800
C                                                                        LIQSAT........7900
      ELSE IF (SLMOD(KREG).EQ."POWR") THEN                               LIQSAT........8000
C        ------------------------                                        LIQSAT........8100
C        MODIFIED POWER-LAW MODEL                                        LIQSAT........8200
C        ------------------------                                        LIQSAT........8300
C........SET PARAMETERS FOR THE CURRENT REGION                           LIQSAT........8400
         SLSATRES = SLPAR(KREG,1)                                        LIQSAT........8500
         ALPHA = SLPAR(KREG,2)                                           LIQSAT........8600
         BETA = SLPAR(KREG,3)                                            LIQSAT........8700
C........VALUE OF LIQUID-WATER SATURATION AS A FUNCTION OF               LIQSAT........8800
C           TEMPERATURE (SL VS. TEMP) ASSUMING SATURATED CONDITIONS      LIQSAT........8900
C           (SLSAT, CALCULATED ON EACH CALL TO LIQSAT)                   LIQSAT........9000
         TPOW = -ALPHA**(-1.D0/BETA)                                     LIQSAT........9100
         IF (TEMPREL.GE.TPOW) THEN                                       LIQSAT........9200
         SLSAT=1.D0                                                      LIQSAT........9300
         ELSE                                                            LIQSAT........9400
            SLSAT=ALPHA*(-TEMPREL)**BETA                                 LIQSAT........9500
         END IF                                                          LIQSAT........9600
         SLSAT = MAX(SLSAT,SLSATRES)                                     LIQSAT........9700
         IF (IALSAT.EQ.1) THEN                                           LIQSAT........9800
C...........DERIVATIVE OF LIQUID-WATER SATURATION WITH RESPECT           LIQSAT........9900
C              TO TEMPERATURE, AS A FUNCTION OF TEMPERATURE              LIQSAT.......10000
C              (DSLDT VS. TEMP) ASSUMING SATURATED CONDITIONS            LIQSAT.......10100
C              (DSLSATDT, CALCULATED ONLY WHEN IALSAT=1)                 LIQSAT.......10200
            IF (SLSAT.GT.SLSATRES) THEN                                  LIQSAT.......10300
               IF (TEMPREL.GE.TPOW) THEN                                 LIQSAT.......10400
                  SLSAT=0.D0                                             LIQSAT.......10500
               ELSE                                                      LIQSAT.......10600
                  SLSAT=-ALPHA*BETA*(-TEMPREL)**(BETA-1.D0)              LIQSAT.......10700
               END IF                                                    LIQSAT.......10800
            ELSE                                                         LIQSAT.......10900
               DSLSATDT = 0.D0                                           LIQSAT.......11000
            END IF                                                       LIQSAT.......11100
         END IF                                                          LIQSAT.......11200
C                                                                        LIQSAT.......11300
      ELSE IF (SLMOD(KREG).EQ."PLIN") THEN                               LIQSAT.......11400
C        ----------------------                                          LIQSAT.......11500
C        PIECEWISE LINEAR MODEL                                          LIQSAT.......11600
C        ----------------------                                          LIQSAT.......11700
C........SET PARAMETERS FOR THE CURRENT REGION                           LIQSAT.......11800
         SLSATRES = SLPAR(KREG,1)                                        LIQSAT.......11900
         TLRES = SLPAR(KREG,2)                                           LIQSAT.......12000
C........VALUE OF LIQUID-WATER SATURATION AS A FUNCTION OF               LIQSAT.......12100
C           TEMPERATURE (SL VS. TEMP) ASSUMING SATURATED FLOW            LIQSAT.......12200
C           (SLSAT, CALCULATED ON EACH CALL TO LIQSAT)                   LIQSAT.......12300
          IF (TEMPREL.LE.TLRES) THEN                                     LIQSAT.......12400
            SLSAT=SLSATRES                                               LIQSAT.......12500
          ELSE                                                           LIQSAT.......12600
            SLSAT=(1.D0-SLSATRES)*(1.D0-TEMPREL/TLRES)+SLSATRES          LIQSAT.......12700
          END IF                                                         LIQSAT.......12800
          IF (IALSAT.EQ.1) THEN                                          LIQSAT.......12900
C...........DERIVATIVE OF LIQUID-WATER SATURATION WITH RESPECT           LIQSAT.......13000
C              TO TEMPERATURE, AS A FUNCTION OF TEMPERATURE              LIQSAT.......13100
C              (DSLDT VS. TEMP) ASSUMING SATURATED CONDITIONS            LIQSAT.......13200
C              (DSLSATDT, CALCULATED ONLY WHEN IALSAT=1)                 LIQSAT.......13300
            IF (TEMPREL.LT.TLRES) THEN                                   LIQSAT.......13400
              DSLSATDT=0.D0                                              LIQSAT.......13500
            ELSE                                                         LIQSAT.......13600
              DSLSATDT=-(1.D0-SLSATRES)/TLRES                            LIQSAT.......13700
            END IF                                                       LIQSAT.......13800
         END IF                                                          LIQSAT.......13900
C                                                                        LIQSAT.......14000
      ELSE IF (SLMOD(KREG).EQ."UDEF") THEN                               LIQSAT.......14100
C                                                                        LIQSAT.......14200
C        ------------------                                              LIQSAT.......14300
C        USER-DEFINED MODEL                                              LIQSAT.......14400
C        ------------------                                              LIQSAT.......14500
C                                                                        LIQSAT.......14600
C........SET PARAMETERS FOR THE CURRENT REGION                           LIQSAT.......14700
C                                                                        LIQSAT.......14800
C        *****************************************************           LIQSAT.......14900
C        *  EXTRACT MODEL PARAMETERS FROM ARRAY SLPAR,       *           LIQSAT.......15000
C        *  WHERE SLPAR(KREG,NP) IS THE NP-th PARAMETER IN   *           LIQSAT.......15100
C        *  UNSATURATED/FREEZING REGION KREG.                *           LIQSAT.......15200
C        *                                                   *           LIQSAT.......15300
C        *  NOTE THAT THE FIRST PARAMETER (NP=1) **MUST**    *           LIQSAT.......15400
C        *  BE THE RESIDUAL LIQUID-WATER SATURATION FOR      *           LIQSAT.......15500
C        *  SATURATED FLOW, SLSATRES.                        *           LIQSAT.......15600
C        *****************************************************           LIQSAT.......15700
         SLSATRES = SLPAR(KREG,1)                                        LIQSAT.......15800
         W = SLPAR(KREG,2)                                               LIQSAT.......15900
C                                                                        LIQSAT.......16000
C........VALUE OF LIQUID-WATER SATURATION AS A FUNCTION OF               LIQSAT.......16100
C           TEMPERATURE (SL VS. TEMP) ASSUMING SATURATED CONDITIONS      LIQSAT.......16200
C           (SLSAT, CALCULATED ON EACH CALL TO LIQSAT)                   LIQSAT.......16300
C                                                                        LIQSAT.......16400
C           THE VALUE OF RELATIVE TEMPERATURE, TEMPREL = TEMP - TFREEZ,  LIQSAT.......16500
C           HAS ALREADY BEEN CALCULATED NEAR THE BEGINNING OF THIS       LIQSAT.......16600
C           SUBROUTINE AND IS AVAILABLE IN CASE THE USER-DEFINED         LIQSAT.......16700
C           FUNCTION IS FORMULATED IN TERMS OF RELATIVE TEMPERATURE.     LIQSAT.......16800
C                                                                        LIQSAT.......16900
C        *****************************************************           LIQSAT.......17000
C        *  ASSIGN A VALUE TO SLSAT.                         *           LIQSAT.......17100
C        *****************************************************           LIQSAT.......17200
         SLEXP=DEXP(-(TEMPREL/W)**2)                                     LIQSAT.......17300
         SLSAT=(1.D0-SLSATRES)*SLEXP+SLSATRES                            LIQSAT.......17400
C                                                                        LIQSAT.......17500
         IF (IALSAT.EQ.1) THEN                                           LIQSAT.......17600
C...........DERIVATIVE OF LIQUID-WATER SATURATION WITH RESPECT           LIQSAT.......17700
C              TO TEMPERATURE, AS A FUNCTION OF TEMPERATURE              LIQSAT.......17800
C              (DSLDT VS. TEMP) ASSUMING SATURATED CONDITIONS            LIQSAT.......17900
C              (DSLSATDT, CALCULATED ONLY WHEN IALSAT=1)                 LIQSAT.......18000
C                                                                        LIQSAT.......18100
C           **************************************************           LIQSAT.......18200
C           *  ASSIGN A VALUE TO DSLSATDT.                   *           LIQSAT.......18300
C           **************************************************           LIQSAT.......18400
            DSLSATDT=-(1.D0-SLSATRES)*(2.D0*(TEMPREL)/(W*W))*SLEXP       LIQSAT.......18500
C                                                                        LIQSAT.......18600
         END IF                                                          LIQSAT.......18700
      END IF                                                             LIQSAT.......18800
C                                                                        LIQSAT.......18900
      RETURN                                                             LIQSAT.......19000
      END                                                                LIQSAT.......19100
C                                                                        LIQSAT.......19200
C     SUBROUTINE        R  E  L  P  E  R  M        SUTRA VERSION 4.0     RELPERM........100
C                                                                        RELPERM........200
C *** PURPOSE :                                                          RELPERM........300
C ***  USER-PROGRAMMED SUBROUTINE GIVING RELATIVE PERMEABILITY AS A      RELPERM........400
C ***  FUNCTION OF LIQUID-WATER SATURATION ( RELK(SL) ).                 RELPERM........500
C ***                                                                    RELPERM........600
C ***  CODE BETWEEN these DASHED LINES ========= MUST BE REPLACED        RELPERM........700
C ***  TO GIVE THE PARTICULAR UNSATURATED PERMEABILITY FUNCTION DESIRED. RELPERM........800
C ***                                                                    RELPERM........900
C ***  DIFFERENT FUNCTIONS MAY BE GIVEN FOR EACH REGION OF THE MESH.     RELPERM.......1000
C ***  REGIONS ARE SPECIFIED BY BOTH NODE NUMBER AND ELEMENT NUMBER      RELPERM.......1100
C ***  IN INPUT DATA FILE FOR UNIT K1 (INP).                             RELPERM.......1200
C                                                                        RELPERM.......1300
C *** SUBROUTINE PROVIDES RELATIVE PERMEABILITY                          RELPERM.......1400
C *** FOR UNSATURATED CONDITIONS.                                        RELPERM.......1500
C ***  -- THREE PRE-PROGRAMMED FUNCTIONS ARE PROVIDED                    RELPERM.......1600
C ***  -- USER MAY ADD CODING FOR A NEW FUNCTION.                        RELPERM.......1700
C ***                                                                    RELPERM.......1800
C *** SUBROUTINE GIVES:                                                  RELPERM.......1900
C ***  RELATIVE PERMEABILITY AS A FUNCTION OF                            RELPERM.......2000
C ***  LIQUID-WATER SATURATION ( RELK(SL) ).                             RELPERM.......2100
C ***  LOWEST ALLOWED RELK VALUE IS A USER-SELECTED VALUE ( RKMIN>0 ).   RELPERM.......2200
C ***                                                                    RELPERM.......2300
C ***  USER MAY ADD CODE BETWEEN THESE LINES "====" TO GIVE THE          RELPERM.......2400
C ***  PARTICULAR RELATIVE-PERMEABILITY FUNCTION DESIRED,                RELPERM.......2500
C ***  WHICH IS REFERRED TO HERE AS THE "UDEF: FUNCTION.                 RELPERM.......2600
C ***                                                                    RELPERM.......2700
C ***  DIFFERENT FUNCTIONS (3 PRE-PROGRAMMED PLUS USER-PROGRAMMED)       RELPERM.......2800
C ***  MAY BE SELECTED IN THE INPUT DATA IN EACH REGION OF THE MESH,     RELPERM.......2900
C ***  AND THEIR PARAMETER VALUES MAY VARY AMONG REGIONS.                RELPERM.......3000
C ***     REGIONS ARE SPECIFIED BY BOTH NODE NUMBER AND ELEMENT NUMBER   RELPERM.......3100
C ***     IN INPUT DATA FILE FOR UNIT K1 (INP).                          RELPERM.......3200
C ***                                                                    RELPERM.......3300
C                                                                        RELPERM.......3400
      SUBROUTINE RELPERM(RELK,SL,PRES,KREG)                              RELPERM.......3500
      USE ALLARR, ONLY : RKMOD,RKPAR,NRKPAR,RKPNM                        RELPERM.......3600
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                                RELPERM.......3700
      DIMENSION KTYPE(2)                                                 RELPERM.......3800
      COMMON /CONTRL/ GNUP,GNUU,UP,DTMULT,DTMAX,ME,ISSFLO,ISSTRA,ITCYC,  RELPERM.......3900
     1   NPCYC,NUCYC,NPRINT,NBCFPR,NBCSPR,NBCPPR,NBCUPR,NLAKPR,IREAD,    RELPERM.......4000
     2   NBGPPR,NBGUPR,ISTORE,NOUMAT,IUNSAT,IFREEZ,IALSAT,KTYPE          RELPERM.......4100
C                                                                        RELPERM.......4200
C                                                                        RELPERM.......4300
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  RELPERM.......4400
C     SELECT RELATIVE-PERMEABILITY FUNCTION:                             RELPERM.......4500
C        NONE = NO FUNCTION SPECIFIED                                    RELPERM.......4600
C        VGEN = VAN GENUCHTEN                                            RELPERM.......4700
C        BCOR = BROOKS-COREY                                             RELPERM.......4800
C        PLIN = PIECEWISE-LINEAR                                         RELPERM.......4900
C        UDEF = USER-DEFINED                                             RELPERM.......5000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  RELPERM.......5100
      IF (RKMOD(KREG).EQ."NONE") THEN                                    RELPERM.......5200
         RELK = 1.D0                                                     RELPERM.......5300
C                                                                        RELPERM.......5400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  RELPERM.......5500
      ELSE IF (RKMOD(KREG).EQ."VGEN") THEN                               RELPERM.......5600
C                                                                        RELPERM.......5700
C        ---------------------------------------------------             RELPERM.......5800
C        VAN GENUCHTEN (1980) RELATIVE-PERMEABILITY FUNCTION             RELPERM.......5900
C        ---------------------------------------------------             RELPERM.......6000
C........SET PARAMETERS FOR THE CURRENT REGION                           RELPERM.......6100
         SLRES = RKPAR(KREG,1)                                           RELPERM.......6200
         VN = RKPAR(KREG,2)                                              RELPERM.......6300
         RKMIN = RKPAR(KREG,3)                                           RELPERM.......6400
C                                                                        RELPERM.......6500
         SLRM1 = 1.D0 - SLRES                                            RELPERM.......6600
         VNF = (VN - 1.D0)/VN                                            RELPERM.......6700
C........RELATIVE PERMEABILITY AS A FUNCTION OF LIQUID SATURATION        RELPERM.......6800
C        (RELK VS. SL)                                                   RELPERM.......6900
         SLSTAR = (SL - SLRES)/SLRM1                                     RELPERM.......7000
         RELK = DSQRT(SLSTAR)*                                           RELPERM.......7100
     1      (1.D0 - (1.D0 - SLSTAR**(1.D0/VNF))**VNF)**2                 RELPERM.......7200
C        ENFORCE THE MINIMUM VALUE OF RELK USING SUTRA PARAMETER RKMIN   RELPERM.......7300
         IF (RELK .LT. RKMIN) RELK=RKMIN                                 RELPERM.......7400
C                                                                        RELPERM.......7500
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  RELPERM.......7600
      ELSE IF (RKMOD(KREG).EQ."BCOR") THEN                               RELPERM.......7700
C                                                                        RELPERM.......7800
C        ----------------------------------------------------            RELPERM.......7900
C        BROOKS & COREY (1964) RELATIVE-PERMEABILITY FUNCTION            RELPERM.......8000
C        ----------------------------------------------------            RELPERM.......8100
C........SET PARAMETERS FOR THE CURRENT REGION                           RELPERM.......8200
         SLRES = RKPAR(KREG,1)                                           RELPERM.......8300
         RLAMB = RKPAR(KREG,2)                                           RELPERM.......8400
         RKMIN = RKPAR(KREG,3)                                           RELPERM.......8500
C                                                                        RELPERM.......8600
         SLREL = (SL - SLRES)/(1.D0 - SLRES)                             RELPERM.......8700
C........RELATIVE PERMEABILITY AS A FUNCTION OF LIQUID SATURATION        RELPERM.......8800
C        (RELK VS. SL)                                                   RELPERM.......8900
         RELK = SLREL**(3.D0 + 2.D0/RLAMB)                               RELPERM.......9000
C        ENFORCE THE MINIMUM VALUE OF RELK USING SUTRA PARAMETER RKMIN   RELPERM.......9100
         IF (RELK .LT. RKMIN) RELK=RKMIN                                 RELPERM.......9200
C                                                                        RELPERM.......9300
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  RELPERM.......9400
      ELSE IF (RKMOD(KREG).EQ."PLIN") THEN                               RELPERM.......9500
C                                                                        RELPERM.......9600
C        -----------------------------------------------                 RELPERM.......9700
C        PIECEWISE LINEAR RELATIVE-PERMEABILITY FUNCTION                 RELPERM.......9800
C        -----------------------------------------------                 RELPERM.......9900
C........SET PARAMETERS FOR THE CURRENT REGION                           RELPERM......10000
         SLRKMIN = RKPAR(KREG,1)                                         RELPERM......10100
         RKMIN = RKPAR(KREG,2)                                           RELPERM......10200
C                                                                        RELPERM......10300
C........RELATIVE PERMEABILITY AS A FUNCTION OF LIQUID SATURATION        RELPERM......10400
C        (RELK VS. SL)                                                   RELPERM......10500
         SLOPEK = -(1.D0 - RKMIN)/(1.D0 - SLRKMIN)                       RELPERM......10600
         RELK = (1.D0 + SLOPEK*(1.D0 - SL))                              RELPERM......10700
C        ENFORCE THE MINIMUM VALUE OF RELK USING SUTRA PARAMETER RKMIN   RELPERM......10800
         IF (RELK .LT. RKMIN) RELK=RKMIN                                 RELPERM......10900
C                                                                        RELPERM......11000
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  RELPERM......11100
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  RELPERM......11200
      ELSE IF (RKMOD(KREG).EQ."UDEF") THEN                               RELPERM......11300
C                                                                        RELPERM......11400
C        -------------------------------------------                     RELPERM......11500
C        USER-DEFINED RELATIVE-PERMEABILITY FUNCTION                     RELPERM......11600
C        -------------------------------------------                     RELPERM......11700
C                                                                        RELPERM......11800
C........RELATIVE PERMEABILITY AS A FUNCTION OF LIQUID-WATER             RELPERM......11900
C           SATURATION (RELK VS. SL)                                     RELPERM......12000
C                                                                        RELPERM......12100
C        ***********************************************************     RELPERM......12200
C        * CODING MUST GIVE A VALUE TO RELATIVE PERMEABILITY, RELK *     RELPERM......12300
C        ***********************************************************     RELPERM......12400
C                                                                        RELPERM......12500
C        **************************************************              RELPERM......12600
C        * EXTRACT FUNCTION PARAMETERS FROM ARRAY RKPAR,  *              RELPERM......12700
C        * WHERE RKPAR(KREG,NP) IS THE NP-th PARAMETER IN *              RELPERM......12800
C        * UNSATURATED/FREEZING REGION KREG               *              RELPERM......12900
C        *                                                *              RELPERM......13000
C        * NOTE THAT THE FIRST PARAMETER (NP=1) **MUST**  *              RELPERM......13100
C        * BE THE RESIDUAL LIQUID SATURATION, SLRES. THIS *              RELPERM......13200
C        * PARAMETER MUST **NOT** BE INCLUDED IN THE LIST *              RELPERM......13300
C        * OF USER-DEFINED PARAMETERS IN THE MODEL INPUT. *              RELPERM......13400
C        * THE VALUE OF THIS PARAMETER IS CALCULATED BY   *              RELPERM......13500
C        * SUTRA AND ASSIGNED AUTOMATICALLY BY SUTRA TO   *              RELPERM......13600
C        * THE FIRST PARAMETER. IT IS THUS MADE AVAILABLE *              RELPERM......13700
C        * TO THIS USER-DEFINED FUNCTION, ALTHOUGH THIS   *              RELPERM......13800
C        * FUNCTION IS **NOT REQUIRED** TO USE THE VALUE  *              RELPERM......13900
C        * OF SLRES IN SETTING RELK. (FOR EXAMPLE, THE    *              RELPERM......14000
C        * PRE-PROGRAMMED 'VGEN' AND 'BCOR' FUNCTIONS USE *              RELPERM......14100
C        * SLRES, BUT THE 'PLIN' FUNCTION DOES NOT.)      *              RELPERM......14200
C        * AFTER SLRES (NP=1), PARAMETERS DEFINED BY THE  *              RELPERM......14300
C        * USER IN THE MODEL INPUT ARE NUMBERED BEGINNING *              RELPERM......14400
C        * WITH NP=2.                                     *              RELPERM......14500
C        **************************************************              RELPERM......14600
C                                                                        RELPERM......14700
C======================================================================= RELPERM......14800
C======================================================================= RELPERM......14900
C======================================================================= RELPERM......15000
C                                                                        RELPERM......15100
C     E X A M P L E   C O D I N G   FOR                                  RELPERM......15200
C     MESH WITH ONE OR MORE REGIONS OF UNSATURATED PROPERTIES USING      RELPERM......15300
C     RELATIVE-PERMEABILITY RELATIONSHIP OF VAN GENUCHTEN(1980)          RELPERM......15400
C        (1) VG RESIDUAL SATURATION PARAMETER, SLRES, IN UNITS [L**0]    RELPERM......15500
C        (2) VG PARAMETER, VN, GIVEN IN UNITS [L**0]                     RELPERM......15600
C        (3) SUTRA PARAMETER, RKMIN, GIVEN IN UNITS [L**0]               RELPERM......15700
C                                                                        RELPERM......15800
C        *************************************************************** RELPERM......15900
C        * GET FUNCTION PARAMETERS FROM ARRAY RKPAR FOR CURRENT REGION * RELPERM......16000
C        *                                                             * RELPERM......16100
C        * NOTE THAT THE FIRST PARAMETER, SWPAR(KREG,1), **MUST** BE   * RELPERM......16200
C        * THE RESIDUAL LIQUID-WATER SATURATION FOR SATURATED FLOW,    * RELPERM......16300
C        * SLSATRES. THE VALUE OF THIS PARAMETER IS AUTOMATICALLY      * RELPERM......16400
C        * CALCULATED AND ASSIGNED BY SUTRA. PARAMETERS DEFINED BY THE * RELPERM......16500
C        * USER IN THE MODEL INPUT ARE NUMBERED BEGINNING WITH 2,      * RELPERM......16600
C        * I.E., THE FIRST USER-DEFINED PARAMETER IS SWPAR(KREG,2).    * RELPERM......16700
C        *************************************************************** RELPERM......16800
         SLRES = RKPAR(KREG,1)                                           RELPERM......16900
         VN = RKPAR(KREG,2)                                              RELPERM......17000
         RKMIN = RKPAR(KREG,3)                                           RELPERM......17100
C                                                                        RELPERM......17200
C        **************************                                      RELPERM......17300
C        * ASSIGN A VALUE TO RELK *                                      RELPERM......17400
C        **************************                                      RELPERM......17500
         SLRM1 = 1.D0 - SLRES                                            RELPERM......17600
         VNF = (VN - 1.D0)/VN                                            RELPERM......17700
         SLSTAR = (SL - SLRES)/SLRM1                                     RELPERM......17800
         RELK = DSQRT(SLSTAR)*                                           RELPERM......17900
     1      (1.D0 - (1.D0 - SLSTAR**(1.D0/VNF))**VNF)**2                 RELPERM......18000
C                                                                        RELPERM......18100
C        *************************************                           RELPERM......18200
C        * ENFORCE THE MINIMUM VALUE OF RELK *                           RELPERM......18300
C        *************************************                           RELPERM......18400
         IF (RELK .LT. RKMIN) RELK=RKMIN                                 RELPERM......18500
C                                                                        RELPERM......18600
C        ----------------------------                                    RELPERM......18700
C        END OF USER-DEFINED FUNCTION                                    RELPERM......18800
C        ----------------------------                                    RELPERM......18900
C                                                                        RELPERM......19000
C======================================================================= RELPERM......19100
C======================================================================= RELPERM......19200
C======================================================================= RELPERM......19300
C                                                                        RELPERM......19400
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  RELPERM......19500
C                                                                        RELPERM......19600
      END IF                                                             RELPERM......19700
C                                                                        RELPERM......19800
 1800 RETURN                                                             RELPERM......19900
C                                                                        RELPERM......20000
      END                                                                RELPERM......20100
