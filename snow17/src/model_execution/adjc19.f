C MEMBER ADJC19
C  (from old member FCPACK19)
C
      SUBROUTINE ADJC19(TWE,SI,ADC,IVER)
C.......................................
C     THIS SUBROUTINE ADJUSTS SNOW MODEL CARRYOVER VALUES FOR A CHANGE
C        IN THE TOTAL WATER-EQUIVALENT.  USED IN THE 'SNOW-17 '
C        OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980
C
CVK     MODIFIED 4/00 BY V. KOREN: TWO NEW STATES ADDED
C
CEA     MODIFIED 11/05 BY E. ANDERSON TO ADD TAPREV TO SNCO19 COMMON
CEA       AND TO COMPUTE A DEPTH VALUE WHEN WATER EQUIVALENT CREATED
CEA       BY A MOD WHEN THERE WAS NONE PREVIOUSLY.  ALSO TO IGNORE
CEA       DEPTH WHEN RUNNING VERSION 1 OF THE OPERATION.
C
CEA     MODIFIED 1/06 BY E. ANDERSON TO CHANGE LOGIC FOR WHEN AEADJ IS
CEA       SET BACK TO ZERO, TO RESET ACCMAX WHEN NEW WATER EQUIVALENT
CEA       IS GT 3*SB, AND FOR MODIFICATIONS ON HOW SNOF IS HANDLED.
C
CEA     MODIFIED 8/06 BY E. ANDERSON TO COMPUTE NORMAL AI VALUE AND 
CEA       CORRECT THE CHECK FOR WHEN AEADJ SHOULD BE SET TO ZERO.
C.......................................
      REAL NEGHS,LIQW
      DIMENSION ADC(11)
C
C     COMMON BLOCK
CVK   ADDED TWO MORE STATES
      COMMON/SNCO19/WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,STORGE,
CVK     1   AEADJ,NEXLAG,EXLAG(7)
CEA     1   AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP
     1   AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP,TAPREV
      COMMON/SNUP19/MFC,SFALLX,WINDC,SCTOL,WETOL,SNOF,UADJC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob90/ohd/ofs/src/fcst_snow/RCS/adjc19.f,v $
     . $',                                                             '
     .$Id: adjc19.f,v 1.6 2006/10/06 11:56:35 hsu Exp $
     . $' /
C    ===================================================================
C
C.......................................
      OLDWE=WE+LIQW
      AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF (AEADJ.EQ.0.0) GO TO 101
      AINORM=AI
      AI=AEADJ
  101 OLDAI=AI
CEA      IF(AEADJ.GT.0.0) AI=AEADJ
CEA      OLDAI=AI
      TEX=0.0
      DO 100 N=1,NEXLAG
  100 TEX=TEX+EXLAG(N)
      FREEW=STORGE+TEX
      QUAL=1.0
      IF(WE.GT.0.0)QUAL=1.0+(LIQW/WE)
      WE=(TWE-FREEW)/QUAL
C      
CVK  SNOW DEPTH IS ADJUSTED BY RATIO OF THE WE CHANGE
CFAN
CFAN  SNOW-17 output table display "NAN" for SNOW-DEPTH column
CFAN  when creating WECHNG mod.
CFAN
CFAN  IF(WE .GT. 0.0) THEN
      IF (IVER.EQ.1) GO TO 105
      IF(WE .GT. 0.0 .AND. OLDWE-LIQW .NE. 0.0) THEN
        SNDPT=SNDPT*WE/(OLDWE-LIQW)
      ELSE
        IF (WE.GT.0.0) THEN
CEA  WE CREATED WHEN NONE EXISTED PREVIOUSLY
CEA    ASSUME DENSITY OF 0.2. AND SNOW TEMPERATURE OF ZERO
          SNDPT=(0.1*WE)/0.2
          SNTMP=0.0
        ELSE
          SNDPT=0.0
          SNTMP=0.0
        ENDIF
      ENDIF
CVK--------------------------------------------------       
  105 LIQW=(QUAL-1.0)*WE
      SWE=WE+LIQW
C     IF SWE GE 3*SB THEN START NEW ACCUMULATION PERIOD AND
C       SET AEADJ TO ZERO
      IF (SWE.LT.3.0*SB) GO TO 106
      ACCMAX=SWE
      AEADJ=0.0
      GO TO 125
  106 IF (AEADJ.EQ.0.0) GO TO 107
C     DETERMINE IF AEADJ SHOULD BE REMOVED - SET TO ZERO
      IF ((AEADJ.LT.AINORM).AND.(SWE.GE.AINORM)) AEADJ=0.0
      IF ((AEADJ.GE.AINORM).AND.(SWE.GE.AEADJ)) AEADJ=0.0
CEA      IF ((AEADJ.LT.OLDAI).AND.(SWE.GE.OLDAI)) AEADJ=0.0
CEA      IF ((AEADJ.GE.OLDAI).AND.(SWE.GE.AEADJ)) AEADJ=0.0
  107 IF(OLDWE.GT.0.8*ACCMAX) GO TO 110
      IF(SWE.GT.ACCMAX) ACCMAX=SWE
      GO TO 120
  110 ACCMAX=SWE*(ACCMAX/OLDWE)
CEA 120  IF (SWE.GE.AEADJ) AEADJ=0.0
  120 AI=ACCMAX
      IF(ACCMAX.GT.SI) AI=SI
      IF (AEADJ.GT.0.0) AI=AEADJ
      IF(SWE.LT.AI) GO TO 130
  125 SB=SWE
      SBWS=SWE
      RETURN
  130 IF((OLDWE.LT.OLDAI).AND.(OLDWE.GT.SB)) GO TO 140
      SB=SWE
CEA      SB=SWE+SNOF
      SBWS=SWE
      R=(SWE/AI)*10.0+1.0
      GO TO 150
  140 R=SWE/OLDWE
      SB=SB*R
      SBWS=SBWS*R
CEA      IF(SBWS.LT.SB+0.75*SNOF)SBWS=SB+0.75*SNOF
      R=(SB/AI)*10.0+1.0
  150 N=R
      FN=N
      R=R-FN
      SBAESC=ADC(N)+(ADC(N+1)-ADC(N))*R
      IF(SBAESC.GT.1.0) SBAESC=1.0
C.......................................
      RETURN
      END
