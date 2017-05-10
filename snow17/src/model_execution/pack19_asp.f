
      DO 200 I=1,NDT

      IF(PCT.LT.0.0) GO TO 102

  102 IF (LAEC.EQ.0) GO TO 104
      DRSL=DRSL+RSL
      NDRSP=NDRSP+1
C
C     FORM OF PRECIP. BASED ON RAIN-SNOW ELEVATION
      IF (RSL.GT.AE(1,1)) GO TO 10
      FRACR=0.0
      GO TO 30
   10 DO 20 J=2,NPTAE
      IF (RSL.GT.AE(1,J)) GO TO 20
      FRACR=AE(2,J-1)+(AE(2,J)-AE(2,J-1))*
     1  ((RSL-AE(1,J-1))/(AE(1,J)-AE(1,J-1)))
      GO TO 30
   20 CONTINUE
      FRACR=1.0
   30 FRACS=1.0-FRACR
      GO TO 105
C
C     FORM OF PRECIP. BASED ON TEMPERATURE.
  104 IF(TPX.GT.PXTEMP) GO TO 103
C
C     SNOW
      FRACS=1.0
      FRACR=0.0
      GO TO 105
C
C     RAIN
  103 FRACS=0.0
      FRACR=1.0
  105 IF(FRACS.EQ.0.0) GO TO 109
C
C     ACCUMULATE SNOWFALL
      TS=TPX
      IF(TS.GT.0.0) TS=0.0
      SFALL=PXI*FRACS*SCF
      IF(IFUT.EQ.0) SFALL=SFALL*SFALLX
      SPX=SPX+SFALL
      SSFALL=SSFALL+SFALL
      DSFALL=DSFALL+SFALL
      PSFALL=PSFALL+SFALL
CFAN
C     IF statements with expression do not work properly here on Linux
CEA   temp = we+liqw
CEA  CHANGED VARIABLE temp to WELIQW
C
      WELIQW = WE+LIQW
C
CEA   REVISED CODE FOR ADJUSTING AESC STATES FOR NEW SNOWFALL
      IF (WELIQW.LE.SBWS) GO TO 106
C     WELIQW>SBWS - 100% COVER - SNOW ON BARE GROUND
      SBWS=SBWS+0.75*SFALL
      GO TO 107
  106 IF (WELIQW.GT.SB) GO TO 1061
C     ON DEPLETION CURVE OR .GE. AI
C     CHECK IF SUFFICIENT NEW SNOW TO LEAVE DEPLETION CURVE
      IF (SFALL.GE.SNOF) GO TO 1062
C     REMAIN ON DEPLETION CURVE
      SB=SB+SFALL
      SBWS=SB
C     SBAESC UPDATED IN AESC19
      GO TO 107
C     WELIQW>SB AND WELIQW LE SBWS - <100% COVER - SNOW ON BARE GROUND
 1061 IF (SFALL.GE.SNOF) GO TO 1062
      SBWS=SBWS+0.75*SFALL
      GO TO 107
 1062 SBWS=WELIQW+0.75*SFALL
CEA   FOLLOWING IS OLD CODE
CFAN  IF ((WE+LIQW).LT.SBWS) GO TO 106
CEA      IF (WELIQW.LT.SBWS) GO TO 106                                    !cfan
CEA      SBWS=SBWS+0.75*SFALL
CFAN  IF((SFALL.GE.SNOF).AND.(SB.GT.WE+LIQW)) SB=WE+LIQW
CEA      IF((SFALL.GE.SNOF).AND.(SB.GT.WELIQW)) SB=WE+LIQW                !cfan
CEA      GO TO 107
CEA  106 IF(SFALL.GE.SNOF) SBWS=WE+LIQW+0.75*SFALL
  107 WE=WE+SFALL
C     IF WE+LIQW.GE.3*SB, ASSUME NEW ACCUMULATION PERIOD
      IF(WE+LIQW.LT.3.0*SB) GO TO 108
      ACCMAX=WE+LIQW
      AEADJ=0.0
  108 CNHSPX=-TS*SFALL/160.0
      IF(SFALL.GT.SFNEW) TINDEX=TS
C
C     RAINFALL AND RAIN MELT.
  109 RAIN=PXI*FRACR
      SPX=SPX+RAIN
      PRAIN=PRAIN+RAIN
      IF(WE.EQ.0.0) GO TO 160
      DRAIN=DRAIN+RAIN
      TR=TPX
      IF(TR.LT.0.0) TR=0.0
      RAINM=0.0125*RAIN*TR
      IF(IBUG.EQ.2) CALL PRCO19
C.......................................
C     MELT AT GROUND-SNOW INTERFACE
  110 IF(WE.GT.GM) GO TO 111
      GMRO=WE+LIQW
      MELT=0.0
      ROBG=RAIN
      RAIN=0.0
      SROBG=SROBG+ROBG
      GO TO 150
  111 GMWLOS=(GM/WE)*LIQW
      GMSLOS=GM
C.......................................
C     COMPUTE SURFACE ENERGY EXCHANGE FOR THE COMPUTATIONAL PERIOD BASED
C        ON 100 PERCENT COVER AND NON-RAIN CONDITIONS -
      IF(MC.EQ.1) GO TO 115
      CALL MELT19(IDN,IMN,ALAT,TA,PMELT,MFMAX,MFMIN,MBASE,TINDEX,TIPM,
     1   PCNHS,NMF,LMFV,SMFV)
      MC=1
C.......................................
C     DETERMINE MELT FOR THE TIME INTERVAL - SURFACE ENERGY EXCHANGE
C        IS UNIFORM DURING THE COMPUTATIONAL PERIOD.
  115 CNHS=PCNHS/FNDT
      NR=1
      IF(RAIN.GT.RFMIN) GO TO 120
C
C     NON-RAIN OR LIGHT DIZZLE INTERVAL
      MELT=PMELT/FNDT
      MELT=MELT*MFC
      MELT=MELT+RAINM
      GO TO 130
C
C     RAIN INTERVAL.
  120 EA=2.7489E8*EXP(-4278.63/(TA+242.792))
C     ASSUME 90 PERCENT RELATIVE HUMIDITY DURING RAIN-ON-SNOW
      EA=0.90*EA
      TAK=(TA+273)*0.01
      TAK4=TAK*TAK*TAK*TAK
      QN=SBCI*(TAK4-55.55)
C
C     UADJC IS UADJ MOD MULTIPLIER added by mike smith 2/12/97
C
      QE=8.5*(EA-6.11)*UADJ*UADJC
        IF(IFUT.EQ.0) QE=QE*WINDC
      QH=7.5*0.000646*PA*UADJ*TA*UADJC
        IF(IFUT.EQ.0) QH=QH*WINDC
      MELT=QN+QE+QH+RAINM
      IF(MELT.LT.0.0) MELT=0.0
      NR=0
C
C.......................................
C     COMPUTE AREAL EXTENT OF SNOW COVER BASED ON CONDITIONS AT THE
C        BEGINNING OF THE TIME INTERVAL ADJUSTED FOR NEW SNOWFALL.
C
  130 CALL AESC19(WE,LIQW,ACCMAX,SB,SBAESC,SBWS,SI,ADC,AEADJ,AESC)
C
C     ADJUST VALUES FOR AESC.
      IF(AESC.EQ.1.0) GO TO 134
      MELT=MELT*AESC
      CNHS=CNHS*AESC
      GMWLOS=GMWLOS*AESC
      GMSLOS=GMSLOS*AESC
C.......................................
C     COMPUTE RAIN FALLING ON BARE GROUND
      ROBG=(1.0-AESC)*RAIN
      RAIN=RAIN-ROBG
      GO TO 135
  134 ROBG=0.0
C.......................................
C     COMPUTE SUM AND CHECK CNHS.
  135 SROBG=SROBG+ROBG
      IF((CNHS+NEGHS).LT.0.0) CNHS=-1.0*NEGHS
C
CVK  CUMULATE FOR TIME PERIOD
      SXFALL=SXFALL+SFALL
      SXMELT=SXMELT+MELT
      SXGMLOS=SXGMLOS+GMSLOS
CVK -------------------------
C
C.......................................
C     ADJUST WE FOR SURFACE AND GROUND MELT
C     GROUND MELT
      WE=WE-GMSLOS
      LIQW=LIQW-GMWLOS
      GMRO=GMSLOS+GMWLOS
C
C     SURFACE MELT
      IF(MELT.LE.0.0) GO TO 137
      IF(MELT.LT.WE) GO TO 136
      MELT=WE+LIQW
      QNET=MELT
      DQNET=DQNET+QNET
      IF(NR.EQ.1) SMELT=SMELT+MELT
      IF(NR.EQ.0) SMELTR=SMELTR+MELT
      GO TO 150
  136 WE=WE-MELT
C     QNET=NET SURFACE ENERGY EXCHANGE IN MILLIMETERS WE.
  137 QNET=MELT-CNHS-CNHSPX
      DQNET=DQNET+QNET
      PQNET=PQNET+QNET
      IF(NR.EQ.1) SMELT=SMELT+MELT
      IF(NR.EQ.0) SMELTR=SMELTR+MELT
      IF(IBUG.EQ.2) CALL PRCO19
C.......................................
C     PERFORM HEAT AND WATER BALANCE FOR THE SNOW COVER.
      WATER=MELT+RAIN
      HEAT=CNHS+CNHSPX
      LIQWMX=PLWHC*WE
      NEGHS=NEGHS+HEAT
C     TEMPERATURE OF SNOW CAN NOT BE BELOW-52.8 DEGC
      IF(NEGHS.LT.0.0) NEGHS=0.0
      IF(NEGHS.GT.0.33*WE) NEGHS=0.33*WE
      IF((WATER+LIQW).LT.(LIQWMX+NEGHS+PLWHC*NEGHS)) GO TO 140
C
C     EXCESS WATER EXISTS.
      EXCESS=WATER+LIQW-LIQWMX-NEGHS-PLWHC*NEGHS
      LIQW=LIQWMX+PLWHC*NEGHS
      WE=WE+NEGHS
C
CEA  CUMULATE REFROZEN WATER - PREVIOUSLY IGNORED AT THIS POINT
      SXRFRZ=SXRFRZ+NEGHS
      NEGHS=0.0
      GO TO 145
CEA   IF VARIABLE KOREN IS SET TO 1 (OPTION NOT INCLUDED), VICTOR
CEA     KOREN'S REFREEZING MODIFICATION CAN BE TESTED.
CEA  140 IF (KOREN.EQ.1) GO TO 800
  140 IF(WATER.LT.NEGHS) GO TO 141
CEA   IF(WATER.LT.NEGHS) GO TO 141
C
C     WATER EXCEEDS NEGHS - LIQUID WATER CONTENT IS INCREASED.
      LIQW=LIQW+WATER-NEGHS
      WE=WE+NEGHS
C
CVK  CUMULATE REFROZEN WATER
      SXRFRZ=SXRFRZ+NEGHS
      NEGHS=0.0
      EXCESS=0.0
      GO TO 145
C
C     ALL WATER IS REFROZEN IN THE SNOW COVER.
  141 WE=WE+WATER
      NEGHS=NEGHS-WATER
      EXCESS=0.0
C
CVK  CUMULATE REFROZEN WATER
      SXRFRZ=SXRFRZ+WATER
CEA      GO TO 145
CEA..............................................................
CEA   SECTION TO TEST VICTOR KOREN'S MODIFICATION FOR REFREEZING
CEA  800 IF((WATER+LIQW).LT.NEGHS) GO TO 841
C
C     WATER+LIQW EXCEEDS NEGHS - LIQUID WATER CONTENT IS CHANGED
C       - LIQW INCREASES IF WATER > NEGHS
C       - LIQW DECREASES IF WATER < NEGHS AND NEGHS > 0.0
CEA      LIQW=LIQW+WATER-NEGHS
CEA      WE=WE+NEGHS
C
CVK  CUMULATE REFROZEN WATER
CEA      SXRFRZ=SXRFRZ+NEGHS
CEA      NEGHS=0.0
CEA      EXCESS=0.0
CEA      GO TO 145
C
C     ALL WATER IS REFROZEN IN THE SNOW COVER.
CEA  841 WE=WE+WATER+LIQW
CEA      NEGHS=NEGHS-WATER-LIQW
CVK  CUMULATE REFROZEN WATER
CEA      SXRFRZ=SXRFRZ+WATER+LIQW
CEA      LIQW=0.0
CEA      EXCESS=0.0
C
CEA   END OF SECTION TESTING KOREN'S MODIFICATION
CEA..............................................................
C
C     IF NO NEGATIVE HEAT - TINDEX MUST BE 0.0.
  145 IF(NEGHS.EQ.0.0) TINDEX=0.0
      IF(IBUG.EQ.2) CALL PRCO19
C.......................................
C     ROUTE EXCESS WATER THROUGH THE SNOW COVER.
      CALL ROUT19(ITPX,EXCESS,WE,AESC,STORGE,NEXLAG,EXLAG,PACKRO)
      IF(IBUG.EQ.2) CALL PRCO19
C.......................................
C     ADD GROUNDMELT RUNOFF TO SNOW COVER OUTFLOW.
      PACKRO=PACKRO+GMRO
      IF(IBUG.EQ.1) WRITE(IODBUG,906) SFALL,RAIN,MELT,NR,CNHS,CNHSPX,
     1AESC,QNET,EXCESS,GMRO,PACKRO,ROBG
  906 FORMAT(1H0,8X,5HSFALL,4X,4HRAIN,4X,4HMELT,2X,2HNR,4X,4HCNHS,2X,6HC
     1NHSPX,4X,4HAESC,4X,4HQNET,2X,6HEXCESS,4X,4HGMRO,2X,6HPACKRO,4X,4HR
     2OBG,/6X,3F8.3,I4,8F8.3)
      GO TO 190
C.......................................
C     SNOW GONE - SET ALL CARRYOVER TO NO SNOW CONDITIONS.
  150 TEX=0.0
      DO 151 N=1,NEXLAG
  151 TEX=TEX+EXLAG(N)
      PACKRO=GMRO+MELT+TEX+STORGE+RAIN
      CALL ZERO19
      AESC=0.0
      SNDEN=-9.99
      GO TO 190
C.......................................
C     NO SNOW COVER - NO NEW SNOWFALL.
  160 ROBG=PXI
      PACKRO=0.0
      AESC=0.0
      SROBG=SROBG+ROBG
C.......................................
C     COMPUTE RAIN+MELT
  190 RM(I)=PACKRO+ROBG
      SRM=SRM+RM(I)
      PSNWRO=PSNWRO+PACKRO
      PROBG=PROBG+ROBG
  200 CONTINUE
C     END OF COMPUTATIONAL PERIOD
C.......................................
C.......................................
C     SET SIMULATED AESC AND TOTAL WATER-EQUIVALENT.
      TEX=0.0
      DO 210 N=1,NEXLAG
  210 TEX=TEX+EXLAG(N)
C
CEA   CHANGED SEQUENCE FOR CALLING SNDEPTH19
C
CVK --  V.KOREN  04/05/00   ---------------------------------
CVK   CALL SNDEPTH19 SUBROUTINE TO CALCULATE SNOW DEPTH
CEA      IF(WE .GT. 0.) THEN
CEA       SLIQ=LIQW+TEX+STORGE
CEA       SXFALL=SXFALL-SXMELT
CEA       IF(SXFALL .LT. 0.) SXFALL=0.0
CEA       CALL SNDEPTH19(WE,SLIQ,SXFALL,SXGMLOS,SXRFRZ,
CEA     +              TA,DTA,IDT,SNDPT,SNDEN,SNTMP)
CEA      ELSE
CEA       SNDPT=0.
CEA       SNTMP=0.
CEA       SNDEN=0.1
CEA      ENDIF
CVK ---------------------------------------------------------
C
      TWE=WE+LIQW+TEX+STORGE
C
CEA  NEW SNOW DEPTH COMPUTATIONAL SEQUENCE
C
CEA      IF (TWE.EQ.0.0) GO TO 215
      IF (TWE.EQ.0.0) GO TO 211
C
CEA  SNOW EXISTS - COMPUTE DEPTH AND TEMPERATURE
C
      IF (IVER.GT.1) THEN
      SLIQ=LIQW+TEX+STORGE
C
      CALL SNDEPTH19(WE,SLIQ,SXFALL,SXMELT,SXGMLOS,SXRFRZ,TA,DTA,IDT,
     1  SNDPT,SNDEN,SNTMP,IBUG)
      ENDIF
      GO TO 212
C
CEA  NO SNOW COVER EXISTS
C
  211 IF (IVER.GT.1) THEN
      SNDPT=0.0
      SNTMP=0.0
      GO TO 215
      ENDIF
C
C     COMPUTE AREAL EXTENT BASED ON CONDITIONS AT THE END OF THE PERIOD.
CEA      CALL AESC19(WE,LIQW,ACCMAX,SB,SBAESC,SBWS,SI,ADC,AEADJ,AESC)
  212 CALL AESC19(WE,LIQW,ACCMAX,SB,SBAESC,SBWS,SI,ADC,AEADJ,AESC)
  215 COVER=AESC
C
C     STORE VALUES SO COMPUTED VALUES WILL BE AVAILABLE FOR PRINTOUT
C        EVEN IF UPDATING OCCURS.
      CWE=TWE
      CAESC=COVER
C.......................................
C     UPDATING SECTION
      IF((OWE.LT.0.0).AND.(OSC.LT.0.0)) GO TO 280
      IF (IBUG.EQ.2) CALL PRCO19
      CALL UPDT19(OWE,OSC,TWE,COVER,IUPWE,IUPSC,WETOL,SCTOL,
     1  SI,ADC,IVER)
C.......................................
C     PASS VALUES TO GRAPHICS INTERFACE IF REQUESTED.
  280 IF (IOUTYP.EQ.0) GO TO 290
      LIQWMX=PLWHC*WE

      CALL ICP19(OPNAME,IMN,IYR,KDA,KHR,PRAIN,PSFALL,RSL,TA,
     1  PQNET,PSNWRO,PROBG,NEGHS,LIQW,LIQWMX,TWE,COVER,
     2  OWE,OSC,WE,TEX,STORGE,ACCMAX,SB,SBAESC,SBWS,
     3  AEADJ,TINDEX,MAINUM,SNDPT,SNTMP,ODPT)
C.......................................

C     CHECK FOR DEBUG OUTPUT.
  290 IF(IBUG.EQ.0) RETURN
CEA  ADD SNDPT,SNDEN, AND SNTMP TO DEBUG
      WRITE(IODBUG,904) TWE,COVER,CWE,CAESC,SNDPT,SNDEN,SNTMP
  904 FORMAT(1H0,5X,17HOUTPUT DATA--TWE=,F6.1,2X,6HCOVER=,F4.2,2X,
     1 4HCWE=,F6.1,2X,6HCAESC=,F4.2,2X,6HSNDPT=,F6.1,2X,6HSNDEN=,F5.2,
     2 2X,6HSNTMP=,F6.1)
      WRITE(IODBUG,905) (RM(I),I=1,NDT)
  905 FORMAT(1H ,5X,3HRM=,24F5.2)
      IF ((TWE.NE.CWE).OR.(COVER.NE.CAESC)) CALL PRCO19
C.......................................
      RETURN
      END
