C$PRAGMA C (ICP19)
C MEMBER PACK19
C  (from old member FCPACK19)
C
CEA  OBS. DEPTH AND VERSION NUMBER ADDED TO ARGUMENT LIST
C
      SUBROUTINE PACK19(KDA,KHR,NDT,TA,PX,PCTS,RSL,OWE,OSC,ODPT,PGM,
     1  RM,TWE,COVER,CWE,CAESC,IFUT,IDT,IBUG,IDN,IMN,IYR,IOUTYP,
     2  OPNAME,IVER)
C
C                             LAST UPDATE: 06/22/95.14:05:09 BY $WC30EA
C
CEA      SUBROUTINE PACK19(KDA,KHR,NDT,TA,PX,PCTS,RSL,OWE,OSC,PGM,RM,TWE,
CEA     1COVER,CWE,CAESC,IFUT,IDT,IBUG,IDN,IMN,IYR,IOUTYP,OPNAME)
C.......................................
C     THIS SUBROUTINE EXECUTES THE 'SNOW-17 ' OPERATIONAL FOR ONE
C        COMPUTATIONAL PERIOD.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980
C
C        UPDATED 4/15/00 BY V. KOREN TO ADD SNOW DEPTH CALCULATIONS
C
C        MODIFIED 11/05 BY E. ANDERSON TO ADD TAPREV TO SNCO19 COMMON
C          AND TO FIX SOME OTHER PROBLEMS.
C
C        MODIFIED 1/06 BY E. ANDERSON TO REVISE PROCEDURE FOR ACCOUNTING
C          FOR NEW SNOWFALL ON A PARTLY BARE AREA AND USE OF SNOF.
C.......................................
      REAL MFMAX,MFMIN,NMF,LIQW,NEGHS,MBASE,MELT,LIQWMX,MFC
      DIMENSION PX(*),PCTS(*),RM(*),OPNAME(2)
C
C     COMMON BLOCKS
      INCLUDE 'common/ionum'
      INCLUDE 'common/fprog'
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FSNWUP/IUPWE,IUPSC
      COMMON/SNPM19/ALAT,SCF,MFMAX,MFMIN,NMF,UADJ,SI,MBASE,PXTEMP,
     1   PLWHC,TIPM,PA,ADC(11),LMFV,SMFV(12),LAEC,NPTAE,AE(2,14)
CVK  ADDED TWO MORE STATES: SNDPT & SNTMP
      COMMON/SNCO19/WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,
CVK     1   STORGE,AEADJ,NEXLAG,EXLAG(7)
CEA     1   STORGE,AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP,OSNDPT
     1   STORGE,AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP,TAPREV     
      COMMON/SUMS19/SPX,SSFALL,SRM,SMELT,SMELTR,SROBG,DSFALL,DRAIN,
     1 DQNET,DRSL,NDRSP
      COMMON/SNUP19/MFC,SFALLX,WINDC,SCTOL,WETOL,SNOF,UADJC
CEA   COMMON SN19FLG NO LONGER NEEDED
CEA      common/sn19flg/ISTRT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob90/ohd/ofs/src/fcst_snow/RCS/pack19.f,v $
     . $',                                                             '
     .$Id: pack19.f,v 1.11 2006/10/03 19:38:45 hsu Exp $
     . $' /
C    ===================================================================
C
CEA*****************************************************************
CEA  DEPTH COMPUTATIONS AND CARRYOVER VARIES WITH VERSION NUMBER
CEA    VERSION 1 - NO DEPTH COMPUTATIONS
CEA      SNDPT & SNTMP = -999.
CEA      TAPREV = -99.
CEA    VERSIONS 2 AND 3 - DEPTH COMPUTED, NO TAPREV CARRYOVER
CEA      SNDPT & SNTMP DEFINED
CEA      TAPREV - START OF RUN - TAPREV = CURRENT AIR TEMPERATURE
CEA             - DURING RUN - TAPREV IS PREV PERIOD TEMPERATURE
CEA    VERSION 4 - DEPTH COMPUTED, TAPREV CARRYOVER EXISTS
CEA      SNDPT, SNTMP, & TAPREV ALL DEFINED.
CEA*****************************************************************
CVK  CHANGES -------------------------------------------
cav      SAVE ISTRT,TPREV,SNDEN
cav      INTEGER ISTRT/0/ 
cav move the istrt variable to e19
CEA  TAPREV NOW SAVED VIA SNCO19 COMMON AND DENSITY COMPUTED
CEA      SAVE TPREV,SNDEN
CVK   CALCULATE AIR TEMPERATURE CHANGE, DTA
CVK   AND KEEP PREVIOUS TIME STEP TEMPERATURE
C
CEA  ISTRT NO LONGER NEEDED - TAPREV MAINTAINED IN EX19
CEA  DENSITY COMPUTED FROM WE AND DEPTH WHEN SNOW EXISTS AND DEPTH
CEA    IS BEING COMPUTED.
CEA      IF(ISTRT .EQ. 0) THEN
CEA       TPREV=TA
CEA       IF(SNDPT .GT. 0.0) SNDEN=(0.1*WE)/SNDPT
CEA       ISTRT=1
CEA      ENDIF 
      SNDEN=-9.99
      IF ((IVER.GT.1).AND.(WE.GT.0.0)) SNDEN=(0.1*WE)/SNDPT
C    COMPUTE TEMPERATURE DIFFERENCE FROM PRECEEDING PERIOD.
CEA  VARIABLE TPREV CHANGED TO TAPREV
      IF (WE.EQ.0.0) THEN
CEA      IF(SNDPT .LE. 0.0) THEN
       DTA=TA
      ELSE 
       DTA=TA-TAPREV
       IF(TAPREV .GT. 0. .AND. TA .GT. 0.) DTA=ABS(DTA)
       IF(TAPREV .GT. 0. .AND. TA .LT. 0.) DTA=TA
      ENDIF
CEA      TPREV=TA
CVK-----------------------------------------------------
C
C     CONSTANTS
C     IF SNOWFALL EXCEEDS SNEW/HR--TINDEX=TPX
      SNEW=1.5
C     IF RAIN EXCEEDS RMIN/HR--USE RAIN-ON-SNOW MELT EQUATION
      RMIN=0.25
C     SBC=STEFAN/BOLTZMAN CONSTANT--MM/(((DEGK/100)**4)*HR)
      SBC=.0612
C.......................................
C     CHECK IF DEBUG OUTPUT REQUESTED.
C
      IF(IBUG.EQ.0) GO TO 100
      WRITE(IODBUG,900) KDA,KHR,NDT,IDT,IFUT,MFC
  900 FORMAT(1H0,18HPACK19 DEBUG--KDA=,I6,2X,4HKHR=,I2,2X,4HNDT=,I2,2X,4
     1HIDT=,I2,2X,5HIFUT=,I1,2X,4HMFC=,F6.3)
CEA  ADD ODPT TO DEBUG
      WRITE(IODBUG,903)TA,RSL,OWE,OSC,PGM,TWE,ODPT
  903 FORMAT(1H ,5X,15HINPUT DATA--TA=,F6.2,2X,4HRSL=,F6.0,
     1  2X,4HOWE=,F6.1,2X,4HOSC=,F7.2,2X,4HPGM=,F5.3,2X,4HTWE=,F6.1,
     2  2X,5HODPT=,F6.1)
      J=NDT
      IF(J.GT.6) J=6
      WRITE(IODBUG,901) (PX(I),PCTS(I),I=1,J)
  901 FORMAT(1H ,5X,12HPX AND PCTS=,6(F6.2,F8.2))
      IF(J.EQ.NDT) GO TO 101
      WRITE(IODBUG,902) (PX(I),PCTS(I),I=7,NDT)
  902 FORMAT(1H ,17X,6(F5.1,F8.2))
  101 CALL PRCO19
C.......................................
C     INITIAL VALUES
  100 ITPX=IDT/NDT
      FITPX=ITPX
      FNDT=NDT
      GM=PGM/FNDT
      SFNEW=SNEW*FITPX
      RFMIN=RMIN*FITPX
      SBCI=SBC*FITPX
      MC=0
      PSFALL=0.0
      PRAIN=0.0
      PQNET=0.0
      PSNWRO=0.0
      PROBG=0.0
C
CVK ----     V.I.K.  04/10/00  --------- 
C    COMPUTE COMPUTATIONAL PERIOD VALUES OF SNOWFALL, SURFACE MELT
C      GROUND MELT, AND REFREEZING OF LIQUID WATER
      SXFALL=0.0
      SXMELT=0.0
CEA  VARIABLE SXGSLOS CHANGED TO SXGMLOS
      SXGMLOS=0.0
      SXRFRZ=0.0
C --------------------------------------     
C.......................................
C.......................................
C     CYCLE THROUGH THE COMPUTATIONAL PERIOD FOR EACH PRECIPITATION
C        TIME INTERVAL
      DO 200 I=1,NDT
      PXI=PX(I)
      IF((PXI.EQ.0.0).AND.(WE.EQ.0.0)) GO TO 160
      SFALL=0.0
      CNHSPX=0.0
      RAIN=0.0
      RAINM=0.0
      IF(PXI.EQ.0.0) GO TO 110
C.......................................
C     DETERMINE FORM OF PRECIP. AND ACCUMULATE SNOW COVER IF SNOW.
      PCT=PCTS(I)
      IF(PCT.GT.1.0) PCT=1.0
      TPX=TA
      IF(PCT.LT.0.0) GO TO 102
C
C     FORM OF PRECIP. INPUT.
      FRACS=PCT
      FRACR=1.0-PCT
      GO TO 105
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
