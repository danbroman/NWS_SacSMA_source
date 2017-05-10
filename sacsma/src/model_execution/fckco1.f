C MEMBER FCKCO1
C  (from old member FCEX1)
C
      SUBROUTINE FCKCO1(KDA,KHR,NOUTZ,NOUTDS,N,IFRZE,BALADJ,IBUG,
     1  IPRINT,IOUT)
C.......................................
C     SUBROUTINE CHECKS AND ADJUSTS SAC-CMA CARRYOVER
C.......................................
C     WRITTEN BY -- ERIC ANDERSON - HRL - DEC 1982
C.......................................
      REAL LZTWM,LZFSM,LZFPM,LZSK,LZPK,LZTWC,LZFSC,LZFPC,LT,LS,LP
      REAL LTC,LSC,LPC
C
C     COMMON BLOCKS
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FSMPM1/UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC,REXP,LZTWM,
     1LZFSM,LZFPM,LZSK,LZPK,PFREE,SIDE,SAVED,PAREA
      COMMON/FSMCO1/UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC,FGCO(6),RSUM(7),
     1PPE,PSC,PTA,PWE
      COMMON/FCOSAC/NSCV,JHSAC(10),ZONEC(6,10),BFMULT(10),FGIX(10)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob90/ohd/ofs/src/fcst_sac/RCS/fckco1.f,v $
     . $',                                                             '
     .$Id: fckco1.f,v 1.1 1995/09/17 18:57:35 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     STORE STARTING VALUES
      UT=UZTWC
      UF=UZFWC
      LT=LZTWC
      LS=LZFSC
      LP=LZFPC
      AD=ADIMC
      FX=FGCO(1)
C.......................................
C     MAKE MOD CHANGE IF INDICATED.
      IF(N.EQ.0) GO TO 100
      IF(ZONEC(1,N).GE.0.0) UZTWC=ZONEC(1,N)
      IF(ZONEC(2,N).GE.0.0) UZFWC=ZONEC(2,N)
      IF(ZONEC(3,N).GE.0.0) LZTWC=ZONEC(3,N)
      IF(ZONEC(6,N).GE.0.0) ADIMC=ZONEC(6,N)
      IF((IFRZE.GT.0).AND.(FGIX(N).GT.-900.0)) FGCO(1)=FGIX(N)
      IF((ZONEC(4,N).GE.0.0).OR.(ZONEC(5,N).GE.0.0)) GO TO 90
      IF(BFMULT(N).LT.0.0) GO TO 95
      LZFSC=LZFSC*BFMULT(N)
      LZFPC=LZFPC*BFMULT(N)
      GO TO 95
   90 IF(ZONEC(4,N).GE.0.0) LZFSC=ZONEC(4,N)
      IF(ZONEC(5,N).GE.0.0) LZFPC=ZONEC(5,N)
   95 IF((IBUG.EQ.0).AND.(IPRINT.EQ.0)) GO TO 100
      WRITE(IOUT,901) UT,UF,LT,LS,LP,AD,FX
  901 FORMAT(1H ,1X,3HOLD,2X,F7.2,F7.3,F7.2,F7.3,2F7.2,3X,5HFGIX=,F6.1)
      WRITE(IOUT,902) UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC,FGCO(1)
  902 FORMAT(1H ,1X,3HNEW,2X,F7.2,F7.3,F7.2,F7.3,2F7.2,3X,5HFGIX=,F6.1)
C.......................................
C     STORE VALUES AFTER MOD.
 100  UTC=UZTWC
      UFC=UZFWC
      LTC=LZTWC
      LSC=LZFSC
      LPC=LZFPC
      ADC=ADIMC
      FGX=FGCO(1)
C.......................................
C     CHECK STATE VARIABLES FOR ILLEGITIMATE VALUES.
      L=0
      IF (UZTWC.LE.UZTWM) GO TO 20
      UZTWC=UZTWM
      L=1
   20 IF (UZFWC.LE.UZFWM) GO TO 21
      UZFWC=UZFWM
      L=1
   21 IF (LZTWC.LE.LZTWM) GO TO 22
      LZTWC=LZTWM
      L=1
   22 IF (LZFSC.LE.LZFSM) GO TO 23
      LZFSC=LZFSM
      L=1
   23 IF (LZFPC.LE.LZFPM) GO TO 24
      LZFPC=LZFPM
      L=1
   24 IF (ADIMC.LE.(UZTWM+LZTWM)) GO TO 25
      ADIMC=UZTWM+LZTWM
      L=1
   25 IF (ADIMC.GE.UZTWC) GO TO 26
      ADIMC=UZTWC
      L=1
   26 IF (IFRZE.EQ.0) GO TO 27
      IF (FGCO(1).LE.0.0) GO TO 27
      FGCO(1)=0.0
      L=1
   27 IF (L.EQ.0) GO TO 110
      CALL MDYH1(KDA,KHR,MO,ID,IY,IH,NOUTZ,NOUTDS,TZ)
      WRITE (IPR,911) MO,ID,IY,IH,TZ
  911 FORMAT (1H0,10X,11H**WARNING**,10X,I2,1H/,I2,1H/,I4,2H--,I2,1X,A4)
      WRITE (IPR,907)
  907 FORMAT (1H ,10X,50HSAC-SMA STATE VARIABLES CONTAIN IMPOSSIBLE VALU
     1ES.,5X,5HUZTWC,2X,5HUZFWC,2X,5HLZTWC,2X,5HLZFSC,2X,5HLZFPC,
     22X,5HADIMC,3X,4HFGIX)
      WRITE (IPR,908) UTC,UFC,LTC,LSC,LPC,ADC,FGX
  908 FORMAT (1H ,41X,19HINITIAL VALUES WERE,3X,6F7.0,F7.1)
      WRITE (IPR,909) UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC,FGCO(1)
  909 FORMAT (1H ,50X,10HCHANGED TO,3X,6F7.0,F7.1)
      CALL WARN
C.......................................
C     COMPUTE BALANCE ADJUSTMENT IF ANY CONTENTS WERE CHANGED.
  110 IF((N.EQ.0).AND.(L.EQ.0)) RETURN
      BALADJ=BALADJ+(UZTWC+UZFWC+LZTWC+LZFPC+LZFSC-UT-UF-LT-LP-LS)
     1*PAREA+(ADIMC-AD)*ADIMP
C.......................................
      RETURN
      END
