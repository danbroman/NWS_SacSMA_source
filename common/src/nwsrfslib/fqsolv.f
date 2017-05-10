C MEMBER FQSOLV
C  (from old member FCFLOOPR)
C
C
C  DESC -- COMPUTE UNKNOWN DISCHARGE GIVEN CURRENT STAGE
C @PROCESS LVL(77)
C
C......................................................................
C
      SUBROUTINE FQSOLV (HF,HP,QP,EPQ,SLE,CONR,DTS,DQ,G,QF,IBUG)
C
C.......................................................................
C
C   THIS SUBROUTINE USES NEWTON ITERATION TO SOLVE FOR THE
C   UNKNOWN DISCHARGE WHEN THE STAGE HYDROGRAPH IS GIVEN.
C  ARGUMENT LIST:
C      HF     - THE CURRENT WATER ELEV.
C      HP     - THE PREVIOUS TIME STEP WATER ELEV.
C      QP     - THE PREVIOUS TIME STEP DISCHARGE
C      EPQ    - ITERATIVE CONVERGENCE CRITERION
C      SLOPE  - CHANNEL BOTTOM SLOPE
C      CONR   = 2./3.*SLOPE/FRLOOP/FRLOOP
C      DTS    - DELTA TIME STEP (SECONDS)
C      DQ     - PREVIOUS CHANGE IN DISCHARGE
C      G      - GRAVITY ACCELERATION CONSTANT
C      QF     - OUTPUT DISCHARGE DETERMINED BY SUBROUTINE
C
C.......................................................................
C
C  SUBROUTINE ORIGINALLY WRITTEN BY --
C      DANNY FREAD -- HRL --731101
C  CONVERTED FOR NWSRFS BY --
C      JONATHAN WETMORE - HRL -801031
C
C......................................................................
C
!CP   INCLUDE 'ionum'
      INCLUDE 'flogm'
!CP   INCLUDE 'common/fdbug'
!CP   INCLUDE 'common/where'
      INCLUDE 'fratng'
      INCLUDE 'facxsc'
C
       DIMENSION SUBNAM(2),OLDSUB(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rc/RCS/fqsolv.f,v $
     . $',                                                             '
     .$Id: fqsolv.f,v 1.2 1998/07/02 17:11:08 page Exp $
     . $' /
C    ===================================================================
C
       DATA LOOP,SUBNAM/4HLOOP,4HQSOL,4HVE  /
C  SET /WHERE/ INFO
C
!CP - common/where NOT used for FEWS module
!     DO 500 I=1,2
!      OLDSUB(I) = OPNAME(I)
!  500 OPNAME(I) = SUBNAM(I)
!      IOLDOP = IOPNUM
!      IOPNUM = 0
C
      IBUG = 0
      IF (IFBUG(LOOP).EQ.1) IBUG = 1
C
!CP   IF (ITRACE.GE.3) WRITE(IODBUG,601)
      IF ( FEWSDEBUG.GE.5 ) THEN
         WRITE(MESSAGESTRING, 601)
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
  601    FORMAT(1H ,21H *** ENTER FQSOLV ***)
      END IF 
C
C  SET ARRAY INDEX
      LH1=LOCH
      LHN=LH1+NRCPTS-1
      LXE1=LXELEV
      LXEN=LXE1+NCROSS-1
      LXT1=LXTOPW
      RCMXH=XRC(LHN)
      RCMXEL=RCMXH+GZERO
C
C  COMPUTE CONSTANTS
      CALL FSECT(HP,A,B,BO,R,DB,DBO,DR,FK)
      FL3=SLOPE+CONR+QP/G/A/DTS
      CALL FSECT (HF,A,B,BO,R,DB,DBO,DR,FK)
      CALL FRICT (HF,CMN,DCMN)
      IF(FLOODN.GT.1.0) GO TO 510
      IF(HP.LE.RCMXEL .OR. FLOODN.LE.0.0) GO TO 510
      CALL FSECT(RCMXEL,XA,BTOP,XBO,XR,XDB,XDBO,XDR,XFK)
      CALL FRICT (RCMXEL,CMN,XDCMN)
      BFLD=B-BTOP
      CMM=(CMN*CMN*BTOP+FLOODN*FLOODN*BFLD)/B
      CMN=SQRT(CMM)
  510 FL2=1.0*A*R/CMN
      LOCFR=EMPTY(1)
      QMIN=XRC(LOCFR+5)
      SMIN=SLOPE
      IF(QMIN.LE.FL2) SMIN=(QMIN/FL2)**2
      IF(SMIN.GE.SLOPE) SMIN=SLOPE
      DHS=(HF-HP)/DTS
      FL4=A*DHS/FK
      FL5=(B+BO-B/FK)*DHS/G/A/A-1./G/A/DTS
      FL6=-CONR/G*B/A/A/A
!CP   IF(IBUG.GE.1) WRITE(IODBUG,602) SMIN,FL2,FL3,FL4,FL5,FL6,CMN
      IF ( FEWSDEBUG.GE.5 ) THEN
         WRITE(MESSAGESTRING, 602) SMIN,FL2,FL3,FL4,FL5,FL6,CMN
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 602     FORMAT(1H0,2X,'SMIN=',E12.5,2X,4HF2 =,E12.5,3X,4HF3 =,E12.5,3X,
     1   3X,'F4=',E12.5,3X,4HF5 =,E12.5,3X,4HF6 =,E12.5,3X,'CMN=',F7.5)
      END IF
C  COMPUTE STARTING VALUE FOR ITERATIVE SOLUTION OF EQ(15)
      QK=FL2*SQRT(SLE)
C  SOLVE EQ(15) BY NEWTON ITERATION
      FOLD=1.0E+10
      DO 10 K=1,20
      FL0=FL3+FL4/QK+FL5*QK+FL6*QK*QK
      IF(FL0.LE.SMIN) THEN
        FL0=SLE
        DF=1.0
        GO TO 15
      END IF
      FL1=-FL4/QK/QK+FL5+2.*FL6*QK
      DF=1.-0.5*FL2*FL1/SQRT(FL0)
   15 F=QK-FL2*SQRT(FL0)
!CP   IF(IBUG.GE.1) WRITE(IODBUG,603) SLE,FL0,FL1,QK,F,DF
      IF ( FEWSDEBUG.GE.5 ) THEN
         WRITE(MESSAGESTRING, 603) SLE,FL0,FL1,QK,F,DF
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 603     FORMAT(1H0,2X,'SLE = ',E12.5,
     1   2X,5HF0 = ,E12.5,3X,5HF1 = ,E12.5,3X,5HQK = ,F10.1,
     1   3X,5HF  = ,F10.1,3X,5HDF = ,F10.1)
      END IF
      IF(ABS(F).GT.ABS(FOLD)) GO TO 35
      FOLD=F
      QKOLD=QK
      QKK=QK-F/DF
      ADQ=ABS(QKK-QK)
      IF(ADQ.LE.EPQ) GO TO 20
   10 QK=QKK
C  SOLVE EQ(15) BY BISECTION METHOD
   35 CONTINUE
!CP   IF(IBUG.GE.1) WRITE(IODBUG,310) QK,F,QKOLD,FOLD
      IF ( FEWSDEBUG.GE.5 ) THEN
         WRITE(MESSAGESTRING, 310) QK,F,QKOLD,FOLD
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 310  FORMAT(/2X,'NONCONVERGENCE OCCURED IN NEWTON-RALPHSON SOLUTION,'
     & /5X,'BISECTION METHOD IS USED, NEW Q,F= ',2F10.0,
     & 5X,'OLD Q,F= ',2F10.0)
      END IF
      SNF=F*FOLD
      IF(SNF.GT.0.) GO TO 350
      QMX=QKOLD
      QMN=QK
      IF(QK.GT.QKOLD) THEN
        QMX=QK
        QMN=QKOLD
      END IF
      QKO=1.0+10
      QK=0.5*(QMX+QMN)
      DO 300 K=1,100
      FL0=FL3+FL4/QK+FL5*QK+FL6*QK*QK
      IF(FL0.LE.SMIN) FL0=SLE
      F=QK-FL2*SQRT(FL0)
!CP   IF(IBUG.GE.1) WRITE(IODBUG,605) SLE,FL0,FL2,QK,F
      IF ( FEWSDEBUG.GE.5 ) THEN
         WRITE(MESSAGESTRING, 605) SLE,FL0,FL2,QK,F
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 605     FORMAT(1H0,2X,'SLE = ',E12.5,
     1   2X,5HF0 = ,E12.5,3X,5HFL2= ,E12.5,
     1   3X,5HQK = ,F10.2,3X,5HF  = ,F10.1)
      END IF
      QKK=QK
      IF (ABS(QK-QKO).LT.EPQ) GO TO 350
      QKO=QK
      IF (F.LT.0.0) QMX=QK
      IF (F.GT.0.0) QMN=QK
      QK=0.5*(QMX+QMN)
  300 CONTINUE
  350 CONTINUE
      IF (ABS(F).LE.1.0) GO TO 20
!CP   IF(IBUG.GE.1) WRITE(IODBUG,360) HF
      IF ( FEWSDEBUG.GE.5 ) THEN
         WRITE(MESSAGESTRING, 360) HF
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 360  FORMAT(/2X,'BOTH NEWTON-RALPHSON AND BISECTION METHOD FAILED,',
     & /5X,'NORMAL FLOW IS USED,  FOR STAGE H= ',F10.2)
      END IF
      ICS=1
      NEEDS=NEEDEX
      LOWES=LOWEXT
      IUPES=IUPEXT
      NWS=NW
      NRANS=NRANGE
      MISIS=MISING
      CARRS=CARRYO
      HFSTG=HF-GZERO
      CALL FHQS1(HFSTG,QKK,ICS,IBUG,NEEDEX,LOWEXT,IUPEXT,
     $      NW,NRANGE,MISING,CARRYO)
      NEEDEX=NEEDS
      LOWEXT=LOWES
      IUPEXT=IUPES
      NW=NWS
      NRANGE=NRANS
      MISING=MISIS
      CARRYO=CARRS
   20 QF=QKK
      SLE=FL0
!CP   IF (ITRACE.GE.2) WRITE(IODBUG,609)
!     OPNAME(1) = OLDSUB(1)
!     OPNAME(2) = OLDSUB(2)
!     IOPNUM = IOLDOP
      IF ( FEWSDEBUG.GE.5 ) THEN
         WRITE(MESSAGESTRING, 609)
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
  609    FORMAT(1H ,21H *** EXIT  FQSOLV ***)
      END IF
C
      RETURN
      END
