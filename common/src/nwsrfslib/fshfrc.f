C MEMBER FSHFRC
C  (from old member FCFSHFRC)
C  NOTE:  Any changes that is made to this file, must be included
C         to the fcfshfrc.f as well. fcfshfrc.f is used in ifp program
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C  DESC -- ADJUSTS A  RATING CURVE BY SHIFTING IT
C                             LAST UPDATE: 06/30/95.13:05:09 BY $WC20SV
C

      SUBROUTINE FSHFRC(IBUG,NEEDEX,LOWEXT,IUPEXT,NW,NRANGE,MISING,
     $ CARRYO,IADJ,XLOG,H,Q,NRCPT,HNEW,QNEW,HD,HU)

C         THIS ROUTINE ADJUSTS A  RATING CURVE BY SHIFTING IT
C         +/- CONST. FLOW, +/- %FLOW, OR BLENDING AN OBSERVED
C         VALUE INTO THE SINGLE-VALUED RATING CURVE.
C
CC      COMMON/FRATNG/RTCVID(2),RIVERN(5),RIVSTA(5),RLAT,RLONG,
CC     $ FPTYPE(5),AREAT,AREAL,FLDSTG,FLOODQ,PVISFS,SCFSTG,
CC     $ WRNSTG,GZERO,NRCPTS,LOCQ,LOCH,STGMIN,NCROSS,LXTOPW,
CC     $ LXELEV,ABELOW,FLOODN,SLOPE,FRLOOP,SHIFT,OPTION,
CC     $ LASDAY,IPOPT,RFSTG,RFQ,IRFDAY,RFCOMT(5),EMPTY(25),XRC(225)
C
CC      COMMON/MODRCS/ NUMRC,RCSID(2,2),NSHIFT(2),IJHSHF(5,2),LJHSHF(5,2
CC     $ ISTYPE(5,2),HNEW(5,2),QNEW(5,2),HL(5,2),HU(5,2)
C
      INCLUDE 'rcshf'
cc    COMMON/RCSHF/QSHIFT,QORG(100),HORG(100),NRCPTO
C
      COMMON/IONUM/IN,IPR,IPU
C
!CP   COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      INCLUDE 'flogm' 
C
      DIMENSION RCSPT(3),Q(*), H(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rc/RCS/fshfrc.f,v $
     . $',                                                             '
     .$Id: fshfrc.f,v 1.2 2002/02/11 13:31:29 michaelo Exp $
     . $' /
C    ===================================================================
C


C                        INPUT PARAMETERS:
C      NRCPT  = NO. OF POINTS IN THE ORIGINAL RATING CURVE (R-C)
C      IADJ   = PARAMETER THAT DESCRIBES TYPE OF SHIFT
C               0 - SHIFT R-C LEFT/RIGHT BY A CONSTANT FLOW (CMS)
C               1 - SHIFT R-C LEFT/RIGHT BY A PERCENT OF FLOW (%)
C               2 - BLEND AND OBSERVED POINT INTO THE R-C
C        Q(I) = DISCHARGE (CFS) IN R-C TABLE
C        H(I) = ELEVATION CORR. TO Q(I) IN R-C TABLE
C
C        QNEW = OBS FLOW (CMS)
C        HNEW = STAGE CORR. TO QNEW IN R-C (M)
C
C          HD = LOWER STAGE WHERE BLENDING WILL COMMENCE (M)
C          HU = UPPER STAGE WHERE BLENDING WILL CEASE (M)

C      CHECK IF TRACE IS ON.
!CP   IF (ITRACE.GE.3) WRITE(IODBUG,1000)
      IF ( ITRACE.GE.1 ) THEN
         WRITE(MESSAGESTRING, 1000)
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 1000    FORMAT(1H0,18H*** FSHFRC ENTERED)
      END IF

      JCONV=1
      IXTRA=0
ccAV
      QU = 0

 9000 FORMAT(/12X,'SHIFTED RATING CURVE'/10X,' I     STAGE   DISCHARGE')

      IF(IADJ-1) 100,200,300

C                SHIFT FLOW BY CONSTANT QSHIFT (CMS)

  100 CALL FHQS1(HNEW,QOLD,JCONV,IBUG,NEEDEX,LOWEXT,
     1   IUPEXT,NW,NRANGE,MISING,CARRYO)
      QSHIFT=QNEW-QOLD
!CP   IF(IBUG.EQ.0) GO TO 500
!CP   WRITE(IODBUG,9000)
      IF ( FEWSDEBUG.LT.5 ) GO TO 500
      WRITE(MESSAGESTRING, 9000)
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      DO 110 I=1,NRCPT
      QSH = Q(I)+QSHIFT
!CP   WRITE(IODBUG,9100) I,H(I),QSH
      WRITE(MESSAGESTRING, 9100) I,H(I),QSH
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 9100 FORMAT(10X,I2,2F10.2)
  110 CONTINUE
      GO TO 500

C         SHIFT FLOW BY QNEW % -- QSH = Q + %Q

  200 CALL FHQS1(HNEW,QOLD,JCONV,IBUG,NEEDEX,LOWEXT,
     1   IUPEXT,NW,NRANGE,MISING,CARRYO)
      SHFT=QNEW-QOLD
      QSHIFT = 1. + SHFT/QOLD
!CP   IF(IBUG.EQ.0) GO TO 500
!CP   WRITE(IODBUG,9000)
      IF ( FEWSDEBUG.LT.5 ) GO TO 500
      WRITE(MESSAGESTRING, 9000)
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      DO 210 I = 1,NRCPT
      QSH = Q(I) * QSHIFT
!CP   WRITE(IODBUG,9100) I,H(I),QSH
      WRITE(MESSAGESTRING, 9100) I,H(I),QSH
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
  210 CONTINUE
      GO TO 500

C                        BLEND A GIVEN POINT
C
C        IF BUBBLE SHIFT USED, SEE IF ENOUGH SPACE IN TEMP ARRAY QSHIFT
C
  300 IF(NRCPT.LE.100) GO TO 302
!CP   WRITE(IPR,1006)
      WRITE(MESSAGESTRING, 1006)
      call logfromfortran(WARNING_LEVEL, MESSAGESTRING)
 1006 FORMAT(1H0,10X,'***WARNING*** THE MAXIMUM ALLOWABLE RATING CURVE P
     *OINTS FOR THE BUBBLE SHIFT OPTION (100) HAS BEEN EXCEEDED.  THEREF
     *ORE THE R.C. WILL NOT BE SHIFTED.')
!CP   CALL WARN
      GO TO 500
C
C        SAVE ORIGINAL RATING CURVE
C
CC  302 DO 304 I=1,NRCPT
CC      HORG(I)=H(I)
CC      QORG(I)=Q(I)
CC  304 CONTINUE
CC      NRCPTO=NRCPTS
C
C        ADD AN EXTRA POINT FOR PROPER EXTRAPOLATION
C
  302 IF(ABS(HU-HORG(NRCPT)).GT.0.0001) GO TO 305
      IXTRA=1
      HXTRA=HORG(NRCPT)+1.0
      CALL FHQS1(HXTRA,QXTRA,JCONV,IBUG,NEEDEX,LOWEXT,
     1   IUPEXT,NW,NRANGE,MISING,CARRYO)
C
  305 ISH = 0
      IVAL = 0
      TH1 = 0.
      IF(XLOG.LT.0.1) TH1=0.
      TH2 = 1. - TH1

      CALL FHQS1(HNEW,QOLD,JCONV,IBUG,NEEDEX,LOWEXT,
     1   IUPEXT,NW,NRANGE,MISING,CARRYO)
      QSHIFT=QNEW-QOLD

      DO 400 I = 1,NRCPT

C              WSEL OUTSIDE BLENDING RANGE

      IF (HORG(I) .GT. HD) GO TO 310
      ISH = ISH + 1
      H(ISH) = HORG(I)
      Q(ISH) = QORG(I)
      IF (ABS(HORG(I) - HD) .LE. .0001)  QD = Q(ISH)
      GO TO 400

 310  DO 320  L = 1,3
      IF (L .EQ. 1)  HVAL = HD
      IF (L .EQ. 2)  HVAL = HNEW
      IF (L .EQ. 3)  HVAL = HU
      IF (HVAL .LT. HORG(I) .AND. HVAL .GT. HORG(I - 1)) GO TO 315
      GO TO 320
  315 ISH = ISH + 1
      H(ISH) = HVAL
      RAT = (HVAL - HORG(I - 1)) / (HORG(I) - HORG(I - 1))
      Q(ISH) = QORG(I - 1) + RAT * (QORG(I) - QORG(I - 1))
      IF(L.EQ.1) QD = Q(ISH)
      IF(L.EQ.3) QU = Q(ISH)
      IF(L.NE.2) GO TO 320
      QO = Q(ISH)
      Q(ISH) = QNEW
  320 CONTINUE

C         WSEL ABOVE BLENDING RANGE - KEEP SAME VALUES

      IF (HORG(I) .LT. HU) GO TO 330
      ISH = ISH + 1
      H(ISH) = HORG(I)
      Q(ISH) = QORG(I)
      IF (ABS(HORG(I) - HU) .LE. 0.0001) QU = Q(ISH)
      GO TO 400

C         LINEARLY INTERPOLATE BETWEEN HD & HNEW

  330  IF(HORG(I) .GT. HNEW) GO TO 340
       ISH = ISH + 1
       RAT = (HORG(I) - HD) / (HNEW - HD)
       Q(ISH) = QD + RAT * (QNEW - QD)
       H(ISH) = HORG(I)
       IF(ABS(HORG(I)-HNEW).LE.0.0001) QO=QORG(I-1)+
     * RAT*(QORG(I)-QORG(I-1))
       GO TO 400

C            FIND QS USING AVERAGE SLOPE  (FLOW BETWEEN HNEW & HU)

  340 ISH = ISH + 1
      H(ISH) = HORG(I)
      IF(IVAL.EQ.1) GO TO 360
C
C            FIND UPPER FLOW VALUE IF IT DOESN'T EXIST
C
      IF(QU.GT.0) GO TO 350
      DO 345 K=I,NPT
      IF(H(K).LT.HU) GO TO 345
      RAT = (HU - H(K - 1)) / (H(K) - H(K - 1))
      QU = Q(K - 1) + RAT * (Q(K) - Q(K - 1))
      GO TO 350
  345 CONTINUE

  350 QO1 = QO
      QM1 = QNEW
      H1 = HNEW
      RATM = (QU - QNEW) / (HU - HNEW)
      IVAL = 1

  360 QO2 = QORG(I)
      QM2 = QNEW + RATM * (HORG(I) - HNEW)
      S1 = TH1 / (QO2 - QO1)
      S2 = TH2 / (QM2 - QM1)
      Q(ISH) = Q(ISH - 1) + 1. / (S1 + S2)
      IF (Q(ISH) .GT. QM2)  Q(ISH) = QM2
      QO1 = QO2
      QM1 = QM2
  400 CONTINUE
      NRCPT  = ISH

      IF(IXTRA.EQ.0) GO TO 405
      NRCPT=NRCPT+1
      H(NRCPT)=HXTRA
      Q(NRCPT)=QXTRA

!CP   405 IF(IBUG.EQ.0) GO TO 500
!CP   WRITE(IODBUG,9000)
  405 IF ( FEWSDEBUG.LT.5 ) GO TO 500
      WRITE(MESSAGESTRING, 9000)
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      DO 410 I=1,NRCPT
!CP   WRITE(IODBUG,9100) I,H(I),Q(I)
      WRITE(MESSAGESTRING, 9100) I,H(I),Q(I)
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
  410 CONTINUE
C
!CP   500  IF (ITRACE.GE.3) WRITE(IODBUG,1100)
 500  IF ( FEWSDEBUG.GE.5 ) THEN
         WRITE(MESSAGESTRING, 1100)
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 1100    FORMAT(1H0,17H*** FSHFRC EXITED)
      END IF
      RETURN
      END
