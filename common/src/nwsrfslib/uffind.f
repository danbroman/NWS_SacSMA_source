C MODULE UFFIND
C-----------------------------------------------------------------------
C
C
      SUBROUTINE UFFIND (ISTRT,ISTOP,ISTAT)
C
C  THIS ROUTINE TO FIND FIELDS IN RECORD USING FREE FORMAT INPUT RULES.
C
      LOGICAL CFCOMA
C
      COMMON /UIOX/ LP,ICD,LPD,LPE,ICDPUN,LSYS
      COMMON /UCMDBX/ ICMPRU,ICMTRC,ICMDBG
      COMMON /UFREEX/ NFIELD,
     *   IFTYPE(50),IFCNT(50),IFSTRT(50),IFSTOP(50),
     *   ICDBUF,ICDSTR,ICDSTP,NRDCRD,IPRBLN,IPRCRD,MAXFLD,IPRMPT,
     *   NPUCRD,PFCOMA,IPRTCD,ICKDAT,ICDTMP,IOPREP,ICDSPC,
     *   ICDQTE,NFLDNQ,CMTCHR
      INCLUDE 'flogm'
      LOGICAL PFCOMA
      CHARACTER*1 CMTCHR
      CHARACTER*100 ICDBUF
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/uffind.f,v $
     . $',                                                             '
     .$Id: uffind.f,v 1.2 1998/07/06 13:21:30 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA IQUOTE/1H'/
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,120) ISTRT,ISTOP
         ENDIF
C
      ISTAT=0
C
      IERR=0
C
C  CHECK VALUE OF FIRST COLUMN TO BE PROCESSED
      IF (ISTRT.LE.0) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,125) ISTRT
         IERR=1
         ENDIF
C
C  CHECK VALUE OF LAST COLUMN TO BE PROCESSED
      IF (ISTOP.LE.0) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,127) ISTOP
         IERR=1
         ENDIF
C
      IF (IERR.GT.0) GO TO 110
C
C  CHECK IF LAST FIELD TO BE PROCESSED IS GREATER THAN FIRST
      IF (ISTOP.LT.ISTRT) THEN
         CALL UEROR (LP,2,-1)
         WRITE (LP,129) ISTOP,ISTRT
         GO TO 110
         ENDIF
C
      NFLDNQ=0
C
      II=ISTRT
      NFIELD=0
C
      DO 10 I=1,MAXFLD
         IFTYPE(I)=-1
         IFSTRT(I)=-1
         IFSTOP(I)=-1
10       CONTINUE
C
C  FIND FIRST NON-BLANK CHARACTER
20    DO 25 IBEGIN=II,ISTOP
         IF (ICDBUF(IBEGIN:IBEGIN).NE.' ') GO TO 27
25       CONTINUE
C
C  PAST LAST COLUMN TO BE PROCESSED
      GO TO 110
C
C  CHECK IF FIELD BEGINS WITH A COMMA
27    CFCOMA=.FALSE.
      IF (ICDBUF(IBEGIN:IBEGIN).EQ.',') CFCOMA=.TRUE.
C
C  CHECK IF CURRENT AND PREVIOUS FIELDS WERE NOT COMMAS
      IF (.NOT.PFCOMA.AND..NOT.CFCOMA) GO TO 50
C
C  CHECK IF PREVIOUS FIELD WAS NOT A COMMA AND CURRENT FIELD IS A COMMA
C  IF TRUE, INTERVENING BLANK IGNORED
      IF (.NOT.PFCOMA.AND.CFCOMA) GO TO 30
         GO TO 40
30       II=IBEGIN+1
         PFCOMA=.TRUE.
         GO TO 20
C
C  CHECK IF PREVIONS FIELD WAS A COMMA AND CURRENT FIELD IS NOT A COMMA
C  IF TRUE, VALID FIELD
40    IF (PFCOMA.AND..NOT.CFCOMA) GO TO 50
C
C  CHECK IF PREVIOUS AND CURRENT FIELDS ARE COMMAS
C  IF TRUE, NULL FIELD
      IEND=IBEGIN
      IF (PFCOMA.AND.CFCOMA) GO TO 90
C
C  FIND FIRST BLANK OR COMMA
50    DO 55 IEND=IBEGIN,ISTOP
         IF (ICDBUF(IEND:IEND).EQ.',') GO TO 57
         IF (ICDBUF(IEND:IEND).EQ.' ') GO TO 57
55       CONTINUE
      IEND=ISTOP+1
C
C  CHECK FOR QUOTE
57    CALL UFIBUF (IQUOTE,IBEGIN,IEND,LQUOTE)
      IF (LQUOTE.EQ.0) GO TO 90
C
C  QUOTED STRING FOUND - FIND END
60    ICOL=LQUOTE+1
      CALL UFIBUF (IQUOTE,ICOL,ISTOP,LQUOTE)
      IF (LQUOTE.GT.0) GO TO 70
         IEND=ISTOP+1
         ISTAT=1
         GO TO 90
C
C  CHECK FOR DOUBLE QUOTE
70    ICOL=LQUOTE+1
      CALL UFIBUF (IQUOTE,ICOL,ISTOP,LFOUND)
      IF (LFOUND.NE.ICOL) GO TO 80
         LQUOTE=LFOUND
         GO TO 60
C
80    IEND=LQUOTE+1
C
C  SET BEGINNING AND END OF FIELD
90    IF (ICDBUF(IEND:IEND).EQ.',') CFCOMA=.TRUE.
      IEND=IEND-1
      NFIELD=NFIELD+1
      IFSTOP(NFIELD)=IEND
      IFSTRT(NFIELD)=IBEGIN
      PFCOMA=CFCOMA
      IF (ISTAT.EQ.1) NFLDNQ=NFIELD
      IF (FEWSDEBUG.GE.6) THEN
CSR         CALL ULINE (ICMPRU,1)
         WRITE (MESSAGESTRING,130) NFIELD,IBEGIN,IEND,NFLDNQ
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      ENDIF
C
      II=IEND+2
      IF (II.GT.ISTOP) GO TO 110
      GO TO 20
C
110   IF (FEWSDEBUG.GE.6) THEN
CSR         CALL ULINE (ICMPRU,1)
         WRITE (MESSAGESTRING,140) ISTAT
	 call logfromfortran(DEBUG_LEVEL,MESSAGESTRING)
      ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
120   FORMAT (' *** ENTER UFFIND : ISTRT=',I2,3X,'ISTOP=',I3)
125   FORMAT ('+*** ERROR - IN UFFIND - VALUE OF FIRST COLUMN TO BE ',
     *   'PROCESSED (',I3,') IS NOT GREATER THAN ZERO.')
127   FORMAT ('+*** ERROR - IN UFFIND - VALUE OF LAST COLUMN TO BE ',
     *   'PROCESSED (',I3,') IS NOT GREATER THAN ZERO.')
129   FORMAT ('+*** ERROR - IN UFFIND - VALUE OF LAST COLUMN TO BE ',
     *   'PROCESSED (',I3,') IS NOT GREATER THAN FIRST (',I3,').')
130   FORMAT (' NFIELD=',I2,3X,'IBEGIN=',I2,3X,'IEND=',I2,3X,
     *   'NFLDNQ=',I2)
140   FORMAT (' *** EXIT UFFIND - ISTAT=',I2)
C
      END
