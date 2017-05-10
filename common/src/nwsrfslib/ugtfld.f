C MODULE UGTFLD
C-----------------------------------------------------------------------
C
C  ROUTINE TO GET NEXT FIELD USING FREE FORMAT.
C
      SUBROUTINE UGTFLD (IREAD,LMXCRD,MAXCRD,ICARDS,
     *   NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,
     *   ISTAT)
C
C  INPUT VARIABLES:
C     NFLD   - FIELD CONTROL
C               0 = READ CARD
C              -1 = DO NOT READ CARD OR GET FIELD FROM ARRAY
C     ISTRT  - FIELD CONTROL
C              >0 = PROCESS NEXT FIELD
C               0 = FIRST TIME UFIELD CALLED
C              -1 = RE-PROCESS CURRENT FIELD
C              -2 = DO NOT READ NEW CARD AFTER LAST FIELD PROCESSED
C              -3 = READ NEXT CARD ONLY IF '&' FOUND
C     LCHAR  - LENGTH OF VARIABLE CHAR
C              >0=LENGTH IS NUMBER OF 4-CHARACTER WORDS
C              <0=LENGTH IS NUMBER OF 1-CHARACTER WORDS
C
C  OUTPUT VARIABLES:
C     NFLD   - NUMBER OF FIELD ON CARD
C              -1 = END OF INPUT
C     ISTRT  - STARTING LOCATION OF FIELD ON CARD
C              -2 = NO MORE NON-COMMENT INPUT FOUND. NEW CARD NOT READ.
C                (WILL ONLY BE THIS VALUE WHEN ISTRT=-2 ON INPUT)
C     LENGTH - LENGTH OF FIELD
C     ITYPE  - FIELD TYPE
C              0 = INTEGER
C              1 = REAL
C              2 = CHARACTER
C     NREP   - REPEAT FACTOR
C              -1 = NO REPEAT FACTOR FOUND
C     INTEGR - VALUE OF FIELD AS INTEGER (FILLED IF ITYPE=0)
C     REAL   - VALUE OF FIELD AS REAL (FILLED IF ITYPE=0 OR 1)
C     CHAR   - VALUE OF FIELD AS CHARACTER (FILLED WITHOUT QUOTES IF
C              ITYPE=0, 1 OR 2 AND WITHOUT N* IF REPEAT FACTOR
C              USED)
C     LLPAR  - LOCATION IN FIELD OF LEFT PARENTHESIS
C     LRPAR  - LOCATION IN FIELD OF RIGHT PARENTHESIS
C     LASK   - LOCATION IN FIELD OF ASTERISK (*)
C     LATSGN - LOCATION IN FIELD OF AT SIGN (@)
C     LAMPS  - LOCATION IN FIELD OF AMPERSAND (&)
C     LEQUAL - LOCATION IN FIELD OF EQUAL SIGN (=)
C     ISTAT  - STATUS CODE
C              1 = NULL FIELD
C              2 = NOT ENOUGH ROOM IN CHAR TO PACK LENGTH
C                  CHARACTERS
C              3 = END OF CARD INPUT (NFLD SET TO -1)
C              4 = INVALID VALUE FOR REPEAT FACTOR
C              5 = VALID DATE FIELD (SET ONLY IF ICKDAT IN /UFREEX/
C                  SET TO 1)
C              6 = ENDING QUOTE NOT FOUND FOR CHARACTER STRING
C              7 = NON-COMMENT CHARACTERS FOUND AFTER LAST COLUMN TO BE
C                  PROCESSED
C
      CHARACTER*1 QUOTE/''''/
      CHARACTER*4 CHAR(1),IFRBUF
      CHARACTER*80 ICARD,ICARDS(1),STRNG
C
      DIMENSION ITBUF(7)      
C
      COMMON /UIOX/ LP,ICD,LPD,LPE,ICDPUN,LSYS
      COMMON /UCMDBX/ ICMPRU,ICMTRC,ICMDBG
      COMMON /UFREEX/ NFIELD,
     *   IFTYPE(50),IFCNT(50),IFSTRT(50),IFSTOP(50),
     *   ICDBUF,ICDSTR,ICDSTP,NRDCRD,IPRBLN,IPRCRD,MAXFLD,IPRMPT,
     *   NPUCRD,PFCOMA,IPRTCD,ICKDAT,ICDTMP,IOPREP,ICDSPC,
     *   ICDQTE,NFLDNQ,CMTCHR
      LOGICAL PFCOMA
      CHARACTER*1 CMTCHR
      CHARACTER*100 ICDBUF
      COMMON /UFREEI/ UFDUM1,
     *   UFDUM2(40),UFDUM3(40),UFDUM4(40),UFDUM5(40),
     *   IFRBUF(80),UFDUM6
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/ugtfld.f,v $
     . $',                                                             '
     .$Id: ugtfld.f,v 1.4 1999/04/22 13:41:52 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,200)
         ENDIF
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' NFLD=',NFLD,
     *      ' ISTRT=',ISTRT,
     *      ' LCHAR=',LCHAR,
     *      ' '
         ENDIF
C
      ISTAT=0
C
      MICARD=LEN(ICARD)
      MSTRNG=LEN(STRNG)
C
      NUMERR=0
      NUMWRN=0
      NREP=-1
      LENGTH=0
      ITYPE=-1
      IF (LCHAR.GT.0) MCHAR=LCHAR*4
      IF (LCHAR.LT.0) MCHAR=-LCHAR
      CALL UREPET (' ',CHAR,MCHAR)
      INTEGR=0
      REAL=0.
      LLPAR=0
      LRPAR=0
      LASK=0
      LATSGN=0
      LAMPS=0
      LEQUAL=0
C
      IPROC=ISTRT
      ISTRT=-2
C
C  CHECK IF NEW CARD NOT TO BE READ WHEN LAST FIELD PROCESSED
      NOREAD=0
      IF (IPROC.LT.-1) NOREAD=1
C
C  CHECK IF FIELD TO BE REREAD
      KPRCRD=IPRCRD
      IF (IPROC.EQ.-1) KPRCRD=0
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' IPROC=',IPROC,
     *      ' KPRCRD=',KPRCRD,
     *      ' '
         ENDIF
C
C  CHECK IF FIRST CARD TO BE READ
      IF (NFLD.EQ.0) GO TO 20
C
C  CHECK IF FIELD TO BE REREAD
      IF (IPROC.EQ.-1) GO TO 100
C
C  CHECK IF NO CARDS TO BE READ OR OBTAINED FROM ARRAY
      IF (NFLD.EQ.-1) THEN
         NFLD=1
         GO TO 90
         ENDIF
C
C  CHECK IF ALL FIELD ON CARD PROCESSED
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' NFIELD=',NFIELD,
     *      ' NOREAD=',NOREAD,
     *      ' '
         ENDIF
      IF (NFLD.NE.NFIELD) THEN
         NFLD=NFLD+1
         GO TO 100
         ENDIF
C
C  READ CARD
10    IF (NOREAD.EQ.1) GO TO 190
20    IF (NRDCRD.NE.LMXCRD) THEN
         NFLD=1
         IF (IPRMPT.EQ.1) THEN
            CALL ULINE (LP,1)
            WRITE (LP,210)
            ENDIF
         IPRTCD=0
         IF (IREAD.EQ.1) THEN
            READ (ICD,220,END=30) ICARD
            ENDIF
         NRDCRD=NRDCRD+1
         IF (IREAD.EQ.0) THEN
            CALL SUBSTR (ICARDS(NRDCRD),1,MICARD,ICARD,1)
            ENDIF
         GO TO 40
         ENDIF
C
C  END OF CARD INPUT
30    NFLD=-1
      ISTAT=3
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,230) ICD
         ENDIF
      GO TO 190
C
C  MOVE CARD INTO BUFFER
40    DO 50 I=1,MICARD
         CALL SUBSTR (ICARD,I,1,ICDBUF(I:I),-1)
50       CONTINUE
C
      IF (ICARD(1:ICDSTP).EQ.' ') GO TO 90
C
      IF (ICDSTP.LT.MICARD) THEN
C     CHECK IF COMMENT CHARACTER FOUND BEFORE LAST COLUMN TO BE
C     PROCESSED
         ICMTCOL=0
         DO 60 I=ICDSTP,1,-1
            IF (ICARD(I:I).EQ.CMTCHR) THEN
               IF (I.GT.1.AND.ICARD(I-1:I-1).NE.' ') GO TO 60
               ICMTCOL=I
               GO TO 70
               ENDIF
60          CONTINUE
70       IF (ICMTCOL.EQ.0) THEN
C        CHECK FOR NON-COMMENT CHARACTERS AFTER LAST COLUMN TO BE
C        PROCESSED
            DO 80 I=MICARD,ICDSTP+1,-1
               IF (ICARD(I:I).NE.' ') THEN
                  ISTAT=7
                  GO TO 90
                  ENDIF
80             CONTINUE
            ENDIF
         ENDIF
C
C  CHECK IF TO PRINT CARD
90    IF (KPRCRD.EQ.-1) CALL UPRCRD (LP)
C
C  FIND FIELDS ON CARD
      CALL UFFIND (ICDSTR,ICDSTP,IERR)
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) ' NFIELD=',NFIELD
         ENDIF
C
C  CHECK IF NO FIELDS FOUND
      IF (NFIELD.GT.0) GO TO 100
C
C  BLANK CARD
      IF (IPRBLN.EQ.1) CALL UPRCRD (LP)
      GO TO 10
C
C  FIND LENGTH OF FIELD
100   IS=IFSTRT(NFLD)
      IE=IFSTOP(NFLD)
      ISTRT=IS
      LENGTH=IE-IS+1
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,240) LENGTH,ICDBUF(IS:IE)
         ENDIF
      IF (LENGTH.GT.0) GO TO 120
C
C  NULL FIELD
110   ISTAT=1
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,260) NFLD
         ENDIF
      GO TO 190
C
C  CHECK IF FIELD HAS UNPAIRED QUOTES
120   IF (NFLD.EQ.NFLDNQ) THEN
         ISTAT=6
         IF (ICDQTE.EQ.1)
     *      CALL UFLDST (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *         LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,
     *         ISTAT,NUMERR,NUMWRN)
         GO TO 190
         ENDIF
C
      IBEG=IS
C
C  CHECK IF FIELD IS ENCLOSED IN QUOTES
130   IF (ICDBUF(IBEG:IBEG).EQ.QUOTE) THEN
         IF (ICDBUF(IE:IE).EQ.QUOTE) THEN
            IF (IE-IS.EQ.1) GO TO 110
            ENDIF
         CALL UFIBUF (QUOTE,IS,IE,LQUOTE)
         IS=LQUOTE+1
140      ISX=LQUOTE+1
         IF (ISX.GT.IE) GO TO 160
            CALL UFIBUF (QUOTE,ISX,IE,LQUOTE)
            IF (LQUOTE.EQ.0) GO TO 160
C        CHECK FOR DOUBLE QUOTE
            ICOL=LQUOTE+1
            CALL UFIBUF (QUOTE,ICOL,IE,LFOUND)
            IF (LFOUND.NE.ICOL) GO TO 160
               ICOL=ICOL+1
               DO 150 I=ICOL,IE
                  ICDBUF(I-1:I-1)=ICDBUF(I:I)
150               CONTINUE
               ICDBUF(IE:IE)=' '
               IE=IE-1
               LQUOTE=LFOUND-1
               IF (LQUOTE.EQ.IE) GO TO 160
               GO TO 140
160      IF (LQUOTE.GT.0) IE=LQUOTE-1
         LENGTH=IE-IS+1
         ENDIF
C
C  GET CHARACTER STRING
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' LENGTH=',LENGTH,
     *      ' MSTRNG=',MSTRNG,
     *      ' '
         ENDIF
      CALL UREPET (' ',STRNG,MSTRNG)
      IF (LENGTH.LT.1) THEN
         ISTAT=2
         GO TO 190
         ENDIF
      IEND=IS+LENGTH-1
      IF (LENGTH.GT.MSTRNG) THEN
         ISTAT=2
         GO TO 190
         ELSE
            IPOS=0
            DO 170 I=IS,IEND
               IPOS=IPOS+1
               CALL SUBSTR (ICDBUF,I,1,STRNG,-IPOS)
170            CONTINUE
            IF (ICMDBG.GT.0) THEN
               CALL ULINE (ICMPRU,1)
               WRITE (ICMPRU,*) ' STRNG=',STRNG
               ENDIF
         ENDIF
      IF (NREP.GT.0) GO TO 180
C
C  CHECK FOR COMMENT
      CALL UINDEX (STRNG,LENGTH,CMTCHR,LEN(CMTCHR),LDOLR)
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) ' LDOLR=',LDOLR
         ENDIF
      IF (LDOLR.EQ.1) THEN
         IF (NFLD.EQ.1.AND.IPRBLN.EQ.1) THEN
            CALL UPRCRD (LP)
            ENDIF
         IF (NOREAD.EQ.0) GO TO 10
         ISTRT=-2
         GO TO 190
         ENDIF
C
C  CHECK FOR AMPERSAND
      CALL UINDEX (STRNG,LENGTH,'&',1,LAMPS)
      IF (LAMPS.EQ.1.AND.LENGTH.EQ.1) THEN
         IF (IPROC.NE.-2) GO TO 20
         GO TO 190
         ENDIF
C
C  CHECK FOR ATSIGN
      CALL UINDEX (STRNG,LENGTH,'@',1,LATSGN)
      IF (LATSGN.EQ.1) THEN
         IF (ICMDBG.GT.0) CALL UPRCRD (LP)
         ENDIF
C
C  PRINT CARD
      IF (NFLD.EQ.1.AND.KPRCRD.EQ.1) CALL UPRCRD (LP)
C
C  CHECK FOR LEFT AND RIGHT PARENTHESIS
      CALL UINDEX (STRNG,LENGTH,'(',1,LLPAR)
      CALL UINDEX (STRNG,LENGTH,')',1,LRPAR)
C
C  CHECK FOR EQUAL SIGN
      CALL UINDEX (STRNG,LENGTH,'=',1,LEQUAL)
C
      IPRERR=0
C
      IBEG=1
      IEND=LENGTH
      NCHAR=IEND-IBEG+1
C
C  CHECK FOR ASTERISK
      CALL UINDEX (STRNG,LENGTH,'*',1,LASK)
      IF (LASK.EQ.0) GO TO 180
C
C  CHECK FOR REPEAT FACTOR
      IF (LLPAR.GT.0) IBEG=IBEG+LLPAR
      IEND=LASK-1
      IF (IEND-IBEG.LT.0) THEN
         NREP=1
         IF (ICMDBG.GT.0) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*)
     *         ' NFLD=',NFLD,
     *         ' NREP=',NREP,
     *         ' '
            ENDIF
         GO TO 180
         ENDIF
      NCHAR=IEND-IBEG+1
      CALL UFA2I (STRNG,IBEG,NCHAR,IVALUE,IPRERR,LP,IERR)
      IF (IERR.GT.0) THEN
         IF (ICMDBG.GT.0) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,250) NFLD,STRNG
            ENDIF
         ISTAT=4
         GO TO 180
         ENDIF
      NREP=IVALUE
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*)
     *      ' LASK=',LASK,
     *      ' NREP=',NREP,
     *      ' '
         ENDIF
      IF (IBEG.EQ.1) THEN
         IBEG=IS+LASK
         GO TO 130
         ENDIF
C
180   IBEG=1
C
C  CHECK IF REPEAT FACTOR SPECIFIED
      IF (NREP.GT.0.AND.IOPREP.EQ.1) THEN
         IBEG=LASK+1
         IS=IS+LASK
         ENDIF
C
C  STORE AS CHARACTER
      NCHAR=LENSTR(STRNG)
      IF (NCHAR.GT.MCHAR) THEN
         IF (ISTAT.EQ.0) ISTAT=2
         ELSE
            CALL SUBSTR (STRNG,1,NCHAR,CHAR,1)
            ITYPE=2
            IF (ICMDBG.GT.0) THEN
               CALL ULINE (ICMPRU,1)
               WRITE (ICMPRU,290) (CHAR(I),I=1,LCHAR)
               ENDIF
         ENDIF
C
C  STORE AS INTEGER
      CALL UFA2I (STRNG,IBEG,NCHAR,IVALUE,IPRERR,LP,IERR)
      IF (IERR.EQ.0) THEN
         INTEGR=IVALUE
         ITYPE=0
         ENDIF
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,270) INTEGR
         ENDIF
C
C  STORE VALUE AS REAL
      NDEC=0
      CALL UFA2F (STRNG,IBEG,NCHAR,NDEC,RVALUE,IPRERR,LP,IERR)
      IF (IERR.EQ.0) THEN
         REAL=RVALUE
         IF (ITYPE.NE.0) ITYPE=1
         ENDIF
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,280) REAL
         ENDIF
C
C  STORE FIELD TYPE IN COMMON BLOCK VARIABLE
      IFTYPE(NFLD)=ITYPE
C
C  CHECK IF DATE TO BE CHECKED IF VALID
      IF (ICKDAT.EQ.0.OR.ISTAT.GT.0) GO TO 190
C
C  CHECK FOR VALID DATE
      CALL UMEMST ('    ',IFRBUF,80)
      DO 185 I=1,LENGTH
         CALL SUBSTR (ICDBUF(ISTRT:ISTRT),I,1,IFRBUF(I),1)
185      CONTINUE         
      CALL HCKDAT (1,LENGTH,ITBUF,IERR)
      IF (IERR.EQ.0) ISTAT=5
C
190   IF (ICMDBG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
         ENDIF
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,300) ISTAT
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
200   FORMAT (' *** ENTER UGTFLD')
210   FORMAT (' ?')
220   FORMAT (A80)
230   FORMAT (' END FILE ENCOUNTERED ON UNIT ',I2)
240   FORMAT (' LENGTH=',I3,3X,'FIELD=',80A1)
250   FORMAT (' FIELD ',I2,' HAS AN INVALID REPEAT FACTOR : ',A)
260   FORMAT (' FIELD ',I2,' IS NULL')
270   FORMAT (' INTEGR=',I6)
280   FORMAT (' REAL=',G15.2)
290   FORMAT (' CHAR=',20A4)
300   FORMAT (' *** EXIT UGTFLD - ISTAT=',I2)
C
      END
