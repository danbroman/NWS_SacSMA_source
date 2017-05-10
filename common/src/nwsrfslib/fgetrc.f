C MODULE FGETRC
C----------------------------------------------------------------------
C
C  ROUTINE TO GET A RATING CURVE DEFINTION
C
      SUBROUTINE FGETRC (GRCID,IERR)
C
C  SUBROUTINE FGETRC RETRIEVES A RATING CURVE FROM FILE FOR BOTH THE
C  OPERATIONAL PROGRAM AND AN MCP RUN. FOR THE OPER PROGRAM, THE
C  RATING CURVE HAS ONLY ONE DEFINITION AND IS FOUND ON A PERMANENT
C  FILE. FOR AN MCP RUN, A RATING CURVE CAN HAVE MULTIPLE DEFINITIONS
C  OVER TIME. THESE DEFINITIONS ARE STORED ON A TEMPORARY FILE AS
C  DEFINED WITH INPUT TO EACH MCP RUN.
C
C  ARGUMENT LIST:
C        GRCID - RATING CURVE IDENTIFIER TO BE GOTTEN
C        IERR  - ERROR RETURN CODE:
C                0 = NO ERROR
C                1 = ERROR
C
C....................................................................
C
C  ROUTINE ORIGINALLY WRITTEN BY - JOE OSTROWSKI - HRL - 8/1980
C
      CHARACTER*8 OLDOPN
C
      DIMENSION GRCID(2),ZRCBUF(108)
      EQUIVALENCE (RTCVID(1),ZRCBUF(1))
C
!CP   INCLUDE 'ionum'
!CP   INCLUDE 'commonb/fdbug'
      INCLUDE 'flogm'
      INCLUDE 'errdat'
      INCLUDE 'fprog'
      INCLUDE 'frcfil'
      INCLUDE 'fratng'
C jgg added to fix MR 1653 - 5/31/02
      INCLUDE 'rcnew'
C jgg      
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rc/RCS/fgetrc.f,v $
     . $',                                                             '
     .$Id: fgetrc.f,v 1.4 2002/10/10 13:32:23 xfan Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=-1
C     CALL FSTWHR ('FGETRC  ',IOPNUM,OLDOPN,IOLDOP)
C
!CP   IF (ITRACE.GE.1) WRITE (IODBUG,*) 'ENTER FGETRC'
      IF ( FEWSDEBUG.GE.5 ) THEN
         WRITE(MESSAGESTRING, '(A12)') 'ENTER FGETRC'
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      END IF
C
      IBUG=IFBUG('RTCV')

      IERR=0
C
C  CHECK IF RATING CURVE IS ALREADY IN /FRATNG/
      IF (GRCID(1).EQ.RTCVID(1).AND.GRCID(2).EQ.RTCVID(2)) GO TO 70
C LC commented out block below; we will treat rating curves
C like an MCP3 run; but don't want to use MAINUM right now
C for FEWS never read ratings from fs5files which FCRDRC does
C we will use the MCP3 approach; read ratings from a TEMP file
C created by PIN
C
C  CHECK IF PROGRAM IS MCP
      GO TO 40
C      IF (MAINUM.EQ.3) GO TO 40
C
C      CALL FINDRC (GRCID,IRCREC,IRETRN)
C      IF (IRETRN.EQ.1) GO TO 30
C         WRITE (IPR,20) GRCID
C20    FORMAT('0**ERROR** RATING CURVE ',2A4,' NOT FOUND.')
C         IERR = 1
C         CALL ERROR
C         GO TO 70
C
C  FILL /FRATNG/ WITH DEFINITION
C jgg First clear the rcnew mod point count, because we're
C jgg  reading in a different RC - fixes HSD bug 20-32 MR 1653 - 5/31/02
 
C jgg 30    CALL FCRDRC (IRCREC,GRCID,KERROR)
C30    NRCPMD = 0
C      CALL FCRDRC (IRCREC,GRCID,KERROR)
C      IF (KERROR.EQ.0)GO TO 70
C         CALL ERROR
C         IERR = 1
C         GO TO 70
C
C  THIS SECTION IS FOR USE IN AN MCP RUN.
C  FIND THE RATING CURVE IN THE LIST OF DEFINED CURVES.
C  LOCK(I) IS THE LOCATION OF THE CURRENT RATING CURVE DEFINITION
C  BEING USED.
40    DO 50 I=1,NDEF
      IF (GRCID(1).NE.RCNAME(1,I).OR.GRCID(2).NE.RCNAME(2,I)) GO TO 50
         ICRNT = LOCK(I)
         GO TO 60
50       CONTINUE
!CP   WRITE (IPR,20) GRCID
      WRITE(MESSAGESTRING, 20) GRCID
      call logfromfortran(WARNING_LEVEL, MESSAGESTRING)
!CP   CALL ERROR
      IERR = 1
      GO TO 70
C
C  READ CURVE FROM SCRATCH FILE
60    READ (IRC,REC=ICRNT) ZRCBUF
C
!CP   70 IF (ITRACE.GE.1) WRITE (IODBUG,*) 'EXIT FGETRC'
70    IF ( FEWSDEBUG.GE.5 ) THEN
         WRITE(MESSAGESTRING, '(A11)') 'EXIT FGETRC'
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      END IF
C
C      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
20    FORMAT('0**ERROR** RATING CURVE ',2A4,' NOT FOUND.')
      RETURN
C
      END
