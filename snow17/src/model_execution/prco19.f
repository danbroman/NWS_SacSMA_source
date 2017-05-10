C MEMBER PRCO19
C  (from old member FCPACK19)
C
      SUBROUTINE PRCO19
C.......................................
C     THIS SUBROUTINE PRINTS VALUES IN COMMON/SNCO19/FOR USE WHEN
C        DEBUGGING THE 'SNOW-17 ' OPERATION
C.......................................
C     WRITTEN BY ERIC ANDERSON - HRL   MAY 1980
c
CVK     MODIFIED 4/00 BY V. KOREN TO ADD TWO NEW STATES: SNDPT, SNTMP
c
CEA     MODIFIED 11/05 BY E. ANDERSON - ADDED TAPREV
C.......................................
      REAL NEGHS,LIQW
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/SNCO19/WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,
CVK     1   STORGE,AEADJ,NEXLAG,EXLAG(7)
CEA     1   STORGE,AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP
     1   STORGE,AEADJ,NEXLAG,EXLAG(7),SNDPT,SNTMP,TAPREV
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob90/ohd/ofs/src/fcst_snow/RCS/prco19.f,v $
     . $',                                                             '
     .$Id: prco19.f,v 1.4 2006/10/03 19:38:49 hsu Exp $
     . $' /
C    ===================================================================
C
C.......................................
      WRITE(IODBUG,903) WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,
C
CVK  TWO MORE STATES
CVK     1   STORGE,AEADJ,(EXLAG(I),I=1,NEXLAG)
CEA     1   STORGE,AEADJ,(EXLAG(I),I=1,NEXLAG),SNDPT,SNTMP
CEA  ADDED TAPREV - CHANGED ORDER
     1  STORGE,AEADJ,SNDPT,SNTMP,TAPREV,(EXLAG(I),I=1,NEXLAG)
  903 FORMAT(1H0,9X,2HWE,2X,5HNEGHS,3X,4HLIQW,1X,6HTINDEX,1X,6HACCMAX,
C
CVK  TWO MORE STATES
CVK     15X,2HSB,1X,6HSBAESC,3X,4HSBWS,1X,6HSTORGE,2X,5HAEADJ,5X,5HEXLAG,
CVK     2/6X,F6.1,5F7.1,F7.2,F7.1,2F7.2,5X,7F5.2)
CEA     1  5X,2HSB,1X,6HSBAESC,3X,4HSBWS,1X,6HSTORGE,2X,5HAEADJ,6H SNDPT,
CEA     2  6H SNTMP,5X,5HEXLAG,/6X,F6.1,5F7.1,F7.2,F7.1,2F7.2,
CEA     3  2F6.1,5X,7F5.2)
CEA  ADDED TAPREV
     1  5X,2HSB,1X,6HSBAESC,3X,4HSBWS,1X,6HSTORGE,2X,5HAEADJ,6H SNDPT,
     2  6H SNTMP,7H TAPREV,2X,5HEXLAG,/6X,F6.1,5F7.1,F7.2,F7.1,2F7.2,
     3  2F6.1,F7.1,1X,7F5.2)
C.......................................
      RETURN
      END