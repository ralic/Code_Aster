      SUBROUTINE JXALLM ( IADZON, ISZON , LISZON , JISZON )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 27/06/2011   AUTEUR LEFEBVRE J-P.LEFEBVRE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRP_6 CRS_505
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             ISZON(*) , LISZON , JISZON
C     ==================================================================
      INTEGER          IADA
      COMMON /IALLJE/  IADA
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE 
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE 
C     ------------------------------------------------------------------
      INTEGER          VALLOC
C     ------------------------------------------------------------------
C             ROUTINE AVEC APPEL SYSTEME  LOC
C
C             HPALLOC
C
C     ==================================================================
      JISZON = 0
C     ------------------------------------------------------------------
      IERR = 0
      IF (IADZON .EQ. 0 ) THEN
         CALL  HPALLOC ( IADA , LISZON , IERR , 0 )
      ELSE
         IADA = IADZON
      ENDIF
      NBDYN = NBDYN+1
      IF ( IERR .EQ. 0 ) THEN
         VALLOC = LOC(ISZON)
         JISZON = (IADA - VALLOC)/LOISEM()
         IMAX = ISMAEM()
         DO 10 I = 1 , LISZON
            ISZON(JISZON+I) = IMAX
   10    CONTINUE
      ELSE
         IF ( IERR .EQ. -1 ) THEN
            CALL U2MESI('F','JEVEUX_49',1,LISZON)
         ELSE IF ( IERR .EQ. -2 ) THEN
            CALL U2MESS('F','JEVEUX_50')
         ENDIF
      ENDIF
C     ==================================================================
      END
