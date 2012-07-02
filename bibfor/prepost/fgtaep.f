      SUBROUTINE FGTAEP(NOMMAT,NOMFO1,NOMNAP,NBCYCL,EPSMIN,EPSMAX,DOM)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)     NOMMAT,NOMFO1,NOMNAP
      REAL*8                          EPSMIN(*),EPSMAX(*)
      REAL*8                   DOM(*)
      INTEGER                  NBCYCL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     -----------------------------------------------------------------
C     CALCUL DU DOMMAGE ELEMENTAIRE PAR TAHERI_MANSON_COFFIN
C     ------------------------------------------------------------------
C IN  NOMMAT : K   : NOM DU MATERIAU
C IN  NOMFO1 : K   : NOM DE LA FONCTION
C IN  NOMNAP : K   : NOM DE LA NAPPE
C IN  NBCYCL : I   : NOMBRE DE CYCLES
C IN  EPSMIN : R   : DEFORMATIONS MINIMALES DES CYCLES
C IN  EPSMAX : R   : DEFORMATIONS MAXIMALES DES CYCLES
C OUT DOM    : R   : VALEURS DES DOMMAGES ELEMENTAIRES
C     ------------------------------------------------------------------
C
      INTEGER ICODRE
      CHARACTER*8  NOMRES,NOMPAR,NOMP(2)
      CHARACTER*10 PHENO
      REAL*8       NRUPT,DELTA,DSIGM,DEPSI,EPMAX,VALP(2)
C-----------------------------------------------------------------------
      INTEGER I ,IER ,NBPAR 
      REAL*8 ZERO 
C-----------------------------------------------------------------------
      DATA ZERO  /1.D-13/
C
      CALL JEMARQ()
C
      EPMAX = 0.D0
      NOMRES = 'MANSON_C'
      NBPAR     = 1
      PHENO     = 'FATIGUE   '
      NOMPAR    = 'EPSI    '
      DO 10 I=1,NBCYCL
        DELTA = (ABS(EPSMAX(I)-EPSMIN(I)))/2.D0
        IF(DELTA.GT.EPMAX-ZERO) THEN
           EPMAX = DELTA
           CALL RCVALE(NOMMAT,PHENO,NBPAR,NOMPAR,DELTA,1,NOMRES,
     &                                           NRUPT,ICODRE,2)
           DOM(I) = 1.D0/NRUPT
        ELSE
           NOMP(1) = 'X'
           NOMP(2) = 'EPSI'
           VALP(1) = EPMAX
           VALP(2) = DELTA
           CALL FOINTE('F ',NOMNAP,2,NOMP,VALP,DSIGM,IER)
           NOMP(2) = 'SIGM'
           CALL FOINTE('F ',NOMFO1,1,NOMP(2),DSIGM,DEPSI,IER)
           CALL RCVALE(NOMMAT,PHENO,NBPAR,NOMPAR,DEPSI,1,NOMRES,
     &                                          NRUPT,ICODRE,2)
           DOM(I) = 1.D0/NRUPT
        ENDIF
 10   CONTINUE
C
      CALL JEDEMA()
      END
