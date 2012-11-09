      SUBROUTINE FGDOMA(NOMMAT,NBCYCL,EPSMIN,EPSMAX,DOM)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*(*)     NOMMAT
      REAL*8                          EPSMIN(*),EPSMAX(*)
      REAL*8                  DOM(*)
      INTEGER                  NBCYCL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C     CALCUL DU DOMMAGE ELEMENTAIRE PAR INTERPOLATION SUR
C     UNE COURBE DE MANSON_COFFIN DONNEE POINT PAR POINT
C     ------------------------------------------------------------------
C IN  NOMMAT : K   : NOM DU MATERIAU
C IN  NBCYCL : I   : NOMBRE DE CYCLES
C IN  EPSMIN : R   : DEFORMATIONS MINIMALES DES CYCLES
C IN  EPSMAX : R   : DEFORMATIONS MAXIMALES DES CYCLES
C OUT DOM    : R   : VALEURS DES DOMMAGES ELEMENTAIRES
C     ------------------------------------------------------------------
C
      INTEGER ICODRE
      CHARACTER*8  NOMRES,NOMPAR
      CHARACTER*10 PHENO
      REAL*8       NRUPT,DELTA
C
C-----------------------------------------------------------------------
      INTEGER I ,NBPAR
C-----------------------------------------------------------------------
      CALL JEMARQ()
      NOMRES = 'MANSON_C '
      NBPAR     = 1
      PHENO     = 'FATIGUE   '
      NOMPAR    = 'EPSI    '
C
      DO 10 I=1,NBCYCL
         DELTA = (ABS(EPSMAX(I)-EPSMIN(I)))/2.D0
         CALL RCVALE(NOMMAT,PHENO,NBPAR,NOMPAR,DELTA,1,NOMRES,
     &                                         NRUPT,ICODRE,2)
         DOM(I) = 1.D0/NRUPT
 10   CONTINUE
C
      CALL JEDEMA()
      END
