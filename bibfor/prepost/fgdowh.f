      SUBROUTINE FGDOWH(NOMMAT,NBCYCL,SIGMIN,SIGMAX,LKE,RKE,
     &                  LHAIGH,RCORR,DOM)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)     NOMMAT
      REAL*8                          SIGMIN(*),SIGMAX(*)
      REAL*8                   RCORR(*),DOM(*),         RKE(*)
      INTEGER                  NBCYCL
      LOGICAL           LHAIGH,                     LKE
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
C     CALCUL DU DOMMAGE ELEMENTAIRE PAR INTERPOLATION SUR
C     UNE COURBE DE WOHLER DONNEE POINT PAR POINT
C     ------------------------------------------------------------------
C IN  NOMMAT : K   : NOM DU MATERIAU
C IN  NBCYCL : I   : NOMBRE DE CYCLES
C IN  SIGMIN : R   : CONTRAINTES MINIMALES DES CYCLES
C IN  SIGMAX : R   : CONTRAINTES MAXIMALES DES CYCLES
C IN  LKE    : L   : INDIQUE LA PRISE EN COMPTE DE KE
C IN  RKE    : R   : VALEURS DE KE
C IN  LHAIGH : L   : PRISE EN COMPTE D'UNE CORRECTION DE HAIGH
C IN  RCORR  : R   : VALEURS DE LA CORRECTION DE HAIGH
C OUT DOM    : R   : VALEURS DES DOMMAGES ELEMENTAIRES
C     ------------------------------------------------------------------
C
      INTEGER ICODRE
      CHARACTER*8  NOMRES,NOMPAR, KBID
      CHARACTER*10 PHENO
      REAL*8        NRUPT,DELTA
      LOGICAL       ENDUR
C
C-----------------------------------------------------------------------
      INTEGER I ,NBPAR 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      DO 10 I=1,NBCYCL
         DELTA = (ABS(SIGMAX(I)-SIGMIN(I)))/2.D0
         IF (LKE) DELTA = DELTA*RKE(I)
         IF (LHAIGH) DELTA = DELTA/RCORR(I)
         NOMRES = 'WOHLER  '
         NBPAR     = 1
         PHENO     = 'FATIGUE   '
         NOMPAR    = 'SIGM    '
         CALL LIMEND(NOMMAT,DELTA,'WOHLER',KBID, ENDUR)
         IF (ENDUR) THEN
            DOM(I) = 0.D0
         ELSE
            CALL RCVALE(NOMMAT,PHENO,NBPAR,NOMPAR,DELTA,1,NOMRES,
     &                                         NRUPT,ICODRE,2)
            DOM(I) = 1.D0/NRUPT
         ENDIF
 10   CONTINUE
C
      CALL JEDEMA()
      END
