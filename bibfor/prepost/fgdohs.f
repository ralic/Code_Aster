      SUBROUTINE FGDOHS(NOMMAT,NBCYCL,SIGMIN,SIGMAX,
     &                  LKE,RKE,LHAIGH,RCORR,DOM)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8       NOMMAT
      REAL*8                          SIGMIN(*),SIGMAX(*)
      REAL*8                RKE(*),    RCORR(*),DOM(*)
      INTEGER                  NBCYCL
      LOGICAL           LKE,    LHAIGH
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
C     UNE COURBE DE WOHLER DONNEE HORS ZONE SINGULIERE
C     ------------------------------------------------------------------
C IN  NOMMAT : K8  : NOM DU MATERIAU
C IN  NBCYCL : I   : NOMBRE DE CYCLES
C IN  SIGMIN : R   : CONTRAINTES MINIMALES DES CYCLES
C IN  SIGMAX : R   : CONTRAINTES MAXIMALES DES CYCLES
C IN  LKE    : L   : PRISE EN COMPTE DU COEFFICIENT KE
C IN  RKE    : R   : VALEURS DES COEFFICIENTS KE
C IN  LHAIGH : L   : PRISE EN COMPTE DE LA CORRECTION DE HAIGH
C IN  RCORR  : R   : VALEURS DES CORRECTIONS DE HAIGH
C OUT DOM    : R   : VALEURS DES DOMMAGES ELEMENTAIRES
C     ------------------------------------------------------------------
C
      INTEGER ICODRE(6)
      CHARACTER*8  NOMPAR
      CHARACTER*8  NOMRES(6)

      REAL*8       DELTA,SALT,X,Y,NRUPT,SLMODI,VAL(6),RBID,RE
C
C-----------------------------------------------------------------------
      INTEGER I ,NBPAR 
C-----------------------------------------------------------------------
      NOMRES(1) = 'E_REFE'
      NOMRES(2) = 'A0'
      NOMRES(3) = 'A1'
      NOMRES(4) = 'A2'
      NOMRES(5) = 'A3'
      NOMRES(6) = 'SL'
      NBPAR     = 0
      NOMPAR    = ' '
      CALL RCVALE(NOMMAT,'FATIGUE',NBPAR,NOMPAR,RBID,6,NOMRES,VAL,
     &            ICODRE,2)
      NOMRES(1) = 'E'
      CALL RCVALE(NOMMAT,'ELAS',NBPAR,NOMPAR,RBID,1,NOMRES,RE,
     &            ICODRE,2)
      DO 10 I=1,NBCYCL
        DELTA = ABS(SIGMAX(I)-SIGMIN(I))
        IF(LKE) DELTA = DELTA * RKE(I)
        IF(LHAIGH) THEN
          DELTA = DELTA / RCORR(I)
          SLMODI  = VAL(6) / RCORR(I)
        ELSE
          SLMODI  = VAL(6)
        ENDIF
        SALT  = 1.D0/2.D0*(VAL(1)/RE)*DELTA
        X  = LOG10 (SALT)
        IF (SALT.GE.SLMODI) THEN
          Y = VAL(2) + VAL(3)*X + VAL(4)*X**2 + VAL(5)*X**3
          NRUPT = 10**Y
          DOM(I) = 1.D0 / NRUPT
        ELSE
          DOM(I) = 0.D0
        ENDIF
 10   CONTINUE
C
      END
