      SUBROUTINE FGRCCM(NBEXTR,EXT,NCYC,SIGMIN,SIGMAX)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      REAL*8                   EXT(*),  SIGMIN(*),SIGMAX(*)
      INTEGER           NBEXTR,    NCYC
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
C     DETERMINATION DES CYCLES PAR LA METHODE DE COMPTAGE RCCM
C     ------------------------------------------------------------------
C IN  NBEXTR : I   : NOMBRE D'EXTREMA
C IN  EXT    : R   : VALEURS DES EXTREMA
C OUT NCYC   : I   : NOMBRE DE CYCLES
C OUT SIGMIN : R   : CONTRAINTES MINIMALES DES CYCLES
C OUT SIGMAX : R   : CONTRAINTES MAXIMALES DES CYCLES
C     ------------------------------------------------------------------
C     -----------------------------------------------------------------
C
      REAL*8   MOYEXT,A
      LOGICAL  CYCZER
C
C ------------------------------------------------------------
C
C --- CALCUL DE LA VALEUR MOYENNE DES CONTRAINTES ---
C
C-----------------------------------------------------------------------
      INTEGER I 
C-----------------------------------------------------------------------
      MOYEXT = 0.D0
      
      CYCZER = .TRUE.

      DO 21 I=2,NBEXTR
         IF ((EXT(I) .GT. EXT(1)) .OR.
     &     (EXT(I) .LT. EXT(1)))  THEN
            CYCZER = .FALSE.
         END IF
 21   CONTINUE
 
      IF (CYCZER) THEN 
         SIGMAX(1) = EXT(1)
         SIGMIN(1) = EXT(1)
         NCYC = 1
         
         CALL U2MESS('A','FATIGUE1_39')
         
         GOTO 999
      ENDIF 
           
      DO 1 I=1,NBEXTR
        MOYEXT = MOYEXT + EXT(I)
  1   CONTINUE
      MOYEXT = MOYEXT/NBEXTR
C
C --- DETECTION DES CYCLES
C
      A = DBLE(NBEXTR/2)
      NCYC = INT(A)
      DO 2 I=1,NCYC
        SIGMAX(I) = EXT(NBEXTR-I+1)
        SIGMIN(I) = EXT(I)
  2   CONTINUE
      IF (NBEXTR.NE.(2*NCYC)) THEN
        NCYC = NCYC + 1
        IF (EXT(NCYC).GE.MOYEXT) THEN
          SIGMAX(NCYC) = EXT(NCYC)
          SIGMIN(NCYC) = -EXT(NCYC) + 2 * MOYEXT
        ELSE
          SIGMAX(NCYC) = -EXT(NCYC) + 2 * MOYEXT
          SIGMIN(NCYC) = EXT(NCYC)
        ENDIF
      ENDIF
C
999   CONTINUE

      END
