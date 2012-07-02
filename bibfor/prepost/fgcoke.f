      SUBROUTINE FGCOKE(NBCYCL,SIGMIN,SIGMAX,N,M,SM,RKE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      REAL*8                   SIGMIN(*),SIGMAX(*)
      REAL*8                                 N,M,SM,RKE(*)
      INTEGER           NBCYCL
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
C     AMPLIFICATION DU CHARGEMENT (KE COEFFICIENT DE CORRECTION
C     ELASTO-PLASTIQUE
C     ------------------------------------------------------------------
C IN  NBCYCL : I   : NOMBRE DE CYCLES
C IN  SIGMIN : R   : CONTRAINTES MINIMALES DES CYCLES
C IN  SIGMAX : R   : CONTRAINTES MAXIMALES DES CYCLES
C IN  N      : R   :
C IN  M      : R   :
C IN  SM     : R   :
C OUT RKE    : R   : VALEURS DU COEFFICIENT KE POUR CHAQUE CYCLE
C     ------------------------------------------------------------------
C
      REAL*8 DELTA
C
C-----------------------------------------------------------------------
      INTEGER I 
C-----------------------------------------------------------------------
      DO 10 I=1,NBCYCL
        DELTA = ABS(SIGMAX(I)-SIGMIN(I))
        IF(DELTA.LE.3.D0*SM) THEN
          RKE(I) = 1.D0
        ELSEIF(DELTA.GT.3.D0*SM.AND.DELTA.LT.3.D0*M*SM) THEN
          RKE(I) = 1.D0 + ((1-N)/(N*(M-1)))*((DELTA/(3.D0*SM))-1.D0)
        ELSEIF(DELTA.GE.3*M*SM) THEN
          RKE(I) = 1.D0/N
        ENDIF
  10  CONTINUE
C
      END
