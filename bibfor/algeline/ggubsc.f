      SUBROUTINE GGUBSC (DSEED,NR,CR)
      IMPLICIT NONE
      REAL*8            DSEED
      COMPLEX*16        CR(*)
      INTEGER                 NR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     GENERATEUR DE NOMBRES (PSEUDO-)ALEATOIRES UNIFORMEMENT REPARTIS
C     ENTRE (0,1)                                     (CF GGUBS DE IMSL)
C     ------------------------------------------------------------------
C     CETTE VERSION NE FONCTIONNE QUE SUR DES MACHINES CODANT REELS OU
C     ENTIERS SUR AU MOINS 32 BITS.
C     ------------------------------------------------------------------
C VAR DSEED  - EN ENTREE UNE VALEUR ENTRE (1.E0, 2147483647.E0).
C            - EN SORTIE UNE AUTRE VALEUR POUR LE PROCHAIN TIRAGE.
C IN  NR     - NOMBRE DE VALEUR A TIRER
C OUT CR     - TABLEAU CONTENANT LES VALEURS TIREES
C     ------------------------------------------------------------------
C             D2P31M=(2**31)-1,    D2P31 =(2**31)
      REAL*8  D2P31M,              D2P31
C-----------------------------------------------------------------------
      INTEGER I ,IDSEED 
      REAL*8 DSEED0 
C-----------------------------------------------------------------------
      DATA    D2P31M/2147483647.D0/, D2P31/2147483648.D0/
C     ------------------------------------------------------------------
      DO 10 I=1,NR
         IDSEED = INT(16807.D0*DSEED/D2P31M)
         DSEED  = 16807.D0*DSEED-IDSEED*D2P31M
         DSEED0 = DSEED
            IDSEED = INT(16807.D0*DSEED/D2P31M)
            DSEED  = 16807.D0*DSEED-IDSEED*D2P31M
            CR(I) = DCMPLX(DSEED0/D2P31,DSEED / D2P31)
   10 CONTINUE
      END
