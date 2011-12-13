      SUBROUTINE RADIPG(SIG1,SIG2,NPG,NBSIG,RADIA,COSANG)
      IMPLICIT NONE
      INTEGER NPG,NBSIG
      REAL*8 SIG1(*),SIG2(*),RADIA(*),COSANG(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/10/2011   AUTEUR DELMAS J.DELMAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C
C     BUT:
C       CALCUL DE L'INDICATEUR LOCAL DE PERTE DE RADIALITE RADIA
C       I = 1- ABS(SIG1:DSIGMA)/(NORME(SIG1)*NORME(DSIGMA)
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   SIG1     : CONTRAINTES INSTANT +
C IN   SIG2     : CONTRAINTES INSTANT -
C IN   NPG      : NOMBRE DE POINT DE GAUSS
C
C      SORTIE :
C-------------
C OUT  RADIA    : INDICATEUR DE PERTE DE RADIALITE
C
C ......................................................................
C
      INTEGER MXCMEL
      PARAMETER (MXCMEL=162)

      INTEGER I,K,IGAU

      REAL*8 DSIGMA(MXCMEL),ZERO,UN,DEUX,S1DSIG,NORM,DNORM,NORSIG
      REAL*8 ZERNOR,R8PREM
C
C ----------------------------------------------------------------------
C
      ZERO = 0.0D0
      UN = 1.0D0
      DEUX = 2.0D0
      ZERNOR = 10.0D0*R8PREM()

C ----    CALCUL DE DSIGMA = SIG2 - SIG1 :
C         ----------------------------------
        K = 0
        DO 10 IGAU = 1,NPG
          DO 20 I = 1,NBSIG
            K = K + 1
            DSIGMA(K) = SIG2(K) - SIG1(K)

  20     CONTINUE
  10   CONTINUE

C ----    CALCUL DE L'INDICATEUR LOCAL DE PERTE DE RADIALITE
C ----    AUX POINTS D'INTEGRATION :
C         ------------------------
        DO 50 IGAU = 1,NPG

C ----       CALCUL DU PRODUIT SIG1:(SIG2-SIG1) :
C            ----------------------------------------
          S1DSIG = ZERO
          DO 30 I = 1,3
            S1DSIG = S1DSIG + SIG1(I+ (IGAU-1)*NBSIG)*
     &               DSIGMA(I+ (IGAU-1)*NBSIG)
  30      CONTINUE

          DO 40 I = 4,NBSIG
            S1DSIG = S1DSIG + DEUX*SIG1(I+ (IGAU-1)*NBSIG)*
     &               DSIGMA(I+ (IGAU-1)*NBSIG)
  40      CONTINUE

C ----       CALCUL DU SECOND INVARIANT DES TENSEURS DES CONTRAINTES :
C            -------------------------------------------------------
          NORM = NORSIG(SIG1(1+ (IGAU-1)*NBSIG),NBSIG)
          DNORM = NORSIG(DSIGMA(1+ (IGAU-1)*NBSIG),NBSIG)

C ----       DANS LE CAS OU NORME(SIG1) = 0  OU NORME(DSIGMA) = 0 :
C ----       ON MET L'INDICATEUR A 0 :
C            -----------------------
          IF (NORM.LE.ZERNOR .OR. DNORM.LE.ZERNOR) THEN
            RADIA(IGAU)  = ZERO
            COSANG(IGAU) = ZERO
          ELSE IF (DNORM.LE.1.0D4*R8PREM()*NORM) THEN
            RADIA(IGAU)  = ZERO
            COSANG(IGAU) = ZERO
          ELSE
            RADIA(IGAU) = UN - ABS(S1DSIG)/NORM/DNORM
            COSANG(IGAU) = S1DSIG/NORM/DNORM
          END IF
  50    CONTINUE
C
      END
