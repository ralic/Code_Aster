        SUBROUTINE BETINC ( MATERF, NMAT, ELGEOM, SIGE, NSEUIL, DPC,
     &                      DPT, SIGF, VERIFC, VERIFT )
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ----------------------------------------------------------------
C       BETON_DOUBLE_DP: CONVEXE ELASTO PLASTIQUE POUR (MATER,SIG,P1,P2)
C                   AVEC UN SEUIL EN COMPRESSION ET UN SEUIL EN TRACTION
C       INCREMENTATION DE LA CONTRAINTE APRES CONVERGENCE
C       IN  MATERF :  COEFFICIENTS MATERIAU A T+DT
C           NMAT   :  DIMENSION MATER
C           ELGEOM :  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
C                     AUX LOIS DE COMPORTEMENT
C           SIGE   :  CONTRAINTE A T+DT (PREDICTION ELASTIQUE)
C           NSEUIL :  SEUIL D'ELASTICITE ACTIVE
C           DPC    :  INCREMENT DE MULTIPLICATEUR PLASTIQUE APRES
C                     CONVERGENCE, EN COMPRESSION
C           DPT    :  INCREMENT DE MULTIPLICATEUR PLASTIQUE APRES
C                     CONVERGENCE, EN TRACTION
C       OUT SIGF   :  CONTRAINTE A T+DT
C           VERIFC :  TEST DE VALIDITE DE LA PROJECTION AU SOMMET DU
C                     CONE COMPRESSION
C           VERIFT :  TEST DE VALIDITE DE LA PROJECTION AU SOMMET DU
C                     CONE TRACTION
C       ----------------------------------------------------------------
        INTEGER         NMAT, NSEUIL, I
        REAL*8          MATERF(NMAT,2), ELGEOM(*), DPC, DPT
        REAL*8          UN  ,  D23 , RAC2 , DEUX , TROIS
        REAL*8          SIGE(6), SIGF(6)
        PARAMETER       ( UN   = 1.D0   )
        PARAMETER       ( DEUX = 2.D0   )
        PARAMETER       ( TROIS = 3.D0   )
        PARAMETER       ( D23  =  .66666666666666D0 )
        REAL*8          DEV(6), SIGEQ , SIGH, P, SIGHF
        REAL*8          K, LAMBDA, MU, E, NU, COEF, BETA
        REAL*8          A, B, C, D
        REAL*8          VERIFC, VERIFT
C       ----------------------------------------------------------------
        INTEGER         NDT  , NDI
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
C
C --- INITIALISATION
C
      RAC2   = SQRT (DEUX)
      E      = MATERF(1,1)
      NU     = MATERF(2,1)
      BETA   = MATERF(3,2)
C
      A = RAC2 * (BETA - UN) / (DEUX * BETA - UN)
      B = RAC2 / TROIS * BETA / (DEUX * BETA - UN)
      C = RAC2
      D = DEUX * RAC2 / TROIS
C
C --- CONTRAINTE EQUIVALENTE
C
      CALL LCDEVI ( SIGE , DEV )
      CALL LCPRSC ( DEV , DEV , P)
      SIGEQ = SQRT (1.5D0 * P)
C
C --- CONTRAINTE HYDROSTATIQUE
C
      CALL LCHYDR ( SIGE , SIGH )
C
C --- COEFFICIENTS DE LAME
C
      LAMBDA = (NU * E)/((UN + NU)*(UN - DEUX * NU))
      MU =         E   /(DEUX*(UN + NU))
C
C --- MODULE DE COMPRESSION HYDROSTATIQUE
C
      K = LAMBDA + D23 * MU
C
C --- MISE A JOUR DE LA CONTRAINTE HYDROSTATIQUE
C
      IF(NSEUIL.LT.4) THEN
         SIGHF = SIGH - K * (DPC * A / B + DPT * C / D)
      ELSE IF(NSEUIL.EQ.11) THEN
         SIGHF = SIGH - K * DPC * A / B
      ELSE IF(NSEUIL.EQ.22) THEN
         SIGHF = SIGH - K * DPT * C / D
      ELSE IF(NSEUIL.EQ.33) THEN
         SIGHF = SIGH - K * (DPC * A / B + DPT * C / D)
      ENDIF
C
C --- MISE A JOUR DU DEVIATEUR DES CONTRAINTES
C
      IF(NSEUIL.LT.4) THEN
         COEF = UN
     &     - RAC2 * MU * (DPC /(B * SIGEQ) + DPT /(D * SIGEQ))
         CALL LCPRSV(COEF,DEV,SIGF)
      ELSE
         COEF = 0.D0
         CALL LCINVE(COEF,SIGF)
      ENDIF
C
C --- MISE A JOUR DES CONTRAINTES
C
      DO 10 I = 1 , NDI
      SIGF(I) = SIGF(I) + SIGHF
 10   CONTINUE
C
C --  VERIFICATION
C
      VERIFC = SIGH - SIGHF - SIGEQ * A * K / (MU * RAC2)
      VERIFT = SIGH - SIGHF - SIGEQ * C * K / (MU * RAC2)
C      IF(NSEUIL.EQ.11.OR.NSEUIL.EQ.33) THEN
C         IF (VERIFC.LT.0.D0) THEN
C            CALL UTMESS ('A','BETINC',' VERIFC NEGATIF' )
C         ENDIF
C      ENDIF
C      IF(NSEUIL.EQ.22) THEN
C         IF (VERIFT.LT.0.D0) THEN
C            CALL UTMESS ('A','BETINC',' VERIFT NEGATIF' )
C         ENDIF
C      ENDIF
C
      END
