      SUBROUTINE FGDEPR(TPS,S,T,FLUPHI,VALDRP,TTAMAX,F1,FP1,FS1,F2,
     *                  FP2,FS2,G1,DG1DS,G2,DG2DS)
      IMPLICIT REAL*8 (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/01/96   AUTEUR F6BHHBO P.DEBONNIERES 
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
C
CDEB
C---------------------------------------------------------------
C     CALCUL DES FONCTIONS F1,G1,F2,G2 ET DE LEURS DERIVEES
C---------------------------------------------------------------
C IN  TPS   :R: TEMPS
C     S     :R: CONTRAINTE EQUIVALENTE SIGMA
C     T     :R: TEMPERATURE DU POINT CONSIDERE
C     FLUPHI:R: FLUX NEUTRONIQUE
C     VALDRP:R: LIMITE ELASTIQUE R_P
C     TTAMAX:R: ANGLE THETA_MAX
C OUT F1    :R: VALEUR DE F1(TPS)
C     FP1   :R: VALEUR DE F'1(TPS)
C     FS1   :R: VALEUR DE F"1(TPS)
C     F2    :R: VALEUR DE F2(TPS)
C     FP2   :R: VALEUR DE F'2(TPS)
C     FS2   :R: VALEUR DE F"2(TPS)
C     G1    :R: VALEUR DE G1(SIGMA,T)
C     DG1DS :R: VALEUR DE DG1/DSIGMA(SIGMA,T)
C     G2    :R: VALEUR DE G2(SIGMA,T)
C     DG2DS :R: VALEUR DE DG2/DSIGMA(SIGMA,T)
C---------------------------------------------------------------
C     CETTE ROUTINE CALCULE LES FONCTIONS F1,G1,F2,G2 DANS :
C
C            EV = F1(TPS)*G1(S,T) + F2(TPS)*G2(S,T)
C
C      ET LEURS DERIVEES F'1,F"1,DG1/DSIGMA,F'2,F"2,DG2/DSIGMA
C---------------------------------------------------------------
CFIN
C
C---------------------------------------------------------------
C---------------------------------------------------------------
C     ECRITURE DE LA LOI DE FLUAGE EN CONTRAINTE ET
C      DEFORMATION VISQUEUSE EQUIVALENTES (AU LIEU DE
C      CONTRAINTE ET DEFORMATION VISQUEUSE CIRCONFERENTIELLES),
C     CE QUI SE TRADUIT PAR LA MODIFICATION DES COEF A1,A2 ET B1 :
C
C     A1 = 1.388D+8/R3S2
C     A2 = 3.29D-5/(R3S2**A3)
C
C     B1 = 2.35D-21/(R3S2**(B4+1))
C
      A1 = 1.603D+8
      A2 = 4.567D-5
      A3 = 2.28D0
      A4 = 0.997D0
      A5 = 0.77D0
      A6 = 0.956D0
      A7 = 23000.D0
      B1 = 3.296D-21
      B2 = 0.811D0
      B3 = 0.595D0
      B4 = 1.352D0
      B5 = 22.91D0
      B6 = 1.58D0
      B7 = 2.228D0
C
C----CALCUL DE F1,FP1,FS1---------------------------------------
C
      F1 = EXP(A5*LOG(TPS))
      FP1= A5*F1/TPS
      FS1= A5*(A5-1)*F1/(TPS*TPS)
C
C----CALCUL DE F2,FP2,FS2---------------------------------------
C
      F2 = EXP(B2*LOG(TPS))
      FP2= B2*F2/TPS
      FS2= B2*(B2-1)*F2/(TPS*TPS)
C
C----CALCUL DE G1,DG1DS-----------------------------------------
C
      G1 = A1*EXP(A4*LOG(SINH(A2*EXP(A3*LOG(S))))+A6*LOG(VALDRP)
     *     -A7/(T+273.15D0))
      DG1DS=EXP((A4-1.D0)*LOG(SINH(A2*EXP(A3*LOG(S))))+A6*LOG(VALDRP)
     *     -A7/(T+273.15D0)+(A3-1.D0)*LOG(S))*A1*A4*A2*A3*
     *      COSH(A2*EXP(A3*LOG(S)))
C
C----CALCUL DE G2,DG2DS-----------------------------------------
C
      IF (FLUPHI.EQ.0.D0) THEN
        G2 = 0.D0
        DG2DS = 0.D0
      ELSE
        G2 = B1*EXP(B3*LOG(FLUPHI)+B4*LOG(S)-B5/(T+273.15D0)
     *     +B6*LOG(VALDRP)+B7*LOG(COS(TTAMAX)))
        DG2DS=B1*B4*EXP(B3*LOG(FLUPHI)+(B4-1.D0)*LOG(S)-B5/(T+273.15D0)
     *     +B6*LOG(VALDRP)+B7*LOG(COS(TTAMAX)))
      ENDIF
C
      END
