      SUBROUTINE LCSMTG(DF,E)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/05/2005   AUTEUR GJBHHEL E.LORENTZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      REAL*8 DF(3,3),E(6)

C ----------------------------------------------------------------------
C       INTEGRATION DES LOIS EN GRANDES DEFORMATIONS SIMO-MIEHE
C   CALCUL DES DERIVEES PAR RAPPORT A UNE VARIATION DE LA DEFORMATION
C ----------------------------------------------------------------------
C IN  DF      INCREMENT DE DEFORMATION PENDANT LE PAS DE TEMPS
C IN  E       DEFORMATION ELASTIQUE (XX,YY,ZZ,RAC2*XY,RAC2*XZ,RAC2*YZ)
C ----------------------------------------------------------------------
C  COMMON SIMO - MIEHE

      INTEGER IND1(6),IND2(6)
      REAL*8  KR(6),RAC2,RC(6)
      REAL*8  LAMBDA,MU,DEUXMU,UNK,TROISK,COTHER
      REAL*8  JM,DJ,JP,DJDF(3,3)
      REAL*8  BEM(6),ETR(6),DVETR(6),EQETR,TRETR,DETRDF(6,3,3)
      REAL*8  SIGMA(6),DSIGDE(6,6),DSIGDJ(6)
      COMMON /LCSMC/
     &          IND1,IND2,KR,RAC2,RC,
     &          LAMBDA,MU,DEUXMU,UNK,TROISK,COTHER,
     &          JM,DJ,JP,DJDF,
     &          BEM,ETR,DVETR,EQETR,TRETR,DETRDF,
     &          SIGMA,DSIGDE,DSIGDJ
C ----------------------------------------------------------------------

      INTEGER IJ,KL,I,J,K,L
      REAL*8  TRE,COEF,DPDFDF(6,6,3,3)
      REAL*8  DDOT
C ----------------------------------------------------------------------



C  CALCUL DE LA DERIVEE DES JACOBIENS DJ / DF = DJDF
C ----------------------------------------------------

      DJDF(1,1)=(DF(2,2)*DF(3,3)-DF(2,3)*DF(3,2))*JM
      DJDF(2,2)=(DF(1,1)*DF(3,3)-DF(1,3)*DF(3,1))*JM
      DJDF(3,3)=(DF(1,1)*DF(2,2)-DF(1,2)*DF(2,1))*JM
      DJDF(1,2)=(DF(3,1)*DF(2,3)-DF(2,1)*DF(3,3))*JM
      DJDF(2,1)=(DF(1,3)*DF(3,2)-DF(1,2)*DF(3,3))*JM
      DJDF(1,3)=(DF(2,1)*DF(3,2)-DF(3,1)*DF(2,2))*JM
      DJDF(3,1)=(DF(1,2)*DF(2,3)-DF(1,3)*DF(2,2))*JM
      DJDF(2,3)=(DF(3,1)*DF(1,2)-DF(1,1)*DF(3,2))*JM
      DJDF(3,2)=(DF(2,1)*DF(1,3)-DF(1,1)*DF(2,3))*JM



C  CALCUL DE LA DERIVEE DE DETR / DF : DETRDF(AB,P,Q)
C ----------------------------------------------------

C    CALCUL DPDF / DF
      CALL R8INIR(36*9,0.D0,DPDFDF,1)
      DO 1000 IJ = 1,6
        I = IND1(IJ)
        J = IND2(IJ)
        DO 1010 KL = 1,6
          K = IND1(KL)
          L = IND2(KL)
          DPDFDF(IJ,KL,I,K)=DPDFDF(IJ,KL,I,K)+RC(IJ)*RC(KL)*DF(J,L)/2
          DPDFDF(IJ,KL,J,L)=DPDFDF(IJ,KL,J,L)+RC(IJ)*RC(KL)*DF(I,K)/2
          DPDFDF(IJ,KL,J,K)=DPDFDF(IJ,KL,J,K)+RC(IJ)*RC(KL)*DF(I,L)/2
          DPDFDF(IJ,KL,I,L)=DPDFDF(IJ,KL,I,L)+RC(IJ)*RC(KL)*DF(J,K)/2
 1010   CONTINUE
 1000 CONTINUE

C    CALCUL DE DETR / DF
      DO 1100 IJ = 1,6
        DO 1110 K = 1,3
          DO 1120 L = 1,3
            DETRDF(IJ,K,L) = -0.5D0*DDOT(6,DPDFDF(IJ,1,K,L),6,BEM,1)
 1120     CONTINUE
 1110   CONTINUE
 1100 CONTINUE



C  DERIVEE PARTIELLE DE SIGMA PAR RAPPORT A J
C --------------------------------------------

      DO 100 IJ = 1,6
        DSIGDJ(IJ) = - SIGMA(IJ)/JP
 100  CONTINUE



C  DERIVEE PARTIELLE DE SIGMA PAR RAPPORT A E
C --------------------------------------------

      CALL R8INIR(36,0.D0,DSIGDE,1)

C    TERME D(E.E) / DE  DE DTAU / DE
      DSIGDE(1,1) = 2*E(1)
      DSIGDE(2,2) = 2*E(2)
      DSIGDE(3,3) = 2*E(3)
      DSIGDE(4,1) = E(4)
      DSIGDE(4,2) = E(4)
      DSIGDE(5,1) = E(5)
      DSIGDE(5,3) = E(5)
      DSIGDE(6,2) = E(6)
      DSIGDE(6,3) = E(6)
      DSIGDE(4,4) = E(1)+E(2)
      DSIGDE(5,5) = E(1)+E(3)
      DSIGDE(6,6) = E(2)+E(3)
      DSIGDE(5,4) = E(6)/RAC2
      DSIGDE(6,4) = E(5)/RAC2
      DSIGDE(6,5) = E(4)/RAC2

      DO 200 IJ = 1,6
        DO 210 KL = IJ+1,6
          DSIGDE(IJ,KL) = DSIGDE(KL,IJ)
 210    CONTINUE
 200  CONTINUE
      CALL DSCAL(36,2*DEUXMU,DSIGDE,1)

C    TERME EN (2E-1) X 1  DE DTAU / DE
      DO 300 IJ = 1,6
        DO 310 KL = 1,3
          DSIGDE(IJ,KL) = DSIGDE(IJ,KL) + LAMBDA*(2*E(IJ)-KR(IJ))
 310    CONTINUE
 300  CONTINUE

C    TERME EN ID  DE DTAU / DE
      TRE = E(1)+E(2)+E(3)
      COEF = 2*(LAMBDA*TRE+COTHER) - DEUXMU
      DSIGDE(1,1) = DSIGDE(1,1) + COEF
      DSIGDE(2,2) = DSIGDE(2,2) + COEF
      DSIGDE(3,3) = DSIGDE(3,3) + COEF
      DSIGDE(4,4) = DSIGDE(4,4) + COEF
      DSIGDE(5,5) = DSIGDE(5,5) + COEF
      DSIGDE(6,6) = DSIGDE(6,6) + COEF

      CALL DSCAL(36,1.D0/JP,DSIGDE,1)


      END
