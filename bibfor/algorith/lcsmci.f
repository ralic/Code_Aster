      SUBROUTINE LCSMCI(FM,DF,EM)

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

      REAL*8 FM(3,3),DF(3,3),EM(6)
C ----------------------------------------------------------------------
C       INTEGRATION DES LOIS EN GRANDES DEFORMATIONS SIMO-MIEHE
C                  CALCUL DES ELEMENTS CINEMATIQUES
C ----------------------------------------------------------------------
C IN  FM    DEFORMATION AU DEBUT DU PAS DE TEMPS
C IN  DF    INCREMENT DE DEFORMATION PENDANT LE PAS DE TEMPS
C IN  EM    DEFORMATION ELASTIQUE (-) AU DEBUT DU PAS DE TEMPS
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
      REAL*8  PDF(6,6),BETR(6)
      REAL*8  DDOT
C ----------------------------------------------------------------------


C  CALCUL DES JACOBIENS
C ----------------------

      JM=FM(1,1)*(FM(2,2)*FM(3,3)-FM(2,3)*FM(3,2))
     &  -FM(2,1)*(FM(1,2)*FM(3,3)-FM(1,3)*FM(3,2))
     &  +FM(3,1)*(FM(1,2)*FM(2,3)-FM(1,3)*FM(2,2))

      DJ=DF(1,1)*(DF(2,2)*DF(3,3)-DF(2,3)*DF(3,2))
     &  -DF(2,1)*(DF(1,2)*DF(3,3)-DF(1,3)*DF(3,2))
     &  +DF(3,1)*(DF(1,2)*DF(2,3)-DF(1,3)*DF(2,2))

      JP=JM*DJ


C  CALCUL DE ETR
C ---------------

C    CALCUL DE BE EN T-
      DO 10 IJ = 1,6
        BEM(IJ) = KR(IJ) - 2*EM(IJ)
 10   CONTINUE


C    CALCUL PDF(AB,KL) = DF(A,K)*DF(B,L) SYMETRISE ET RACINE DE 2
      DO 100 IJ = 1,6
        I = IND1(IJ)
        J = IND2(IJ)
        DO 110 KL = 1,6
          K = IND1(KL)
          L = IND2(KL)
          PDF(IJ,KL)=RC(IJ)*RC(KL)*(DF(I,K)*DF(J,L)+DF(J,K)*DF(I,L))/2
 110    CONTINUE
 100  CONTINUE


C    CALCUL DE BE TRIAL : BETR(AB) = PDF(AB,IJ):BEM(IJ)  ET  E TRIAL
       DO 200 IJ = 1,6
         BETR(IJ) = DDOT(6, PDF(IJ,1),6, BEM,1)
         ETR(IJ)  = (KR(IJ)-BETR(IJ))/2
 200   CONTINUE


C    CALCUL DES INVARIANTS DE E TRIAL
      TRETR = ETR(1)+ETR(2)+ETR(3)
      DO 300 IJ = 1,6
        DVETR(IJ) = ETR(IJ) - TRETR/3.D0*KR(IJ)
 300  CONTINUE
      EQETR = SQRT(1.5D0 * DDOT(6,DVETR,1,DVETR,1))

      END
