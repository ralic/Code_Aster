      SUBROUTINE GDSMCI(FM,DF,EM)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/02/2007   AUTEUR MICHEL S.MICHEL 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
      REAL*8  FM(3,3),DF(3,3),EM(6)

C ----------------------------------------------------------------------
C       INTEGRATION DES LOIS EN GRANDES DEFORMATIONS SIMO-MIEHE  
C                  CALCUL DES ELEMENTS CINEMATIQUES              
C ----------------------------------------------------------------------
C IN  FM    : DEFORMATION AU DEBUT DU PAS DE TEMPS
C IN  DF    : INCREMENT DE DEFORMATION PENDANT LE PAS DE TEMPS
C IN  EM    : DEFORMATION ELASTIQUE AU DEBUT DU PAS DE TEMPS (VIM)
C ----------------------------------------------------------------------
C COMMON GRANDES DEFORMATIONS SIMO - MIEHE      
      
      INTEGER IND(3,3),IND1(6),IND2(6)
      REAL*8  KR(6),RAC2,RC(6),ID(6,6)
      REAL*8 BEM(6),BETR(6),DVBETR(6),EQBETR,TRBETR
      REAL*8 JP,DJ,JM,DFB(3,3)  
      REAL*8 DJDF(3,3),DBTRDF(6,3,3)
             
      COMMON /GDSMC/
     &            BEM,BETR,DVBETR,EQBETR,TRBETR,
     &            JP,DJ,JM,DFB,
     &            DJDF,DBTRDF,
     &            KR,ID,RAC2,RC,IND,IND1,IND2
C ----------------------------------------------------------------------

      INTEGER IJ,KL,I,J,K,L
      REAL*8  PDF(6,6),DDOT
      
      REAL*8 E(3,3),DETE,FP(3,3)
C ----------------------------------------------------------------------


C 1 - CALCUL DES JACOBIENS ET DE DF-BARRE
C ----------------------------------------

      JM=FM(1,1)*(FM(2,2)*FM(3,3)-FM(2,3)*FM(3,2))
     &  -FM(2,1)*(FM(1,2)*FM(3,3)-FM(1,3)*FM(3,2))
     &  +FM(3,1)*(FM(1,2)*FM(2,3)-FM(1,3)*FM(2,2))

      DJ=DF(1,1)*(DF(2,2)*DF(3,3)-DF(2,3)*DF(3,2))
     &  -DF(2,1)*(DF(1,2)*DF(3,3)-DF(1,3)*DF(3,2))
     &  +DF(3,1)*(DF(1,2)*DF(2,3)-DF(1,3)*DF(2,2))

      JP=JM*DJ

      CALL DCOPY(9,DF,1,DFB,1)
      CALL DSCAL(9,DJ**(-1.D0/3.D0),DFB,1)



C  CALCUL DE BETR
C ---------------

C    CALCUL DE BE-BARRE EN T-
      DO 10 IJ = 1,6
        BEM(IJ) = (KR(IJ) - 2*EM(IJ))/JM**(2.D0/3.D0)
 10   CONTINUE


C    CALCUL PDF(IJ,KL) = DFB(I,K)*DFB(J,L) SYMETRISE ET RACINE DE 2
      DO 100 IJ = 1,6
       I = IND1(IJ)
       J = IND2(IJ)
        DO 110 KL = 1,6
          K = IND1(KL)
          L = IND2(KL)
          PDF(IJ,KL)=RC(IJ) * RC(KL) * 
     &           (DFB(I,K)*DFB(J,L)+DFB(J,K)*DFB(I,L) ) / 2.D0
 110    CONTINUE
 100  CONTINUE


C    CALCUL DE BE TRIAL : BETR(AB) = PDF(AB,IJ):BEM(IJ) 
      DO 200 IJ = 1,6
         BETR(IJ) = DDOT(6, PDF(IJ,1),6, BEM,1)
 200  CONTINUE


      DO 300 I = 1,3
        DO 320 J = 1,3
           FP(I,J) = 0
          DO 340 K = 1,3
            FP(I,J) = FP(I,J) + DF(I,K)*FM(K,J)
 340      CONTINUE
 320    CONTINUE
 300  CONTINUE 
       
      DO 400 I = 1,3
        DO 420 J = 1,3
           E(I,J) = 0
          DO 440 K = 1,3
             E(I,J) = E(I,J) + FP(I,K)*FP(J,K)
 440      CONTINUE
 420    CONTINUE
 400  CONTINUE 
       
      DETE=E(1,1)*(E(2,2)*E(3,3)-E(2,3)*E(3,2))
     &  -E(2,1)*(E(1,2)*E(3,3)-E(1,3)*E(3,2))
     &  +E(3,1)*(E(1,2)*E(2,3)-E(1,3)*E(2,2))
     
      DO 500 I = 1,3
        DO 520 J = 1,3
          E(I,J) = E(I,J)/DETE**(1.D0/3.D0)
 520     CONTINUE
 500   CONTINUE 
       

C 3.4 - CALCUL DE LA PARTIE DEVIATORIQUE ET INVARIANTS DE BETR EN T+

      TRBETR = BETR(1)+BETR(2)+BETR(3)
      DO 600 IJ = 1,6
        DVBETR(IJ) = BETR(IJ) - TRBETR/3.D0*KR(IJ)
 600  CONTINUE
      EQBETR = SQRT(1.5D0 * DDOT(6,DVBETR,1,DVBETR,1))

      END
