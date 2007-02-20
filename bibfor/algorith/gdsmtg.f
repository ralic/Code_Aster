      SUBROUTINE GDSMTG()

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

C ----------------------------------------------------------------------
C       INTEGRATION DES LOIS EN GRANDES DEFORMATIONS SIMO-MIEHE
C   CALCUL DES DERIVEES PAR RAPPORT A UNE VARIATION DE LA DEFORMATION
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

      INTEGER IJ,I,J,L,IL,JL
C ----------------------------------------------------------------------


C  CALCUL DE LA DERIVEE DES JACOBIENS DJ / DF = DJDF
C ----------------------------------------------------

      CALL R8INIR(9,0.D0,DJDF,1)
      DJDF(1,1) = JP
      DJDF(2,2) = JP
      DJDF(3,3) = JP


C  CALCUL DE LA DERIVEE DE DBTR / DF : DBTRDF(AB,P,Q)
C ----------------------------------------------------

      CALL R8INIR(54,0.D0,DBTRDF,1)
      
      DO 1100 IJ = 1,6
        I = IND1(IJ)
        J = IND2(IJ)
        DO 1110 L = 1,3
          IL = IND(I,L)
          JL = IND(J,L)
          DBTRDF(IJ,I,L) = DBTRDF(IJ,I,L) + RC(IJ)*BETR(JL)/RC(JL)
          DBTRDF(IJ,J,L) = DBTRDF(IJ,J,L) + RC(IJ)*BETR(IL)/RC(IL)
          DBTRDF(IJ,L,L) = DBTRDF(IJ,L,L) - 2.D0/3.D0*BETR(IJ)
 1110   CONTINUE
 1100 CONTINUE

      END
