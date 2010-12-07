      SUBROUTINE GDSMIN()

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
C                  INITIALISATION DES VARIABLES DE BASE           
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
C ----------------------------------------------------------------------


      RAC2 = SQRT(2.D0)
      RC(1)=1.D0
      RC(2)=1.D0
      RC(3)=1.D0
      RC(4)=RAC2
      RC(5)=RAC2
      RC(6)=RAC2
      
C    KRONECKER      
      KR(1)=1.D0
      KR(2)=1.D0
      KR(3)=1.D0
      KR(4)=0.D0
      KR(5)=0.D0
      KR(6)=0.D0
      
C    MATRICE IDENTITE      
      CALL R8INIR(36,0.D0,ID,1)
      ID(1,1) = 1
      ID(2,2) = 1
      ID(3,3) = 1
      ID(4,4) = 1
      ID(5,5) = 1
      ID(6,6) = 1
      
C    MANIPULATION DES INDICES : IJ -> I
      IND1(1) = 1
      IND1(2) = 2
      IND1(3) = 3
      IND1(4) = 2
      IND1(5) = 3
      IND1(6) = 3

C    MANIPULATION DES INDICES : IJ -> J
      IND2(1) = 1
      IND2(2) = 2
      IND2(3) = 3
      IND2(4) = 1
      IND2(5) = 1
      IND2(6) = 2

C    MANIPULATION DES INDICES : I,J -> IJ
      IND(1,1)=1
      IND(1,2)=4
      IND(1,3)=5
      IND(2,1)=4
      IND(2,2)=2
      IND(2,3)=6
      IND(3,1)=5
      IND(3,2)=6
      IND(3,3)=3
      
      END
