      SUBROUTINE PK2SIG(NDIM,F,JAC,PK2,SIG,IND)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/01/2011   AUTEUR PROIX J-M.PROIX 
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

C ----------------------------------------------------------------------
C     SI IND=1
C     CALCUL DES CONTRAINTES DE CAUCHY, CONVERSION PK2 -> CAUCHY
C     SI IND=-1
C     CALCUL DES CONTRAINTES DE PIOLA-KIRCHHOFF-2 A PARTIR DE CAUCHY
C ----------------------------------------------------------------------
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  F       : GRADIENT TRANSFORMATION EN T+
C IN  JAC     : DET DU GRADIENT TRANSFORMATION EN T-
C IN/OUT  PK2 : CONTRAINTES DE PIOLA-KIRCHHOFF 2EME ESPECE
C IN/OUT  SIG : CONTRAINTES DE CAUCHY 
C IN  IND     : CHOIX

      IMPLICIT NONE
      INTEGER NDIM,INDI(6),INDJ(6),IND,PQ,KL
      REAL*8 F(3,3),JAC,PK2(2*NDIM),SIG(2*NDIM),FTF,RIND1(6),FMM(3,3)
      REAL*8 R8BID
      DATA    INDI / 1 , 2 , 3 , 1, 1, 2 /
      DATA    INDJ / 1 , 2 , 3 , 2, 3, 3 /
      DATA    RIND1 / 0.5D0 , 0.5D0 , 0.5D0 , 1.D0, 1.D0, 1.D0 /
      
      IF (IND.EQ.1) THEN
         DO 190 PQ = 1,2*NDIM
            SIG(PQ) = 0.D0
            DO 200 KL = 1,2*NDIM
               FTF = (F(INDI(PQ),INDI(KL))*F(INDJ(PQ),INDJ(KL)) +
     &            F(INDI(PQ),INDJ(KL))*F(INDJ(PQ),INDI(KL)))*RIND1(KL)
               SIG(PQ) =  SIG(PQ)+ FTF*PK2(KL)
 200        CONTINUE
            SIG(PQ) = SIG(PQ)/JAC
 190     CONTINUE
      ELSEIF (IND.EQ.-1) THEN
         CALL MATINV('S',3,F,FMM,R8BID)
         DO 127 PQ = 1,2*NDIM
            PK2(PQ) = 0.D0
            DO 128 KL = 1,2*NDIM
               FTF=(FMM(INDI(PQ),INDI(KL))*FMM(INDJ(PQ),INDJ(KL))+
     &              FMM(INDI(PQ),INDJ(KL))*FMM(INDJ(PQ),INDI(KL)))
     &             *RIND1(KL)
               PK2(PQ) =  PK2(PQ)+ FTF*SIG(KL)
 128        CONTINUE
            PK2(PQ) = PK2(PQ)*JAC
 127     CONTINUE
      ENDIF
      END
