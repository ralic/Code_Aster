      SUBROUTINE PK2SIG(NDIM,F,JAC,PK2,SIG,IND)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/11/2011   AUTEUR PROIX J-M.PROIX 
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
C     LES CONTRAINTES PK2 EN ENTREE ONT DES RAC2
C     LES CONTRAINTES DE CAUCHY SIG EN SORTIE N'ONT PAS DE RAC2
C     SI IND=-1
C     CALCUL DES CONTRAINTES DE PIOLA-KIRCHHOFF-2 A PARTIR DE CAUCHY
C     LES CONTRAINTES DE CAUCHY SIG EN ENTREE N'ONT PAS DE RAC2
C     LES CONTRAINTES PK2 EN SORTIE N'ONT PAS DE RAC2
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
      REAL*8 R8BID,RIND(6)
      DATA    INDI / 1 , 2 , 3 , 1, 1, 2 /
      DATA    INDJ / 1 , 2 , 3 , 2, 3, 3 /
      DATA    RIND / 0.5D0,0.5D0,0.5D0,0.70710678118655D0,
     &               0.70710678118655D0,0.70710678118655D0 /
      DATA    RIND1 / 0.5D0 , 0.5D0 , 0.5D0 , 1.D0, 1.D0, 1.D0 /
      
C     SEPARATION DIM 2 ET DIM 3 POUR GAGNER DU TEMPS CPU
      IF (IND.EQ.1) THEN
      
         IF (NDIM.EQ.2) THEN
         
            DO 112 PQ = 1,4
               SIG(PQ) = 0.D0
               DO 122 KL = 1,4
                  FTF = (F(INDI(PQ),INDI(KL))*F(INDJ(PQ),INDJ(KL))+
     &                   F(INDI(PQ),INDJ(KL))*F(INDJ(PQ),INDI(KL)))
     &                  *RIND(KL)
                  SIG(PQ) =  SIG(PQ)+ FTF*PK2(KL)
 122           CONTINUE
               SIG(PQ) = SIG(PQ)/JAC
 112        CONTINUE
 
         ELSEIF (NDIM.EQ.3) THEN
 
            DO 113 PQ = 1,6
               SIG(PQ) = 0.D0
               DO 123 KL = 1,6
                  FTF = (F(INDI(PQ),INDI(KL))*F(INDJ(PQ),INDJ(KL))+
     &                   F(INDI(PQ),INDJ(KL))*F(INDJ(PQ),INDI(KL)))
     &                  *RIND(KL)
                  SIG(PQ) =  SIG(PQ)+ FTF*PK2(KL)
 123           CONTINUE
               SIG(PQ) = SIG(PQ)/JAC
 113        CONTINUE
 
         ENDIF
      
      ELSEIF (IND.EQ.-1) THEN
      
         CALL MATINV('S',3,F,FMM,R8BID)
         
         IF (NDIM.EQ.2) THEN
         
            DO 212 PQ = 1,4
               PK2(PQ) = 0.D0
               DO 222 KL = 1,4
                  FTF=(FMM(INDI(PQ),INDI(KL))*FMM(INDJ(PQ),INDJ(KL))+
     &                 FMM(INDI(PQ),INDJ(KL))*FMM(INDJ(PQ),INDI(KL)))
     &                *RIND1(KL)
                  PK2(PQ) =  PK2(PQ)+ FTF*SIG(KL)
 222           CONTINUE
               PK2(PQ) = PK2(PQ)*JAC
 212        CONTINUE
 
         ELSEIF (NDIM.EQ.3) THEN
         
            DO 213 PQ = 1,6
               PK2(PQ) = 0.D0
               DO 223 KL = 1,6
                  FTF=(FMM(INDI(PQ),INDI(KL))*FMM(INDJ(PQ),INDJ(KL))+
     &                 FMM(INDI(PQ),INDJ(KL))*FMM(INDJ(PQ),INDI(KL)))
     &                *RIND1(KL)
                  PK2(PQ) =  PK2(PQ)+ FTF*SIG(KL)
 223           CONTINUE
               PK2(PQ) = PK2(PQ)*JAC
 213        CONTINUE
 
         ENDIF
      
      ENDIF
      
      END
