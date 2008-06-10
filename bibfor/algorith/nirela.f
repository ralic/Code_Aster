      SUBROUTINE NIRELA(JP,GM,GP,AM,AP,BP,BOA,AA,BB,DAA,DBB,DBOA,D2BOA)

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
      REAL*8 JP,GM,GP
      REAL*8 AM,AP,BP,BOA,AA,BB,DAA,DBB,DBOA,D2BOA
C-----------------------------------------------------------------------
C          CALCUL DES OPTIONS DE MECANIQUE NON LINEAIRE
C           GRANDES DEFORMATIONS QUASI-INCOMPRESSIBLES
C
C  INITIALISATION DES FONCTIONS POUR LA RELATION : B(J) = B(A(G))
C-----------------------------------------------------------------------
C IN  JP     CHANGEMENT DE VOLUME EN T+
C IN  GM     GONFLEMENT EN T-
C IN  GP     GONFLEMENT EN T+
C OUT AM     A(GM)
C OUT AP     A(GP)
C OUT BP     B(JP)
C OUT BOA    B O A EN T+
C OUT AA(G)  DA/DG / A(G)
C OUT BB(J)  J * DB/DJ
C OUT DBOA   D(B O A)/DG
C OUT D2BOA  D2(B O A)/DG2
C OUT DAA    DAA/DG
C OUT DBB    DBB/DJ
C-----------------------------------------------------------------------
C    APPLICATION: A(G) = 1+G   ET   B(J) = J
C-----------------------------------------------------------------------

      AM    = 1+GM
      AP    = 1+GP
      BP    = JP
      BOA   = AP
      AA    = 1/AP
      BB    = JP
      DAA   = -1/(1+GP)**2
      DBB   = 1
      DBOA  = 1 
      D2BOA = 0

      END
