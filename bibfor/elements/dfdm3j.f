      SUBROUTINE DFDM3J(NNO,IPG,IDFDE,COOR,JAC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INCLUDE 'jeveux.h'
      INTEGER     IPG, IDFDE, NNO
      REAL*8      COOR(1),JAC
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DU JACOBIEN (AVEC SIGNE)
C               POUR LES ELEMENTS 3D
C
C    - ARGUMENTS:
C        DONNEES:     NNO           -->  NOMBRE DE NOEUDS
C              DFDRDE,DFRDN,DFRDK   -->  DERIVEES FONCTIONS DE FORME
C                     COOR          -->  COORDONNEES DES NOEUDS
C
C        RESULTATS:  JAC           <--  JACOBIEN AU POINT DE GAUSS
C ......................................................................
C
      INTEGER      I, J, II, K
      REAL*8       G(3,3)
      REAL*8       DE,DN,DK,J11,J21,J31


      DO 1 I=1,3
      DO 1 J=1,3
         G(I,J) = 0.D0
1     CONTINUE
C
      DO 100 I=1,NNO
         K  = 3*NNO*(IPG-1)
         II = 3*(I-1)
         DE = ZR(IDFDE-1+K+II+1)
         DN = ZR(IDFDE-1+K+II+2)
         DK = ZR(IDFDE-1+K+II+3)
         DO 101 J=1,3
            G(1,J) = G(1,J) + COOR(II+J) * DE
            G(2,J) = G(2,J) + COOR(II+J) * DN
            G(3,J) = G(3,J) + COOR(II+J) * DK
101      CONTINUE
100   CONTINUE

      J11 = G(2,2) * G(3,3) - G(2,3) * G(3,2)
      J21 = G(3,1) * G(2,3) - G(2,1) * G(3,3)
      J31 = G(2,1) * G(3,2) - G(3,1) * G(2,2)

      JAC = G(1,1) * J11 + G(1,2) * J21 + G(1,3) * J31
      END
