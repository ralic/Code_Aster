      SUBROUTINE DFDM2J ( NNO,IPG,IDFDE,COOR,JAC )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER             NNO,IPG,IDFDE
      REAL*8              COOR(1),JAC
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
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DU JACOBIEN (AVEC SIGNE)
C               POUR LES ELEMENTS 2D
C
C    - ARGUMENTS:
C        DONNEES:     NNO           -->  NOMBRE DE NOEUDS
C                     DFRDE,DFRDK   -->  DERIVEES FONCTIONS DE FORME
C                     COOR          -->  COORDONNEES DES NOEUDS
C
C        RESULTATS:   JAC           <--  JACOBIEN AU POINT DE GAUSS
C ......................................................................
C
      INTEGER     I, II, K
      REAL*8      DE, DK, DXDE, DXDK, DYDE, DYDK


      DXDE = 0.D0
      DXDK = 0.D0
      DYDE = 0.D0
      DYDK = 0.D0
      DO 100 I = 1 , NNO
         K = 2*NNO*(IPG-1)
         II = 2*(I-1)
         DE = ZR(IDFDE-1+K+II+1)
         DK = ZR(IDFDE-1+K+II+2)
         DXDE = DXDE + COOR(2*I-1)*DE
         DXDK = DXDK + COOR(2*I-1)*DK
         DYDE = DYDE + COOR(2*I  )*DE
         DYDK = DYDK + COOR(2*I  )*DK
 100  CONTINUE

      JAC = DXDE*DYDK - DXDK*DYDE

      END
