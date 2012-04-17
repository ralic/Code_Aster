      SUBROUTINE DFDM2J ( NNO,IPG,IDFDE,COOR,JAC )
      IMPLICIT NONE
      INTEGER             NNO,IPG,IDFDE
      REAL*8              COOR(1),JAC
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 22/07/2008   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
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
