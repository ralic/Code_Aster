      SUBROUTINE VFF2DN( NDIM,NNO,IPG,IPOIDS,IDFDE,COOR,NX,NY,JAC )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
      IMPLICIT NONE
      INTEGER            NDIM,NNO,IPOIDS,IDFDE,IPG,I,II,K
      REAL *8            DX,COOR(1),NX,NY,JAC,DXDS,DYDS
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER  ZI
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
C ......................................................................
C    - FONCTION REALISEE:  VALEUR DU POIDS D'INTEGRATION POUR UN SEGMENT
C                          A 2 OU 3 NOEUDS
C    - ARGUMENTS:
C        DONNEES:          NNO      -->  NOMBRE DE NOEUDS
C                          POIDS    -->  POIDS DE GAUSS
C                          DFDE     -->  DERIVEES DES FONCTIONS DE FORME
C                          COOR     -->  COORDONNEES DES NOEUDS
C
C      RESULTATS:          NX,NY    <--  COMPOSANTES DE LA NORMALE
C                          JAC      <--  PRODUIT DU JACOBIEN ET DU POIDS
C ......................................................................
C
C
      DXDS = 0.D0
      DYDS = 0.D0
      DO 1 I = 1,NNO
         K = NDIM*NNO*(IPG-1)
         II = NDIM*(I-1)
         DX = ZR(IDFDE-1+K+II+1)
         DXDS = DXDS + DX * COOR(2*I-1)
         DYDS = DYDS + DX * COOR(2*I)
 1    CONTINUE
      JAC = SQRT(DXDS**2 + DYDS**2)
      NX  =  DYDS/JAC
      NY  = -DXDS/JAC
      JAC = ZR(IPOIDS + IPG-1) * JAC
      END
