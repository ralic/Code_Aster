      SUBROUTINE INVJAC ( NNO, IPG, IPOIDS, IDFDE, COOR, INVJA, JAC )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/01/2007   AUTEUR DESROCHES X.DESROCHES 
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
      INTEGER     IPG, IPOIDS, IDFDE, NNO
      REAL*8      COOR(1), JAC
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DE L'INVERSE DE LA MATRICE JACOBIENNE
C                          POUR LES ELEMENTS 3D
C
C    - ARGUMENTS:
C        DONNEES:     NNO           -->  NOMBRE DE NOEUDS
C                     POIDS         -->  POIDS DU POINT DE GAUSS
C              DFDRDE,DFRDN,DFRDK   -->  DERIVEES FONCTIONS DE FORME
C                     COOR          -->  COORDONNEES DES NOEUDS
C
C      RESULTAT :   INVJA         <--  INVERSE DE LA MATRICE JACOBIENNE
C                   JAC           <--  JACOBIEN
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
      INTEGER      I, J, II, K, IADZI, IAZK24
      REAL*8       POIDS,G(3,3), R8GAEM
      REAL*8       DE,DN,DK,INVJA(3,3)
      REAL*8       J11,J12,J13,J21,J22,J23,J31,J32,J33
      CHARACTER*8  NOMAIL
C
      POIDS = ZR(IPOIDS+IPG-1)
C
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
C
      J11 = G(2,2) * G(3,3) - G(2,3) * G(3,2)
      J21 = G(3,1) * G(2,3) - G(2,1) * G(3,3)
      J31 = G(2,1) * G(3,2) - G(3,1) * G(2,2)
      J12 = G(1,3) * G(3,2) - G(1,2) * G(3,3)
      J22 = G(1,1) * G(3,3) - G(1,3) * G(3,1)
      J32 = G(1,2) * G(3,1) - G(3,2) * G(1,1)
      J13 = G(1,2) * G(2,3) - G(1,3) * G(2,2)
      J23 = G(2,1) * G(1,3) - G(2,3) * G(1,1)
      J33 = G(1,1) * G(2,2) - G(1,2) * G(2,1)
      JAC = G(1,1) * J11 + G(1,2) * J21 + G(1,3) * J31
C
      INVJA(1,1) = J11/JAC
      INVJA(1,2) = J12/JAC
      INVJA(1,3) = J13/JAC
      INVJA(2,1) = J21/JAC
      INVJA(2,2) = J22/JAC
      INVJA(2,3) = J23/JAC
      INVJA(3,1) = J31/JAC
      INVJA(3,2) = J32/JAC
      INVJA(3,3) = J33/JAC
C
      JAC = ABS(JAC)
      JAC = JAC*POIDS      
C
      END
