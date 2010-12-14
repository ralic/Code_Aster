      SUBROUTINE DFDM3B ( NNO, IPG, IPOIDS, IDFDE, COOR,
     &                    DFDX, DFDY, DFDZ, JAC )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/12/2010   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J-M.PROIX
      IMPLICIT NONE

      INTEGER IPG, IPOIDS, IDFDE, NNO

      REAL*8 COOR(1), DFDX(1), DFDY(1), DFDZ(1), JAC

C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES DERIVEES DES FONCTIONS DE FORME
C      DE LA PARTIE BULLE PAR RAPPORT A UN ELEMENT COURANT EN UN POINT
C      DE GAUSS POUR LES ELEMENTS 3D
C
C    - ARGUMENTS:
C        DONNEES:     NNO           -->  NOMBRE DE NOEUDS
C                     POIDS         -->  POIDS DU POINT DE GAUSS
C              DFDRDE,DFRDN,DFRDK   -->  DERIVEES FONCTIONS DE FORME
C                     COOR          -->  COORDONNEES DES NOEUDS
C
C        RESULTATS:   DFDX          <--  DERIVEES DES F. DE F. / X
C                     DFDY          <--  DERIVEES DES F. DE F. / Y
C                     DFDZ          <--  DERIVEES DES F. DE F. / Z
C                     JAC           <--  JACOBIEN AU POINT DE GAUSS
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

      INTEGER I, J, II, K, IADZI, IAZK24

      REAL*8 POIDS, G(3,3), R8GAEM, PBULLE
      REAL*8 DE,DN,DK,J11,J12,J13,J21,J22,J23,J31,J32,J33

      CHARACTER*8 NOMAIL

      POIDS = ZR(IPOIDS+IPG-1)
      PBULLE = 4.D0

      CALL MATINI(3,3,0.D0,G)

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
      J12 = G(1,3) * G(3,2) - G(1,2) * G(3,3)
      J22 = G(1,1) * G(3,3) - G(1,3) * G(3,1)
      J32 = G(1,2) * G(3,1) - G(3,2) * G(1,1)
      J13 = G(1,2) * G(2,3) - G(1,3) * G(2,2)
      J23 = G(2,1) * G(1,3) - G(2,3) * G(1,1)
      J33 = G(1,1) * G(2,2) - G(1,2) * G(2,1)

      JAC = G(1,1) * J11 + G(1,2) * J21 + G(1,3) * J31

C SUR UN SOUS-ELEMENT, ON A JAC=JAC/PBULLE
      JAC=JAC/PBULLE

      IF(ABS(JAC).LE.1.D0/R8GAEM()) THEN
         CALL TECAEL(IADZI,IAZK24)
         NOMAIL= ZK24(IAZK24-1+3)(1:8)
         CALL U2MESK('F','ALGORITH2_59',1,NOMAIL)
      ENDIF

      DO 200 I=1,NNO
         K  = 3*NNO*(IPG-1)
         II = 3*(I-1)
         DE = ZR(IDFDE-1+K+II+1)
         DN = ZR(IDFDE-1+K+II+2)
         DK = ZR(IDFDE-1+K+II+3)
         DFDX(I) =  ( J11*DE + J12*DN + J13*DK ) / JAC
         DFDY(I) =  ( J21*DE + J22*DN + J23*DK ) / JAC
         DFDZ(I) =  ( J31*DE + J32*DN + J33*DK ) / JAC
200   CONTINUE

      JAC = ABS(JAC)*POIDS

      END
