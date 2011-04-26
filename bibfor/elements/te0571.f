      SUBROUTINE TE0571(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C.......................................................................

C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UN CHARGEMENT EN PRESSION SUIVEUSE
C          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 2D

C          OPTION : 'CHAR_MECA_PRSU_R '

C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................

      IMPLICIT NONE
      LOGICAL AXI
      CHARACTER*16 NOMTE,OPTION
      INTEGER DIMGEO,NNO,NNOS,JGANO,NDIM,NPG
      INTEGER IDFDE
      INTEGER IPOIDS,IVF,IGEOM,IPRES,IRES
      INTEGER IDEPM,IDEPP
      INTEGER KPG,KDEC,N,I
      REAL*8 PR(2,4),MATNS(0:35)




C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------


      DIMGEO = 2
      AXI = NOMTE(3:4) .EQ. 'AX'

C    DIMENSIONS CARACTERISTIQUES DE L'ELEMENT
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      IF (NNO.GT.3) CALL U2MESS('F','ELEMENTS4_35')
      IF (NPG.GT.4) CALL U2MESS('F','ELEMENTS4_36')

C    AUTRES VARIABLES DE L'OPTION
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLMR','L',IDEPM)
      CALL JEVECH('PDEPLPR','L',IDEPP)
      CALL JEVECH('PPRESSR','L',IPRES)
      CALL JEVECH('PVECTUR','E',IRES)


C    REACTUALISATION DE LA GEOMETRIE PAR LE DEPLACEMENT
      DO 10 I = 0,NNO*DIMGEO - 1
        ZR(IGEOM+I) = ZR(IGEOM+I) + ZR(IDEPM+I) + ZR(IDEPP+I)
   10 CONTINUE

C    CALCUL DE LA PRESSION AUX POINTS DE GAUSS (A PARTIR DES NOEUDS)
      DO 30 KPG = 1,NPG
        KDEC = (KPG-1)*NNO
        PR(1,KPG) = 0.D0
        PR(2,KPG) = 0.D0
        DO 20 N = 0,NNO - 1
          PR(1,KPG) = PR(1,KPG) + ZR(IPRES+2*N)*ZR(IVF+KDEC+N)
          PR(2,KPG) = PR(2,KPG) + ZR(IPRES+2*N+1)*ZR(IVF+KDEC+N)
   20   CONTINUE
   30 CONTINUE

C    CALCUL EFFECTIF DU SECOND MEMBRE
      CALL NMPR2D(NDIM,AXI,1,NNO,NPG,IPOIDS,IVF,IDFDE,ZR(IGEOM),
     &            PR,ZR(IRES),MATNS)

      END
