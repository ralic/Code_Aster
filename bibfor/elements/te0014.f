      SUBROUTINE TE0014(OPTION,NOMTE)
      IMPLICIT REAL*8 (A-H,O-Z)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
      CHARACTER*16      OPTION,NOMTE
C.......................................................................
C
C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          ELEMENTS ISOPARAMETRIQUES 3D
C
C          OPTION : 'CHAR_MECA_ROTA_R '
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
      CHARACTER*2        CODRET
      CHARACTER*8        ELREFE
      CHARACTER*16       PHENOM
      CHARACTER*24       CHVAL,CHCTE
      REAL*8             AMM(81,81),FT(81),X(27),Y(27),Z(27)
      REAL*8             XI,XIJ
      REAL*8             DFDX(27),DFDY(27),DFDZ(27),POIDS
      REAL*8             RHO,OM1,OM2,OM3,OMM,OMO,RRI
      INTEGER            IPOIDS,IVF,IDFDE,IDFDN,IDFDK,IGEOM,IMATE
      INTEGER            NDL,NNO,KP,NPG,II,JJ,I,J,IDEPLM,IDEPLP
      INTEGER            JIN,NDIM,NBFPG,JVAL,IVECTU,IROTA,L,K,IC
      INTEGER            NBPG(10)
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      CALL ELREF1(ELREFE)
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM  = ZI(JIN+1-1)
      NNO   = ZI(JIN+2-1)
      NDL   = 3 * NNO
      NBFPG = ZI(JIN+3-1)
      DO 1 I = 1,NBFPG
        NBPG(I) = ZI(JIN+3-1+I)
  1   CONTINUE
      NPG = NBPG(1)
      IF(ELREFE.NE.'PYRAM5  '.AND.ELREFE.NE.'PYRAM13 ') THEN
        NPG = NBPG(2)
      ENDIF
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IF(ELREFE.NE.'PYRAM5  '.AND.ELREFE.NE.'PYRAM13 ') THEN
        IPOIDS = IPOIDS + NBPG(1) + (NDIM+1)*NNO*NBPG(1)
      ENDIF
      IVF    = IPOIDS + NPG
      IDFDE  = IVF    + NPG*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL TECACH(.TRUE.,.FALSE.,'PDEPLMR',1,IDEPLM)
      CALL TECACH(.TRUE.,.FALSE.,'PDEPLPR',1,IDEPLP)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PROTATR','L',IROTA)
      CALL JEVECH('PVECTUR','E',IVECTU)
C
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
      CALL RCVALA(ZI(IMATE),PHENOM,1,' ',R8B,1,'RHO',RHO,CODRET,'FM')
C
      DO 2 I = 1,NDL
        DO 2 J = 1,NDL
          AMM(I,J) = 0.D+00
  2   CONTINUE
C
      OMM = ZR(IROTA) * ZR(IROTA)
      OM1 = ZR(IROTA) * ZR(IROTA+1)
      OM2 = ZR(IROTA) * ZR(IROTA+2)
      OM3 = ZR(IROTA) * ZR(IROTA+3)
      IF(IDEPLM.EQ.0.OR.IDEPLP.EQ.0) THEN
        DO 200 I = 1,NNO
          X(I) =  ZR(IGEOM+3*(I-1)) - ZR(IROTA+4)
          Y(I) =  ZR(IGEOM+3*I-2)   - ZR(IROTA+5)
          Z(I) =  ZR(IGEOM+3*I-1)   - ZR(IROTA+6)
200     CONTINUE
      ELSE
        DO 210 I = 1,NNO
          X(I) =  ZR(IGEOM+3*I-3)+ZR(IDEPLM+3*I-3)+ZR(IDEPLP+3*I-3)
     +           -ZR(IROTA+4)
          Y(I) =  ZR(IGEOM+3*I-2)+ZR(IDEPLM+3*I-2)+ZR(IDEPLP+3*I-2)
     +           -ZR(IROTA+5)
          Z(I) =  ZR(IGEOM+3*I-1)+ZR(IDEPLM+3*I-1)+ZR(IDEPLP+3*I-1)
     +           -ZR(IROTA+6)
210     CONTINUE
      ENDIF
      DO 182 I=1,NNO
        OMO = OM1 * X(I) + OM2 * Y(I) + OM3 * Z(I)
        FT(3*I-2) = OMM * X(I) - OMO * OM1
        FT(3*I-1) = OMM * Y(I) - OMO * OM2
        FT(3*I)   = OMM * Z(I) - OMO * OM3
  182 CONTINUE
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 101 KP=1,NPG
C
        L=(KP-1)*NNO
        K=(KP-1)*NNO*3
        CALL DFDM3D (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &               ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
        DO 106 I=1,NNO
          XI = RHO * POIDS * ZR(IVF+L+I-1)
          II = 3*(I-1)
          DO 107 J=1,NNO
            XIJ = XI * ZR(IVF+L+J-1)
            JJ = 3*(J-1)
            DO 108 IC=1,3
              AMM(II+IC,JJ+IC) = AMM(II+IC,JJ+IC) + XIJ
108         CONTINUE
107       CONTINUE
106     CONTINUE
101   CONTINUE
C
      DO 183 I = 1,NDL
        RRI = 0.D0
        DO 184 J = 1,NDL
          RRI = RRI + AMM(I,J) * FT(J)
  184   CONTINUE
        AMM(I,I) = RRI
  183 CONTINUE
C
      DO 185 I = 1,NDL
        ZR(IVECTU+I-1) =  AMM(I,I)
  185 CONTINUE
C
      END
