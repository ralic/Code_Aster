      SUBROUTINE TE0084 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/01/2013   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INCLUDE 'jeveux.h'

      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CHAR_MECA_ROTA_R'
C                          2D PLAN ET AXISYMETRIQUE
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*16       PHENOM
      INTEGER ICODRE
      REAL*8             DFDX(9),DFDY(9),POIDS,RX,RY,R8MIEM
      INTEGER            NNO,KP,K,NPG1,I,IVECTU,IROTA,JGANO,NDIM,NNOS
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE
      LOGICAL            LTEATT
C
C
C-----------------------------------------------------------------------
      REAL*8 R8B ,RHO
C-----------------------------------------------------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PROTATR','L',IROTA)
C
C VERIFICATIONS SUR LE CHARGEMENT ROTATION
C
      IF(NOMTE(3:4).EQ.'DP'.OR.NOMTE(3:4).EQ.'CP') THEN
C AXE=direction Oz
         IF(ZR(IROTA+3).LE.R8MIEM()) THEN
            CALL U2MESS('F','MODELISA9_99')
         END IF
         IF(ZR(IROTA+1).GT.R8MIEM().OR.ZR(IROTA+2).GT.R8MIEM()) THEN
            CALL U2MESS('F','MODELISA10_3')
         END IF
      ELSEIF(NOMTE(3:4).EQ.'AX') THEN
C AXE=Oy et CENTRE=ORIGINE
         IF(ZR(IROTA+1).GT.R8MIEM().OR.ZR(IROTA+3).GT.R8MIEM()) THEN
            CALL U2MESS('F','MODELISA10_1')
         END IF
         IF(ZR(IROTA+4).GT.R8MIEM().OR.ZR(IROTA+5).GT.R8MIEM()
     &     .OR.ZR(IROTA+6).GT.R8MIEM()) THEN
            CALL U2MESS('F','MODELISA10_2')
         END IF
      ENDIF
      CALL JEVECH('PVECTUR','E',IVECTU)
C
      CALL RCCOMA(ZI(IMATE),'ELAS',1,PHENOM,ICODRE)
      CALL RCVALB('FPG1',1,1,'+',ZI(IMATE),' ',PHENOM,0,' ',R8B,
     &              1,'RHO',RHO,ICODRE,1)
C
      DO 101 KP=1,NPG1
         K=(KP-1)*NNO
         CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
         POIDS = POIDS * RHO * ZR(IROTA)**2
         RX= 0.D0
         RY= 0.D0
         DO 102 I=1,NNO
           RX= RX+ ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
           RY= RY+ ZR(IGEOM+2*I-1)*ZR(IVF+K+I-1)
102      CONTINUE
         IF ( LTEATT(' ','AXIS','OUI') ) THEN
           POIDS = POIDS*RX
           DO 103 I=1,NNO
             K=(KP-1)*NNO
             ZR(IVECTU+2*I-2) = ZR(IVECTU+2*I-2) +
     &                             POIDS*ZR(IROTA+2)**2*RX*ZR(IVF+K+I-1)
103        CONTINUE
         ELSE
           RX = RX - ZR(IROTA+4)
           RY = RY - ZR(IROTA+5)
           DO 104 I=1,NNO
             K=(KP-1)*NNO
             ZR(IVECTU+2*I-2) = ZR(IVECTU+2*I-2) +
     &                             POIDS*ZR(IROTA+3)**2*RX*ZR(IVF+K+I-1)
             ZR(IVECTU+2*I-1) = ZR(IVECTU+2*I-1) +
     &                             POIDS*ZR(IROTA+3)**2*RY*ZR(IVF+K+I-1)
104        CONTINUE
         ENDIF
101   CONTINUE
      END
