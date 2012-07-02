      SUBROUTINE TE0199 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CHAR_MECA_FR2D2D  '
C                          ELEMENTS AXISYMETRIQUES FOURIER
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      REAL*8             DFDX(9),DFDY(9),POIDS,R
      INTEGER            NNO,KP,NPG1,I,IFR2D,IVECTU,NNOS,NDIM,JGANO
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM
C
C
C-----------------------------------------------------------------------
      INTEGER K ,L 
C-----------------------------------------------------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PFR2D2D','L',IFR2D)
      CALL JEVECH('PVECTUR','E',IVECTU)
C
      DO 101 KP=1,NPG1
        K=(KP-1)*NNO
        CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
C
           R = 0.D0
           DO 102 I=1,NNO
             R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
102        CONTINUE
           POIDS = POIDS*R
C
        DO 103 I=1,NNO
           K=(KP-1)*NNO
           L=(KP-1)*3
           ZR(IVECTU+3*I-3) = ZR(IVECTU+3*I-3) +
     &        POIDS * ZR(IFR2D+L  ) * ZR(IVF+K+I-1)
           ZR(IVECTU+3*I-2) = ZR(IVECTU+3*I-2) +
     &        POIDS * ZR(IFR2D+L+1) * ZR(IVF+K+I-1)
           ZR(IVECTU+3*I-1) = ZR(IVECTU+3*I-1) +
     &        POIDS * ZR(IFR2D+L+2) * ZR(IVF+K+I-1)
103     CONTINUE
101   CONTINUE
      END
