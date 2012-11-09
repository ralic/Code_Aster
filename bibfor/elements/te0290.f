      SUBROUTINE TE0290 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C                          OPTION : 'CALC_NOEU_BORD  '
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      REAL*8             COOR(8),DX(4),DY(4),NX(9),NY(9),SENS
C
C
C-----------------------------------------------------------------------
      INTEGER I ,IDFDE ,IGEOM ,IPOIDS ,IVECTU ,IVF ,JGANO
      INTEGER NDIM ,NNO ,NPG ,NSOM
C-----------------------------------------------------------------------
      CALL JEMARQ()

      CALL ELREF4(' ','RIGI',NDIM,NNO,NSOM,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PVECTUR','E',IVECTU)
      DO 1 I=1,NSOM
        COOR(2*I-1) = ZR(IGEOM+2*(I-1))
        COOR(2*I)   = ZR(IGEOM+2*I-1)
1     CONTINUE
      DO 2 I=1,NSOM-1
        DX(I) = COOR(2*I+1)-COOR(2*I-1)
        DY(I) = COOR(2*I+2)-COOR(2*I)
2     CONTINUE
      DX(NSOM) = COOR(1)-COOR(2*NSOM-1)
      DY(NSOM) = COOR(2)-COOR(2*NSOM)
C
C   INITIALISATION A 0.
C
      DO 3 I=1,NNO
        ZR(IVECTU+2*I-2) = 0.D0
        ZR(IVECTU+2*I-1) = 0.D0
        NX(I) = 0.D0
        NY(I) = 0.D0
3     CONTINUE
      NX(1)  =  (DY(NSOM)+DY(1))
      NY(1)  = -(DX(NSOM)+DX(1))
      DO 4 I=2,NSOM
        NX(I)  =  (DY(I-1)+DY(I))
        NY(I)  = -(DX(I-1)+DX(I))
4     CONTINUE
      IF(NNO.NE.NSOM) THEN
        DO 6 I=NSOM+1,2*NSOM
          NX(I) =  DY(I-NSOM)
          NY(I) = -DX(I-NSOM)
6       CONTINUE
      ENDIF
C
C   VERIFICATION DU SENS DE L'ELEMENT
C
      SENS = DY(1)*DX(NSOM)-DX(1)*DY(NSOM)
      IF(SENS.EQ.0.D0) THEN
        CALL U2MESS('F','ELEMENTS3_67')
      ELSE IF(SENS.LT.0.D0) THEN
        DO 7 I=1,NNO
          NX(I) = -NX(I)
          NY(I) = -NY(I)
7       CONTINUE
      ENDIF
C
      DO 5 I=1,NNO
        ZR(IVECTU+2*I-2) = NX(I)
        ZR(IVECTU+2*I-1) = NY(I)
5     CONTINUE
      CALL JEDEMA()
      END
