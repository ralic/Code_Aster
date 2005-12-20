      SUBROUTINE TE0016 ( OPTION, NOMTE )
      IMPLICIT     NONE
      CHARACTER*16        OPTION, NOMTE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C          ELEMENTS ISOPARAMETRIQUES 3D

C          OPTION : 'CHAR_MECA_FORC_R '

C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C
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

      INTEGER      IPOIDS,IVF,IDFDE,IGEOM,IFORC,IRET
      INTEGER      JGANO,NNO,NDL,KP,NPG,II,I,J,L,IVECTU,NDIM,NNOS
      REAL*8       DFDX(27), DFDY(27), DFDZ(27), POIDS, FX, FY, FZ
C     ------------------------------------------------------------------

      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PVECTUR','E',IVECTU)

      NDL = 3*NNO
      DO 20 I = 1,NDL
        ZR(IVECTU+I-1) = 0.0D0
   20 CONTINUE

C     POUR LE CAS DES FORCES VOLUMIQUES
      CALL TECACH('ONN','PNFORCER',1,IFORC,IRET)
      IF (IFORC.NE.0) THEN
        CALL JEVECH('PNFORCER','L',IFORC)

C       BOUCLE SUR LES POINTS DE GAUSS
        DO 50 KP = 1,NPG
          L = (KP-1)*NNO
          CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                  ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )

C         CALCUL DE LA FORCE AUX PG (A PARTIR DES NOEUDS) ---
          FX = 0.0D0
          FY = 0.0D0
          FZ = 0.0D0
          DO 30 I = 1,NNO
            II = 3*(I-1)
            FX = FX + ZR(IVF-1+L+I)*ZR(IFORC+II  )
            FY = FY + ZR(IVF-1+L+I)*ZR(IFORC+II+1)
            FZ = FZ + ZR(IVF-1+L+I)*ZR(IFORC+II+2)
   30     CONTINUE

          DO 40 I = 1,NNO
            II = 3* (I-1)
            ZR(IVECTU+II  ) = ZR(IVECTU+II  ) + POIDS*ZR(IVF+L+I-1)*FX
            ZR(IVECTU+II+1) = ZR(IVECTU+II+1) + POIDS*ZR(IVF+L+I-1)*FY
            ZR(IVECTU+II+2) = ZR(IVECTU+II+2) + POIDS*ZR(IVF+L+I-1)*FZ
   40     CONTINUE
   50   CONTINUE

      ELSE

        CALL JEVECH('PFR3D3D','L',IFORC)

C    BOUCLE SUR LES POINTS DE GAUSS

        DO 80 KP = 1,NPG

          L = (KP-1)*NNO
          CALL DFDM3D ( NNO, KP, IPOIDS, IDFDE,
     &                  ZR(IGEOM), DFDX, DFDY, DFDZ, POIDS )

          DO 70 I = 1,NNO
            II = 3* (I-1)

            DO 60 J = 1,3
              ZR(IVECTU+II+J-1) = ZR(IVECTU+II+J-1) +
     &                            POIDS*ZR(IVF+L+I-1)*ZR(IFORC+J-1)
   60       CONTINUE

   70     CONTINUE

   80   CONTINUE

      END IF

      END
