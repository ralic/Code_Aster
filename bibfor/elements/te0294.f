      SUBROUTINE TE0294(OPTION,NOMTE)
      IMPLICIT NONE
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 10/04/2012   AUTEUR DELMAS J.DELMAS 
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
C RESPONSABLE DELMAS J.DELMAS
C
C     BUT:
C         CALCUL DES VECTEURS ELEMENTAIRES
C         OPTION : 'SECM_ZZ1'
C
C ......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER I,K,KP,NNO,NNOS,NPG,NDIM,NBCMP
      INTEGER IPOIDS,IVF,IDFDE,JGANO,IGEOM,ISIEF
      INTEGER IVECT1,IVECT2,IVECT3,IVECT4,IVECT5,IVECT6

      REAL*8 DFDX(27),DFDY(27),DFDZ(27),POIDS,R

      LOGICAL LTEATT,LAXI
C
C ----------------------------------------------------------------------
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
      IF (NDIM.EQ.2) THEN
        NBCMP = 4
      ELSE IF (NDIM.EQ.3) THEN
        NBCMP = 6
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PSIEF_R','L',ISIEF)
      CALL JEVECH('PVECTR1','E',IVECT1)
      CALL JEVECH('PVECTR2','E',IVECT2)
      CALL JEVECH('PVECTR3','E',IVECT3)
      CALL JEVECH('PVECTR4','E',IVECT4)
      IF (NDIM.EQ.3) THEN
        CALL JEVECH('PVECTR5','E',IVECT5)
        CALL JEVECH('PVECTR6','E',IVECT6)
      ENDIF
C
      LAXI = .FALSE.
      IF (LTEATT(' ','AXIS','OUI')) LAXI = .TRUE.

      DO 10 KP=1,NPG
        K=(KP-1)*NNO
        IF (NDIM.EQ.2) THEN
          CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
        ELSE
          CALL DFDM3D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),
     &                                         DFDX,DFDY,DFDZ,POIDS)
        ENDIF
C
        IF (LAXI) THEN
           R = 0.D0
           DO 20 I=1,NNO
             R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
20         CONTINUE
           POIDS = POIDS*R
        ENDIF
C
        DO 30 I=1,NNO
           K=(KP-1)*NNO
           ZR(IVECT1+I-1) = ZR(IVECT1+I-1) + POIDS
     &                    * ZR(IVF+K+I-1) * ZR(ISIEF+NBCMP*(KP-1))
           ZR(IVECT2+I-1) = ZR(IVECT2+I-1) + POIDS
     &                    * ZR(IVF+K+I-1) * ZR(ISIEF+NBCMP*(KP-1)+1)
           ZR(IVECT3+I-1) = ZR(IVECT3+I-1) + POIDS
     &                    * ZR(IVF+K+I-1) * ZR(ISIEF+NBCMP*(KP-1)+2)
           ZR(IVECT4+I-1) = ZR(IVECT4+I-1) + POIDS
     &                    * ZR(IVF+K+I-1) * ZR(ISIEF+NBCMP*(KP-1)+3)
           IF (NDIM.EQ.3) THEN
             ZR(IVECT5+I-1) = ZR(IVECT5+I-1) + POIDS
     &                      * ZR(IVF+K+I-1) * ZR(ISIEF+NBCMP*(KP-1)+4)
             ZR(IVECT6+I-1) = ZR(IVECT6+I-1) + POIDS
     &                      * ZR(IVF+K+I-1) * ZR(ISIEF+NBCMP*(KP-1)+5)
           ENDIF
30      CONTINUE
10    CONTINUE
C
      END
