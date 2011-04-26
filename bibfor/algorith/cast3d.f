      SUBROUTINE CAST3D(PROJ,GAMMA,DH,DEF,NNO,KPG,NUB,NU,
     &                  DSIDEP,CALBN,BN,JAC,MATUU)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C     CALCUL DES TERMES DE STABILISATION POUR LE HEXA8 SOUS INTEGRE
C     STABILISE PAR LA METHODE ASSUMED STRAIN => HEXAS8
C-----------------------------------------------------------------------

      IMPLICIT NONE

      LOGICAL CALBN
      INTEGER KPG,I,J,K,NNO,PROJ,IC,IADPG
      REAL*8 DSIDEP(6,6),BN(6,3,8)
      REAL*8 GAMMA(4,8),DH(4,24)
      REAL*8 JAC,XF(8),YF(8),ZF(8)
      REAL*8 MATUU(*)
      REAL*8 NUB,NU,UNT,DEUT,DEF(6,3,8)
      REAL*8 X12(8),Y12(8),X13(8),Z13(8),Y23(8),Z23(8)
      REAL*8 X14(8),Y24(8),Z34(8)
      REAL*8 X2(8),Y1(8),X3(8),Z1(8),Y3(8),Z2(8)

C    PROJ : INDICATEUR DE LA PROJECTION
C           0 AUCUNE
C           1 ADS
C           2 ASBQI

      IADPG = 3*(KPG-1)
C
C   CALCUL DE TERMES INTERMEDIAIRES

      DO 1 I = 1,NNO
        XF(I) = 0.D0
        YF(I) = 0.D0
        ZF(I) = 0.D0
1     CONTINUE
C
      DO 2 IC = 1,4
        DO 3 I = 1,NNO
          XF(I) = XF(I) + DH(IC,IADPG+1) * GAMMA(IC,I)
          YF(I) = YF(I) + DH(IC,IADPG+2) * GAMMA(IC,I)
          ZF(I) = ZF(I) + DH(IC,IADPG+3) * GAMMA(IC,I)
3       CONTINUE
2     CONTINUE
C
      DO 4 I = 1,6
        DO 4 J = 1,3
          DO 4 K = 1,NNO
            BN(I,J,K) =0.D0
4     CONTINUE
C
C         HEXAS8 SANS PROJECTION
C         ----------------------
C
      IF (PROJ.EQ.0) THEN
        DO 5 I = 1,NNO
          BN(1,1,I) = XF(I)
          BN(2,2,I) = YF(I)
          BN(3,3,I) = ZF(I)
          BN(4,1,I) = YF(I)
          BN(5,1,I) = ZF(I)
          BN(4,2,I) = XF(I)
          BN(6,2,I) = ZF(I)
          BN(5,3,I) = XF(I)
          BN(6,3,I) = YF(I)
5       CONTINUE
C
      ELSE IF (PROJ.EQ.1.OR.PROJ.EQ.2) THEN
      DO 100 I =1,8
         X12(I) = 0.D0
         Y12(I) = 0.D0
         X13(I) = 0.D0
         Z13(I) = 0.D0
         Y23(I) = 0.D0
         Z23(I) = 0.D0
100   CONTINUE
C
C   CALCUL DE X12 Y12 Y13 Z13 Y23 Z23
C
      DO 6 IC = 1,2
        DO 7 I = 1,NNO
          X12(I) = X12(I) + DH(IC,IADPG+1) * GAMMA(IC,I)
          Y12(I) = Y12(I) + DH(IC,IADPG+2) * GAMMA(IC,I)
7       CONTINUE
6     CONTINUE
C
      DO 8 IC = 1,3,2
        DO 9 I = 1,NNO
          X13(I) = X13(I) + DH(IC,IADPG+1) * GAMMA(IC,I)
          Z13(I) = Z13(I) + DH(IC,IADPG+3) * GAMMA(IC,I)
9       CONTINUE
8     CONTINUE
C
      DO 10 IC = 2,3
        DO 11 I = 1,NNO
          Y23(I) = Y23(I) + DH(IC,IADPG+2) * GAMMA(IC,I)
          Z23(I) = Z23(I) + DH(IC,IADPG+3) * GAMMA(IC,I)
11      CONTINUE
10    CONTINUE
C
C    ADS
C
      IF (PROJ.EQ.1) THEN
        UNT  = 1.D0/3.D0
        DEUT = 2.D0/3.D0
        DO 12 I = 1,NNO
          BN(1,1,I) = DEUT * XF(I)
          BN(2,2,I) = DEUT * YF(I)
          BN(3,3,I) = DEUT * ZF(I)
          BN(2,1,I) = -UNT * XF(I)
          BN(3,1,I) = BN(2,1,I)
          BN(1,2,I) = -UNT * YF(I)
          BN(3,2,I) = BN(1,2,I)
          BN(1,3,I) = -UNT * ZF(I)
          BN(2,3,I) = BN(1,3,I)
          BN(4,1,I) = Y12(I)
          BN(4,2,I) = X12(I)
          BN(5,1,I) = Z13(I)
          BN(5,3,I) = X13(I)
          BN(6,2,I) = Z23(I)
          BN(6,3,I) = Y23(I)
12      CONTINUE
C
C   ASQBI
C
      ELSE IF (PROJ.EQ.2) THEN
      DO 200 I =1,8
         X14(I)= 0.D0
         Y24(I)= 0.D0
         Z34(I)= 0.D0
200   CONTINUE
C
      DO 13 IC = 1,4,3
        DO 14 I = 1,NNO
          X14(I) = X14(I) + DH(IC,IADPG+1) * GAMMA(IC,I)
14       CONTINUE
13     CONTINUE
C
      DO 15 IC = 2,4,2
        DO 16 I = 1,NNO
          Y24(I) = Y24(I) + DH(IC,IADPG+2) * GAMMA(IC,I)
16       CONTINUE
15     CONTINUE
C
      DO 17 IC = 3,4
        DO 18 I = 1,NNO
          Z34(I) = Z34(I) + DH(IC,IADPG+3) * GAMMA(IC,I)
18      CONTINUE
17    CONTINUE
C
        DO 19 I = 1,NNO
          X2(I) = DH(2,IADPG+1) * GAMMA(2,I)
          X3(I) = DH(3,IADPG+1) * GAMMA(3,I)
          Y1(I) = DH(1,IADPG+2) * GAMMA(1,I)
          Y3(I) = DH(3,IADPG+2) * GAMMA(3,I)
          Z1(I) = DH(1,IADPG+3) * GAMMA(1,I)
          Z2(I) = DH(2,IADPG+3) * GAMMA(2,I)
19      CONTINUE
C
        DO 30 I = 1,NNO
          BN(1,1,I) = XF(I)
          BN(2,1,I) = -NUB * X3(I) - NU * X14(I)
          BN(3,1,I) = -NUB * X2(I) - NU * X14(I)
          BN(4,1,I) = Y12(I)
          BN(5,1,I) = Z13(I)
          BN(6,1,I) = 0.0D0
          BN(1,2,I) = -NUB * Y3(I) - NU * Y24(I)
          BN(2,2,I) = YF(I)
          BN(3,2,I) = -NUB * Y1(I) - NU * Y24(I)
          BN(4,2,I) = X12(I)
          BN(5,2,I) = 0.0D0
          BN(6,2,I) = Z23(I)
          BN(1,3,I) = -NUB * Z2(I) - NU * Z34(I)
          BN(2,3,I) = -NUB * Z1(I) - NU * Z34(I)
          BN(3,3,I) = ZF(I)
          BN(4,3,I) = 0.0D0
          BN(5,3,I) = X13(I)
          BN(6,3,I) = Y23(I)
30      CONTINUE
      ENDIF
      ENDIF
      IF(.NOT.CALBN) THEN
        CALL CAATDB(NNO,BN,DSIDEP,BN,JAC,MATUU)
        CALL CAATDB(NNO,BN,DSIDEP,DEF,JAC,MATUU)
        CALL CAATDB(NNO,DEF,DSIDEP,BN,JAC,MATUU)
      ENDIF
      END
