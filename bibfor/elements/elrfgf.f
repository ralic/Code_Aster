      SUBROUTINE ELRFGF(ELREFZ,NUFAPG,NBPG,DCOO,COOPG,DPOI,POIPG)
      IMPLICIT NONE
      INTEGER NUFAPG,NBPG(*),DCOO,DPOI
      REAL*8 COOPG(*),POIPG(*)
      CHARACTER*(*) ELREFZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/10/2002   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20

C BUT: CALCUL DES POIDS ET POINTS DE GAUSS

C ----------------------------------------------------------------------
C   IN   ELREFZ : NOM DE L'ELREFE (K8)
C        NUFAPG : NUMERO DE LA FAMILLE DE PTS DE GAUSS
C        NBPG   : NOMBRE DE POINTS DE GAUSS
C        DCOO   : DIMENSIONDE COOPG
C        DPOI   : DIMENSION DE POIPG
C   OUT  COOPG  : COORDONNEES DES POINTS DE GAUSS
C        POIPG  : POIDS DES POINTS DE GAUSS
C   -------------------------------------------------------------------
      CHARACTER*8 ELREFE
      INTEGER I,NPG,NCMP,NPAR,NPI,IX,IY,IZ,NPX,NPYZ
      INTEGER NDIM,NNO,NNOS,NBFPG,NBPG1(10),II
      REAL*8 XPG(27),YPG(27),ZPG(27),HPG(27),A(3),H(3)
      REAL*8 ATY(7),ATZ(7),HT(7)
      REAL*8 AA,BB,CC,HH,H1,H2,H3,RAC5,RAC15,A1,B1,B6,C1,C8,D1,D12
      REAL*8 U,X(27),XA,XB,XC,XD,P1,P2,P3,T
      REAL*8 ZERO,UNQUAR,UNDEMI,UN,DEUX

C -----  FONCTIONS FORMULES
      T(U) = 2.0D0*U - 1.0D0
C DEB ------------------------------------------------------------------

      ELREFE = ELREFZ
      ZERO = 0.0D0
      UNQUAR = 0.25D0
      UNDEMI = 0.5D0
      UN = 1.0D0
      DEUX = 2.0D0
      NPG = NBPG(NUFAPG)

C     ------------------------------------------------------------------
      IF (ELREFE.EQ.'HEXA8' .OR. ELREFE.EQ.'HEXI8' .OR.
     &    ELREFE.EQ.'HEXA20' .OR. ELREFE.EQ.'HEXS20' .OR.
     &    ELREFE.EQ.'HEXD20' .OR. ELREFE.EQ.'HEXI20' .OR.
     &    ELREFE.EQ.'HEXA27') THEN

        NCMP = 3

        IF (NPG.EQ.8) THEN

C --------- FORMULE DE QUADRATURE DE GAUSS A 2 POINTS DANS CHAQUE
C           DIRECTION ( ORDRE 3 )

          NPAR = 2
          A(1) = -0.577350269189626D00
          A(2) = -A(1)
          H(1) = UN
          H(2) = UN

        ELSE IF (NPG.EQ.27) THEN

C --------- FORMULE DE QUADRATURE DE GAUSS A 3 POINTS DANS CHAQUE
C           DIRECTION ( ORDRE 5 )

          NPAR = 3
          A(1) = -0.774596669241483D00
          A(2) = ZERO
          A(3) = -A(1)
          H(1) = 0.555555555555556D00
          H(2) = 0.888888888888889D00
          H(3) = H(1)
        END IF

        NPI = 0
        DO 30 IX = 1,NPAR
          DO 20 IY = 1,NPAR
            DO 10 IZ = 1,NPAR
              NPI = NPI + 1
              XPG(NPI) = A(IX)
              YPG(NPI) = A(IY)
              ZPG(NPI) = A(IZ)
              HPG(NPI) = H(IX)*H(IY)*H(IZ)
   10       CONTINUE
   20     CONTINUE
   30   CONTINUE
C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'PENTA6' .OR. ELREFE.EQ.'PENTA15' .OR.
     &         ELREFE.EQ.'PENTA15D' .OR. ELREFE.EQ.'PENTA18') THEN

        NCMP = 3

        IF (NPG.EQ.6) THEN

          NPX = 2
          NPYZ = 3
          A(1) = -0.577350269189626D00
          A(2) = 0.577350269189626D00
          ATY(1) = UNDEMI
          ATY(2) = ZERO
          ATY(3) = UNDEMI
          ATZ(1) = UNDEMI
          ATZ(2) = UNDEMI
          ATZ(3) = ZERO
          H(1) = UN
          H(2) = UN
          HT(1) = 0.166666666666667D00
          HT(2) = HT(1)
          HT(3) = HT(1)

        ELSE IF (NPG.EQ.8) THEN

C --------- FORMULE A 4 * 2 POINTS :  (CF TOUZOT PAGE 297)
C                   2 POINTS DE GAUSS  EN X   (ORDRE 3)
C                   4 POINTS DE HAMMER EN Y Z (ORDRE 3 EN Y Z)

C --------- FORMULE DE GAUSS

          NPX = 2
          A(1) = -0.577350269189626D00
          A(2) = -A(1)
          H(1) = UN
          H(2) = UN

C --------- FORMULE DE HAMMER

          NPYZ = 4
          ATY(1) = 0.333333333333333D00
          ATY(2) = 0.6D00
          ATY(3) = 0.2D00
          ATY(4) = 0.2D00
          ATZ(1) = 0.333333333333333D00
          ATZ(2) = 0.2D00
          ATZ(3) = 0.6D00
          ATZ(4) = 0.2D00
          HT(1) = -27.D00/96.D00
          HT(2) = 25.D00/96.D00
          HT(3) = HT(2)
          HT(4) = HT(2)

        ELSE IF (NPG.EQ.21) THEN

C --------- FORMULE A 7 * 3 POINTS :   (CF TOUZOT PAGE 298)
C                   3 POINTS DE GAUSS EN X (ORDRE 5)
C                   7 POINTS DE HAMMER EN Y Z (ORDRE 5 EN Y Z)

C --------- FORMULE DE GAUSS

          NPX = 3
          A(1) = -0.774596669241483D00
          A(2) = ZERO
          A(3) = -A(1)
          H(1) = 0.555555555555556D00
          H(2) = 0.888888888888889D00
          H(3) = H(1)

C --------- FORMULE DE HAMMER

          NPYZ = 7
          ATY(1) = 0.333333333333333D00
          ATZ(1) = 0.333333333333333D00
          ATY(2) = 0.470142064105115D00
          ATZ(2) = 0.470142064105115D00
          ATY(3) = UN - DEUX*ATY(2)
          ATZ(3) = 0.470142064105115D00
          ATY(4) = 0.470142064105115D00
          ATZ(4) = UN - DEUX*ATY(2)
          ATY(5) = 0.101286507323456D00
          ATZ(5) = 0.101286507323456D00
          ATY(6) = UN - DEUX*ATY(5)
          ATZ(6) = 0.101286507323456D00
          ATY(7) = 0.101286507323456D00
          ATZ(7) = UN - DEUX*ATY(5)
          HT(1) = 9.D00/80.D00
          HT(2) = 0.0661970763942530D00
          HT(3) = HT(2)
          HT(4) = HT(2)
          HT(5) = 0.0629695902724135D00
          HT(6) = HT(5)
          HT(7) = HT(5)
        END IF

        NPI = 0
        DO 50 IX = 1,NPX
          DO 40 IY = 1,NPYZ
            NPI = NPI + 1
            XPG(NPI) = A(IX)
            YPG(NPI) = ATY(IY)
            ZPG(NPI) = ATZ(IY)
            HPG(NPI) = H(IX)*HT(IY)
   40     CONTINUE
   50   CONTINUE

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TETRA4' .OR. ELREFE.EQ.'TETRA10' .OR.
     &         ELREFE.EQ.'TETRA10D' .OR. ELREFE.EQ.'TETRI4' .OR.
     &         ELREFE.EQ.'TETRI10') THEN

        NCMP = 3

        IF (NPG.EQ.4) THEN

C --------- FORMULE A 4 POINTS :  (CF TOUZOT PAGE 300)
C                   ORDRE 2 EN X Y Z

          RAC5 = SQRT(5.D00)
          AA = (5.D00-RAC5)/20.D00
          BB = (5.D00+3.D00*RAC5)/20.D00
          HH = UN/24.D00
          NPI = 0
          DO 60 I = 1,4
            NPI = NPI + 1
            XPG(NPI) = AA
            YPG(NPI) = AA
            ZPG(NPI) = AA
            HPG(NPI) = HH
   60     CONTINUE
          ZPG(2) = BB
          YPG(3) = BB
          XPG(4) = BB

        ELSE IF (NPG.EQ.5) THEN

C --------- FORMULE A 5 POINTS :  (CF TOUZOT PAGE 300)
C                   ORDRE 3 EN X Y Z

          AA = 0.25D00
          BB = UN/6.D00
          CC = UNDEMI
          H1 = -DEUX/15.D00
          H2 = 3.D00/40.D00
          XPG(1) = AA
          YPG(1) = AA
          ZPG(1) = AA
          HPG(1) = H1
          DO 70 I = 2,5
            XPG(I) = BB
            YPG(I) = BB
            ZPG(I) = BB
            HPG(I) = H2
   70     CONTINUE
          ZPG(3) = CC
          YPG(4) = CC
          XPG(5) = CC

        ELSE IF (NPG.EQ.15) THEN

C --------- FORMULE A 15 POINTS :  (CF TOUZOT PAGE 300)
C                   ORDRE 5 EN X Y Z

          RAC15 = SQRT(15.0D00)
          XPG(1) = 0.25D00
          YPG(1) = 0.25D00
          ZPG(1) = 0.25D00
          HPG(1) = 8.0D00/405.0D00

          XPG(2) = (7.0D00+RAC15)/34.0D00
          YPG(2) = XPG(2)
          ZPG(2) = XPG(2)
          HPG(2) = (2665.0D00-14.0D00*RAC15)/226800.0D00
          XPG(3) = XPG(2)
          YPG(3) = XPG(2)
          ZPG(3) = (13.0D00-3.0D00*RAC15)/34.0D00
          HPG(3) = HPG(2)
          XPG(4) = XPG(2)
          YPG(4) = (13.0D00-3.0D00*RAC15)/34.0D00
          ZPG(4) = XPG(2)
          HPG(4) = HPG(2)
          XPG(5) = (13.0D00-3.0D00*RAC15)/34.0D00
          YPG(5) = XPG(2)
          ZPG(5) = XPG(2)
          HPG(5) = HPG(2)

          XPG(6) = (7.0D00-RAC15)/34.0D00
          YPG(6) = XPG(6)
          ZPG(6) = XPG(6)
          HPG(6) = (2665.0D00+14.0D00*RAC15)/226800.0D00
          XPG(7) = XPG(6)
          YPG(7) = XPG(6)
          ZPG(7) = (13.0D00+3.0D00*RAC15)/34.0D00
          HPG(7) = HPG(6)
          XPG(8) = XPG(6)
          YPG(8) = (13.0D00+3.0D00*RAC15)/34.0D00
          ZPG(8) = XPG(6)
          HPG(8) = HPG(6)
          XPG(9) = (13.0D00+3.0D00*RAC15)/34.0D00
          YPG(9) = XPG(6)
          ZPG(9) = XPG(6)
          HPG(9) = HPG(6)

          XPG(10) = (5.0D00-RAC15)/20.0D00
          YPG(10) = XPG(10)
          ZPG(10) = (5.0D00+RAC15)/20.0D00
          HPG(10) = 5.0D00/567.0D00
          XPG(11) = XPG(10)
          YPG(11) = (5.0D00+RAC15)/20.0D00
          ZPG(11) = XPG(10)
          HPG(11) = 5.0D00/567.0D00
          XPG(12) = (5.0D00+RAC15)/20.0D00
          YPG(12) = XPG(10)
          ZPG(12) = XPG(10)
          HPG(12) = 5.0D00/567.0D00

          XPG(13) = XPG(10)
          YPG(13) = (5.0D00+RAC15)/20.0D00
          ZPG(13) = YPG(13)
          HPG(13) = 5.0D00/567.0D00
          XPG(14) = YPG(13)
          YPG(14) = XPG(10)
          ZPG(14) = YPG(13)
          HPG(14) = 5.0D00/567.0D00
          XPG(15) = YPG(13)
          YPG(15) = YPG(13)
          ZPG(15) = XPG(10)
          HPG(15) = 5.0D00/567.0D00
        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'PYRAM5' .OR. ELREFE.EQ.'PYRAM13D' .OR.
     &         ELREFE.EQ.'PYRAM13') THEN

        NCMP = 3

        IF (NPG.EQ.5) THEN

          P1 = 0.1333333333333333D0
          H1 = 0.1531754163448146D0
          H2 = 0.6372983346207416D0

          XPG(1) = UNDEMI
          XPG(2) = ZERO
          XPG(3) = -UNDEMI
          XPG(4) = ZERO
          XPG(5) = ZERO

          YPG(1) = ZERO
          YPG(2) = UNDEMI
          YPG(3) = ZERO
          YPG(4) = -UNDEMI
          YPG(5) = ZERO

          ZPG(1) = H1
          ZPG(2) = H1
          ZPG(3) = H1
          ZPG(4) = H1
          ZPG(5) = H2

          HPG(1) = P1
          HPG(2) = P1
          HPG(3) = P1
          HPG(4) = P1
          HPG(5) = P1

        ELSE IF (NPG.EQ.6) THEN

          P1 = 0.1024890634400000D0
          P2 = 0.1100000000000000D0
          P3 = 0.1467104129066667D0

          AA = 0.5702963741068025D0
          H1 = 0.1666666666666667D0
          H2 = 0.8063183038464675D-1
          H3 = 0.6098484849057127D0

          XPG(1) = AA
          XPG(2) = ZERO
          XPG(3) = -AA
          XPG(4) = ZERO
          XPG(5) = ZERO
          XPG(6) = ZERO

          YPG(1) = ZERO
          YPG(2) = AA
          YPG(3) = ZERO
          YPG(4) = -AA
          YPG(5) = ZERO
          YPG(6) = ZERO

          ZPG(1) = H1
          ZPG(2) = H1
          ZPG(3) = H1
          ZPG(4) = H1
          ZPG(5) = H2
          ZPG(6) = H3

          HPG(1) = P1
          HPG(2) = P1
          HPG(3) = P1
          HPG(4) = P1
          HPG(5) = P2
          HPG(6) = P3

C --- POUR L'INSTANT L'INTEGRATION AVEC 27 POINTS N'EST PAS UTLISEE
C     -------------------------------------------------------------
        ELSE IF (NPG.EQ.27) THEN

          A1 = 0.788073483D0
          B6 = 0.499369002D0
          B1 = 0.848418011D0
          C8 = 0.478508449D0
          C1 = 0.652816472D0
          D12 = 0.032303742D0
          D1 = 1.106412899D0

          ZPG(1) = UNDEMI
          ZPG(2) = UNDEMI
          ZPG(3) = UNDEMI
          ZPG(4) = UNDEMI
          ZPG(5) = UNDEMI
          ZPG(6) = UNDEMI* (UN-B1)
          ZPG(7) = UNDEMI* (UN+B1)
          ZPG(8) = UNDEMI* (UN-C1)
          ZPG(9) = UNDEMI* (UN-C1)
          ZPG(10) = UNDEMI* (UN-C1)
          ZPG(11) = UNDEMI* (UN-C1)
          ZPG(12) = UNDEMI* (UN+C1)
          ZPG(13) = UNDEMI* (UN+C1)
          ZPG(14) = UNDEMI* (UN+C1)
          ZPG(15) = UNDEMI* (UN+C1)
          ZPG(16) = UNDEMI* (UN-D1)
          ZPG(17) = UNDEMI* (UN-D1)
          ZPG(18) = UNDEMI* (UN-D1)
          ZPG(19) = UNDEMI* (UN-D1)
          ZPG(20) = UNDEMI
          ZPG(21) = UNDEMI
          ZPG(22) = UNDEMI
          ZPG(23) = UNDEMI
          ZPG(24) = UNDEMI* (UN+D1)
          ZPG(25) = UNDEMI* (UN+D1)
          ZPG(26) = UNDEMI* (UN+D1)
          ZPG(27) = UNDEMI* (UN+D1)

          XPG(1) = ZERO
          XPG(2) = UNDEMI*B1* (UN-ZPG(2))
          XPG(3) = -UNDEMI*B1* (UN-ZPG(3))
          XPG(4) = -UNDEMI*B1* (UN-ZPG(4))
          XPG(5) = UNDEMI*B1* (UN-ZPG(5))
          XPG(6) = ZERO
          XPG(7) = ZERO
          XPG(8) = C1* (UN-ZPG(8))
          XPG(9) = ZERO
          XPG(10) = -C1* (UN-ZPG(10))
          XPG(11) = ZERO
          XPG(12) = C1* (UN-ZPG(12))
          XPG(13) = ZERO
          XPG(14) = -C1* (UN-ZPG(14))
          XPG(15) = ZERO
          XPG(16) = UNDEMI*D1* (UN-ZPG(16))
          XPG(17) = -UNDEMI*D1* (UN-ZPG(17))
          XPG(18) = -UNDEMI*D1* (UN-ZPG(18))
          XPG(19) = UNDEMI*D1* (UN-ZPG(19))
          XPG(20) = D1* (UN-ZPG(20))
          XPG(21) = ZERO
          XPG(22) = -D1* (UN-ZPG(22))
          XPG(23) = ZERO
          XPG(24) = UNDEMI*D1* (UN-ZPG(24))
          XPG(25) = -UNDEMI*D1* (UN-ZPG(25))
          XPG(26) = -UNDEMI*D1* (UN-ZPG(26))
          XPG(27) = UNDEMI*D1* (UN-ZPG(27))

          YPG(1) = ZERO
          YPG(2) = XPG(2)
          YPG(3) = -XPG(3)
          YPG(4) = XPG(4)
          YPG(5) = -XPG(5)
          YPG(6) = XPG(6)
          YPG(7) = XPG(7)
          YPG(8) = ZERO
          YPG(9) = C1* (UN-ZPG(9))
          YPG(10) = ZERO
          YPG(11) = -C1* (UN-ZPG(11))
          YPG(12) = ZERO
          YPG(13) = C1* (UN-ZPG(13))
          YPG(14) = ZERO
          YPG(15) = -C1* (UN-ZPG(15))
          YPG(16) = XPG(16)
          YPG(17) = -XPG(17)
          YPG(18) = XPG(18)
          YPG(19) = -XPG(19)
          YPG(20) = ZERO
          YPG(21) = D1* (UN-ZPG(21))
          YPG(22) = ZERO
          YPG(23) = -D1* (UN-ZPG(23))
          YPG(24) = XPG(24)
          YPG(25) = -XPG(25)
          YPG(26) = XPG(26)
          YPG(27) = -XPG(27)

          HPG(1) = A1
          HPG(2) = B6
          HPG(3) = B6
          HPG(4) = B6
          HPG(5) = B6
          HPG(6) = B6
          HPG(7) = B6
          HPG(8) = C8
          HPG(9) = C8
          HPG(10) = C8
          HPG(11) = C8
          HPG(12) = C8
          HPG(13) = C8
          HPG(14) = C8
          HPG(15) = C8
          HPG(16) = D12
          HPG(17) = D12
          HPG(18) = D12
          HPG(19) = D12
          HPG(20) = D12
          HPG(21) = D12
          HPG(22) = D12
          HPG(23) = D12
          HPG(24) = D12
          HPG(25) = D12
          HPG(26) = D12
          HPG(27) = D12

          DO 80 I = 1,27
            HPG(I) = HPG(I)*UNQUAR* (UN-ZPG(I))* (UN-ZPG(I))
   80     CONTINUE
        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'FACE4' .OR. ELREFE.EQ.'FACE8' .OR.
     &         ELREFE.EQ.'FACE9') THEN

        NCMP = 2

        IF (NPG.EQ.4) THEN
          NPAR = 2
          A(1) = -0.577350269189626D00
          A(2) = -A(1)
          H(1) = UN
          H(2) = UN
        ELSE IF (NPG.EQ.9) THEN
          NPAR = 3
          A(1) = -0.774596669241483D00
          A(2) = ZERO
          A(3) = -A(1)
          H(1) = 0.555555555555556D00
          H(2) = 0.888888888888889D00
          H(3) = H(1)
        END IF
        NPI = 0
        DO 100 IX = 1,NPAR
          DO 90 IY = 1,NPAR
            NPI = NPI + 1
            XPG(NPI) = A(IX)
            YPG(NPI) = A(IY)
            HPG(NPI) = H(IX)*H(IY)
   90     CONTINUE
  100   CONTINUE

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'FACE3') THEN

        NCMP = 2

        XPG(1) = UNDEMI
        XPG(2) = ZERO
        XPG(3) = UNDEMI
        YPG(1) = UNDEMI
        YPG(2) = UNDEMI
        YPG(3) = ZERO
        HPG(1) = UN/6.D00
        HPG(2) = UN/6.D00
        HPG(3) = UN/6.D00

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'FACE6') THEN

        NCMP = 2

        IF (NPG.EQ.4) THEN

          XPG(1) = 0.333333333333333D00
          XPG(2) = 0.2D00
          XPG(3) = 0.6D00
          XPG(4) = 0.2D00
          YPG(1) = 0.333333333333333D00
          YPG(2) = 0.2D00
          YPG(3) = 0.2D00
          YPG(4) = 0.6D00
          HPG(1) = -27.D00/96.D00
          HPG(2) = 25.D00/96.D00
          HPG(3) = 25.D00/96.D00
          HPG(4) = 25.D00/96.D00

        ELSE IF (NPG.EQ.6) THEN

          P1 = 0.111690794839005D0
          P2 = 0.054975871827661D0
          XA = 0.445948490915965D0
          XB = 0.091576213509771D0

          HPG(1) = P1
          HPG(2) = P1
          HPG(3) = P1
          HPG(4) = P2
          HPG(5) = P2
          HPG(6) = P2
          XPG(1) = XA
          YPG(1) = XA
          XPG(2) = UN - DEUX*XA
          YPG(2) = XA
          XPG(3) = XA
          YPG(3) = UN - DEUX*XA
          XPG(4) = XB
          YPG(4) = XB
          XPG(5) = UN - DEUX*XB
          YPG(5) = XB
          XPG(6) = XB
          YPG(6) = UN - DEUX*XB
        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TRIA3' .OR. ELREFE.EQ.'TRIA3L' .OR.
     &         ELREFE.EQ.'TRIA3H' .OR. ELREFE.EQ.'TRIA6H' .OR.
     &         ELREFE.EQ.'TRIA6' .OR. ELREFE.EQ.'TRIA6D' .OR.
     &         ELREFE.EQ.'TRIL6' .OR. ELREFE.EQ.'TRII3' .OR.
     &         ELREFE.EQ.'TRII6' .OR. ELREFE.EQ.'TRIA7  ' .OR.
     &         ELREFE.EQ.'TRIA3D') THEN

        NCMP = 2

        IF (NPG.EQ.1) THEN
          HPG(1) = DEUX
          XPG(1) = -UN/3D0
          YPG(1) = XPG(1)
        ELSE IF (NPG.EQ.3) THEN
          HPG(1) = DEUX/3.D0
          HPG(2) = HPG(1)
          HPG(3) = HPG(1)
          XPG(1) = -HPG(1)
          YPG(1) = UN/3.D0
          XPG(2) = XPG(1)
          YPG(2) = XPG(1)
          XPG(3) = YPG(1)
          YPG(3) = XPG(1)
        ELSE IF (NPG.EQ.4) THEN
          HPG(1) = 25.D0/24.D0
          HPG(2) = 25.D0/24.D0
          HPG(3) = 25.D0/24.D0
          HPG(4) = -27.D0/24.D0
          XPG(1) = -3.D0/5.D0
          YPG(1) = UN/5.D0
          XPG(2) = -3.D0/5.D0
          YPG(2) = -3.D0/5.D0
          XPG(3) = UN/5.D0
          YPG(3) = -3.D0/5.D0
          XPG(4) = -UN/3.D0
          YPG(4) = -UN/3.D0
        ELSE IF (NPG.EQ.6) THEN
          P1 = 0.111690794839005D0
          P2 = 0.054975871827661D0
          HPG(6) = 4.D0*P1
          HPG(4) = 4.D0*P1
          HPG(5) = 4.D0*P1
          HPG(2) = 4.D0*P2
          HPG(3) = 4.D0*P2
          HPG(1) = 4.D0*P2
          XA = 0.445948490915965D0
          XB = 0.091576213509771D0
          XPG(6) = T(XA)
          XPG(4) = T(UN-DEUX*XA)
          XPG(5) = T(XA)
          XPG(2) = T(XB)
          XPG(3) = T(UN-DEUX*XB)
          XPG(1) = T(XB)
          YPG(6) = T(XA)
          YPG(4) = T(XA)
          YPG(5) = T(UN-DEUX*XA)
          YPG(2) = T(XB)
          YPG(3) = T(XB)
          YPG(1) = T(UN-DEUX*XB)
        ELSE IF (NPG.EQ.12) THEN
          P1 = 0.025422453185103D0
          P2 = 0.058393137863189D0
          P3 = 0.041425537809187D0
          HPG(1) = 4.D0*P1
          HPG(2) = 4.D0*P1
          HPG(3) = 4.D0*P1
          HPG(4) = 4.D0*P2
          HPG(5) = 4.D0*P2
          HPG(6) = 4.D0*P2
          HPG(7) = 4.D0*P3
          HPG(8) = 4.D0*P3
          HPG(9) = 4.D0*P3
          HPG(10) = 4.D0*P3
          HPG(11) = 4.D0*P3
          HPG(12) = 4.D0*P3
          XA = 0.063089014491502D0
          XB = 0.249286745170910D0
          XC = 0.310352451033785D0
          XD = 0.053145049844816D0
          XPG(1) = T(XA)
          XPG(2) = T(UN-DEUX*XA)
          XPG(3) = T(XA)
          XPG(4) = T(XB)
          XPG(5) = T(UN-DEUX*XB)
          XPG(6) = T(XB)
          XPG(7) = T(XC)
          XPG(8) = T(XD)
          XPG(9) = T(UN- (XC+XD))
          XPG(10) = T(UN- (XC+XD))
          XPG(11) = T(XC)
          XPG(12) = T(XD)
          YPG(1) = T(XA)
          YPG(2) = T(XA)
          YPG(3) = T(UN-DEUX*XA)
          YPG(4) = T(XB)
          YPG(5) = T(XB)
          YPG(6) = T(UN-DEUX*XB)
          YPG(7) = T(XD)
          YPG(8) = T(XC)
          YPG(9) = T(XC)
          YPG(10) = T(XD)
          YPG(11) = T(UN- (XC+XD))
          YPG(12) = T(UN- (XC+XD))
        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QUAD4' .OR. ELREFE.EQ.'QUAD4L' .OR.
     &         ELREFE.EQ.'QUAD8' .OR. ELREFE.EQ.'QUA8D' .OR.
     &         ELREFE.EQ.'QUAS8' .OR. ELREFE.EQ.'QUAI4' .OR.
     &         ELREFE.EQ.'QUAI8' .OR. ELREFE.EQ.'QUAD9' .OR.
     &         ELREFE.EQ.'QUAD4D') THEN

        NCMP = 2

        IF (NPG.EQ.4) THEN
          HPG(1) = UN
          HPG(2) = UN
          HPG(3) = UN
          HPG(4) = UN
          XPG(1) = -UN/SQRT(3.0D0)
          YPG(1) = -XPG(1)
          XPG(2) = XPG(1)
          YPG(2) = -YPG(1)
          XPG(3) = -XPG(2)
          YPG(3) = YPG(2)
          XPG(4) = XPG(3)
          YPG(4) = -YPG(3)
        ELSE IF (NPG.EQ.9) THEN
          HPG(1) = 25.D0/81.0D0
          HPG(2) = HPG(1)
          HPG(3) = HPG(1)
          HPG(4) = HPG(1)
          HPG(5) = 40.D0/81.0D0
          HPG(6) = HPG(5)
          HPG(7) = HPG(5)
          HPG(8) = HPG(5)
          HPG(9) = 64.D0/81.0D0
          XPG(1) = -0.774596669241483D0
          YPG(1) = 0.774596669241483D0
          XPG(2) = -0.774596669241483D0
          YPG(2) = -0.774596669241483D0
          XPG(3) = 0.774596669241483D0
          YPG(3) = -0.774596669241483D0
          XPG(4) = 0.774596669241483D0
          YPG(4) = 0.774596669241483D0
          XPG(5) = -0.774596669241483D0
          YPG(5) = ZERO
          XPG(6) = ZERO
          YPG(6) = -0.774596669241483D0
          XPG(7) = 0.774596669241483D0
          YPG(7) = ZERO
          XPG(8) = ZERO
          YPG(8) = 0.774596669241483D0
          XPG(9) = ZERO
          YPG(9) = ZERO
        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'SEG2' .OR. ELREFE.EQ.'SEG3' .OR.
     &         ELREFE.EQ.'CABPOU' .OR. ELREFE.EQ.'THCOSE2' .OR.
     &         ELREFE.EQ.'THCOSE3' .OR. ELREFE.EQ.'MET3SEG3' .OR.
     &         ELREFE.EQ.'MET6SEG3' .OR. ELREFE.EQ.'MET3SEG4') THEN

        NCMP = 1

        IF (NPG.EQ.1) THEN
          XPG(1) = ZERO
          HPG(1) = DEUX

        ELSE IF (NPG.EQ.2) THEN
          XPG(1) = 0.577350269189626D0
          XPG(2) = -XPG(1)
          HPG(1) = UN
          HPG(2) = HPG(1)

        ELSE IF (NPG.EQ.3) THEN
          XPG(1) = -0.774596669241483D0
          XPG(2) = 0.D0
          XPG(3) = 0.774596669241483D0
          HPG(1) = 0.555555555555556D0
          HPG(2) = 0.888888888888889D0
          HPG(3) = 0.555555555555556D0

        ELSE IF (NPG.EQ.4) THEN
          XPG(1) = 0.339981043584856D0
          XPG(2) = -XPG(1)
          XPG(3) = 0.861136311594053D0
          XPG(4) = -XPG(3)
          HPG(1) = 0.652145154862546D0
          HPG(2) = HPG(1)
          HPG(3) = 0.347854845137454D0
          HPG(4) = HPG(3)
        END IF
      END IF

C     ------------------------------------------------------------------

      DO 110 I = 0,NPG - 1
        COOPG(NCMP*I+1) = XPG(I+1)
        IF ((NCMP.EQ.2) .OR. (NCMP.EQ.3)) THEN
          COOPG(NCMP*I+2) = YPG(I+1)
        END IF
  110 CONTINUE

      IF (NCMP.EQ.3) THEN
        DO 120 I = 0,NPG - 1
          COOPG(NCMP*I+3) = ZPG(I+1)
  120   CONTINUE
      END IF

      DO 130 I = 1,NPG
        POIPG(I) = HPG(I)
  130 CONTINUE

      IF ((DCOO.LT.NCMP) .OR. (DPOI.LT.NPG)) THEN
        CALL UTMESS('F','ELRFGF',' ERREUR PROGRAMMEUR'//
     &              ' ECRASEMENT DE TABLEAU')
      END IF

      END
