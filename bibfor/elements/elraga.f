      SUBROUTINE ELRAGA ( ELREFZ, FAPZ, NDIM, NBPG, COOPG, POIPG )
      IMPLICIT  NONE
      INTEGER             NBPG, NDIM
      REAL*8              COOPG(*), POIPG(*)
      CHARACTER*(*)       ELREFZ, FAPZ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE VABHHTS J.PELLET
C TOLE CRP_20
C ----------------------------------------------------------------------
C BUT: CALCUL DES POIDS ET POINTS DE GAUSS
C ----------------------------------------------------------------------
C   IN   ELREFZ : NOM DE L'ELREFA (K8)
C        FAPG   : NOM DE LA FAMILLE DE POINTS DE GAUSS
C   OUT  NDIM   : DIMENSION DE L'ESPACE (=NB COORDONNEES)
C        NBPG   : NOMBRE DE POINTS DE GAUSS
C        COOPG  : COORDONNEES DES POINTS DE GAUSS
C        POIPG  : POIDS DES POINTS DE GAUSS
C   -------------------------------------------------------------------

      INTEGER NBPGMX,NBFAMX
      PARAMETER (NBPGMX=1000,NBFAMX=20)

      CHARACTER*8 ELREFA,FAPG,NOFPG(NBFAMX)
      CHARACTER*24 VALK(2)
      INTEGER I,NPAR,NPI,IX,IY,IZ,NPX,NPYZ,N,NPG,J,CPT
      INTEGER NNO,NNOS,NBFPG,NBPG1(NBFAMX),INO,IFAM,INDIK8
      REAL*8 XPG(NBPGMX),YPG(NBPGMX),ZPG(NBPGMX),HPG(NBPGMX),A(4),H(4)
      REAL*8 ATY(7),ATZ(7),HT(7),U,T
      REAL*8 AA,BB,CC,HH,H1,H2,H3,RAC5,RAC15,A1,B1,B6,C1,C8,D1,D12
      REAL*8 P1,P2,P3,P4,P5,XXG5(5),PXG5(5),XA,XB,XC,XD
      REAL*8 ZERO,UNQUAR,UNDEMI,UN,DEUX,XNO(3*27),VOL,A2,B2
C -----  FONCTIONS FORMULES
      T(U) = 2.0D0*U - 1.0D0
C DEB ------------------------------------------------------------------

      ELREFA = ELREFZ
      FAPG   = FAPZ

      ZERO   = 0.0D0
      UNQUAR = 0.25D0
      UNDEMI = 0.5D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      RAC5   = SQRT(5.D0)

C     -- CALCUL DE NBPG,NDIM,VOL,NNO,XNO :
C     ------------------------------------
      CALL ELRACA(ELREFA,NDIM,NNO,NNOS,NBFPG,NOFPG,NBPG1,XNO,VOL)
      IFAM = INDIK8(NOFPG,FAPG,1,NBFPG)
      IF ( IFAM .LE. 0 ) THEN
        VALK (1) = ELREFA
        VALK (2) = FAPG
         CALL U2MESG('F', 'ELEMENTS4_84',2,VALK,0,0,0,0.D0)
      ENDIF
      NBPG = NBPG1(IFAM)
      CALL ASSERT((NDIM.GE.0).AND.(NDIM.LE.3))


C     -- TRAITEMENT GENERIQUE DE FAPG='NOEU' :
C     -----------------------------------------
      IF (FAPG.EQ.'NOEU') THEN
        CALL ASSERT(NBPG.EQ.NNO)
        DO 10,INO = 1,NNO
          HPG(INO) = VOL/NNO
          IF (NDIM.GE.1) XPG(INO) = XNO(NDIM* (INO-1)+1)
          IF (NDIM.GE.2) YPG(INO) = XNO(NDIM* (INO-1)+2)
          IF (NDIM.EQ.3) ZPG(INO) = XNO(NDIM* (INO-1)+3)
   10   CONTINUE
        GO TO 170
      END IF


C     -- TRAITEMENT GENERIQUE DE FAPG='NOEU_S' :
C     -----------------------------------------
      IF (FAPG.EQ.'NOEU_S') THEN
        CALL ASSERT(NBPG.EQ.NNOS)
        DO 20,INO = 1,NNOS
          HPG(INO) = VOL/NNOS
C         -- ON UTILISE LE FAIT QUE LES SOMMETS SONT TOUJOURS
C            NUMEROTES EN PREMIER :
          IF (NDIM.GE.1) XPG(INO) = XNO(NDIM* (INO-1)+1)
          IF (NDIM.GE.2) YPG(INO) = XNO(NDIM* (INO-1)+2)
          IF (NDIM.EQ.3) ZPG(INO) = XNO(NDIM* (INO-1)+3)
   20   CONTINUE
        GO TO 170
      END IF


C     -- TRAITEMENT GENERIQUE DE FAPG='FPG1' :
C     -----------------------------------------
      IF (FAPG.EQ.'FPG1') THEN
        CALL ASSERT(NBPG.EQ.1)
        XPG(1) = 0.D0
        IF (NDIM.GE.1) XPG(1) = 0.D0
        IF (NDIM.GE.2) YPG(1) = 0.D0
        IF (NDIM.EQ.3) ZPG(1) = 0.D0
        DO 30,INO = 1,NNO
          IF (NDIM.GE.1) XPG(1) = XPG(1) + XNO(NDIM* (INO-1)+1)
          IF (NDIM.GE.2) YPG(1) = YPG(1) + XNO(NDIM* (INO-1)+2)
          IF (NDIM.EQ.3) ZPG(1) = ZPG(1) + XNO(NDIM* (INO-1)+3)
   30   CONTINUE
        IF (NDIM.GE.1) XPG(1) = XPG(1)/NNO
        IF (NDIM.GE.2) YPG(1) = YPG(1)/NNO
        IF (NDIM.EQ.3) ZPG(1) = ZPG(1)/NNO
        HPG(1) = VOL
        GO TO 170
      END IF


C     ------------------------------------------------------------------
      IF (ELREFA.EQ.'HE8' .OR. ELREFA.EQ.'H20' .OR.
     &    ELREFA.EQ.'H27') THEN

        NPAR = 0

        IF (FAPG.EQ.'FPG8') THEN
C --------- FORMULE DE QUADRATURE DE GAUSS A 2 POINTS DANS CHAQUE
C           DIRECTION ( ORDRE 3 )
          NPAR = 2
          A(1) = -0.577350269189626D0
          A(2) = -A(1)
          H(1) = UN
          H(2) = UN

        ELSE IF (FAPG.EQ.'FPG27') THEN
C --------- FORMULE DE QUADRATURE DE GAUSS A 3 POINTS DANS CHAQUE
C           DIRECTION ( ORDRE 5 )
          NPAR = 3
          A(1) = -0.774596669241483D0
          A(2) = ZERO
          A(3) = -A(1)
          H(1) = 0.555555555555556D0
          H(2) = 0.888888888888889D0
          H(3) = H(1)

        ELSE IF (FAPG.EQ.'FPG64') THEN
C --------- FORMULE DE QUADRATURE DE GAUSS A 4 POINTS DANS CHAQUE
C           DIRECTION ( ORDRE 7 )
          NPAR = 4
          A(1) = -0.339981043584856D0
          A(2) = -A(1)
          A(3) = -0.861136311594053D0
          A(4) = -A(3)
          H(1) = 0.652145154862546D0
          H(2) = H(1)
          H(3) = 0.347854845137454D0
          H(4) = H(3)

        ELSE IF (FAPG.EQ.'FPG8NOS') THEN
C ------- POUR LES POINTS DE GAUSS -------------------------------------
          NPAR = 2
          A(1) = -0.577350269189626D0
          A(2) = -A(1)
          H(1) = UN
          H(2) = UN
C ------- POUR LES SOMMETS ---------------------------------------------
          DO 300,INO = 1,NNOS
             HPG(INO+8) = VOL/NNOS
C ---------- ON UTILISE LE FAIT QUE LES SOMMETS SONT TOUJOURS ----------
C ---------- NUMEROTES EN PREMIER --------------------------------------
             XPG(INO+8) = XNO(NDIM* (INO-1)+1)
             IF (NDIM.GE.2) YPG(INO+8) = XNO(NDIM* (INO-1)+2)
             IF (NDIM.EQ.3) ZPG(INO+8) = XNO(NDIM* (INO-1)+3)
 300      CONTINUE
        ENDIF

        NPI = 0
        DO 60 IX = 1,NPAR
          DO 50 IY = 1,NPAR
            DO 40 IZ = 1,NPAR
              NPI = NPI + 1
              XPG(NPI) = A(IX)
              YPG(NPI) = A(IY)
              ZPG(NPI) = A(IZ)
              HPG(NPI) = H(IX)*H(IY)*H(IZ)
   40       CONTINUE
   50     CONTINUE
   60   CONTINUE

        IF (FAPG.EQ.'SHB5') THEN
C --------- FORMULE DE QUADRATURE DE GAUSS A 5 POINTS DANS
C           L EPAISSEUR POUR LE SHB8, AU CENTRE DE L'ELEMENT
          XXG5(1) = -0.906179845938664D0
          XXG5(2) = -0.538469310105683D0
          XXG5(3) = 0.D0
          XXG5(4) = 0.538469310105683D0
          XXG5(5) = 0.906179845938664D0

          PXG5(1) = 0.236926885056189D0
          PXG5(2) = 0.478628670499366D0
          PXG5(3) = 0.568888888888889D0
          PXG5(4) = 0.478628670499366D0
          PXG5(5) = 0.236926885056189D0
C         IL FAUT MULTIPLIER LES POIDS PAR 4 POUR OBTENIR VOL=8
          DO 70 IZ = 1,5
            XPG(IZ) = 0.D0
            YPG(IZ) = 0.D0
            ZPG(IZ) = XXG5(IZ)
            HPG(IZ) = PXG5(IZ)*4.D0
   70     CONTINUE
        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'PE6' .OR. ELREFA.EQ.'P15') THEN

        IF (FAPG.EQ.'FPG6') THEN
          NPX = 2
          NPYZ = 3
          A(1) = -0.577350269189626D0
          A(2) = -A(1)
          ATY(1) = UNDEMI
          ATY(2) = ZERO
          ATY(3) = UNDEMI
          ATZ(1) = UNDEMI
          ATZ(2) = UNDEMI
          ATZ(3) = ZERO
          H(1) = UN
          H(2) = UN
          HT(1) = UN/6.D0
          HT(2) = HT(1)
          HT(3) = HT(1)

        ELSEIF (FAPG.EQ.'FPG6B') THEN
          NPX  = 2
          NPYZ = 3
          A(1) = -0.577350269189626D0
          A(2) = -A(1)
          ATY(1) = 1.D0/6.D0
          ATY(2) = 2.D0/3.D0
          ATY(3) = 1.D0/6.D0
          ATZ(1) = 1.D0/6.D0
          ATZ(2) = 1.D0/6.D0
          ATZ(3) = 2.D0/3.D0
          H(1) = UN
          H(2) = UN
          HT(1) = 1.D0/6.D0
          HT(2) = HT(1)
          HT(3) = HT(1)

        ELSE IF (FAPG.EQ.'FPG8') THEN

C --------- FORMULE A 4 * 2 POINTS :  (CF TOUZOT PAGE 297)
C                   2 POINTS DE GAUSS  EN X   (ORDRE 3)
C                   4 POINTS DE HAMMER EN Y Z (ORDRE 3 EN Y Z)

C --------- FORMULE DE GAUSS

          NPX = 2
          A(1) = -0.577350269189626D0
          A(2) = -A(1)
          H(1) = UN
          H(2) = UN

C --------- FORMULE DE HAMMER

          NPYZ = 4
          ATY(1) = 0.333333333333333D0
          ATY(2) = 0.6D0
          ATY(3) = 0.2D0
          ATY(4) = 0.2D0
          ATZ(1) = 0.333333333333333D0
          ATZ(2) = 0.2D0
          ATZ(3) = 0.6D0
          ATZ(4) = 0.2D0
          HT(1) = -27.D0/96.D0
          HT(2) =  25.D0/96.D0
          HT(3) = HT(2)
          HT(4) = HT(2)

        ELSE IF (FAPG.EQ.'FPG21') THEN

C --------- FORMULE A 7 * 3 POINTS :   (CF TOUZOT PAGE 298)
C                   3 POINTS DE GAUSS EN X (ORDRE 5)
C                   7 POINTS DE HAMMER EN Y Z (ORDRE 5 EN Y Z)

C --------- FORMULE DE GAUSS

          NPX = 3
          A(1) = -0.774596669241483D0
          A(2) = ZERO
          A(3) = -A(1)
          H(1) = 0.555555555555556D0
          H(2) = 0.888888888888889D0
          H(3) = H(1)

C --------- FORMULE DE HAMMER

          NPYZ = 7
          ATY(1) = 0.333333333333333D0
          ATZ(1) = 0.333333333333333D0
          ATY(2) = 0.470142064105115D0
          ATZ(2) = 0.470142064105115D0
          ATY(3) = UN - DEUX*ATY(2)
          ATZ(3) = 0.470142064105115D0
          ATY(4) = 0.470142064105115D0
          ATZ(4) = UN - DEUX*ATY(2)
          ATY(5) = 0.101286507323456D0
          ATZ(5) = 0.101286507323456D0
          ATY(6) = UN - DEUX*ATY(5)
          ATZ(6) = 0.101286507323456D0
          ATY(7) = 0.101286507323456D0
          ATZ(7) = UN - DEUX*ATY(5)
          HT(1) = 9.D0/80.D0
          HT(2) = 0.0661970763942530D0
          HT(3) = HT(2)
          HT(4) = HT(2)
          HT(5) = 0.0629695902724135D0
          HT(6) = HT(5)
          HT(7) = HT(5)

        ELSEIF (FAPG.EQ.'FPG6NOS') THEN
C ------- POUR LES POINTS DE GAUSS -------------------------------------
          NPX = 2
          NPYZ = 3
          A(1) = -0.577350269189626D0
          A(2) = 0.577350269189626D0
          ATY(1) = UNDEMI
          ATY(2) = ZERO
          ATY(3) = UNDEMI
          ATZ(1) = UNDEMI
          ATZ(2) = UNDEMI
          ATZ(3) = ZERO
          H(1) = UN
          H(2) = UN
          HT(1) = 0.166666666666667D0
          HT(2) = HT(1)
          HT(3) = HT(1)

C ------- POUR LES SOMMETS ---------------------------------------------
          DO 280,INO = 1,NNOS
             HPG(INO+6) = VOL/NNOS
C ---------- ON UTILISE LE FAIT QUE LES SOMMETS SONT TOUJOURS ----------
C ---------- NUMEROTES EN PREMIER --------------------------------------
             XPG(INO+6) = XNO(NDIM* (INO-1)+1)
             IF (NDIM.GE.2) YPG(INO+6) = XNO(NDIM* (INO-1)+2)
             IF (NDIM.EQ.3) ZPG(INO+6) = XNO(NDIM* (INO-1)+3)
 280      CONTINUE
        END IF

        NPI = 0
        DO 90 IX = 1,NPX
          DO 80 IY = 1,NPYZ
            NPI = NPI + 1
            XPG(NPI) = A(IX)
            YPG(NPI) = ATY(IY)
            ZPG(NPI) = ATZ(IY)
            HPG(NPI) = H(IX)*HT(IY)
 80       CONTINUE
 90     CONTINUE

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'TE4' .OR. ELREFA.EQ.'T10') THEN

        IF (FAPG.EQ.'FPG4') THEN

C --------- FORMULE A 4 POINTS :  (CF TOUZOT PAGE 300)
C                   ORDRE 2 EN X Y Z

          AA = (5.D0-RAC5)/20.D0
          BB = (5.D0+3.D0*RAC5)/20.D0
          HH = UN/24.D0
          NPI = 0
          DO 100 I = 1,4
            NPI = NPI + 1
            XPG(NPI) = AA
            YPG(NPI) = AA
            ZPG(NPI) = AA
            HPG(NPI) = HH
  100     CONTINUE
          ZPG(2) = BB
          YPG(3) = BB
          XPG(4) = BB

        ELSE IF (FAPG.EQ.'FPG5') THEN

C --------- FORMULE A 5 POINTS :  (CF TOUZOT PAGE 300)
C                   ORDRE 3 EN X Y Z

          AA = 0.25D0
          BB = UN/6.D0
          CC = UNDEMI
          H1 = -DEUX/15.D0
          H2 = 3.D0/40.D0
          XPG(1) = AA
          YPG(1) = AA
          ZPG(1) = AA
          HPG(1) = H1
          DO 110 I = 2,5
            XPG(I) = BB
            YPG(I) = BB
            ZPG(I) = BB
            HPG(I) = H2
  110     CONTINUE
          ZPG(3) = CC
          YPG(4) = CC
          XPG(5) = CC

        ELSE IF (FAPG.EQ.'FPG15') THEN

C --------- FORMULE A 15 POINTS :  (CF TOUZOT PAGE 300)
C                   ORDRE 5 EN X Y Z

          RAC15 = SQRT(15.0D0)
          XPG(1) = 0.25D0
          YPG(1) = 0.25D0
          ZPG(1) = 0.25D0
          HPG(1) = 8.0D0/405.0D0

          XPG(2) = (7.0D0+RAC15)/34.0D0
          YPG(2) = XPG(2)
          ZPG(2) = XPG(2)
          HPG(2) = (2665.0D0-14.0D0*RAC15)/226800.0D0
          XPG(3) = XPG(2)
          YPG(3) = XPG(2)
          ZPG(3) = (13.0D0-3.0D0*RAC15)/34.0D0
          HPG(3) = HPG(2)
          XPG(4) = XPG(2)
          YPG(4) = (13.0D0-3.0D0*RAC15)/34.0D0
          ZPG(4) = XPG(2)
          HPG(4) = HPG(2)
          XPG(5) = (13.0D0-3.0D0*RAC15)/34.0D0
          YPG(5) = XPG(2)
          ZPG(5) = XPG(2)
          HPG(5) = HPG(2)

          XPG(6) = (7.0D0-RAC15)/34.0D0
          YPG(6) = XPG(6)
          ZPG(6) = XPG(6)
          HPG(6) = (2665.0D0+14.0D0*RAC15)/226800.0D0
          XPG(7) = XPG(6)
          YPG(7) = XPG(6)
          ZPG(7) = (13.0D0+3.0D0*RAC15)/34.0D0
          HPG(7) = HPG(6)
          XPG(8) = XPG(6)
          YPG(8) = (13.0D0+3.0D0*RAC15)/34.0D0
          ZPG(8) = XPG(6)
          HPG(8) = HPG(6)
          XPG(9) = (13.0D0+3.0D0*RAC15)/34.0D0
          YPG(9) = XPG(6)
          ZPG(9) = XPG(6)
          HPG(9) = HPG(6)

          XPG(10) = (5.0D0-RAC15)/20.0D0
          YPG(10) = XPG(10)
          ZPG(10) = (5.0D0+RAC15)/20.0D0
          HPG(10) = 5.0D0/567.0D0
          XPG(11) = XPG(10)
          YPG(11) = (5.0D0+RAC15)/20.0D0
          ZPG(11) = XPG(10)
          HPG(11) = 5.0D0/567.0D0
          XPG(12) = (5.0D0+RAC15)/20.0D0
          YPG(12) = XPG(10)
          ZPG(12) = XPG(10)
          HPG(12) = 5.0D0/567.0D0

          XPG(13) = XPG(10)
          YPG(13) = (5.0D0+RAC15)/20.0D0
          ZPG(13) = YPG(13)
          HPG(13) = 5.0D0/567.0D0
          XPG(14) = YPG(13)
          YPG(14) = XPG(10)
          ZPG(14) = YPG(13)
          HPG(14) = 5.0D0/567.0D0
          XPG(15) = YPG(13)
          YPG(15) = YPG(13)
          ZPG(15) = XPG(10)
          HPG(15) = 5.0D0/567.0D0

        ELSEIF (FAPG.EQ.'FPG4NOS') THEN
C ------- POUR LES POINTS DE GAUSS -------------------------------------
          AA = (5.D0-RAC5)/20.D0
          BB = (5.D0+3.D0*RAC5)/20.D0
          HH = UN/24.D0
          NPI = 0
          DO 140 I = 1,4
            NPI = NPI + 1
            XPG(NPI) = AA
            YPG(NPI) = AA
            ZPG(NPI) = AA
            HPG(NPI) = HH
  140     CONTINUE
          ZPG(2) = BB
          YPG(3) = BB
          XPG(4) = BB
C ------- POUR LES SOMMETS ---------------------------------------------
          DO 260,INO = 1,NNOS
             HPG(INO+4) = VOL/NNOS
C ---------- ON UTILISE LE FAIT QUE LES SOMMETS SONT TOUJOURS ----------
C ---------- NUMEROTES EN PREMIER --------------------------------------
             XPG(INO+4) = XNO(NDIM* (INO-1)+1)
             IF (NDIM.GE.2) YPG(INO+4) = XNO(NDIM* (INO-1)+2)
             IF (NDIM.EQ.3) ZPG(INO+4) = XNO(NDIM* (INO-1)+3)
 260      CONTINUE
        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'PY5' .OR. ELREFA.EQ.'P13') THEN

        IF (FAPG.EQ.'FPG5') THEN

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

        ELSE IF (FAPG.EQ.'FPG6') THEN

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
        ELSE IF (FAPG.EQ.'FPG27') THEN

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

          DO 120 I = 1,27
            HPG(I) = HPG(I)*UNQUAR* (UN-ZPG(I))* (UN-ZPG(I))
  120     CONTINUE

        ELSEIF (FAPG.EQ.'FPG5NOS') THEN
C ------- POUR LES POINTS DE GAUSS -------------------------------------
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
C --- POUR LES SOMMETS -------------------------------------------------
          DO 240,INO = 1,NNOS
             HPG(INO+5) = VOL/NNOS
C ---------- ON UTILISE LE FAIT QUE LES SOMMETS SONT TOUJOURS ----------
C ---------- NUMEROTES EN PREMIER --------------------------------------
             XPG(INO+5) = XNO(NDIM* (INO-1)+1)
             IF (NDIM.GE.2) YPG(INO+5) = XNO(NDIM* (INO-1)+2)
             IF (NDIM.EQ.3) ZPG(INO+5) = XNO(NDIM* (INO-1)+3)
 240      CONTINUE
        ELSE
          CALL ASSERT(.FALSE.)
        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'TR3' .OR. ELREFA.EQ.'TR6' .OR.
     &         ELREFA.EQ.'TR7' ) THEN

       IF (FAPG.EQ.'FPG1') THEN
          XPG(1) = UN/3.D0
          YPG(1) = UN/3.D0
          HPG(1) = UN/DEUX
        ELSE IF (FAPG.EQ.'FPG3') THEN
          XPG(1) = UN/6.D0
          YPG(1) = UN/6.D0
          XPG(2) = 2.D0/3.D0
          YPG(2) = UN/6.D0
          XPG(3) = UN/6.D0
          YPG(3) = 2.D0/3.D0
          HPG(1) = UN/6.D0
          HPG(2) = UN/6.D0
          HPG(3) = UN/6.D0
        ELSEIF (FAPG.EQ.'FPG4') THEN
          XPG(1) = 0.2D0
          YPG(1) = 0.2D0
          XPG(2) = 0.6D0
          YPG(2) = 0.2D0
          XPG(3) = 0.2D0
          YPG(3) = 0.6D0
          XPG(4) = UN/3.D0
          YPG(4) = UN/3.D0
          HPG(1) =  25.D0/96.D0
          HPG(2) =  25.D0/96.D0
          HPG(3) =  25.D0/96.D0
          HPG(4) = -27.D0/96.D0
        ELSEIF (FAPG.EQ.'FPG6') THEN
          P1 = 0.111690794839005D0
          P2 = 0.054975871827661D0
          XA = 0.445948490915965D0
          XB = 0.091576213509771D0
          XPG(3) = ( T(XB) + UN ) / DEUX
          YPG(3) = ( T(UN-DEUX*XB) + UN ) / DEUX
          XPG(1) = ( T(XB) + UN ) / DEUX
          YPG(1) = ( T(XB) + UN ) / DEUX
          XPG(2) = ( T(UN-DEUX*XB) + UN ) / DEUX
          YPG(2) = ( T(XB) + UN ) / DEUX
          XPG(6) = ( T(UN-DEUX*XA) + UN ) / DEUX
          YPG(6) = ( T(XA) + UN ) / DEUX
          XPG(4) = ( T(XA) + UN ) / DEUX
          YPG(4) = ( T(UN-DEUX*XA) + UN ) / DEUX
          XPG(5) = ( T(XA) + UN ) / DEUX
          YPG(5) = ( T(XA) + UN ) / DEUX
          HPG(1) = P2
          HPG(2) = P2
          HPG(3) = P2
          HPG(4) = P1
          HPG(5) = P1
          HPG(6) = P1
        ELSEIF (FAPG.EQ.'FPG7') THEN
          P1 = 0.066197076394253D0
          P2 = 0.062969590272413D0
          A2 = 0.470142064105115D0
          B2 = 0.101286507323456D0
          XPG(1) = 0.333333333333333D0
          YPG(1) = 0.333333333333333D0
          XPG(2) = A2
          YPG(2) = A2
          XPG(3) = UN - DEUX*A2
          YPG(3) = A2
          XPG(4) = A2
          YPG(4) = UN - DEUX*A2
          XPG(5) = B2
          YPG(5) = B2
          XPG(6) = UN - DEUX*B2
          YPG(6) = B2
          XPG(7) = B2
          YPG(7) = UN - DEUX*B2
          HPG(1) = 9.D0/80.D0
          HPG(2) = P1
          HPG(3) = P1
          HPG(4) = P1
          HPG(5) = P2
          HPG(6) = P2
          HPG(7) = P2
        ELSEIF (FAPG.EQ.'FPG12') THEN
          A1=0.063089014491502D0
          B1=0.249286745170910D0
          C1=0.310352451033785D0
          D1=0.053145049844816D0
          XPG(1) = A1
          YPG(1) = A1
          XPG(2) = UN - DEUX*A1
          YPG(2) = A1
          XPG(3) = A1
          YPG(3) = UN - DEUX*A1
          XPG(4) = B1
          YPG(4) = B1
          XPG(5) = UN - DEUX*B1
          YPG(5) = B1
          XPG(6) = B1
          YPG(6) = UN - DEUX*B1
          XPG(7) = C1
          YPG(7) = D1
          XPG(8) = D1
          YPG(8) = C1
          XPG(9) = UN - C1 - D1
          YPG(9) = C1
          XPG(10) = UN - C1 - D1
          YPG(10) = D1
          XPG(11) = C1
          YPG(11) = UN - C1 - D1
          XPG(12) = D1
          YPG(12) = UN - C1 - D1
          P1=0.025422453185103D0
          P2=0.058393137863189D0
          P3=0.041425537809187D0
          HPG(1) = P1
          HPG(2) = P1
          HPG(3) = P1
          HPG(4) = P2
          HPG(5) = P2
          HPG(6) = P2
          HPG(7) = P3
          HPG(8) = P3
          HPG(9) = P3
          HPG(10) = P3
          HPG(11) = P3
          HPG(12) = P3
        ELSEIF (FAPG.EQ.'FPG13') THEN

C         FORMULE A 13 POINTS : ORDRE 7  (CF BATHE : 
C         FINITE ELEMENT PROCEDURES IN ENGINEERING ANALYSIS, PAGE 280)

          XPG(1) = 0.0651301029022D0
          YPG(1) = 0.0651301029022D0
          XPG(2) = 0.8697397941956D0
          YPG(2) = 0.0651301029022D0
          XPG(3) = 0.0651301029022D0
          YPG(3) = 0.8697397941956D0
          XPG(4) = 0.3128654960049D0
          YPG(4) = 0.0486903154253D0
          XPG(5) = 0.6384441885698D0
          YPG(5) = 0.3128654960049D0
          XPG(6) = 0.0486903154253D0
          YPG(6) = 0.6384441885698D0
          XPG(7) = 0.6384441885698D0
          YPG(7) = 0.0486903154253D0
          XPG(8) = 0.3128654960049D0
          YPG(8) = 0.6384441885698D0
          XPG(9) = 0.0486903154253D0
          YPG(9) = 0.3128654960049D0
          XPG(10) = 0.2603459660790D0
          YPG(10) = 0.2603459660790D0
          XPG(11) = 0.4793080678419D0
          YPG(11) = 0.2603459660790D0
          XPG(12) = 0.2603459660790D0
          YPG(12) = 0.4793080678419D0
          XPG(13) = 0.3333333333333D0
          YPG(13) = 0.3333333333333D0
          P1= 0.0533472356088D0/2.D0
          P2= 0.0771137608903D0/2.D0
          P3= 0.1756152574332D0/2.D0
          P4= -0.1495700444677D0/2.D0
          HPG(1) = P1
          HPG(2) = P1
          HPG(3) = P1
          HPG(4) = P2
          HPG(5) = P2
          HPG(6) = P2
          HPG(7) = P2
          HPG(8) = P2
          HPG(9) = P2
          HPG(10) = P3
          HPG(11) = P3
          HPG(12) = P3
          HPG(13) = P4
        ELSEIF (FAPG.EQ.'FPG16') THEN
          XPG(1) = 0.333333333333333D0
          YPG(1) = 0.333333333333333D0
          XPG(2) = 0.081414823414554D0
          YPG(2) = 0.459292588292723D0
          XPG(3) = 0.459292588292723D0
          YPG(3) = 0.081414823414554D0
          XPG(4) = 0.459292588292723D0
          YPG(4) = 0.459292588292723D0
          XPG(5) = 0.658861384496480D0
          YPG(5) = 0.170569307751760D0
          XPG(6) = 0.170569307751760D0
          YPG(6) = 0.658861384496480D0
          XPG(7) = 0.170569307751760D0
          YPG(7) = 0.170569307751760D0
          XPG(8) = 0.898905543365938D0
          YPG(8) = 0.050547228317031D0
          XPG(9) = 0.050547228317031D0
          YPG(9) = 0.898905543365938D0
          XPG(10) = 0.050547228317031D0
          YPG(10) = 0.050547228317031D0
          XPG(11) = 0.008394777409958D0
          YPG(11) = 0.728492392955404D0
          XPG(12) = 0.728492392955404D0
          YPG(12) = 0.008394777409958D0
          XPG(13) = 0.263112829634638D0
          YPG(13) = 0.008394777409958D0
          XPG(14) = 0.008394777409958D0
          YPG(14) = 0.263112829634638D0
          XPG(15) = 0.263112829634638D0
          YPG(15) = 0.728492392955404D0
          XPG(16) = 0.728492392955404D0
          YPG(16) = 0.263112829634638D0
          P1= 0.144315607677787D0/2.D0
          P2= 0.095091634267285D0/2.D0
          P3= 0.103217370534718D0/2.D0
          P4= 0.032458497623198D0/2.D0
          P5= 0.027230314174435D0/2.D0
          HPG(1) = P1
          HPG(2) = P2
          HPG(3) = P2
          HPG(4) = P2
          HPG(5) = P3
          HPG(6) = P3
          HPG(7) = P3
          HPG(8) = P4
          HPG(9) = P4
          HPG(10) = P4
          HPG(11) = P5
          HPG(12) = P5
          HPG(13) = P5
          HPG(14) = P5
          HPG(15) = P5
          HPG(16) = P5
  
        ELSE IF (FAPG.EQ.'COT3') THEN
          XPG(1) = UNDEMI
          YPG(1) = UNDEMI
          XPG(2) = ZERO
          YPG(2) = UNDEMI
          XPG(3) = UNDEMI
          YPG(3) = ZERO
          HPG(1) = UN/6.D0
          HPG(2) = UN/6.D0
          HPG(3) = UN/6.D0
        ELSE IF (FAPG.EQ.'FPG3NOS') THEN
C ------- POUR LES POINTS DE GAUSS -------------------------------------
          XPG(1) = UN/6.D0
          YPG(1) = UN/6.D0
          XPG(2) = 2.D0/3.D0
          YPG(2) = UN/6.D0
          XPG(3) = UN/6.D0
          YPG(3) = 2.D0/3.D0
          HPG(1) = UN/6.D0
          HPG(2) = UN/6.D0
          HPG(3) = UN/6.D0
C ------- POUR LES SOMMETS ---------------------------------------------
          DO 220,INO = 1,NNOS
             HPG(INO+3) = VOL/NNOS
C ---------- ON UTILISE LE FAIT QUE LES SOMMETS SONT TOUJOURS ----------
C ---------- NUMEROTES EN PREMIER --------------------------------------
             XPG(INO+3) = XNO(NDIM* (INO-1)+1)
             IF (NDIM.GE.2) YPG(INO+3) = XNO(NDIM* (INO-1)+2)
             IF (NDIM.EQ.3) ZPG(INO+3) = XNO(NDIM* (INO-1)+3)
 220      CONTINUE
        ELSE
        VALK (1) = ELREFA
        VALK (2) = FAPG
          CALL U2MESG('F', 'ELEMENTS4_85',2,VALK,0,0,0,0.D0)
        END IF

C     ------------------------------------------------------------------
      ELSE IF ( ELREFA.EQ.'QU4' .OR. ELREFA.EQ.'QU8' .OR.
     &          ELREFA.EQ.'QU9') THEN

        IF (FAPG.EQ.'FPG1') THEN
          XPG(1) = ZERO
          YPG(1) = ZERO
          HPG(1) = 4.D0
        ELSEIF (FAPG.EQ.'FIS2') THEN
C ------- ELEMENT PARTICULIER DE FISSURE, S'APPUIE SUR UN SEG2
          XPG(1) = -0.577350269189626D0
          YPG(1) = ZERO
          XPG(2) =  0.577350269189626D0
          YPG(2) = ZERO
          HPG(1) = DEUX
          HPG(2) = DEUX
        ELSEIF (FAPG.EQ.'FPG4') THEN
          XPG(1) = -0.577350269189626D0
          YPG(1) = -0.577350269189626D0
          XPG(2) =  0.577350269189626D0
          YPG(2) = -0.577350269189626D0
          XPG(3) =  0.577350269189626D0
          YPG(3) =  0.577350269189626D0
          XPG(4) = -0.577350269189626D0
          YPG(4) =  0.577350269189626D0
          HPG(1) = UN
          HPG(2) = UN
          HPG(3) = UN
          HPG(4) = UN
        ELSEIF (FAPG.EQ.'FPG9') THEN
          HPG(1) = 25.D0/81.0D0
          HPG(2) = 25.D0/81.0D0
          HPG(3) = 25.D0/81.0D0
          HPG(4) = 25.D0/81.0D0
          HPG(5) = 40.D0/81.0D0
          HPG(6) = 40.D0/81.0D0
          HPG(7) = 40.D0/81.0D0
          HPG(8) = 40.D0/81.0D0
          HPG(9) = 64.D0/81.0D0
          XPG(1) = -0.774596669241483D0
          YPG(1) = -0.774596669241483D0
          XPG(2) =  0.774596669241483D0
          YPG(2) = -0.774596669241483D0
          XPG(3) =  0.774596669241483D0
          YPG(3) =  0.774596669241483D0
          XPG(4) = -0.774596669241483D0
          YPG(4) =  0.774596669241483D0
          XPG(5) = ZERO
          YPG(5) = -0.774596669241483D0
          XPG(6) =  0.774596669241483D0
          YPG(6) = ZERO
          XPG(7) = ZERO
          YPG(7) =  0.774596669241483D0
          XPG(8) = -0.774596669241483D0
          YPG(8) = ZERO
          XPG(9) = ZERO
          YPG(9) = ZERO
        ELSEIF (FAPG.EQ.'FPG16') THEN
          H(1) =  0.652145154862546D0
          H(2) =  H(1)
          H(3) =  0.347854845137454D0
          H(4) =  H(3)
          A(1) = -0.339981043584856D0
          A(2) = -A(1)
          A(3) = -0.861136311594053D0
          A(4) = -A(3)
          NPAR = 4
          NPI = 0
          DO 130 IX = 1,NPAR
            DO 132 IY = 1,NPAR
              NPI = NPI + 1
              XPG(NPI) = A(IX)
              YPG(NPI) = A(IY)
              HPG(NPI) = H(IX)*H(IY)
 132        CONTINUE
 130      CONTINUE
        ELSEIF (FAPG.EQ.'FPG4NOS') THEN
C ------- POUR LES POINTS DE GAUSS -------------------------------------
          XPG(1) = -0.577350269189626D0
          YPG(1) = -0.577350269189626D0
          XPG(2) =  0.577350269189626D0
          YPG(2) = -0.577350269189626D0
          XPG(3) =  0.577350269189626D0
          YPG(3) =  0.577350269189626D0
          XPG(4) = -0.577350269189626D0
          YPG(4) =  0.577350269189626D0
          HPG(1) = UN
          HPG(2) = UN
          HPG(3) = UN
          HPG(4) = UN
C ------- POUR LES SOMMETS ---------------------------------------------
          DO 200,INO = 1,NNOS
             HPG(INO+4) = VOL/NNOS
C ---------- ON UTILISE LE FAIT QUE LES SOMMETS SONT TOUJOURS ----------
C ---------- NUMEROTES EN PREMIER --------------------------------------
             XPG(INO+4) = XNO(NDIM* (INO-1)+1)
             IF (NDIM.GE.2) YPG(INO+4) = XNO(NDIM* (INO-1)+2)
             IF (NDIM.EQ.3) ZPG(INO+4) = XNO(NDIM* (INO-1)+3)
 200      CONTINUE
        ELSE
        VALK (1) = ELREFA
        VALK (2) = FAPG
          CALL U2MESG('F', 'ELEMENTS4_85',2,VALK,0,0,0,0.D0)
        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'SE2' .OR. ELREFA.EQ.'SE3' .OR.
     &         ELREFA.EQ.'SE4') THEN

        IF (FAPG.EQ.'FPG2') THEN
          XPG(1) = 0.577350269189626D0
          XPG(2) = -XPG(1)
          HPG(1) = UN
          HPG(2) = HPG(1)

        ELSE IF (FAPG.EQ.'FPG3') THEN
          XPG(1) = -0.774596669241483D0
          XPG(2) = 0.D0
          XPG(3) = 0.774596669241483D0
          HPG(1) = 0.555555555555556D0
          HPG(2) = 0.888888888888889D0
          HPG(3) = 0.555555555555556D0

        ELSE IF (FAPG.EQ.'FPG4') THEN
          XPG(1) = 0.339981043584856D0
          XPG(2) = -XPG(1)
          XPG(3) = 0.861136311594053D0
          XPG(4) = -XPG(3)
          HPG(1) = 0.652145154862546D0
          HPG(2) = HPG(1)
          HPG(3) = 0.347854845137454D0
          HPG(4) = HPG(3)

        ELSE
        VALK (1) = ELREFA
        VALK (2) = FAPG
          CALL U2MESG('F', 'ELEMENTS4_85',2,VALK,0,0,0,0.D0)
        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'PO1' ) THEN
          HPG(1) = 1.D0


C     ------------------------------------------------------------------
      ELSE
        VALK (1) = ELREFA
        CALL U2MESG('F', 'ELEMENTS4_88',1,VALK,0,0,0,0.D0)
        CALL ASSERT(.FALSE.)

      END IF


  170 CONTINUE
C     ------------------------------------------------------------------
      DO 180 I = 1,NBPG
        POIPG(I) = HPG(I)
        IF (NDIM.GE.1) COOPG(NDIM* (I-1)+1) = XPG(I)
        IF (NDIM.GE.2) COOPG(NDIM* (I-1)+2) = YPG(I)
        IF (NDIM.EQ.3) COOPG(NDIM* (I-1)+3) = ZPG(I)
  180 CONTINUE

      END
