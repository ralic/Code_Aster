      SUBROUTINE ELRAGA(ELREFZ,FAPG,NDIM,NBPG,COOPG,POIPG)
      IMPLICIT NONE
      REAL*8 COOPG(*),POIPG(*)
      CHARACTER*(*) ELREFZ
      INTEGER NBPG,NDIM
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2003   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*8 ELREFA,FAPG,NOFPG(10)
      INTEGER I,NPAR,NPI,IX,IY,IZ,NPX,NPYZ
      INTEGER NNO,NNOS,NBFPG,NBPG1(10),INO,IFAM,INDIK8
      REAL*8 XPG(27),YPG(27),ZPG(27),HPG(27),A(3),H(3)
      REAL*8 ATY(7),ATZ(7),HT(7)
      REAL*8 AA,BB,CC,HH,H1,H2,H3,RAC5,RAC15,A1,B1,B6,C1,C8,D1,D12
      REAL*8 P1,P2,P3,XXG5(5),PXG5(5)
      REAL*8 ZERO,UNQUAR,UNDEMI,UN,DEUX,XNO(3*27),VOL
C DEB ------------------------------------------------------------------

      ELREFA = ELREFZ

      ZERO = 0.0D0
      UNQUAR = 0.25D0
      UNDEMI = 0.5D0
      UN = 1.0D0
      DEUX = 2.0D0


C     -- CALCUL DE NBPG,NDIM,VOL,NNO,XNO :
C     ------------------------------------
      CALL ELRACA(ELREFA,NDIM,NNO,NNOS,NBFPG,NOFPG,NBPG1,XNO,VOL)
      IFAM = INDIK8(NOFPG,FAPG,1,NBFPG)
      CALL ASSERT(IFAM.GT.0)
      NBPG = NBPG1(IFAM)
      CALL ASSERT(NDIM.GE.1)
      CALL ASSERT(NDIM.LE.3)


C     -- TRAITEMENT GENERIQUE DE FAPG='NOEU' :
C     -----------------------------------------
      IF (FAPG.EQ.'NOEU') THEN
        CALL ASSERT(NBPG.EQ.NNO)
        DO 10,INO = 1,NNO
          HPG(INO) = VOL/NNO
          XPG(INO) = XNO(NDIM* (INO-1)+1)
          IF (NDIM.GE.2) YPG(INO) = XNO(NDIM* (INO-1)+2)
          IF (NDIM.EQ.3) ZPG(INO) = XNO(NDIM* (INO-1)+3)
   10   CONTINUE
        GO TO 120
      END IF


C     -- TRAITEMENT GENERIQUE DE FAPG='NOEU_S' :
C     -----------------------------------------
      IF (FAPG.EQ.'NOEU_S') THEN
        CALL ASSERT(NBPG.EQ.NNOS)
        DO 20,INO = 1,NNOS
          HPG(INO) = VOL/NNOS
C         -- ON UTILISE LE FAIT QUE LES SOMMETS SONT TOUJOURS
C            NUMEROTES EN PREMIER :
          XPG(INO) = XNO(NDIM* (INO-1)+1)
          IF (NDIM.GE.2) YPG(INO) = XNO(NDIM* (INO-1)+2)
          IF (NDIM.EQ.3) ZPG(INO) = XNO(NDIM* (INO-1)+3)
   20   CONTINUE
        GO TO 120
      END IF


C     -- TRAITEMENT GENERIQUE DE FAPG='FPG1' :
C     -----------------------------------------
      IF (FAPG.EQ.'FPG1') THEN
        CALL ASSERT(NBPG.EQ.1)
        XPG(1) = 0.D0
        IF (NDIM.GE.2) YPG(1) = 0.D0
        IF (NDIM.EQ.3) ZPG(1) = 0.D0
        DO 30,INO = 1,NNO
          XPG(1) = XPG(1) + XNO(NDIM* (INO-1)+1)
          IF (NDIM.GE.2) YPG(1) = YPG(1) + XNO(NDIM* (INO-1)+2)
          IF (NDIM.EQ.3) ZPG(1) = ZPG(1) + XNO(NDIM* (INO-1)+3)
   30   CONTINUE
        XPG(1) = XPG(1)/NNO
        YPG(1) = YPG(1)/NNO
        ZPG(1) = ZPG(1)/NNO
        HPG(1) = VOL
        GO TO 120
      END IF


C     ------------------------------------------------------------------
      IF (ELREFA.EQ.'HE8' .OR. ELREFA.EQ.'H20' .OR.
     &    ELREFA.EQ.'H27') THEN
     
        NPAR=0
        
        IF (FAPG.EQ.'FPG8') THEN
C --------- FORMULE DE QUADRATURE DE GAUSS A 2 POINTS DANS CHAQUE
C           DIRECTION ( ORDRE 3 )
          NPAR = 2
          A(1) = -0.577350269189626D00
          A(2) = -A(1)
          H(1) = UN
          H(2) = UN

        ELSE IF (FAPG.EQ.'FPG27') THEN
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
          XXG5(3) =  0.D0
          XXG5(4) =  0.538469310105683D0
          XXG5(5) =  0.906179845938664D0
C
          PXG5(1) =  0.236926885056189D0
          PXG5(2) =  0.478628670499366D0
          PXG5(3) =  0.568888888888889D0
          PXG5(4) =  0.478628670499366D0
          PXG5(5) =  0.236926885056189D0
             
          DO 140 IZ = 1,5
             XPG(IZ) = 0.D0
             YPG(IZ) = 0.D0
             ZPG(IZ) = XXG5(IZ)
             HPG(IZ) = PXG5(IZ)
 140      CONTINUE

        END IF
C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'PE6' .OR. ELREFA.EQ.'P15') THEN

        IF (FAPG.EQ.'FPG6') THEN
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

        ELSE IF (FAPG.EQ.'FPG8') THEN

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

        ELSE IF (FAPG.EQ.'FPG21') THEN

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
        DO 80 IX = 1,NPX
          DO 70 IY = 1,NPYZ
            NPI = NPI + 1
            XPG(NPI) = A(IX)
            YPG(NPI) = ATY(IY)
            ZPG(NPI) = ATZ(IY)
            HPG(NPI) = H(IX)*HT(IY)
   70     CONTINUE
   80   CONTINUE

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'TE4' .OR. ELREFA.EQ.'T10') THEN

        IF (FAPG.EQ.'FPG4') THEN

C --------- FORMULE A 4 POINTS :  (CF TOUZOT PAGE 300)
C                   ORDRE 2 EN X Y Z

          RAC5 = SQRT(5.D00)
          AA = (5.D00-RAC5)/20.D00
          BB = (5.D00+3.D00*RAC5)/20.D00
          HH = UN/24.D00
          NPI = 0
          DO 90 I = 1,4
            NPI = NPI + 1
            XPG(NPI) = AA
            YPG(NPI) = AA
            ZPG(NPI) = AA
            HPG(NPI) = HH
   90     CONTINUE
          ZPG(2) = BB
          YPG(3) = BB
          XPG(4) = BB

        ELSE IF (FAPG.EQ.'FPG5') THEN

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
          DO 100 I = 2,5
            XPG(I) = BB
            YPG(I) = BB
            ZPG(I) = BB
            HPG(I) = H2
  100     CONTINUE
          ZPG(3) = CC
          YPG(4) = CC
          XPG(5) = CC

        ELSE IF (FAPG.EQ.'FPG15') THEN

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

          DO 110 I = 1,27
            HPG(I) = HPG(I)*UNQUAR* (UN-ZPG(I))* (UN-ZPG(I))
  110     CONTINUE
        END IF

      ELSE
        CALL ASSERT(.FALSE.)
      END IF


  120 CONTINUE
C     ------------------------------------------------------------------
      DO 130 I = 1,NBPG
        POIPG(I) = HPG(I)
        COOPG(NDIM* (I-1)+1) = XPG(I)
        IF (NDIM.GE.2) COOPG(NDIM* (I-1)+2) = YPG(I)
        IF (NDIM.EQ.3) COOPG(NDIM* (I-1)+3) = ZPG(I)
  130 CONTINUE

      END
