      SUBROUTINE ELRFGF(ELREFZ,NUFAPG,NBPG,DCOO,COOPG,DPOI,POIPG)
      IMPLICIT NONE
      INTEGER NUFAPG,NBPG(*),DCOO,DPOI
      REAL*8 COOPG(*),POIPG(*)
      CHARACTER*(*) ELREFZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/09/2003   AUTEUR VABHHTS J.PELLET 
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
      INTEGER I,NPG,NCMP,NPAR,NPI,IX,IY
      REAL*8 XPG(27),YPG(27),HPG(27),A(3),H(3)
      REAL*8 U,XA,XB,XC,XD,P1,P2,P3,T
      REAL*8 ZERO,UNDEMI,UN,DEUX

C -----  FONCTIONS FORMULES
      T(U) = 2.0D0*U - 1.0D0
C DEB ------------------------------------------------------------------

      ELREFE = ELREFZ
      ZERO = 0.0D0
      UNDEMI = 0.5D0
      UN = 1.0D0
      DEUX = 2.0D0
      NPG = NBPG(NUFAPG)

      IF (ELREFE.EQ.'HE8' .OR. ELREFE.EQ.'H20' .OR. ELREFE.EQ.'H27' .OR.
     &    ELREFE.EQ.'PE6' .OR. ELREFE.EQ.'P15' .OR. ELREFE.EQ.'TE4' .OR.
     &    ELREFE.EQ.'T10' .OR. ELREFE.EQ.'PY5' .OR.
     &    ELREFE.EQ.'P13') THEN
C       ELRFGF N'EST PAS ADAPTEE AUX ELREFA :
C       IL FAUT UTILISER ELRACA ET ELRAGA
        CALL ASSERT(.FALSE.)


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
        DO 20 IX = 1,NPAR
          DO 10 IY = 1,NPAR
            NPI = NPI + 1
            XPG(NPI) = A(IX)
            YPG(NPI) = A(IY)
            HPG(NPI) = H(IX)*H(IY)
   10     CONTINUE
   20   CONTINUE

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
     &         ELREFE.EQ.'QUAD4D' .OR. ELREFE.EQ.'QUAS4') THEN

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
        ELSE IF (NPG.EQ.1) THEN
          HPG(1) = 4.D0
          XPG(1) = ZERO
          YPG(1) = ZERO
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

      DO 30 I = 0,NPG - 1
        COOPG(NCMP*I+1) = XPG(I+1)
        IF ((NCMP.EQ.2) .OR. (NCMP.EQ.3)) THEN
          COOPG(NCMP*I+2) = YPG(I+1)
        END IF
   30 CONTINUE

      CALL ASSERT(NCMP.LE.2)

      DO 40 I = 1,NPG
        POIPG(I) = HPG(I)
   40 CONTINUE

      IF ((DCOO.LT.NCMP) .OR. (DPOI.LT.NPG)) THEN
        CALL UTMESS('F','ELRFGF',' ERREUR PROGRAMMEUR'//
     &              ' ECRASEMENT DE TABLEAU')
      END IF

      END
