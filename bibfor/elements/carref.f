      SUBROUTINE CARREF(ELREFZ,NDIM,NNO,NNOS,NBFPG,NBPG,X)
      IMPLICIT NONE
      INTEGER NDIM,NNO,NNOS,NBFPG,NBPG(*)
      REAL*8 X(*)
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

C BUT :  RETOURNE LES CARACTERISTIQUES DE L'ELREFE
C ----------------------------------------------------------------------
C   IN   ELREFZ : NOM DE L'ELREFE (K8)
C   OUT  NDIM   : DIMENSION DU PROBLEME
C        NNO    : NOMBRE DE NOEUDS
C        NNOS   : NOMBRE DE NOEUDS SOMMETS
C        NBFPG  : NOMBRE DE FAMILLE DE POINTS DE GAUSS
C        NBPG(*): NOMBRE DE POINTS DE GAUSS POUR CHAQUE FAMILLE
C        X(*)   : COORDONNEES DES NOEUDS DE L'ELREFE
C   -------------------------------------------------------------------
      CHARACTER*8 ELREFE
      INTEGER I,J
      REAL*8 XIN(27),YIN(27),ZIN(27),UN,UNTIER,ZERO
C DEB ------------------------------------------------------------------

      ELREFE = ELREFZ
      UN = 1.0D0
      UNTIER = 1.0D0/3.0D0
      ZERO = 0.0D0
C     ------------------------------------------------------------------
      IF (ELREFE.EQ.'HEXA8' .OR. ELREFE.EQ.'HEXI8') THEN
        NDIM = 3
        NNO = 8
        NNOS = NNO
        NBFPG = 2
        NBPG(1) = 8
        NBPG(2) = 8
        IF (ELREFE.EQ.'HEXI8') NBPG(2) = 27

        DO 10 I = 1,8
          XIN(I) = -1.D0
          YIN(I) = -1.D0
          ZIN(I) = -1.D0
   10   CONTINUE
        DO 20 I = 1,4
          ZIN(I+4) = +1.D0
   20   CONTINUE
        DO 30 I = 1,2
          XIN(I+1) = +1.D0
          XIN(I+5) = +1.D0
          YIN(I+2) = +1.D0
          YIN(I+6) = +1.D0
   30   CONTINUE

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'HEXA20' .OR. ELREFE.EQ.'HEXD20' .OR.
     &         ELREFE.EQ.'HEXS20' .OR. ELREFE.EQ.'HEXI20') THEN

        NDIM = 3
        NNO = 20
        NNOS = 8
        IF (ELREFE.EQ.'HEXA20') THEN
          NBFPG = 3
          NBPG(1) = 27
          NBPG(2) = 27
          NBPG(3) = 8
        ELSE IF (ELREFE.EQ.'HEXS20') THEN
          NBFPG = 3
          NBPG(1) = 8
          NBPG(2) = 27
          NBPG(3) = 8
        ELSE IF (ELREFE.EQ.'HEXD20') THEN
          NBFPG = 4
          NBPG(1) = 27
          NBPG(2) = 27
          NBPG(3) = 8
          NBPG(4) = 8
        ELSE IF (ELREFE.EQ.'HEXI20') THEN
          NBFPG = 2
          NBPG(1) = 8
          NBPG(2) = 27
        END IF

        DO 40 I = 1,20
          XIN(I) = -1.D0
          YIN(I) = -1.D0
          ZIN(I) = -1.D0
   40   CONTINUE

        XIN(2) = +1.D0
        XIN(3) = +1.D0
        YIN(3) = +1.D0
        YIN(4) = +1.D0

        ZIN(5) = +1.D0
        XIN(6) = +1.D0
        ZIN(6) = +1.D0
        XIN(7) = +1.D0
        YIN(7) = +1.D0
        ZIN(7) = +1.D0
        YIN(8) = +1.D0
        ZIN(8) = +1.D0

        XIN(9) = 0.D0
        XIN(10) = +1.D0
        YIN(10) = 0.D0
        XIN(11) = 0.D0
        YIN(11) = +1.D0
        YIN(12) = 0.D0

        ZIN(13) = 0.D0
        XIN(14) = +1.D0
        ZIN(14) = 0.D0
        XIN(15) = +1.D0
        YIN(15) = +1.D0
        ZIN(15) = 0.D0
        YIN(16) = +1.D0
        ZIN(16) = 0.D0

        XIN(17) = 0.D0
        ZIN(17) = +1.D0
        XIN(18) = +1.D0
        YIN(18) = 0.D0
        ZIN(18) = +1.D0
        XIN(19) = 0.D0
        YIN(19) = +1.D0
        ZIN(19) = +1.D0
        YIN(20) = 0.D0
        ZIN(20) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'HEXA27') THEN

        NDIM = 3
        NNO = 27
        NNOS = 8
        NBFPG = 3
        NBPG(1) = 27
        NBPG(2) = 27
        NBPG(3) = 8

        DO 50 I = 1,20
          XIN(I) = -1.D0
          YIN(I) = -1.D0
          ZIN(I) = -1.D0
   50   CONTINUE

C   NOEUDS SOMMETS

        XIN(2) = +1.D0
        XIN(3) = +1.D0
        YIN(3) = +1.D0
        YIN(4) = +1.D0

        ZIN(5) = +1.D0
        XIN(6) = +1.D0
        ZIN(6) = +1.D0
        XIN(7) = +1.D0
        YIN(7) = +1.D0
        ZIN(7) = +1.D0
        YIN(8) = +1.D0
        ZIN(8) = +1.D0

C   NOEUDS MILIEUX DES ARETES

        XIN(9) = 0.D0
        XIN(10) = +1.D0
        YIN(10) = 0.D0
        XIN(11) = 0.D0
        YIN(11) = +1.D0
        YIN(12) = 0.D0

        ZIN(13) = 0.D0
        XIN(14) = +1.D0
        ZIN(14) = 0.D0
        XIN(15) = +1.D0
        YIN(15) = +1.D0
        ZIN(15) = 0.D0
        YIN(16) = +1.D0
        ZIN(16) = 0.D0

        XIN(17) = 0.D0
        ZIN(17) = +1.D0
        XIN(18) = +1.D0
        YIN(18) = 0.D0
        ZIN(18) = +1.D0
        XIN(19) = 0.D0
        YIN(19) = +1.D0
        ZIN(19) = +1.D0
        YIN(20) = 0.D0
        ZIN(20) = +1.D0

C    NOEUDS MILIEUX DES FACES ET BARYCENTRE

        DO 60 I = 21,27
          XIN(I) = 0.D0
          YIN(I) = 0.D0
          ZIN(I) = 0.D0
   60   CONTINUE
        ZIN(21) = -1.D0
        YIN(22) = -1.D0
        XIN(23) = +1.D0
        YIN(24) = +1.D0
        XIN(25) = -1.D0
        ZIN(26) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TETRA4' .OR. ELREFE.EQ.'TETRI4') THEN

        NDIM = 3
        NNO = 4
        NNOS = NNO
        NBFPG = 2
        NBPG(1) = 4
        NBPG(2) = 4
        IF (ELREFE.EQ.'TETRI4') NBPG(2) = 15

        DO 70 I = 1,4
          XIN(I) = 0.D0
          YIN(I) = 0.D0
          ZIN(I) = 0.D0
   70   CONTINUE
        YIN(1) = +1.D0
        ZIN(2) = +1.D0
        XIN(4) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TETRA10' .OR. ELREFE.EQ.'TETRA10D' .OR.
     &         ELREFE.EQ.'TETRI10') THEN

        NDIM = 3
        NNO = 10
        NNOS = 4
        IF (ELREFE.EQ.'TETRA10') THEN
          NBFPG = 3
          NBPG(1) = 15
          NBPG(2) = 15
          NBPG(3) = 4
        ELSE IF (ELREFE.EQ.'TETRA10D') THEN
          NBFPG = 4
          NBPG(1) = 15
          NBPG(2) = 15
          NBPG(3) = 4
          NBPG(4) = 4
        ELSE IF (ELREFE.EQ.'TETRI10') THEN
          NBFPG = 2
          NBPG(1) = 4
          NBPG(2) = 15
        END IF

        DO 80 I = 1,10
          XIN(I) = 0.D0
          YIN(I) = 0.D0
          ZIN(I) = 0.D0
   80   CONTINUE
        XIN(4) = +1.D0
        YIN(1) = +1.D0
        ZIN(2) = +1.D0
        XIN(8) = +0.5D0
        XIN(9) = +0.5D0
        XIN(10) = +0.5D0
        YIN(5) = +0.5D0
        YIN(7) = +0.5D0
        YIN(8) = +0.5D0
        ZIN(5) = +0.5D0
        ZIN(6) = +0.5D0
        ZIN(9) = +0.5D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'PENTA6') THEN

        NDIM = 3
        NNO = 6
        NNOS = NNO
        NBFPG = 2
        NBPG(1) = 6
        NBPG(2) = 6

        DO 90 I = 1,6
          YIN(I) = 0.D0
          ZIN(I) = 0.D0
   90   CONTINUE
        DO 100 I = 1,3
          XIN(I) = -1.D0
          XIN(I+3) = 1.D0
  100   CONTINUE
        YIN(1) = +1.D0
        YIN(4) = +1.D0
        ZIN(2) = +1.D0
        ZIN(5) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'PENTA15' .OR. ELREFE.EQ.'PENTA15D') THEN

        NDIM = 3
        NNO = 15
        NNOS = 6
        NBFPG = 3
        NBPG(1) = 21
        NBPG(2) = 21
        NBPG(3) = 21
        IF (ELREFE.EQ.'PENTA15D') THEN
          NBFPG = 4
          NBPG(3) = 6
          NBPG(4) = 21
        END IF

        DO 110 I = 1,15
          XIN(I) = 0.D0
          YIN(I) = 0.D0
          ZIN(I) = 0.D0
  110   CONTINUE
        DO 120 I = 1,3
          XIN(I) = -1.D0
          XIN(I+6) = -1.D0
          XIN(I+3) = 1.D0
          XIN(I+12) = 1.D0
  120   CONTINUE
        YIN(1) = 1.D0
        YIN(4) = 1.D0
        YIN(10) = 1.D0
        ZIN(2) = 1.D0
        ZIN(5) = 1.D0
        ZIN(11) = 1.D0
        DO 140 I = 1,2
          DO 130 J = 1,2
            YIN(6*I+2*J-1) = +0.5D0
            ZIN(6*I+J) = +0.5D0
  130     CONTINUE
  140   CONTINUE

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'PYRAM5') THEN

        NDIM = 3
        NNO = 5
        NNOS = NNO
        NBFPG = 1
        NBPG(1) = 5

        XIN(1) = +1.D0
        YIN(1) = 0.D0
        ZIN(1) = 0.D0

        XIN(2) = 0.D0
        YIN(2) = +1.D0
        ZIN(2) = 0.D0

        XIN(3) = -1.D0
        YIN(3) = 0.D0
        ZIN(3) = 0.D0

        XIN(4) = 0.D0
        YIN(4) = -1.D0
        ZIN(4) = 0.D0

        XIN(5) = 0.D0
        YIN(5) = 0.D0
        ZIN(5) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'PYRAM13' .OR. ELREFE.EQ.'PYRAM13D') THEN

        NDIM = 3
        NNO = 13
        NNOS = 5
        NBFPG = 1
        NBPG(1) = 27
        IF (ELREFE.EQ.'PYRAM13D') THEN
          NBFPG = 2
          NBPG(2) = 5
        END IF

        XIN(1) = +1.D0
        YIN(1) = 0.D0
        ZIN(1) = 0.D0

        XIN(2) = 0.D0
        YIN(2) = +1.D0
        ZIN(2) = 0.D0

        XIN(3) = -1.D0
        YIN(3) = 0.D0
        ZIN(3) = 0.D0

        XIN(4) = 0.D0
        YIN(4) = -1.D0
        ZIN(4) = 0.D0

        XIN(5) = 0.D0
        YIN(5) = 0.D0
        ZIN(5) = +1.D0

        XIN(6) = +0.5D0
        YIN(6) = +0.5D0
        ZIN(6) = 0.D0

        XIN(7) = -0.5D0
        YIN(7) = +0.5D0
        ZIN(7) = 0.D0

        XIN(8) = -0.5D0
        YIN(8) = -0.5D0
        ZIN(8) = 0.D0

        XIN(9) = +0.5D0
        YIN(9) = -0.5D0
        ZIN(9) = 0.D0

        XIN(10) = +0.5D0
        YIN(10) = 0.D0
        ZIN(10) = +0.5D0

        XIN(11) = 0.D0
        YIN(11) = +0.5D0
        ZIN(11) = +0.5D0

        XIN(12) = -0.5D0
        YIN(12) = 0.D0
        ZIN(12) = +0.5D0

        XIN(13) = 0.D0
        YIN(13) = -0.5D0
        ZIN(13) = +0.5D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'FACE4') THEN

        NDIM = 2
        NNO = 4
        NNOS = NNO
        NBFPG = 2
        NBPG(1) = 4
        NBPG(2) = 4

        XIN(1) = -1.D0
        YIN(1) = -1.D0
        XIN(2) = +1.D0
        YIN(2) = -1.D0
        XIN(3) = +1.D0
        YIN(3) = +1.D0
        XIN(4) = -1.D0
        YIN(4) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'FACE8') THEN

        NDIM = 2
        NNO = 8
        NNOS = NNO
        NBFPG = 2
        NBPG(1) = 9
        NBPG(2) = 9

        XIN(1) = -1.D0
        XIN(5) = 0.D0
        XIN(2) = +1.D0
        XIN(6) = +1.D0
        XIN(3) = +1.D0
        XIN(7) = 0.D0
        XIN(4) = -1.D0
        XIN(8) = -1.D0
        YIN(1) = -1.D0
        YIN(5) = -1.D0
        YIN(2) = -1.D0
        YIN(6) = +0.D0
        YIN(3) = +1.D0
        YIN(7) = +1.D0
        YIN(4) = +1.D0
        YIN(8) = +0.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'FACE9') THEN

        NDIM = 2
        NNO = 9
        NNOS = NNO
        NBFPG = 2
        NBPG(1) = 9
        NBPG(2) = 9

        XIN(1) = -1.D0
        XIN(5) = 0.D0
        XIN(2) = +1.D0
        XIN(6) = +1.D0
        XIN(3) = +1.D0
        XIN(7) = 0.D0
        XIN(4) = -1.D0
        XIN(8) = -1.D0
        XIN(9) = 0.D0
        YIN(1) = -1.D0
        YIN(5) = -1.D0
        YIN(2) = -1.D0
        YIN(6) = +0.D0
        YIN(3) = +1.D0
        YIN(7) = +1.D0
        YIN(4) = +1.D0
        YIN(8) = +0.D0
        YIN(9) = 0.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'FACE3') THEN

        NDIM = 2
        NNO = 3
        NNOS = NNO
        NBFPG = 2
        NBPG(1) = 3
        NBPG(2) = 3

        XIN(1) = 0.D0
        XIN(2) = +1.D0
        XIN(3) = 0.D0
        YIN(1) = 0.D0
        YIN(2) = 0.D0
        YIN(3) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'FACE6') THEN

        NDIM = 2
        NNO = 6
        NNOS = NNO
        NBFPG = 2
        NBPG(1) = 4
        NBPG(2) = 6

        XIN(1) = 0.D0
        XIN(4) = +0.5D0
        XIN(2) = +1.D0
        XIN(5) = +0.5D0
        XIN(3) = 0.D0
        XIN(6) = 0.D0
        YIN(1) = 0.D0
        YIN(4) = 0.D0
        YIN(2) = 0.D0
        YIN(5) = +0.5D0
        YIN(3) = +1.D0
        YIN(6) = +0.5D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'MEDKTR3' .OR. ELREFE.EQ.'MEGRDKT' .OR.
     &         ELREFE.EQ.'MEDSTR3' .OR. ELREFE.EQ.'MEGRDKT') THEN
        NNO = 3
        NNOS = 3
        NDIM = 2
        NBFPG = 1
        NBPG(1) = 3

        XIN(1) = 0.D0
        XIN(2) = +1.D0
        XIN(3) = 0.D0
        YIN(1) = 0.D0
        YIN(2) = 0.D0
        YIN(3) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'MEDKQU4' .OR. ELREFE.EQ.'MEGRDKQ' .OR.
     &         ELREFE.EQ.'MEDSQU4' .OR. ELREFE.EQ.'MEQ4QU4') THEN
        NNO = 4
        NNOS = 4
        NDIM = 2
        NBFPG = 1
        NBPG(1) = 4

        XIN(1) = -1.D0
        XIN(2) = +1.D0
        XIN(3) = +1.D0
        XIN(4) = -1.D0
        YIN(1) = -1.D0
        YIN(2) = -1.D0
        YIN(3) = +1.D0
        YIN(4) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TRIA3' .OR. ELREFE.EQ.'TRIA3L' .OR.
     &         ELREFE.EQ.'TRIA3H' .OR. ELREFE.EQ.'TRII3') THEN

        NDIM = 2
        NNO = 3
        NNOS = NNO
        IF (ELREFE.EQ.'TRIA3' .OR. ELREFE.EQ.'TRIA3L' .OR.
     &      ELREFE.EQ.'TRIA3H') THEN
          NBFPG = 4
          NBPG(1) = 1
          NBPG(2) = 3
          NBPG(3) = 3
          NBPG(4) = 3
        ELSE IF (ELREFE.EQ.'TRII3') THEN
          NBFPG = 1
          NBPG(1) = 3
        END IF

        XIN(1) = -1.D0
        YIN(1) = +1.D0
        XIN(2) = -1.D0
        YIN(2) = -1.D0
        XIN(3) = +1.D0
        YIN(3) = -1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QUAD4' .OR. ELREFE.EQ.'QUAD4L' .OR.
     &         ELREFE.EQ.'QUAI4') THEN

        NDIM = 2
        NNO = 4
        NNOS = NNO
        IF (ELREFE.EQ.'QUAD4' .OR. ELREFE.EQ.'QUAD4L') THEN
          NBFPG = 4
          NBPG(1) = 4
          NBPG(2) = 4
          NBPG(3) = 4
          NBPG(4) = 0
        ELSE IF (ELREFE.EQ.'QUAI4') THEN
          NBFPG = 1
          NBPG(1) = 9
        END IF
        XIN(1) = -1.D0
        XIN(2) = -1.D0
        XIN(3) = +1.D0
        XIN(4) = +1.D0
        YIN(1) = +1.D0
        YIN(2) = -1.D0
        YIN(3) = -1.D0
        YIN(4) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TRIA6' .OR. ELREFE.EQ.'TRIA6H' .OR.
     &         ELREFE.EQ.'TRIA6D' .OR. ELREFE.EQ.'TRII6') THEN

        NDIM = 2
        NNO = 6
        NNOS = 3

        IF (ELREFE.EQ.'TRII6') THEN
          NBFPG = 2
          NBPG(1) = 3
          NBPG(2) = 3
        ELSE
          NBFPG = 4
          NBPG(1) = 3
          NBPG(2) = 6
          NBPG(3) = 6
          IF (ELREFE.EQ.'TRIA6D') NBPG(3) = 3
          NBPG(4) = 3
        END IF

        XIN(1) = -1.D0
        YIN(1) = +1.D0
        XIN(2) = -1.D0
        YIN(2) = -1.D0
        XIN(3) = +1.D0
        YIN(3) = -1.D0
        XIN(4) = -1.D0
        YIN(4) = 0.D0
        XIN(5) = 0.D0
        YIN(5) = -1.D0
        XIN(6) = 0.D0
        YIN(6) = 0.D0
        IF (ELREFE.EQ.'TRIA6D') THEN
          XIN(4) = 0.D0
          YIN(5) = 0.D0
        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QUAD8' .OR. ELREFE.EQ.'QUA8D' .OR.
     &         ELREFE.EQ.'QUAS8' .OR. ELREFE.EQ.'QUAI8') THEN

        NDIM = 2
        NNO = 8
        NNOS = 4
        IF (ELREFE.EQ.'QUAI8') THEN
          NBFPG = 2
          NBPG(1) = 9
          NBPG(2) = 9
        ELSE
          NBFPG = 4
          NBPG(1) = 9
          IF (ELREFE.EQ.'QUAS8') NBPG(1) = 4
          NBPG(2) = 9
          NBPG(3) = NNO
          IF (ELREFE.EQ.'QUA8D') NBPG(3) = 4
          NBPG(4) = 4
        END IF
        XIN(1) = -1.D0
        YIN(1) = -XIN(1)
        XIN(2) = XIN(1)
        YIN(2) = -YIN(1)
        XIN(3) = -XIN(2)
        YIN(3) = YIN(2)
        XIN(4) = XIN(3)
        YIN(4) = -YIN(3)
        XIN(5) = XIN(1)
        YIN(5) = 0.D0
        XIN(6) = 0.D0
        YIN(6) = YIN(2)
        XIN(7) = XIN(3)
        YIN(7) = 0.D0
        XIN(8) = 0.D0
        YIN(8) = YIN(4)
        IF (ELREFE.EQ.'QUA8D') THEN
          XIN(5) = 0.D0
          YIN(6) = 0.D0
          XIN(7) = 0.D0
          YIN(8) = 0.D0
        END IF


C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QUAD9') THEN

        NDIM = 2
        NNO = 9
        NNOS = 4
        NBFPG = 4
        NBPG(1) = 9
        NBPG(2) = 9
        NBPG(3) = NNO
        NBPG(4) = 4

        XIN(1) = -1.D0
        YIN(1) = -XIN(1)
        XIN(2) = XIN(1)
        YIN(2) = -YIN(1)
        XIN(3) = -XIN(2)
        YIN(3) = YIN(2)
        XIN(4) = XIN(3)
        YIN(4) = -YIN(3)
        XIN(5) = XIN(1)
        YIN(5) = 0.D0
        XIN(6) = 0.D0
        YIN(6) = YIN(2)
        XIN(7) = XIN(3)
        YIN(7) = 0.D0
        XIN(8) = 0.D0
        YIN(8) = YIN(4)
        XIN(9) = 0.D0
        YIN(9) = 0.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TRIL6') THEN

        NDIM = 2
        NNO = 3
        NNOS = 3
        NBFPG = 4
        NBPG(1) = 1
        NBPG(2) = 3
        NBPG(3) = NNO
        NBPG(4) = 3
        XIN(1) = -1.D0
        YIN(1) = +1.D0
        XIN(2) = -1.D0
        YIN(2) = -1.D0
        XIN(3) = +1.D0
        YIN(3) = -1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TRIA7') THEN

        NDIM = 2
        NNO = 7
        NNOS = 3
        NBFPG = 4
        NBPG(1) = 6
        NBPG(2) = 6
        NBPG(3) = NNO
        NBPG(4) = 3
        XIN(1) = 0.D0
        XIN(2) = 0.D0
        XIN(3) = 0.D0
        XIN(4) = 0.D0
        XIN(5) = 0.D0
        XIN(6) = 0.D0
        XIN(7) = 0.D0
        YIN(1) = 0.D0
        YIN(2) = 0.D0
        YIN(3) = 0.D0
        YIN(4) = 0.D0
        YIN(5) = 0.D0
        YIN(6) = 0.D0
        YIN(7) = 0.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'SEG2' .OR. ELREFE.EQ.'THCOSE2') THEN
        NDIM = 1
        NNOS = 2
        NNO = 2
        NBFPG = 1
        NBPG(1) = 2
        XIN(1) = -1.D0
        XIN(2) = 1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'SEG3' .OR. ELREFE.EQ.'THCOSE3') THEN
        NDIM = 1
        NNO = 3
        NNOS = 2
        NBFPG = 1
        NBPG(1) = 4
        XIN(1) = -1.D0
        XIN(2) = 1.D0
        XIN(3) = 0.D0
C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'CABPOU') THEN
        NDIM = 1
        NNO = 2
        NNOS = 2
        NBFPG = 1
        NBPG(1) = 1
        XIN(1) = -1.D0
        XIN(2) = 1.D0

C_______________________________________________________________________
      ELSE IF (ELREFE.EQ.'MET3SEG3' .OR. ELREFE.EQ.'MET6SEG3') THEN
        NDIM = 1
        NNO = 3
        NNOS = 2
        NBFPG = 2
        NBPG(1) = 3
        NBPG(2) = 3
        XIN(1) = -UN
        XIN(2) = UN
        XIN(3) = ZERO

C_______________________________________________________________________

      ELSE IF (ELREFE.EQ.'MET3SEG4') THEN
        NDIM = 1
        NNO = 4
        NNOS = 2
        NBFPG = 2
        NBPG(1) = 3
        NBPG(2) = 4
        XIN(1) = -UN
        XIN(2) = UN
        XIN(3) = -UNTIER
        XIN(4) = UNTIER
      ELSE
        CALL ASSERT(.FALSE.)
      END IF

C     ------------------------------------------------------------------

      DO 150 I = 0,NNO - 1
        X(NDIM*I+1) = XIN(I+1)
        IF ((NDIM.EQ.2) .OR. (NDIM.EQ.3)) THEN
          X(NDIM*I+2) = YIN(I+1)
        END IF
  150 CONTINUE

      IF (NDIM.EQ.3) THEN
        DO 160 I = 0,NNO - 1
          X(NDIM*I+3) = ZIN(I+1)
  160   CONTINUE
      END IF

      END
