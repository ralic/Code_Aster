      SUBROUTINE CARREF(ELREFZ,NDIM,NNO,NNOS,NBFPG,NBPG,X)
      IMPLICIT NONE
      INTEGER NDIM,NNO,NNOS,NBFPG,NBPG(*)
      REAL*8 X(*)
      CHARACTER*(*) ELREFZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 08/09/2003   AUTEUR VABHHTS J.PELLET 
C TOLE CRP_20
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
      INTEGER I
      REAL*8 XIN(27),YIN(27),UN,UNTIER,ZERO
C DEB ------------------------------------------------------------------

      ELREFE = ELREFZ
      UN = 1.0D0
      UNTIER = 1.0D0/3.0D0
      ZERO = 0.0D0


C     ------------------------------------------------------------------
      IF (ELREFE.EQ.'HE8' .OR. ELREFE.EQ.'H20' .OR. ELREFE.EQ.'H27' .OR.
     &    ELREFE.EQ.'PE6' .OR. ELREFE.EQ.'P15' .OR. ELREFE.EQ.'TE4' .OR.
     &    ELREFE.EQ.'T10' .OR. ELREFE.EQ.'PY5' .OR.
     &    ELREFE.EQ.'P13') THEN
C       CARREF N'EST PAS ADAPTEE AUX ELREFA :
C       IL FAUT UTILISER ELRACA ET ELRAGA
        CALL ASSERT(.FALSE.)


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
     &         ELREFE.EQ.'QUAI4' .OR. ELREFE.EQ.'QUAS4') THEN

        NDIM = 2
        NNO = 4
        NNOS = NNO
        IF (ELREFE.EQ.'QUAD4' .OR. ELREFE.EQ.'QUAD4L') THEN
          NBFPG = 4
          NBPG(1) = 4
          NBPG(2) = 4
          NBPG(3) = 4
          NBPG(4) = 0
        ELSE IF (ELREFE.EQ.'QUAS4') THEN
          NBFPG = 2
          NBPG(1) = 1
          NBPG(2) = 4
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

      DO 10 I = 0,NNO - 1
        X(NDIM*I+1) = XIN(I+1)
        IF ((NDIM.EQ.2) .OR. (NDIM.EQ.3)) THEN
          X(NDIM*I+2) = YIN(I+1)
        END IF
   10 CONTINUE

      CALL ASSERT(NDIM.LE.2)

      END
