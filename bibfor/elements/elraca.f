      SUBROUTINE ELRACA(ELREFZ,NDIM,NNO,NNOS,NBFPG,FAPG,NBPG,X,VOL)
      IMPLICIT NONE
      INTEGER NDIM,NNO,NNOS,NBFPG,NBPG(*)
      REAL*8 X(*),VOL
      CHARACTER*8 FAPG(*)
      CHARACTER*(*) ELREFZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 05/09/2005   AUTEUR VABHHTS J.PELLET 
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

C BUT :  RETOURNE LES CARACTERISTIQUES DE L'ELREFA
C ----------------------------------------------------------------------
C   IN   ELREFZ : NOM DE L'ELREFA (K8)
C   OUT  NDIM   : DIMENSION TOPOLOGIQUE : 1/2/3
C        NNO    : NOMBRE DE NOEUDS
C        NNOS   : NOMBRE DE NOEUDS SOMMETS
C        NBFPG  : NOMBRE DE FAMILLE DE POINTS DE GAUSS
C        FAPG(*): NOMS DES FAMILLES DE POINTS DE GAUSS
C        NBPG(*): NOMBRE DE POINTS DES FAMILLES DE POINTS DE GAUSS
C        X(*)   : COORDONNEES DES NOEUDS DE L'ELREFA
C        VOL    : VOLUME DE L'ELREFA
C   -------------------------------------------------------------------
      CHARACTER*8 ELREFA
      INTEGER I,J
      REAL*8 XIN(27),YIN(27),ZIN(27)
C DEB ------------------------------------------------------------------

      ELREFA = ELREFZ
C     ------------------------------------------------------------------
C LES ELEMENTS 3D
C     ------------------------------------------------------------------

      IF (ELREFA.EQ.'HE8') THEN
        NDIM = 3
        NNO = 8
        NNOS = 8
        VOL = 8.D0

        NBFPG = 7

        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 8
        NBPG(5) = 27
        NBPG(6) = 5
        NBPG(7) = 16

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG8'
        FAPG(5) = 'FPG27'
        FAPG(6) = 'SHB5'
        FAPG(7) = 'FPG8NOS'

        DO 10 I = 1,8
          XIN(I) = -1.D0
          YIN(I) = -1.D0
          ZIN(I) = -1.D0
   10   CONTINUE
        DO 12 I = 1,4
          ZIN(I+4) = +1.D0
   12   CONTINUE
        DO 14 I = 1,2
          XIN(I+1) = +1.D0
          XIN(I+5) = +1.D0
          YIN(I+2) = +1.D0
          YIN(I+6) = +1.D0
   14   CONTINUE

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'H20') THEN
        NDIM = 3
        NNO = 20
        NNOS = 8
        VOL = 8.D0

        NBFPG = 6

        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 8
        NBPG(5) = 27
        NBPG(6) = 16

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG8'
        FAPG(5) = 'FPG27'
        FAPG(6) = 'FPG8NOS'

        DO 20 I = 1,20
          XIN(I) = -1.D0
          YIN(I) = -1.D0
          ZIN(I) = -1.D0
   20   CONTINUE

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

        XIN(9)  =  0.D0
        XIN(10) = +1.D0
        YIN(10) =  0.D0
        XIN(11) =  0.D0
        YIN(11) = +1.D0
        YIN(12) =  0.D0

        ZIN(13) =  0.D0
        XIN(14) = +1.D0
        ZIN(14) =  0.D0
        XIN(15) = +1.D0
        YIN(15) = +1.D0
        ZIN(15) =  0.D0
        YIN(16) = +1.D0
        ZIN(16) =  0.D0

        XIN(17) =  0.D0
        ZIN(17) = +1.D0
        XIN(18) = +1.D0
        YIN(18) =  0.D0
        ZIN(18) = +1.D0
        XIN(19) =  0.D0
        YIN(19) = +1.D0
        ZIN(19) = +1.D0
        YIN(20) =  0.D0
        ZIN(20) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'H27') THEN
        NDIM = 3
        NNO = 27
        NNOS = 8
        VOL = 8.D0

        NBFPG = 5
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 8
        NBPG(5) = 27

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG8'
        FAPG(5) = 'FPG27'

        DO 30 I = 1,20
          XIN(I) = -1.D0
          YIN(I) = -1.D0
          ZIN(I) = -1.D0
   30   CONTINUE

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

        XIN(9)  =  0.D0
        XIN(10) = +1.D0
        YIN(10) =  0.D0
        XIN(11) =  0.D0
        YIN(11) = +1.D0
        YIN(12) =  0.D0

        ZIN(13) =  0.D0
        XIN(14) = +1.D0
        ZIN(14) =  0.D0
        XIN(15) = +1.D0
        YIN(15) = +1.D0
        ZIN(15) =  0.D0
        YIN(16) = +1.D0
        ZIN(16) =  0.D0

        XIN(17) =  0.D0
        ZIN(17) = +1.D0
        XIN(18) = +1.D0
        YIN(18) =  0.D0
        ZIN(18) = +1.D0
        XIN(19) =  0.D0
        YIN(19) = +1.D0
        ZIN(19) = +1.D0
        YIN(20) =  0.D0
        ZIN(20) = +1.D0

C    NOEUDS MILIEUX DES FACES ET BARYCENTRE

        DO 32 I = 21,27
          XIN(I) = 0.D0
          YIN(I) = 0.D0
          ZIN(I) = 0.D0
   32   CONTINUE
        ZIN(21) = -1.D0
        YIN(22) = -1.D0
        XIN(23) = +1.D0
        YIN(24) = +1.D0
        XIN(25) = -1.D0
        ZIN(26) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'TE4') THEN
        NDIM = 3
        NNO = 4
        NNOS = 4
        VOL = 1.D0/6.D0

        NBFPG = 7

        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 4
        NBPG(5) = 5
        NBPG(6) = 15
        NBPG(7) = 8

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG4'
        FAPG(5) = 'FPG5'
        FAPG(6) = 'FPG15'
        FAPG(7) = 'FPG4NOS'

        DO 40 I = 1,4
          XIN(I) = 0.D0
          YIN(I) = 0.D0
          ZIN(I) = 0.D0
   40   CONTINUE
        YIN(1) = +1.D0
        ZIN(2) = +1.D0
        XIN(4) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'T10') THEN
        NDIM = 3
        NNO = 10
        NNOS = 4
        VOL = 1.D0/6.D0

        NBFPG = 7

        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 4
        NBPG(5) = 5
        NBPG(6) = 15
        NBPG(7) = 8

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG4'
        FAPG(5) = 'FPG5'
        FAPG(6) = 'FPG15'
        FAPG(7) = 'FPG4NOS'

        DO 50 I = 1,10
          XIN(I) = 0.D0
          YIN(I) = 0.D0
          ZIN(I) = 0.D0
   50   CONTINUE
        XIN(4)  = +1.D0
        YIN(1)  = +1.D0
        ZIN(2)  = +1.D0
        XIN(8)  = +0.5D0
        XIN(9)  = +0.5D0
        XIN(10) = +0.5D0
        YIN(5)  = +0.5D0
        YIN(7)  = +0.5D0
        YIN(8)  = +0.5D0
        ZIN(5)  = +0.5D0
        ZIN(6)  = +0.5D0
        ZIN(9)  = +0.5D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'PE6') THEN
        NDIM = 3
        NNO = 6
        NNOS = 6
        VOL = 1.D0

        NBFPG = 8

        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 6
        NBPG(5) = 6
        NBPG(6) = 8
        NBPG(7) = 21
        NBPG(8) = 12

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG6'
        FAPG(5) = 'FPG6B'
        FAPG(6) = 'FPG8'
        FAPG(7) = 'FPG21'
        FAPG(8) = 'FPG6NOS'

        DO 60 I = 1,6
          YIN(I) = 0.D0
          ZIN(I) = 0.D0
   60   CONTINUE
        DO 62 I = 1,3
          XIN(I  ) = -1.D0
          XIN(I+3) =  1.D0
   62   CONTINUE
        YIN(1) = +1.D0
        YIN(4) = +1.D0
        ZIN(2) = +1.D0
        ZIN(5) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'P15') THEN
        NDIM = 3
        NNO = 15
        NNOS = 6
        VOL = 1.D0

        NBFPG = 7

        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 6
        NBPG(5) = 8
        NBPG(6) = 21
        NBPG(7) = 12

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG6'
        FAPG(5) = 'FPG8'
        FAPG(6) = 'FPG21'
        FAPG(7) = 'FPG6NOS'

        DO 80 I = 1,15
          XIN(I) = 0.D0
          YIN(I) = 0.D0
          ZIN(I) = 0.D0
   80   CONTINUE
        DO 82 I = 1,3
          XIN(I)    = -1.D0
          XIN(I+6)  = -1.D0
          XIN(I+3)  = +1.D0
          XIN(I+12) = +1.D0
   82   CONTINUE
        YIN(1)  = +1.D0
        YIN(4)  = +1.D0
        YIN(10) = +1.D0
        ZIN(2)  = +1.D0
        ZIN(5)  = +1.D0
        ZIN(11) = +1.D0
        DO 84 I = 1,2
          DO 86 J = 1,2
            YIN(6*I+2*J-1) = +0.5D0
            ZIN(6*I+J)     = +0.5D0
   86     CONTINUE
   84   CONTINUE

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'PY5') THEN
        NDIM = 3
        NNO = 5
        NNOS = 5
        VOL = 2.D0/3.D0

        NBFPG = 7

        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 5
        NBPG(5) = 6
        NBPG(6) = 27
        NBPG(7) = 10

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG5'
        FAPG(5) = 'FPG6'
        FAPG(6) = 'FPG27'
        FAPG(7) = 'FPG5NOS'

        XIN(1) = +1.D0
        YIN(1) =  0.D0
        ZIN(1) =  0.D0

        XIN(2) =  0.D0
        YIN(2) = +1.D0
        ZIN(2) =  0.D0

        XIN(3) = -1.D0
        YIN(3) =  0.D0
        ZIN(3) =  0.D0

        XIN(4) =  0.D0
        YIN(4) = -1.D0
        ZIN(4) =  0.D0

        XIN(5) =  0.D0
        YIN(5) =  0.D0
        ZIN(5) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'P13') THEN
        NDIM = 3
        NNO = 13
        NNOS = 5
        VOL = 2.D0/3.D0

        NBFPG = 7

        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 5
        NBPG(5) = 6
        NBPG(6) = 27
        NBPG(7) = 10

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG5'
        FAPG(5) = 'FPG6'
        FAPG(6) = 'FPG27'
        FAPG(7) = 'FPG5NOS'

        XIN(1) = +1.D0
        YIN(1) =  0.D0
        ZIN(1) =  0.D0

        XIN(2) =  0.D0
        YIN(2) = +1.D0
        ZIN(2) =  0.D0

        XIN(3) = -1.D0
        YIN(3) =  0.D0
        ZIN(3) =  0.D0

        XIN(4) =  0.D0
        YIN(4) = -1.D0
        ZIN(4) =  0.D0

        XIN(5) =  0.D0
        YIN(5) =  0.D0
        ZIN(5) = +1.D0

        XIN(6) = +0.5D0
        YIN(6) = +0.5D0
        ZIN(6) =  0.D0

        XIN(7) = -0.5D0
        YIN(7) = +0.5D0
        ZIN(7) =  0.D0

        XIN(8) = -0.5D0
        YIN(8) = -0.5D0
        ZIN(8) =  0.D0

        XIN(9) = +0.5D0
        YIN(9) = -0.5D0
        ZIN(9) =  0.D0

        XIN(10) = +0.5D0
        YIN(10) =  0.D0
        ZIN(10) = +0.5D0

        XIN(11) =  0.D0
        YIN(11) = +0.5D0
        ZIN(11) = +0.5D0

        XIN(12) = -0.5D0
        YIN(12) =  0.D0
        ZIN(12) = +0.5D0

        XIN(13) =  0.D0
        YIN(13) = -0.5D0
        ZIN(13) = +0.5D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'TR3') THEN
        NDIM = 2
        NNO = 3
        NNOS = 3
        VOL = 1.D0/2.D0

        NBFPG = 10
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 3
        NBPG(5) = 4
        NBPG(6) = 6
        NBPG(7) = 7
        NBPG(8) = 12
        NBPG(9) = 3
        NBPG(10) = 6

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG3'
        FAPG(5) = 'FPG4'
        FAPG(6) = 'FPG6'
        FAPG(7) = 'FPG7'
        FAPG(8) = 'FPG12'
        FAPG(9) = 'COT3'
        FAPG(10) = 'FPG3NOS'

        XIN(1) = 0.D0
        YIN(1) = 0.D0

        XIN(2) = +1.D0
        YIN(2) =  0.D0

        XIN(3) =  0.D0
        YIN(3) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'TR6') THEN
        NDIM = 2
        NNO = 6
        NNOS = 3
        VOL = 1.D0/2.D0

        NBFPG = 9
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 3
        NBPG(5) = 4
        NBPG(6) = 6
        NBPG(7) = 7
        NBPG(8) = 12
        NBPG(9) = 6

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG3'
        FAPG(5) = 'FPG4'
        FAPG(6) = 'FPG6'
        FAPG(7) = 'FPG7'
        FAPG(8) = 'FPG12'
        FAPG(9) = 'FPG3NOS'

        XIN(1) = 0.D0
        YIN(1) = 0.D0

        XIN(2) = +1.D0
        YIN(2) =  0.D0

        XIN(3) =  0.D0
        YIN(3) = +1.D0

        XIN(4) = 0.5D0
        YIN(4) = 0.D0

        XIN(5) = 0.5D0
        YIN(5) = 0.5D0

        XIN(6) = 0.D0
        YIN(6) = 0.5D0

C     -------------------------------------------------------
      ELSE IF (ELREFA.EQ.'TR7') THEN
        NDIM = 2
        NNO = 7
        NNOS = 3
        VOL  = 1.D0/2.D0

        NBFPG = 8
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 3
        NBPG(5) = 4
        NBPG(6) = 6
        NBPG(7) = 7
        NBPG(8) = 12

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG3'
        FAPG(5) = 'FPG4'
        FAPG(6) = 'FPG6'
        FAPG(7) = 'FPG7'
        FAPG(8) = 'FPG12'

        XIN(1) = 0.D0
        YIN(1) = 0.D0

        XIN(2) = +1.D0
        YIN(2) =  0.D0

        XIN(3) =  0.D0
        YIN(3) = +1.D0

        XIN(4) = 0.5D0
        YIN(4) = 0.D0

        XIN(5) = 0.5D0
        YIN(5) = 0.5D0

        XIN(6) = 0.D0
        YIN(6) = 0.5D0

        XIN(7) = 1.0D0/3.0D0
        YIN(7) = 1.0D0/3.0D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'QU4') THEN
        NDIM = 2
        NNO = 4
        NNOS = 4
        VOL = 4.D0

        NBFPG = 8
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 4
        NBPG(5) = 9
        NBPG(6) = 16
        NBPG(7) = 2
        NBPG(8) = 8

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG4'
        FAPG(5) = 'FPG9'
        FAPG(6) = 'FPG16'
        FAPG(7) = 'FIS2'
        FAPG(8) = 'FPG4NOS'

        XIN(1) = -1.D0
        YIN(1) = -1.D0

        XIN(2) = +1.D0
        YIN(2) = -1.D0

        XIN(3) = +1.D0
        YIN(3) = +1.D0

        XIN(4) = -1.D0
        YIN(4) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'QU8') THEN
        NDIM = 2
        NNO = 8
        NNOS = 4
        VOL = 4.D0

        NBFPG = 6
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 4
        NBPG(5) = 9
        NBPG(6) = 8

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG4'
        FAPG(5) = 'FPG9'
        FAPG(6) = 'FPG4NOS'

        XIN(1) = -1.D0
        YIN(1) = -1.D0

        XIN(2) = +1.D0
        YIN(2) = -1.D0

        XIN(3) = +1.D0
        YIN(3) = +1.D0

        XIN(4) = -1.D0
        YIN(4) = +1.D0

        XIN(5) =  0.D0
        YIN(5) = -1.D0

        XIN(6) = +1.D0
        YIN(6) = +0.D0

        XIN(7) =  0.D0
        YIN(7) = +1.D0

        XIN(8) = -1.D0
        YIN(8) =  0.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'QU9') THEN
        NDIM = 2
        NNO = 9
        NNOS = 4
        VOL = 4.D0

        NBFPG = 5
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 4
        NBPG(5) = 9

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG4'
        FAPG(5) = 'FPG9'

        XIN(1) = -1.D0
        YIN(1) = -1.D0

        XIN(2) = +1.D0
        YIN(2) = -1.D0

        XIN(3) = +1.D0
        YIN(3) = +1.D0

        XIN(4) = -1.D0
        YIN(4) = +1.D0

        XIN(5) =  0.D0
        YIN(5) = -1.D0

        XIN(6) = +1.D0
        YIN(6) = +0.D0

        XIN(7) =  0.D0
        YIN(7) = +1.D0

        XIN(8) = -1.D0
        YIN(8) = +0.D0

        XIN(9) =  0.D0
        YIN(9) =  0.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'PO1') THEN
        NDIM = 0
        NNO = 1
        NNOS = 1
        VOL = 1.D0

        NBFPG = 3
        NBPG(1) = 1
        NBPG(2) = 1
        NBPG(3) = 1

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'SE2') THEN
        NDIM = 1
        NNO = 2
        NNOS = 2
        VOL = 2.D0

        NBFPG = 6
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 2
        NBPG(5) = 3
        NBPG(6) = 4

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG2'
        FAPG(5) = 'FPG3'
        FAPG(6) = 'FPG4'

        XIN(1) = -1.D0
        XIN(2) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'SE3') THEN
        NDIM = 1
        NNO = 3
        NNOS = 2
        VOL = 2.D0

        NBFPG = 6
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 2
        NBPG(5) = 3
        NBPG(6) = 4

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG2'
        FAPG(5) = 'FPG3'
        FAPG(6) = 'FPG4'

        XIN(1) = -1.D0
        XIN(2) = +1.D0
        XIN(3) =  0.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'SE4') THEN
        NDIM = 1
        NNO = 4
        NNOS = 2
        VOL = 2.D0

        NBFPG = 6
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 2
        NBPG(5) = 3
        NBPG(6) = 4

        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG2'
        FAPG(5) = 'FPG3'
        FAPG(6) = 'FPG4'

        XIN(1) = -1.D0
        XIN(2) = +1.D0
        XIN(3) = -1.D0/3.D0
        XIN(4) = +1.D0/3.D0


C     ------------------------------------------------------------------
      ELSE
         CALL UTMESS('F','ELRACA','ELREFA INCONNU: '//ELREFA)
      END IF

      DO 200 I = 0,NNO - 1
        X(NDIM*I+1) = XIN(I+1)
        IF (NDIM.EQ.2) THEN
          X(NDIM*I+2) = YIN(I+1)
        ELSE IF (NDIM.EQ.3) THEN
          X(NDIM*I+2) = YIN(I+1)
          X(NDIM*I+3) = ZIN(I+1)
        END IF
  200 CONTINUE

      END
