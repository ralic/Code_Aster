      SUBROUTINE ELRACA(ELREFZ,NDIM,NNO,NNOS,NBFPG,FAPG,NBPG,X,VOL)
      IMPLICIT NONE
      INTEGER NDIM,NNO,NNOS,NBFPG,NBPG(*)
      REAL*8 X(*),VOL
      CHARACTER*8 FAPG(*)
      CHARACTER*(*) ELREFZ
C ----------------------------------------------------------------------
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
      IF (ELREFA.EQ.'HE8') THEN
        NDIM = 3
        NNO = 8
        NNOS = 8
        VOL = 8.D0

        NBFPG = 6
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 8
        NBPG(5) = 27
        NBPG(6) = 5
        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG8'
        FAPG(5) = 'FPG27'
        FAPG(6) = 'SHB5'

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
      ELSE IF (ELREFA.EQ.'H20') THEN
        NDIM = 3
        NNO = 20
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
      ELSE IF (ELREFA.EQ.'TE4') THEN
        NDIM = 3
        NNO = 4
        NNOS = 4
        VOL = 1.D0/6.D0

        NBFPG = 6
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 4
        NBPG(5) = 5
        NBPG(6) = 15
        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG4'
        FAPG(5) = 'FPG5'
        FAPG(6) = 'FPG15'

        DO 70 I = 1,4
          XIN(I) = 0.D0
          YIN(I) = 0.D0
          ZIN(I) = 0.D0
   70   CONTINUE
        YIN(1) = +1.D0
        ZIN(2) = +1.D0
        XIN(4) = +1.D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFA.EQ.'T10') THEN
        NDIM = 3
        NNO = 10
        NNOS = 4
        VOL = 1.D0/6.D0

        NBFPG = 6
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 4
        NBPG(5) = 5
        NBPG(6) = 15
        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG4'
        FAPG(5) = 'FPG5'
        FAPG(6) = 'FPG15'

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
      ELSE IF (ELREFA.EQ.'PE6') THEN
        NDIM = 3
        NNO = 6
        NNOS = 6
        VOL = 1.D0

        NBFPG = 6
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 6
        NBPG(5) = 8
        NBPG(6) = 21
        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG6'
        FAPG(5) = 'FPG8'
        FAPG(6) = 'FPG21'

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
      ELSE IF (ELREFA.EQ.'P15') THEN
        NDIM = 3
        NNO = 15
        NNOS = 6
        VOL = 1.D0

        NBFPG = 6
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 6
        NBPG(5) = 8
        NBPG(6) = 21
        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG6'
        FAPG(5) = 'FPG8'
        FAPG(6) = 'FPG21'

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
      ELSE IF (ELREFA.EQ.'PY5') THEN
        NDIM = 3
        NNO = 5
        NNOS = 5
        VOL = 2.D0/3.D0

        NBFPG = 6
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 5
        NBPG(5) = 6
        NBPG(6) = 27
        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG5'
        FAPG(5) = 'FPG6'
        FAPG(6) = 'FPG27'

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
      ELSE IF (ELREFA.EQ.'P13') THEN
        NDIM = 3
        NNO = 13
        NNOS = 5
        VOL = 2.D0/3.D0

        NBFPG = 6
        NBPG(1) = NNO
        NBPG(2) = NNOS
        NBPG(3) = 1
        NBPG(4) = 5
        NBPG(5) = 6
        NBPG(6) = 27
        FAPG(1) = 'NOEU'
        FAPG(2) = 'NOEU_S'
        FAPG(3) = 'FPG1'
        FAPG(4) = 'FPG5'
        FAPG(5) = 'FPG6'
        FAPG(6) = 'FPG27'

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
