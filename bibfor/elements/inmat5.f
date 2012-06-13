      SUBROUTINE INMAT5(ELREFA,NNO,NNOS,NPG,MGANOS,MGANO2)
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER NNOS,NPG,NNO,NBPGMX,NBNOMX
      PARAMETER (NBPGMX=1000,NBNOMX=27)
      REAL*8 MGANOS(NBPGMX,NBNOMX),MGANO2(NBPGMX,NBNOMX)
      CHARACTER*8 ELREFA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C     BUT :
C     POUR LES ELEMENTS QUADRATIQUES (NNOS /= NNO),
C     CALCULER LA MATRICE DE PASSAGE GAUSS -> NOEUDS (MGANO2)
C     A PARTIR DE LA MATRICE DE PASSAGE GAUSS -> NOEUDS_SOMMETS (MGANOS)
C ----------------------------------------------------------------------
      INTEGER KPG,KNO,KNOS,K
      REAL*8 DEMI,NOSOM(NBNOMX,NBNOMX)
     
C     NBPGMX, NBNOMX SE REFERER A ELRACA

C DEB ------------------------------------------------------------------
      DEMI = 0.5D0


C     -- SI NNO=NNOS, IL N'Y A QU'A COPIER :
C     --------------------------------------
      IF (NNOS.EQ.NNO) THEN
        DO 20,KPG = 1,NPG
          DO 10,KNO = 1,NNO
            MGANO2(KPG,KNO) = MGANOS(KPG,KNO)
   10     CONTINUE
   20   CONTINUE
        GO TO 100
      END IF


C     1) CALCUL DE NOSOM :
C     ---------------------
      DO 40,KNO = 1,NNO
        DO 30,KNOS = 1,NNOS
          NOSOM(KNO,KNOS) = 0.D0
   30   CONTINUE
   40 CONTINUE

C     1.1) LES NOEUDS SOMMETS SONT TOUJOURS LES 1ERS :
      DO 50,KNOS = 1,NNOS
        NOSOM(KNOS,KNOS) = 1.D0
   50 CONTINUE


C     1.2) LES NOEUDS MILIEUX SE DEDUISENT DES SOMMETS :
      IF ((ELREFA.EQ.'H20') .OR. (ELREFA.EQ.'H27')) THEN
        CALL ASSERT(NNOS.EQ.8)
        NOSOM(9,1) = DEMI
        NOSOM(9,2) = DEMI
        NOSOM(10,2) = DEMI
        NOSOM(10,3) = DEMI
        NOSOM(11,3) = DEMI
        NOSOM(11,4) = DEMI
        NOSOM(12,1) = DEMI
        NOSOM(12,4) = DEMI
        NOSOM(13,1) = DEMI
        NOSOM(13,5) = DEMI
        NOSOM(14,2) = DEMI
        NOSOM(14,6) = DEMI
        NOSOM(15,3) = DEMI
        NOSOM(15,7) = DEMI
        NOSOM(16,4) = DEMI
        NOSOM(16,8) = DEMI
        NOSOM(17,5) = DEMI
        NOSOM(17,6) = DEMI
        NOSOM(18,6) = DEMI
        NOSOM(18,7) = DEMI
        NOSOM(19,7) = DEMI
        NOSOM(19,8) = DEMI
        NOSOM(20,5) = DEMI
        NOSOM(20,8) = DEMI

        IF (ELREFA.EQ.'H27') THEN
          NOSOM(21,1) = DEMI/2
          NOSOM(21,2) = DEMI/2
          NOSOM(21,3) = DEMI/2
          NOSOM(21,4) = DEMI/2

          NOSOM(22,1) = DEMI/2
          NOSOM(22,2) = DEMI/2
          NOSOM(22,5) = DEMI/2
          NOSOM(22,6) = DEMI/2

          NOSOM(23,2) = DEMI/2
          NOSOM(23,3) = DEMI/2
          NOSOM(23,6) = DEMI/2
          NOSOM(23,7) = DEMI/2

          NOSOM(24,3) = DEMI/2
          NOSOM(24,4) = DEMI/2
          NOSOM(24,7) = DEMI/2
          NOSOM(24,8) = DEMI/2

          NOSOM(25,1) = DEMI/2
          NOSOM(25,4) = DEMI/2
          NOSOM(25,5) = DEMI/2
          NOSOM(25,8) = DEMI/2

          NOSOM(26,5) = DEMI/2
          NOSOM(26,6) = DEMI/2
          NOSOM(26,7) = DEMI/2
          NOSOM(26,8) = DEMI/2

          DO 60,K = 1,8
            NOSOM(NBNOMX,K) = DEMI/4
   60     CONTINUE
        END IF


      ELSE IF ((ELREFA.EQ.'P15').OR.(ELREFA.EQ.'P18')) THEN
        CALL ASSERT(NNOS.EQ.6)
        NOSOM(7,1) = DEMI
        NOSOM(7,2) = DEMI
        NOSOM(8,2) = DEMI
        NOSOM(8,3) = DEMI
        NOSOM(9,1) = DEMI
        NOSOM(9,3) = DEMI
        NOSOM(10,1) = DEMI
        NOSOM(10,4) = DEMI
        NOSOM(11,2) = DEMI
        NOSOM(11,5) = DEMI
        NOSOM(12,3) = DEMI
        NOSOM(12,6) = DEMI
        NOSOM(13,4) = DEMI
        NOSOM(13,5) = DEMI
        NOSOM(14,5) = DEMI
        NOSOM(14,6) = DEMI
        NOSOM(15,4) = DEMI
        NOSOM(15,6) = DEMI

        IF (ELREFA.EQ.'P18') THEN

          NOSOM(16,2) = DEMI/2
          NOSOM(16,1) = DEMI/2
          NOSOM(16,4) = DEMI/2
          NOSOM(16,5) = DEMI/2

          NOSOM(17,2) = DEMI/2
          NOSOM(17,5) = DEMI/2
          NOSOM(17,6) = DEMI/2
          NOSOM(17,3) = DEMI/2

          NOSOM(18,1) = DEMI/2
          NOSOM(18,3) = DEMI/2
          NOSOM(18,6) = DEMI/2
          NOSOM(18,4) = DEMI/2

        END IF

      ELSE IF (ELREFA.EQ.'T10') THEN
        CALL ASSERT(NNOS.EQ.4)
        NOSOM(5,1) = DEMI
        NOSOM(5,2) = DEMI
        NOSOM(6,2) = DEMI
        NOSOM(6,3) = DEMI
        NOSOM(7,1) = DEMI
        NOSOM(7,3) = DEMI
        NOSOM(8,1) = DEMI
        NOSOM(8,4) = DEMI
        NOSOM(9,2) = DEMI
        NOSOM(9,4) = DEMI
        NOSOM(10,3) = DEMI
        NOSOM(10,4) = DEMI


      ELSE IF (ELREFA.EQ.'P13') THEN
        CALL ASSERT(NNOS.EQ.5)
        NOSOM(6,1) = DEMI
        NOSOM(6,2) = DEMI
        NOSOM(7,2) = DEMI
        NOSOM(7,3) = DEMI
        NOSOM(8,3) = DEMI
        NOSOM(8,4) = DEMI
        NOSOM(9,1) = DEMI
        NOSOM(9,4) = DEMI
        NOSOM(10,1) = DEMI
        NOSOM(10,5) = DEMI
        NOSOM(11,2) = DEMI
        NOSOM(11,5) = DEMI
        NOSOM(12,3) = DEMI
        NOSOM(12,5) = DEMI
        NOSOM(13,4) = DEMI
        NOSOM(13,5) = DEMI


      ELSE IF (ELREFA.EQ.'TR6') THEN
        CALL ASSERT(NNOS.EQ.3)
        NOSOM(4,1) = DEMI
        NOSOM(4,2) = DEMI
        NOSOM(5,2) = DEMI
        NOSOM(5,3) = DEMI
        NOSOM(6,3) = DEMI
        NOSOM(6,1) = DEMI


      ELSE IF (ELREFA.EQ.'TR7') THEN
        CALL ASSERT(NNOS.EQ.3)
        NOSOM(4,1) = DEMI
        NOSOM(4,2) = DEMI
        NOSOM(5,2) = DEMI
        NOSOM(5,3) = DEMI
        NOSOM(6,3) = DEMI
        NOSOM(6,1) = DEMI
        NOSOM(7,1) = DEMI/2
        NOSOM(7,2) = DEMI/2
        NOSOM(7,3) = DEMI/2


      ELSE IF (ELREFA.EQ.'QU8') THEN
        CALL ASSERT(NNOS.EQ.4)
        NOSOM(5,1) = DEMI
        NOSOM(5,2) = DEMI
        NOSOM(6,2) = DEMI
        NOSOM(6,3) = DEMI
        NOSOM(7,3) = DEMI
        NOSOM(7,4) = DEMI
        NOSOM(8,4) = DEMI
        NOSOM(8,1) = DEMI


      ELSE IF (ELREFA.EQ.'QU9') THEN
        CALL ASSERT(NNOS.EQ.4)
        NOSOM(5,1) = DEMI
        NOSOM(5,2) = DEMI
        NOSOM(6,2) = DEMI
        NOSOM(6,3) = DEMI
        NOSOM(7,3) = DEMI
        NOSOM(7,4) = DEMI
        NOSOM(8,4) = DEMI
        NOSOM(8,1) = DEMI
        NOSOM(9,1) = DEMI/2
        NOSOM(9,2) = DEMI/2
        NOSOM(9,3) = DEMI/2
        NOSOM(9,4) = DEMI/2


      ELSE IF (ELREFA.EQ.'SE3') THEN
        CALL ASSERT(NNOS.EQ.2)
        NOSOM(3,1) = DEMI
        NOSOM(3,2) = DEMI


      ELSE IF (ELREFA.EQ.'SE4') THEN
        CALL ASSERT(NNOS.EQ.2)
        NOSOM(3,1) = 2.D0/3.D0
        NOSOM(3,2) = 1.D0/3.D0
        NOSOM(4,1) = 1.D0/3.D0
        NOSOM(4,2) = 2.D0/3.D0

      ELSE
        CALL ASSERT(.FALSE.)
      END IF


C     2) ON MULTIPLIE : MGANO2=MGANOS * NOSOM :
C     ----------------------------------
      DO 90,KNO = 1,NNO
        DO 80,KPG = 1,NPG
          DO 70,KNOS = 1,NNOS
            MGANO2(KPG,KNO) = MGANO2(KPG,KNO) +
     &                        MGANOS(KPG,KNOS)*NOSOM(KNO,KNOS)

   70     CONTINUE
   80   CONTINUE
   90 CONTINUE



  100 CONTINUE
      END
