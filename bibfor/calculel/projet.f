      SUBROUTINE PROJET(NDIM,NPG1,NNO,VECT,RES)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================

C=======================================================================

C DEFINITION :
C     SORTIE AUX NOEUDS A PARTIR DES VALEURS AUX POINTS DE GAUSS

C FONCTIONNEMENT :
C        CALCUL DE L'ENTHALPIE AUX NOEUDS 'RES' A PARTIR DE
C             L'ENTHALPIE AUX POINTS D INTEGRATION 'VECT'

C=======================================================================
      IMPLICIT NONE

      REAL*8 VLOC(3,27),VL(81),VH(50)
      INTEGER J8(8),J27(27)
      REAL*8 VECT(50),RES(50),MPT(4,4),MPP(6,6),MP(3,3),MPQ(4,4)

C-----------------------------------------------------------------------
      INTEGER I ,IC ,IN ,J ,NDIM ,NNO ,NPG1 

      REAL*8 A ,B ,C ,CINQ ,DE ,S ,TROIS 
      REAL*8 UN ,XAB ,ZERO 
C-----------------------------------------------------------------------
      DATA ZERO,TROIS,CINQ/0.D0,3.D0,5.D0/
      DATA DE,UN/.5D0,1.D0/
      DATA J8/1,5,7,3,2,6,8,4/
      DATA J27/1,19,25,7,3,21,27,9,10,22,16,4,13,2,11,20,23,26,17,8,5,
     &     14,12,24,18,6,15/
      DATA VL/-1.D0,-1.D0,-1.D0,1.D0,-1.D0,-1.D0,1.D0,1.D0,-1.D0,-1.D0,
     &     1.D0,-1.D0,-1.D0,-1.D0,1.D0,1.D0,-1.D0,1.D0,1.D0,1.D0,1.D0,
     &     -1.D0,1.D0,1.D0,0.D0,-1.D0,-1.D0,1.D0,0.D0,-1.D0,0.D0,1.D0,
     &     -1.D0,-1.D0,0.D0,-1.D0,-1.D0,-1.D0,0.D0,1.D0,-1.D0,0.D0,1.D0,
     &     1.D0,0.D0,-1.D0,1.D0,0.D0,0.D0,-1.D0,1.D0,1.D0,0.D0,1.D0,
     &     0.D0,1.D0,1.D0,-1.D0,0.D0,1.D0,0.D0,0.D0,-1.D0,0.D0,-1.D0,
     &     0.D0,1.D0,0.D0,0.D0,0.D0,1.D0,0.D0,-1.D0,0.D0,0.D0,0.D0,0.D0,
     &     0.D0,0.D0,0.D0,1.D0/
      IF (NDIM.EQ.2) THEN

C     ========= TRIANGLES ======

        IF (NNO.EQ.3 .OR. NNO.EQ.6) THEN
          MP(1,1) = CINQ/TROIS
          MP(1,2) = -UN/TROIS
          MP(1,3) = MP(1,2)
          MP(2,1) = MP(1,2)
          MP(2,2) = MP(1,1)
          MP(2,3) = MP(1,2)
          MP(3,1) = MP(1,2)
          MP(3,2) = MP(1,2)
          MP(3,3) = MP(1,1)

          DO 20 I = 1,3
            S = 0.D0
            DO 10 J = 1,3
              S = S + MP(I,J)*VECT(J)
   10       CONTINUE
            RES(I) = S
   20     CONTINUE
          IF (NNO.EQ.6) THEN
            RES(4) = DE* (RES(1)+RES(2))
            RES(5) = DE* (RES(2)+RES(3))
            RES(6) = DE* (RES(3)+RES(1))
          END IF
        END IF

C     ========= QUADRANGLES =====

        IF (NNO.EQ.4 .OR. NNO.EQ.8) THEN
          MPQ(1,1) = UN + SQRT(TROIS)*DE
          MPQ(1,2) = -DE
          MPQ(1,3) = UN - SQRT(TROIS)*DE
          MPQ(1,4) = MPQ(1,2)
          MPQ(2,1) = MPQ(1,2)
          MPQ(2,2) = MPQ(1,1)
          MPQ(2,3) = MPQ(1,2)
          MPQ(2,4) = MPQ(1,3)
          MPQ(3,1) = MPQ(1,3)
          MPQ(3,2) = MPQ(1,2)
          MPQ(3,3) = MPQ(1,1)
          MPQ(3,4) = MPQ(1,2)
          MPQ(4,1) = MPQ(1,2)
          MPQ(4,2) = MPQ(1,3)
          MPQ(4,3) = MPQ(1,2)
          MPQ(4,4) = MPQ(1,1)

          DO 40 I = 1,4
            S = 0.D0
            DO 30 J = 1,4
              S = S + MPQ(I,J)*VECT(J)
   30       CONTINUE
            RES(I) = S
   40     CONTINUE
          IF (NNO.EQ.8) THEN
            RES(5) = DE* (RES(1)+RES(2))
            RES(6) = DE* (RES(2)+RES(3))
            RES(7) = DE* (RES(3)+RES(4))
            RES(8) = DE* (RES(4)+RES(1))
          END IF
        END IF

      END IF

      IF (NDIM.EQ.3) THEN

C     ========= HEXAEDRES ======

        IF (NNO.EQ.8 .OR. NNO.EQ.20 .OR. NNO.EQ.27) THEN

C-----CONSTRUCTION DE VLOC

          DO 50 I = 1,50
            RES(I) = ZERO
   50     CONTINUE
C-------- MISE A ZERO DE VLOC
          DO 70 I = 1,3
            DO 60 J = 1,27
              VLOC(I,J) = ZERO
   60       CONTINUE
   70     CONTINUE

          XAB = SQRT(TROIS)
          IF (NNO.EQ.20 .OR. NNO.EQ.27) XAB = SQRT(CINQ/TROIS)

          DO 90 I = 1,3
            DO 80 J = 1,NNO
              VLOC(I,J) = XAB*VL(I+ (J-1)*3)
   80       CONTINUE
   90     CONTINUE

C       CALCUL AUX NOEUDS

C-----BOUCLE SUR LE NOMBRE DE NOEUDS NNO

          DO 110 IN = 1,NNO
            CALL INTRPO(VLOC(1,IN),VLOC(2,IN),VLOC(3,IN),NNO,VH)
            DO 100 J = 1,NPG1
              IC = J27(J)
              IF (NNO.EQ.8) IC = J8(J)
              RES(IN) = RES(IN) + VH(J)*VECT(IC)
  100       CONTINUE
  110     CONTINUE

        END IF

C     ========= TETRAEDRES ======

        IF (NNO.EQ.4 .OR. NNO.EQ.10) THEN
          A = (5.D0-SQRT(5.D0))/20.D0
          B = (5.D0+3.D0*SQRT(5.D0))/20.D0
          C = A - B
          MPT(1,1) = A/C
          MPT(1,2) = MPT(1,1)
          MPT(1,3) = (A-1.D0)/C
          MPT(1,4) = MPT(1,1)
          MPT(2,1) = MPT(1,1)
          MPT(2,2) = MPT(1,3)
          MPT(2,3) = MPT(1,1)
          MPT(2,4) = MPT(1,1)
          MPT(3,1) = MPT(1,3)
          MPT(3,2) = MPT(1,1)
          MPT(3,3) = MPT(1,1)
          MPT(3,4) = MPT(1,1)
          MPT(4,1) = MPT(1,1)
          MPT(4,2) = MPT(1,1)
          MPT(4,3) = MPT(1,1)
          MPT(4,4) = MPT(1,3)

          DO 130 I = 1,4
            S = 0.D0
            DO 120 J = 1,4
              S = S + MPT(I,J)*VECT(J)
  120       CONTINUE
            RES(I) = S
  130     CONTINUE
          IF (NNO.EQ.10) THEN
            RES(5) = DE* (RES(1)+RES(2))
            RES(6) = DE* (RES(2)+RES(3))
            RES(7) = DE* (RES(3)+RES(1))
            RES(8) = DE* (RES(1)+RES(4))
            RES(9) = DE* (RES(2)+RES(4))
            RES(10) = DE* (RES(3)+RES(4))
          END IF

        END IF

C     ========= PENTAEDRES =====

        IF (NNO.EQ.6 .OR. NNO.EQ.15) THEN
          A = (SQRT(3.D0)+1.D0)/2.D0
          MPP(1,1) = A
          MPP(1,2) = -A
          MPP(1,3) = A
          MPP(1,4) = 1.D0 - A
          MPP(1,5) = A - 1.D0
          MPP(1,6) = 1.D0 - A
          MPP(2,1) = A
          MPP(2,2) = A
          MPP(2,3) = -A
          MPP(2,4) = 1.D0 - A
          MPP(2,5) = 1.D0 - A
          MPP(2,6) = A - 1.D0
          MPP(3,1) = -A
          MPP(3,2) = A
          MPP(3,3) = A
          MPP(3,4) = A - 1.D0
          MPP(3,5) = 1.D0 - A
          MPP(3,6) = 1.D0 - A
          MPP(4,1) = 1.D0 - A
          MPP(4,2) = A - 1.D0
          MPP(4,3) = 1.D0 - A
          MPP(4,4) = A
          MPP(4,5) = -A
          MPP(4,6) = A
          MPP(5,1) = 1.D0 - A
          MPP(5,2) = 1.D0 - A
          MPP(5,3) = A - 1.D0
          MPP(5,4) = A
          MPP(5,5) = A
          MPP(5,6) = -A
          MPP(6,1) = A - 1.D0
          MPP(6,2) = 1.D0 - A
          MPP(6,3) = 1.D0 - A
          MPP(6,4) = -A
          MPP(6,5) = A
          MPP(6,6) = A

          DO 150 I = 1,6
            S = 0.D0
            DO 140 J = 1,6
              S = S + MPP(I,J)*VECT(J)
  140       CONTINUE
            RES(I) = S
  150     CONTINUE
          IF (NNO.EQ.15) THEN
            RES(7) = DE* (RES(1)+RES(2))
            RES(8) = DE* (RES(2)+RES(3))
            RES(9) = DE* (RES(3)+RES(1))
            RES(10) = DE* (RES(4)+RES(1))
            RES(11) = DE* (RES(5)+RES(2))
            RES(12) = DE* (RES(6)+RES(3))
            RES(13) = DE* (RES(5)+RES(4))
            RES(14) = DE* (RES(5)+RES(6))
            RES(15) = DE* (RES(6)+RES(4))
          END IF
        END IF

      END IF

      END
