      SUBROUTINE DSFCH3 ( NNO, NNF, POIDS, DPDEF, DPDNF, DPDKF, DSDEEF,
     &                    DSDNNF, DSDKKF, DSDENF, DSDEKF, DSDNKF, COOR,
     &                    DPDEG, DPDNG, DPDKG, DSDEEG, DSDNNG, DSDKKG,
     &                    DSDENG, DSDEKG, DSDNKG, DSDXXF, DSDYYF,
     &                    DSDZZF, DSDXYF, DSDYZF, DSDXZF, JAC )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 19/06/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21
      IMPLICIT NONE
C      REAL*8 (A-H,O-Z)
      REAL*8 VALR
      INTEGER           NNO, NNF
      REAL*8            POIDS,DPDEG(1),DPDNG(1),DPDKG(1),DSDEEG(1),
     &                  DSDNNG(1),DSDKKG(1),DSDENG(1),DSDNKG(1),
     &                  DSDEKG(1),COOR(1)
      REAL*8            DPDEF(1),DPDNF(1),DPDKF(1),DSDEEF(1),DSDNNF(1),
     &                  DSDKKF(1),DSDENF(1),DSDNKF(1),DSDEKF(1)
      REAL*8            DSDXXF(1),DSDYYF(1),DSDZZF(1),DSDXYF(1),
     &                  DSDXZF(1),DSDYZF(1),JAC
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES DERIVEES DES FONCTIONS DE FORME
C               PAR RAPPORT A UN ELEMENT COURANT EN UN POINT DE GAUSS
C               POUR LES ELEMENTS 3D NON ISOPARAMETRIQUES
C                                    ====================
C    - ARGUMENTS:
C        DONNEES:
C          NNO          -->  NOMBRE DE NOEUDS
C          NNF          -->  NOMBRE DE FONCTIONS DE FORME
C          POIDS        -->  POIDS DU POINT DE GAUSS
C    DPDEG,DFDNG,DPDKG  -->  DERIVEES 1ERE FONCTIONS DE FORME (GEOMETR
C    DPDEF,DFDNF,DPDKF  -->  DERIVEES 1ERE FONCTIONS DE FORME (VARIABL
C    DSDEEG,...,DSDNKG  -->  DERIVEES 1ERE FONCTIONS DE FORME (GEOMETR
C    DSDEEF,...,DSDNKF  -->  DERIVEES 1ERE FONCTIONS DE FORME (VARIABL
C    COOR               -->  COORDONNEES DES NOEUDS
C
C        RESULTATS:
C          DSDXXF        <--  DERIVEES 2EME DES F. DE F. / XX
C          DSDYYF        <--  DERIVEES 2EME DES F. DE F. / YY
C          DSDZZF        <--  DERIVEES 2EME DES F. DE F. / ZZ
C          DSDXYF        <--  DERIVEES 2EME DES F. DE F. / XY
C          DSDYZF        <--  DERIVEES 2EME DES F. DE F. / YZ
C          DSDXZF        <--  DERIVEES 2EME DES F. DE F. / XZ
C          JAC           <--  JACOBIEN AU POINT DE GAUSS
C ......................................................................
C
      INTEGER  I,J,K,II
      REAL*8   G(3,3),DE,DK,DN,JAC2IV,J11,J12,J13,J21,J22,J23,J31,J32,
     +         J33,T2(6,6),T1(6,3),CJ(6,3),C2(6,3)
C
C     --- INITIALISATION DE LA MATRICE JACOBIENNE A ZERO
C
      DO 10 I=1,3
         DO 10 J=1,3
            G(I,J) = 0.D0
10    CONTINUE
C
C     --- CALCUL DE LA MATRICE JACOBIENNE (TRANSFORMATION GEOMETRIQUE)
C
      DO 100 I = 1, NNO
         II = 3*(I-1)
         DE = DPDEG(I)
         DN = DPDNG(I)
         DK = DPDKG(I)
         DO 110 J = 1, 3
            G(1,J) = G(1,J) + COOR(II+J) * DE
            G(2,J) = G(2,J) + COOR(II+J) * DN
            G(3,J) = G(3,J) + COOR(II+J) * DK
110      CONTINUE
100   CONTINUE
C
C     --- CALCUL DE L'INVERSE DE LA MATRICE JACOBIENNE
C                  (AU DETERMINANT PRES)
C
      J11 = G(2,2) * G(3,3) - G(2,3) * G(3,2)
      J21 = G(3,1) * G(2,3) - G(2,1) * G(3,3)
      J31 = G(2,1) * G(3,2) - G(3,1) * G(2,2)
      J12 = G(1,3) * G(3,2) - G(1,2) * G(3,3)
      J22 = G(1,1) * G(3,3) - G(1,3) * G(3,1)
      J32 = G(1,2) * G(3,1) - G(3,2) * G(1,1)
      J13 = G(1,2) * G(2,3) - G(1,3) * G(2,2)
      J23 = G(2,1) * G(1,3) - G(2,3) * G(1,1)
      J33 = G(1,1) * G(2,2) - G(1,2) * G(2,1)
C
C     --- DETERMINANT DE LA MATRICE JACOBIENNE
C
      JAC = G(1,1) * J11 + G(1,2) * J21 + G(1,3) * J31
      IF(JAC.LE.0.0D0) THEN
         VALR = JAC
         CALL U2MESG('A','ELEMENTS5_31',0,' ',0,0,1,VALR)
      ENDIF
C
C     --- CALCUL DA LA MATRICE T1
C
      JAC2IV = 1.D0 / (JAC * JAC)
C
      T2(1,1) = J11 * J11 * JAC2IV
      T2(1,2) = J12 * J12 * JAC2IV
      T2(1,3) = J13 * J13 * JAC2IV
      T2(2,1) = J21 * J21 * JAC2IV
      T2(2,2) = J22 * J22 * JAC2IV
      T2(2,3) = J23 * J23 * JAC2IV
      T2(3,1) = J31 * J31 * JAC2IV
      T2(3,2) = J32 * J32 * JAC2IV
      T2(3,3) = J33 * J33 * JAC2IV
C
      T2(1,4) = 2.D0 * J11 * J12 * JAC2IV
      T2(1,5) = 2.D0 * J12 * J13 * JAC2IV
      T2(1,6) = 2.D0 * J13 * J11 * JAC2IV
      T2(2,4) = 2.D0 * J21 * J22 * JAC2IV
      T2(2,5) = 2.D0 * J22 * J23 * JAC2IV
      T2(2,6) = 2.D0 * J23 * J21 * JAC2IV
      T2(3,4) = 2.D0 * J31 * J32 * JAC2IV
      T2(3,5) = 2.D0 * J32 * J33 * JAC2IV
      T2(3,6) = 2.D0 * J33 * J31 * JAC2IV
C
      T2(4,1) = J11 * J21 * JAC2IV
      T2(4,2) = J12 * J22 * JAC2IV
      T2(4,3) = J13 * J23 * JAC2IV
      T2(5,1) = J21 * J31 * JAC2IV
      T2(5,2) = J22 * J32 * JAC2IV
      T2(5,3) = J23 * J33 * JAC2IV
      T2(6,1) = J31 * J11 * JAC2IV
      T2(6,2) = J32 * J12 * JAC2IV
      T2(6,3) = J33 * J13 * JAC2IV
C
      T2(4,4) = (J11 * J22 + J12 * J21) * JAC2IV
      T2(4,5) = (J12 * J23 + J13 * J22) * JAC2IV
      T2(4,6) = (J11 * J23 + J13 * J21) * JAC2IV
      T2(5,4) = (J21 * J32 + J22 * J31) * JAC2IV
      T2(5,5) = (J22 * J33 + J23 * J32) * JAC2IV
      T2(5,6) = (J21 * J33 + J23 * J31) * JAC2IV
      T2(6,4) = (J31 * J12 + J32 * J11) * JAC2IV
      T2(6,5) = (J32 * J13 + J33 * J12) * JAC2IV
      T2(6,6) = (J31 * J13 + J33 * J11) * JAC2IV
C
C     --- CALCUL DE LA MATRICE C2
C
      DO 300 I = 1, 6
         DO 300 J = 1, 3
             C2(I,J) = 0.D0
 300  CONTINUE
C
      DO 400 I = 1, NNO
         II = 3 * (I-1)
         C2(1,1) = C2(1,1) + COOR(II+1) * DSDEEG(I)
         C2(1,2) = C2(1,2) + COOR(II+2) * DSDEEG(I)
         C2(1,3) = C2(1,3) + COOR(II+3) * DSDEEG(I)
         C2(2,1) = C2(2,1) + COOR(II+1) * DSDNNG(I)
         C2(2,2) = C2(2,2) + COOR(II+2) * DSDNNG(I)
         C2(2,3) = C2(2,3) + COOR(II+3) * DSDNNG(I)
         C2(3,1) = C2(3,1) + COOR(II+1) * DSDKKG(I)
         C2(3,2) = C2(3,2) + COOR(II+2) * DSDKKG(I)
         C2(3,3) = C2(3,3) + COOR(II+3) * DSDKKG(I)
         C2(4,1) = C2(4,1) + COOR(II+1) * DSDENG(I)
         C2(4,2) = C2(4,2) + COOR(II+2) * DSDENG(I)
         C2(4,3) = C2(4,3) + COOR(II+3) * DSDENG(I)
         C2(5,1) = C2(5,1) + COOR(II+1) * DSDNKG(I)
         C2(5,2) = C2(5,2) + COOR(II+2) * DSDNKG(I)
         C2(5,3) = C2(5,3) + COOR(II+3) * DSDNKG(I)
         C2(6,1) = C2(6,1) + COOR(II+1) * DSDEKG(I)
         C2(6,2) = C2(6,2) + COOR(II+2) * DSDEKG(I)
         C2(6,3) = C2(6,3) + COOR(II+3) * DSDEKG(I)
 400  CONTINUE
C
C     --- CALCUL DE LA MATRICE T1
C
      DO 500 I = 1, 6
         CJ(I,1) = C2(I,1) * J11 + C2(I,2) * J21 + C2(I,3) * J31
         CJ(I,2) = C2(I,1) * J12 + C2(I,2) * J22 + C2(I,3) * J32
         CJ(I,3) = C2(I,1) * J13 + C2(I,2) * J23 + C2(I,3) * J33
 500  CONTINUE
C
      DO 510 I = 1, 6
      DO 510 J = 1, 3
         CJ(I,J) = CJ(I,J) / JAC
 510  CONTINUE
C
      DO 520 I = 1, 6
         DO 530 J = 1, 3
            T1(I,J) = 0.D0
            DO 540 K = 1, 6
               T1(I,J) = T1(I,J) + T2(I,K) * CJ(K,J)
 540        CONTINUE
            T1(I,J) = - T1(I,J)
 530     CONTINUE
 520  CONTINUE
C
C     --- CALCUL DES DERIVEES EN ESPACE DES FONCTIONS DE FORME
C         DES VARIABLES
C
      DO 600 I = 1, NNF
C
         DSDXXF(I) =
     +    T1(1,1)*DPDEF(I)+T1(1,2)*DPDNF(I)+T1(1,3)*DPDKF(I)
     +  + T2(1,1)*DSDEEF(I)+T2(1,2)*DSDNNF(I)+T2(1,3)*DSDKKF(I)
     +  + T2(1,4)*DSDENF(I)+T2(1,5)*DSDNKF(I)+T2(1,6)*DSDEKF(I)
C
         DSDYYF(I) =
     +    T1(2,1)*DPDEF(I)+T1(2,2)*DPDNF(I)+T1(2,3)*DPDKF(I)
     +  + T2(2,1)*DSDEEF(I)+T2(2,2)*DSDNNF(I)+T2(2,3)*DSDKKF(I)
     +  + T2(2,4)*DSDENF(I)+T2(2,5)*DSDNKF(I)+T2(2,6)*DSDEKF(I)
C
         DSDZZF(I) =
     +    T1(3,1)*DPDEF(I)+T1(3,2)*DPDNF(I)+T1(3,3)*DPDKF(I)
     +  + T2(3,1)*DSDEEF(I)+T2(3,2)*DSDNNF(I)+T2(3,3)*DSDKKF(I)
     +  + T2(3,4)*DSDENF(I)+T2(3,5)*DSDNKF(I)+T2(3,6)*DSDEKF(I)
C
         DSDXYF(I) =
     +    T1(4,1)*DPDEF(I)+T1(4,2)*DPDNF(I)+T1(4,3)*DPDKF(I)
     +  + T2(4,1)*DSDEEF(I)+T2(4,2)*DSDNNF(I)+T2(4,3)*DSDKKF(I)
     +  + T2(4,4)*DSDENF(I)+T2(4,5)*DSDNKF(I)+T2(4,6)*DSDEKF(I)
C
         DSDYZF(I) =
     +    T1(5,1)*DPDEF(I)+T1(5,2)*DPDNF(I)+T1(5,3)*DPDKF(I)
     +  + T2(5,1)*DSDEEF(I)+T2(5,2)*DSDNNF(I)+T2(5,3)*DSDKKF(I)
     +  + T2(5,4)*DSDENF(I)+T2(5,5)*DSDNKF(I)+T2(5,6)*DSDEKF(I)
C
         DSDXZF(I) =
     +    T1(6,1)*DPDEF(I)+T1(6,2)*DPDNF(I)+T1(6,3)*DPDKF(I)
     +  + T2(6,1)*DSDEEF(I)+T2(6,2)*DSDNNF(I)+T2(6,3)*DSDKKF(I)
     +  + T2(6,4)*DSDENF(I)+T2(6,5)*DSDNKF(I)+T2(6,6)*DSDEKF(I)
C
600   CONTINUE
C
      JAC = ABS(JAC) * POIDS
C
      END
